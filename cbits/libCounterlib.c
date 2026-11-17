/* Fast word counter library to be accessed from Haskell.
 *
 * The trie uses fixed-size arrays of child pointers, one slot for each
 * lower-case ASCII letter [a-z]. Upper-case letters use the same slots;
 * all other bytes separate words. This is not a Unicode word counter.
 *
 * Nodes are allocated together in an arena owned by each counting call.
 * Traversal accumulates the path in a shared prefix buffer, copying each
 * completed word once into the caller-owned output buffers.
 */
#include "../include/libCounterlib.h"
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* A positive value marks the end of a word and stores its frequency.
 * A terminal node may also have children (for example, "a" and "ant"). */
typedef struct Node {
  int value;
  struct Node *child[26]; /* One slot for each letter; NULL means no child. */
} Node;

/* Allocation state is local to a call, so separate calls cannot overwrite
 * each other's next-free-node pointer. calloc initializes all nodes to zero. */
typedef struct Arena {
  Node *next; /* Next free node, including the root on the first allocation. */
  Node *end; /* One past the last node. */
} Arena;

/* The pointer array refers into the contiguous Word heap. Keeping the heap
 * base separately avoids using words[0] as both storage and an output entry.
 * used replaces the former static traversal index and starts at zero per call. */
typedef struct Output {
  Word **words;
  Word *heap;
  size_t used;
} Output;

/* Reserve one zero-initialized node. The end pointer is never dereferenced;
 * return NULL when full so the caller can report an error and free the arena. */
static Node *newnode(Arena *arena) {
  if (arena->next >= arena->end)
    return NULL;
  return arena->next++;
}

/* Test for [a-zA-Z], independently of the current locale. */
static bool letter(unsigned char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

/* Map a letter to 0..25, treating A=a, B=b, etc. Requires letter(c) == true. */
static unsigned int toInd(unsigned char c) {
  return c >= 'a' ? c - 'a' : c - 'A';
}

/* Add each word to the trie, incrementing the node at its final letter.
 * Explicit byte bounds allow embedded NULs and avoid reading beyond the input.
 * Returns zero on success or a negative CounterError; a partial trie is never
 * dumped after an error. */
static int fill(Arena *arena, Node *trie, const char *text, size_t length) {
  size_t i = 0;
  while (i < length) {
    /* Skip separators, including NUL bytes in the length-delimited input. */
    while (i < length && !letter((unsigned char)text[i]))
      ++i;
    if (i == length)
      break;

    Node *node = trie;
    size_t wordlength = 0;
    while (i < length && letter((unsigned char)text[i])) {
      /* Reserve one byte in each result for its terminating NUL. Checking here
       * also bounds recursion depth during reconstruction. */
      if (++wordlength >= MAXWORDLENGTH)
        return COUNTER_WORD_TOO_LONG;
      unsigned int c = toInd((unsigned char)text[i]);
      if (node->child[c] == NULL) {
        Node *child = newnode(arena);
        if (child == NULL)
          return COUNTER_ARENA_FULL;
        node->child[c] = child;
      }
      node = node->child[c];
      ++i;
    }
    /* Do not let a very frequent word overflow the signed count field. */
    if (node->value == INT_MAX)
      return COUNTER_COUNT_OVERFLOW;
    ++node->value;
  }
  return 0;
}

/* Reconstruct words top-down, visiting terminal nodes before their children
 * and child slots in a-z order. The resulting pointer array is alphabetical.
 * prefix[0..depth) is the path to this node; bytes after it may be stale.
 * Each descent writes one letter, and each terminal copies its complete word
 * once, avoiding repeated suffix copies and scans while unwinding recursion.
 * fill has checked that every path fits in MAXWORDLENGTH - 1 bytes. */
static int dump(const Node *trie, Output *output, char *prefix, size_t depth) {
  if (trie->value) {
    if (output->used >= MAXUNIQUEWORDS)
      return COUNTER_TOO_MANY_WORDS;
    /* Pointer arithmetic advances by Word structures, not bytes. */
    Word *word = &output->heap[output->used];
    memcpy(word->word, prefix, depth);
    word->word[depth] = '\0'; /* Do not copy stale suffix bytes. */
    word->count = trie->value;
    output->words[output->used++] = word;
  }

  for (int i = 0; i < 26; ++i) {
    const Node *child = trie->child[i];
    if (child != NULL) {
      /* Siblings reuse the same buffer. The caller's depth restores the
       * logical prefix length on return; no copying or clearing is needed. */
      prefix[depth] = (char)('a' + i);
      int status = dump(child, output, prefix, depth + 1);
      if (status < 0)
        return status;
    }
  }
  return 0;
}

/* Length-delimited entry point used by the Haskell ByteString wrapper.
 * text: input bytes, borrowed for this call only;
 * words: caller-owned array of pointers to emitted Word structures;
 * wordheap: caller-owned storage for those structures.
 * Both output arrays must have MAXUNIQUEWORDS entries (see the public header).
 * This function allocates and frees only the internal trie arena. */
int counts_n(const char *text, size_t length, Word **words, Word *wordheap) {
  if ((text == NULL && length != 0) || words == NULL || wordheap == NULL)
    return COUNTER_INVALID_ARGUMENT;

  Node *nodeheap = calloc(MAXTRIESIZE, sizeof(*nodeheap));
  if (nodeheap == NULL)
    return COUNTER_NO_MEMORY;
  /* All mutable traversal/allocation state belongs to this invocation. */
  Arena arena = {nodeheap, nodeheap + MAXTRIESIZE};
  Output output = {words, wordheap, 0};
  Node *trie = newnode(&arena);
  int status = trie == NULL ? COUNTER_ARENA_FULL
                           : fill(&arena, trie, text, length);
  if (status == 0) {
    /* One scratch buffer per call, shared by all recursive traversal frames. */
    char prefix[MAXWORDLENGTH];
    status = dump(trie, &output, prefix, 0);
  }
  /* Free the arena on both success and failure. Output on failure is partial
   * and must be discarded; the caller still owns and frees its two buffers. */
  free(nodeheap);
  return status < 0 ? status : (int)output.used;
}

/* Compatibility entry point for callers using NUL-terminated C strings. */
int counts(const char *text, Word **words, Word *wordheap) {
  if (text == NULL)
    return COUNTER_INVALID_ARGUMENT;
  return counts_n(text, strlen(text), words, wordheap);
}

/* Error descriptions are immutable string literals; callers must not free them. */
const char *counts_error(int code) {
  switch (code) {
  case COUNTER_NO_MEMORY: return "cannot allocate trie arena";
  case COUNTER_ARENA_FULL: return "trie arena capacity exceeded";
  case COUNTER_WORD_TOO_LONG: return "word exceeds maximum length";
  case COUNTER_TOO_MANY_WORDS: return "unique-word capacity exceeded";
  case COUNTER_COUNT_OVERFLOW: return "word count exceeds INT_MAX";
  case COUNTER_INVALID_ARGUMENT: return "invalid counter argument";
  default: return "unknown counter error";
  }
}
