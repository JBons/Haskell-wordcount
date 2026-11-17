/* Focused tests with small capacities. Include the implementation to inject
 * allocation failure and exercise otherwise impractical INT_MAX boundaries. */
#include <assert.h>
#include <limits.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool fail_allocation;
static void *test_calloc(size_t n, size_t size) {
  return fail_allocation ? NULL : calloc(n, size);
}
#define calloc test_calloc
#include "../cbits/libCounterlib.c"
#undef calloc

static void check_words(void) {
  Word heap[MAXUNIQUEWORDS];
  Word *words[MAXUNIQUEWORDS];
  /* Exercise repeated calls with dirty buffers, including an empty call. */
  for (int repeat = 0; repeat < 4; ++repeat) {
    memset(heap, 0xa5, sizeof heap);
    memset(words, 0xa5, sizeof words);
    assert(counts("Ant a AN ant", words, heap) == 3);
    assert(strcmp(words[0]->word, "a") == 0 && words[0]->count == 1);
    assert(strcmp(words[1]->word, "an") == 0 && words[1]->count == 1);
    assert(strcmp(words[2]->word, "ant") == 0 && words[2]->count == 2);
    assert(counts("", words, heap) == 0);
    assert(counts("z Z", words, heap) == 1);
    assert(strcmp(words[0]->word, "z") == 0 && words[0]->count == 2);
  }
}

static void *thread_main(void *unused) {
  (void)unused;
  check_words();
  return NULL;
}

/* Returning from a long branch must not leave suffix bytes in a shorter
 * sibling. Also check words that are prefixes of others and alphabetical order. */
static void check_reconstruction(void) {
  Word heap[MAXUNIQUEWORDS];
  Word *words[MAXUNIQUEWORDS];
  const char *expected[] = {"a", "abcdefghijklmnopqrs", "ant", "ants",
                            "b", "bat", "be", "z"};
  memset(heap, 0xa5, sizeof heap);
  assert(counts("z be bat b ants ant abcdefghijklmnopqrs a ANT", words, heap) == 8);
  for (int i = 0; i < 8; ++i) {
    assert(words[i] == &heap[i]);
    assert(strcmp(words[i]->word, expected[i]) == 0);
    assert(words[i]->count == (i == 2 ? 2 : 1));
  }
}

int main(void) {
  Word heap[MAXUNIQUEWORDS];
  Word *words[MAXUNIQUEWORDS];
  check_words();
  check_reconstruction();

  const char bytes[] = {'A', '\0', 'b', (char)0xff, 'A'};
  assert(counts_n(bytes, sizeof bytes, words, heap) == 2);
  assert(strcmp(words[0]->word, "a") == 0 && words[0]->count == 2);
  assert(strcmp(words[1]->word, "b") == 0 && words[1]->count == 1);
  assert(counts_n(NULL, 0, words, heap) == 0);
  assert(counts_n(NULL, 1, words, heap) == COUNTER_INVALID_ARGUMENT);
  assert(counts(NULL, words, heap) == COUNTER_INVALID_ARGUMENT);
  assert(counts("a", NULL, heap) == COUNTER_INVALID_ARGUMENT);
  assert(counts("a", words, NULL) == COUNTER_INVALID_ARGUMENT);

  char longword[MAXWORDLENGTH + 1];
  memset(longword, 'a', sizeof longword);
  longword[MAXWORDLENGTH - 1] = '\0';
  assert(counts(longword, words, heap) == 1);
  assert(strlen(words[0]->word) == MAXWORDLENGTH - 1);
  longword[MAXWORDLENGTH - 1] = 'a';
  longword[MAXWORDLENGTH] = '\0';
  assert(counts(longword, words, heap) == COUNTER_WORD_TOO_LONG);
  check_words(); /* A failed call must not affect later calls. */

  /* Eight distinct one-letter words fit the test output capacity; nine do not. */
  assert(MAXUNIQUEWORDS == 8);
  assert(counts("a b c d e f g h", words, heap) == MAXUNIQUEWORDS);
  assert(counts("a b c d e f g h i", words, heap) == COUNTER_TOO_MANY_WORDS);

  /* Exhaust the 128-node test arena using seven disjoint 19-letter paths. */
  assert(MAXTRIESIZE == 128 && MAXWORDLENGTH == 20);
  char arena_input[7 * MAXWORDLENGTH + 1];
  for (int i = 0; i < 7; ++i) {
    memset(arena_input + i * MAXWORDLENGTH, 'a' + i, MAXWORDLENGTH - 1);
    arena_input[i * MAXWORDLENGTH + MAXWORDLENGTH - 1] = ' ';
  }
  arena_input[sizeof arena_input - 1] = '\0';
  assert(counts(arena_input, words, heap) == COUNTER_ARENA_FULL);

  /* Verify exactly the last node can be allocated, but not one past it. */
  Node nodes[2] = {0};
  Arena arena = {nodes, nodes + 2};
  assert(newnode(&arena) == &nodes[0]);
  assert(newnode(&arena) == &nodes[1]);
  assert(newnode(&arena) == NULL);
  nodes[0].child[0] = &nodes[1];
  nodes[1].value = INT_MAX;
  assert(fill(&arena, &nodes[0], "a", 1) == COUNTER_COUNT_OVERFLOW);
  assert(nodes[1].value == INT_MAX);

  fail_allocation = true;
  assert(counts("a", words, heap) == COUNTER_NO_MEMORY);
  fail_allocation = false;
  check_words();

  pthread_t threads[4];
  for (int i = 0; i < 4; ++i)
    assert(pthread_create(&threads[i], NULL, thread_main, NULL) == 0);
  for (int i = 0; i < 4; ++i)
    assert(pthread_join(threads[i], NULL) == 0);
  puts("counterlib: all tests passed");
  return 0;
}
