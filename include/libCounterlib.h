#ifndef HWC_COUNTERLIB_H
#define HWC_COUNTERLIB_H

#include <stddef.h>

/* Fixed capacities. Tests may override these when compiling the C code;
 * production callers must use matching limits and the same Word layout. */
#ifndef MAXWORDLENGTH
#define MAXWORDLENGTH 20 /* Bytes per word, including its terminating NUL. */
#endif
#ifndef MAXTRIESIZE
#define MAXTRIESIZE 100000 /* Total arena nodes, including the root. */
#endif
#ifndef MAXUNIQUEWORDS
#define MAXUNIQUEWORDS 50000 /* Entries required in each caller-owned array. */
#endif

/* Result record shared with the Haskell Storable instance. With the production
 * limits and a 4-byte int, count is at byte 20 and the record occupies 24 bytes.
 * Changes to this layout must also be reflected in the Haskell wrappers. */
struct Word {
    char word[MAXWORDLENGTH];
    int count;
};
typedef struct Word Word;

/* Negative return values are errors, never word counts or array lengths. */
enum CounterError {
    COUNTER_NO_MEMORY = -1,
    COUNTER_ARENA_FULL = -2,
    COUNTER_WORD_TOO_LONG = -3,
    COUNTER_TOO_MANY_WORDS = -4,
    COUNTER_COUNT_OVERFLOW = -5,
    COUNTER_INVALID_ARGUMENT = -6
};

/* Each call owns its trie state. Concurrent calls require separate output
 * buffers. The caller must provide MAXUNIQUEWORDS entries in both arrays.
 * Returns the number of words, or a negative CounterError. On error, discard
 * all output. Words contain at most MAXWORDLENGTH - 1 ASCII letters.
 * counts expects a NUL-terminated string; counts_n accepts arbitrary bytes,
 * including embedded NULs, which are treated as word separators.
 * Neither function modifies the input or frees the caller's buffers.
 * counts_n permits NULL input only when length is zero; output buffers must
 * always be non-NULL. Successful output is ordered alphabetically. */
int counts(const char *text, Word **words, Word *wordheap);
int counts_n(const char *text, size_t length, Word **words, Word *wordheap);
/* Returns a borrowed, immutable description; no allocation or free is needed. */
const char *counts_error(int code);

#endif
