/*  Fast word counter library to be accessed from Haskell
 *
 *  Implemented as a custom-built fast trie, which uses fixed-size
 *  pointer arrays for child nodes, indexed by the lower-case
 *  ASCII characters [a-z].
 *  Limitation: works only for languages where [a-z] is sufficient.
 */

#include "libCounterlib.h" // Separate header file to allow for eventual Haskell integration
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* Declaration of the node struct for the trie */
struct Node {
  int value;
  struct Node *child[26]; // One slot for each letter
};
typedef struct Node Node;

/* Global variables and constants - see if can make these into static instead */
Node *nextnode; // Next free node address
Node *maxnode;  // Last possible node address

/* Function prototypes */
Node *newnode();           // Initialise new trie node
void fill(Node *, char *); // Build trie from string
int dump(Node *, Word **); // Dumps trie to an array of Word structs

/* The counting function to be used from Haskell.
 * Returns the number of unique words.
 * Parameters are:
 * text:     input string (null-terminated);
 * words:    array of pointers to Word structures holding the unique words and
 * their counts;
 * wordheap: memory allocated for the Word sructures
 * Caller is responsible for allocating memory for words and wordheap */
int counts(char *text, Word *words[], Word *wordheap) {
  words[0] = wordheap; // Synchronise the array and the Word heap

  Node *nodeheap = calloc(MAXTRIESIZE, sizeof(Node));
  maxnode = nodeheap + MAXTRIESIZE * sizeof(Node);
  nextnode = nodeheap;
  Node *lexicon = newnode();

  fill(lexicon, text);

  int wc = dump(lexicon, words);

  free(nodeheap);

  return wc;
}

/* Update pos of next and return pointer */
Node *newnode() {
  if (nextnode > maxnode)
    abort();
  Node *node = nextnode;
  nextnode++;
  return node;
}

/*  Test if char belongs to [a-zA-z] */
static inline bool letter(char c) {
  if ((c > 64 && c < 91) || (c > 96 && c < 123))
    return (true);
  else
    return (false);
}

/*  Make letters into index 0..25.
*   ONLY WORKS FOR [a-zA-Z]         */
static inline char toInd(char c) {
  if (c < 91)
    return (c - 65);
  else
    return (c - 97);
}

/* Build the trie by adding words from the string.
   The actual count comes at the last letter.*/
void fill(Node *trie, char *string) {
  int i = 0;         // index into the string
  Node *node = trie; // initialise at root node

  while (true) {
    while (!letter(string[i])) {
      if (string[i] == 0)
        return; // Exit function on terminal null character
      i++;      // Skip other non-letters
    }

    while (letter(string[i])) // then process while have letters
    {
      char c =
          toInd(string[i]); // upper and lower case to index where A=a=0, Z=z=25
      if (node->child[c] == NULL)
        node->child[c] = newnode();
      node = node->child[c];
      i++;
    }
    (node->value)++;
    node = trie;
  }
}

/* Dump the trie to a list of Word structures with counts. */
int dump(Node *trie, Word *words[]) {
  static int ind = 0;

  int val = trie->value;
  if (val) // If we are at word end...
  {
    words[ind] =
        words[0] + ind; // Reserve new Word; to check that not ind*sizeof(Word)
    words[ind]->count = val; // ...save the value to counts...
    ind++;                   // ...and increase the index to counts
  }

  int appendfrom = ind;        // using "windows" for appending the first letter
  for (int i = 0; i < 26; i++) // "for each letter a-z"
  {
    Node *n = trie->child[i];
    if (n != NULL) {
      dump(n, words); // add tails after node n to wordlist

      char c = 97 + i; // The letter corresponding to child node n

      for (int j = appendfrom; j < ind; j++) {
        // append c to words[j] ==> CHECK POINTERS
        char buffer[20] = "";
        strcpy(buffer, words[j]->word);
        words[j]->word[0] = c;
        words[j]->word[1] = 0;
        strcat(words[j]->word, buffer);
      }
      appendfrom = ind;
    }
  }
  return (ind); // Number of unique words
}
