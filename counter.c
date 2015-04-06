/*  Fast word counter function to be called from Haskell code
 *
 *  Implemented as a custom-built fast trie, which uses fixed-size
 *  pointer arrays for child nodes, indexed by the lower-case 
 *  ASCII characters [a-z].
 *  
 *  Ensuing limitation: works only for languages where [a-z] is
 *  sufficient.
 *
 *  Haskell interface implemented as a function with arguments for
 *  size in bytes of the output buffers,
 *  pointers to the input string, and two output arrays, one for 
 *  the words and one for the counts.
 *  Return the length of the output arrays. */

/* Implementation of the counter function */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "counter.h"

/* Declaration of the node struct for the trie */
struct Node
{
    int value;
    struct Node *child[26];
};
typedef struct Node Node;

/* Global variables and constants */

Node *nodeheap;
Node *nextnode;
Node *maxnode;
Node *lexicon;
const int MAXTRIESIZE = 100000;

/* Function prototypes */
Node* newnode();
void fill(Node*,char*);
int dump(Node*, char**, int*);

/* Implementation */

int counts(char *text, int space, char **words, int *wordcounts)
{
    nodeheap = calloc(MAXTRIESIZE, sizeof(Node));
    maxnode = nodeheap + MAXTRIESIZE * sizeof(Node);
    nextnode = nodeheap;
    lexicon  = newnode();
    
    fill(lexicon, text);

    int size = dump(lexicon, words, wordcounts);
    
    return size;    
}

/* Update pos of next and return pointer */
Node* newnode()
{
    if (nextnode > maxnode) abort();
    Node *node = nextnode;
    nextnode = nextnode + sizeof(Node);
    return node;
}

void fill(Node *trie, char *string)
{
    trie->value=1; /* DUMMY CODE TO CHANGE */
}


/* Dumps the trie (pointer to root) to two arrays:
 * first of the words (strings) stored in the trie,
 * second of the corresponding word counts. */
int dump(Node *trie, char **words, int *counts)
{
    int i = 0;

    /* CODE TO ADD */

    i++;
    return i;
}

