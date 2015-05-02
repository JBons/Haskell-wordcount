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
 *  Return the length of the output arrays.
 *
 *  Rethinking the Haskell interface:
 *  
 *  will use Foreign.Marshal.Array, and
 *  do 3 mallocArray calls: 
 *      1. for the array of counts
 *      2. for the array of words (pointers to strings)
 *      3. for the strings themselves
 *  The resulting pointers will be passed to the C code, so that
 *  the words[0] = &strings.
 *  Then on return, use peekArray to words to convert to a list of
 *  pointers to strings; then use peekArray0 to convert the arrays pointed at by 
 *  the elements of words into lists of chars (=Haskell strings).
 *
 *  NEED TO REFACTOR CODE AGAIN TO GET THIS DONE.
 *
 *  */

/* Implementation of the counter function */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>    // For using stat
#include <sys/stat.h>  // For using stat
#include <sys/types.h> // For using stat
#include "counter.h"


#define MAXTRIESIZE 100000 
#define MAXUNIQUEWORDS 50000
#define A 141 
#define DELIM ",.-;:!?"

/* Declaration of the node struct for the trie */
struct Node
{
    int value;
    struct Node *child[26]; // One slot for each letter
};
typedef struct Node Node;



/* Global variables and constants  */
Node *nodeheap;  // Start of calloc block
Node *nextnode;  // Next free node address
Node *maxnode;   // Last possible node address
Node *lexicon;   // Lexicon trie root node
int ind = 0;     // Counter for building the output arrays in function "dump"


/* Function prototypes */
Node* newnode();
void fill(Node*,char*);
int dump(Node*, Word**);

/* Compare words based on counts - sort descending */
// why cannot make inline???
int compare(const void *first, const void *second)
{
    return( ((Word*)second)->count - ((Word*)first)->count );
}

/* Implementation */

int main(int argc, char *argv[])
{
    /* Read and process command line arguments */
    if (argc != 3)
    {
        printf("Usage: cwc <lines> <filepath>, where lines is the number of top words to show");
        return(-1);
    }
    int lines = atoi(argv[1]);
    if (lines == 0)
    {
        printf("Invalid number of lines to show.");
        return(-1);
    }
    char *filePath = argv[2];
    
    /* Get file size */
    struct stat fileStat;
    if (stat(filePath, &fileStat) < 0)
    { 
        printf("Some problem with the source file.");
        return(-1);
    }
    size_t fileSize = fileStat.st_size; // Size in bytes
    
    /* Allocate memory and read in the file */    
    char *text = calloc(fileSize + 1, sizeof(char));
    if (text == NULL)
    {
        printf("Memory allocation issue. Exiting.");
        return(-1);
    }
    FILE *file = fopen(filePath,"r");
    if (fread(text, sizeof(char), fileSize, file) != fileSize)
    {
        printf("Error reading the file.");
        return(-1);
    }
    fclose(file);
    text[fileSize] = '\0'; // Ensure null-termination

    /* Allocate space for words and set array of pointer to the words */
    Word *wordHeap = calloc(MAXUNIQUEWORDS, sizeof(Word));
    Word *words[MAXUNIQUEWORDS];
    words[0] = wordHeap;
    
    /* Get counts of all words in the text; wc = number of different words */
    int wc = counts(text,words);
    
    /* Sort the words by frequency */
    qsort(words, wc, sizeof(Word*), compare);
    
    /* Print the results */
    for (int i = 0; i < lines; i++)
        printf("%s : %i \n", words[i]->word, words[i]->count);    
        
    free(text);
    free(wordHeap);    

    return 0;
}


/* The exported counting function.
 * returns the number of unique words. Parameters are
 * text: input string; words: string array for unique words;  
 * wordcounts: int array for number of occurences; and
 * stringHeap: pointer to memory block for storing the word strings
 * Caller is responsible for allocating memory for the output arrays. */
int counts(char *text, Word *words[])
{
    nodeheap = calloc(MAXTRIESIZE, sizeof(Node));    // calloc inits w/0
    maxnode = nodeheap + MAXTRIESIZE * sizeof(Node); 
    nextnode = nodeheap;                             
    lexicon  = newnode();                            
    
    fill(lexicon, text);

    int wc = dump(lexicon, words);
    
    free(nodeheap);
    
    return wc;    
}

/* Update pos of next and return pointer */
Node* newnode()
{
    if (nextnode > maxnode) abort();
    Node *node = nextnode;
    nextnode = nextnode + sizeof(Node);
    return node;
}

/* Build the trie by adding words from the string. 
   The actual count comes at the last letter.*/
void fill(Node *trie, char *string)
{
    for (char *word = strtok(string, DELIM); word != NULL; word = strtok(NULL, DELIM)) // REDO with custom code
    {
        Node *node = trie;
        int i = 0;
        while (word[i] != '\0')
        {
            int j = word[i] - A; // turn letter into index with a=0, z=25
            if (node->child[j] == NULL)
                node->child[j] = newnode();
            node = node->child[j];
            i++;
        }
        node->value = (node->value) + 1;
    }
}

// REFACTOR DUMP TO WORK WITH NEW EXPORTED SIGNATURE
/* Dump the trie (pointer to root) to two arrays:
 * first of the words (strings) stored in the trie,
 * second of the corresponding word counts.
 *
 * NOTE: building the words backwards!
 *
 * TO DO: make words, counts into global vars.
 * !!! NEED newWord() !!!
 */
int dump(Node *trie, Word *words[])
{
    static int ind = 0;            // 

    int val = trie -> value;
    if (val)                       // If we are at word end...
    {
        words[ind] -> count = val; // ...save the value to counts...
        ind++;                     // ...and increase the index to counts  
    }

    int appendfrom = 0;            // using "windows" for appending the first letter
    for (int i = 0; i < 26; i++)   // "for each letter a-z"
    {
        Node *n = trie->child[i];
        if (n != NULL) 
        {
            dump(n,words);         // add tails after node n to wordlist
            
            char c = A+i;          // The letter corresponding to child node n

            for (int j = appendfrom; j < ind; j++)
            {
                // append c to words[j] ==> CHECK POINTERS
                char buffer[20];    
                strcpy(buffer, words[j] -> word);
                words[j] -> word[0] = c; 
                words[j] -> word[1] = 0;
                strcat(words[j] -> word, buffer);
            }

            appendfrom = ind; // or ind +1?
        }
    }
    appendfrom = 0;
    
    return(ind + 1);               // Number of unique words
}

