/*  Fast word counter function 
 *
 *  Implemented as a custom-built fast trie, which uses fixed-size
 *  pointer arrays for child nodes, indexed by the lower-case 
 *  ASCII characters [a-z].
 *  
 *  Limitation: works only for languages where [a-z] is sufficient.
 *
 *  Plan to access functionality also from Haskell. 
 *  For that, will move actual counting functionality to separate library file. */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>    // For using stat
#include <sys/stat.h>  // For using stat
#include <sys/types.h> // For using stat
#include "counter.h"   // Separate header file to allow for eventual Haskell integration

#define MAXTRIESIZE 100000    // Max number of nodes; 100 000 should suffice 
#define MAXUNIQUEWORDS 50000  // Max number of unique words to list; 50 000 should suffice

/* Declaration of the node struct for the trie */
struct Node
{
    int value;
    struct Node *child[26]; // One slot for each letter
};
typedef struct Node Node;

/* Global variables and constants - see if can make these into static instead */
Node *nextnode;  // Next free node address
Node *maxnode;   // Last possible node address

/* Function prototypes */
Node* newnode();           // Initialise new trie node
void fill(Node*,char*);    // Build trie from string
int dump(Node*, Word**);   // Dumps trie to an array of Word structs

/* Compare words based on counts - sort descending */
static inline int compare(const void *first, const void *second)
{
    return( ((*(Word**)second)->count) - ((*(Word**)first)->count ));
}

/* Main function handling UI */
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
        printf("Problem with the source file.");
        return(-1);
    }
    size_t fileSize = fileStat.st_size; // Size in bytes
    
    /* Allocate memory for source data string */    
    char *text = calloc(fileSize + 1, sizeof(char));
    if (text == NULL)
    {
        printf("Memory allocation issue. Exiting.");
        return(-1);
    }
    
    /* Read source data string into memory just allocated */
    FILE *file = fopen(filePath,"r");
    if (fread(text, sizeof(char), fileSize, file) != fileSize)
    {
        printf("Error reading the file.");
        return(-1);
    }
    fclose(file);
    text[fileSize] = '\0'; // Ensure null-termination   

    /* Allocate space for words and set array of pointers to the words */
    Word *wordHeap = calloc(MAXUNIQUEWORDS, sizeof(Word));
    Word *words[MAXUNIQUEWORDS];
   
    /* Get counts of all words in the text; wc = number of unique words */
    int wc = counts(text, words, wordHeap);
    
    /* Sort the words by frequency */
    qsort(words, wc, sizeof(Word*), compare);
    
    /* Print the results */
    if (wc<lines) lines = wc; // Ensure that print only existing lines    
    for (int i = 0; i < lines; i++)
        printf("%s : %i \n", words[i]->word, words[i]->count);    
        
    free(text);
    free(wordHeap);    
    return 0;
}

/* The counting function to be also exported to Haskell.
 * Returns the number of unique words. 
 * Parameters are:
 * text:     input string; 
 * words:    array of pointers to Word structures holding the unique words and their counts;
 * wordheap: memory allocated for the Word sructures
 * Caller is responsible for allocating memory for words and wordheap */
int counts(char *text, Word *words[], Word *wordheap)
{
    words[0] = wordheap; // Synchronise the array and the Word heap
    
    Node *nodeheap = calloc(MAXTRIESIZE, sizeof(Node));   
    maxnode = nodeheap + MAXTRIESIZE * sizeof(Node); 
    nextnode = nodeheap;                             
    Node *lexicon  = newnode();                            
    
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
    nextnode++;
    return node;
}

/*  Test if char belongs to [a-zA-z] */
static inline bool letter(char c)
{
    if ((c>64 && c<91) || (c>96 && c <123)) return(true); 
    else return(false);
} 

/*  Make letters into index 0..25. 
*   ONLY WORKS FOR [a-zA-Z]         */
static inline char toInd(char c)
{
    if (c<91) return(c-65);
    else return(c-97);
}

/* Build the trie by adding words from the string. 
   The actual count comes at the last letter.*/
void fill(Node *trie, char *string)
{
    int i = 0;          // index into the string
    Node *node = trie;  // initialise at root node
    
    while (true) 
    {
        while (! letter(string[i])) 
        {
            if (string[i] == 0) return;  // Exit function on terminal null character
            i++;                         // Skip other non-letters
        }
        
        while (letter(string[i]))        // then process while have letters
        {
            char c = toInd(string[i]);   // upper and lower case to index where A=a=0, Z=z=25
            if (node -> child[c] == NULL) 
                node -> child[c] = newnode();
            node = node -> child[c];
            i++;    
        }
        (node -> value)++; 
        node = trie;
    }
}

/* Dump the trie to a list of Word structures with counts. */
int dump(Node *trie, Word *words[])
{
    static int ind = 0;             

    int val = trie -> value;
    if (val)                         // If we are at word end...
    {
        words[ind] = words[0] + ind; // Reserve new Word; to check that not ind*sizeof(Word)
        words[ind] -> count = val;   // ...save the value to counts...
        ind++;                       // ...and increase the index to counts  
    }

    int appendfrom = ind;            // using "windows" for appending the first letter
    for (int i = 0; i < 26; i++)     // "for each letter a-z"
    {
        Node *n = trie->child[i];
        if (n != NULL) 
        {
            dump(n,words);           // add tails after node n to wordlist
            
            char c = 97+i;           // The letter corresponding to child node n

            for (int j = appendfrom; j < ind; j++)
            {
                // append c to words[j] ==> CHECK POINTERS
                char buffer[20] = "";    
                strcpy(buffer, words[j] -> word);
                words[j] -> word[0] = c; 
                words[j] -> word[1] = 0;
                strcat(words[j] -> word, buffer);
            }
            appendfrom = ind; 
        }
    }
    return(ind);               // Number of unique words
}