/*  Fast word counter function using the trie-based code in counterlib.c 
 *
 *  Limitation: works only for languages where [a-z] is sufficient. */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>    // For using stat
#include <sys/stat.h>  // For using stat
#include <sys/types.h> // For using stat
#include "counterlib.h"   // Separate header file to allow for eventual Haskell integration

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

