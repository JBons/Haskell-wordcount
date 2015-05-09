/* Headers for the counting function */

#define MAXWORDLENGTH 20
#define MAXTRIESIZE 100000    // Max number of nodes; 100 000 should suffice 
#define MAXUNIQUEWORDS 50000  // Max number of unique words to list; 50 000 should suffice

/* Declaration of word count structure */
struct Word
{
    char word[MAXWORDLENGTH];
    int count;
};
typedef struct Word Word;

/* The main counting function, the only one exported */
int counts(char*, Word**, Word*); 
