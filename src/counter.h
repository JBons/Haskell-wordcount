/* Headers for the counting function */

#define MAXWORDLENGTH 20

/* Declaration of word count structure */
struct Word
{
    char word[MAXWORDLENGTH];
    int count;
};
typedef struct Word Word;

/* The main counting function, the only one exported */
int counts(char*, Word**, Word*);


/* Need to check compilation / linking options:
 *
 * gcc -fPIC -c filename.c
 *
 * may be right? */
