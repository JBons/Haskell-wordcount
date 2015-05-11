# Word frequency counts

This toy project tests a few alternative implementations of a programme that counts occurences of words in a text file. Various Haskell versions are tested against the classic simple UNIX shell script and a trie-based version written in C.

Testing with the full text of Tolstoy's War and Peace on late 2014 Macbook Pro 15" gives the following results:
* A simple shell script chaining UNIX utilities (tr, sort, uniq, sed): 0.7 sec
* Haskell implementing the same algorithm as the UNIX script: 3.7 sec
* Haskell using immutable trie to count word occurences (wc.hs in the main folder): 3.4 sec
* Haskell using mutable version of trie: 2.3 sec
* Haskell using (mutable) hash table to count words: 1.9 sec
* C programme implementing a light-weight trie to count words: 0.04 sec (!!)
  * The C code implements essentially the same trie as the mutable trie Haskell code
* Haskell programme calling the trie algorithm written in C: 0.28 sec

The more than 50-fold difference speed difference between the trie algorithm in C and Haskell is unexpectedly big. Need to investigate ways to get order-of-magnitude improvement to the Haskell code (if possible).

