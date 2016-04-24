# Word frequency counts

This toy project tests a few alternative implementations of a programme that counts occurences of words in a text file. Various Haskell versions are tested against the classic simple UNIX shell script and a trie-based version written in C.

Testing with the full text of Tolstoy's War and Peace on late 2014 Macbook Pro 15" gives the following results (updated for GHC 7.10.3 and packages in Stack lts-5.13):
* A simple shell script chaining UNIX utilities (tr, sort, uniq, sed): 0.7 sec
* Haskell implementing the same algorithm as the UNIX script: 3.4 sec
* Haskell using immutable trie to count words : 1.6 sec (with strictness annotations and specialisation pragmas on two key functions; 2.7 sec without these optimisations.)
* Haskell using mutable version of trie: 2.3 sec
* Haskell using Data.Edison.Assoc.TernaryTrie: 2.3 sec
* Haskell using (mutable) hash table to count words: 1.6 sec
* C programme implementing a light-weight trie to count words: 0.04 sec (!!)
  * The C code implements essentially the same trie as the mutable trie Haskell code
* Haskell programme calling the trie algorithm written in C: 0.5 sec

The 40-fold difference speed difference between the trie algorithm in C and Haskell is unexpectedly big. Need to investigate ways to get order-of-magnitude improvement to the Haskell code (if possible).

Interesting changes in performance relative to GHC 7.8.4 (however possibly partly due to some minor changes in set-up):
* Simple algorithm almost 10% faster
* Immutable trie implementation about 20% faster
* Hash-table implementation about 15% faster
* The C library 0.2 sec slower (0.48 vs 0.28). Is there something slowing down  FFI? Absolute difference is not big, but relative is.
