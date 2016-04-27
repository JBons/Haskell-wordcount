# Word frequency counts

This toy project tests a few alternative implementations of a programme that counts occurences of words in a text file. Various Haskell versions are tested against the classic simple UNIX shell script and a trie-based version written in C.

Testing with the full text of Tolstoy's War and Peace on late 2014 Macbook Pro 15". The benchmark shell script (pipeline of tr, sort, uniq and sed) executes in 0.7 sec while a quick-and-dirty trie data structure coded in C completes in 0.04 sec.

The various tested Haskell approaches perform as follows (GHC 7.10.3 and Stack lts-5.13) - run hwc to test the various methods:
* The same algorithm as the UNIX script using standard list functions: 3.4 sec
* Ternary trie from Data.Edison.Assoc.TernaryTrie: 2.5 sec
* Multiway trie in the Trie library in this project 1.6 sec
    * Mutable version of trie in the MTrie library performs much worse: 2.6 sec
* Very simple bag data structure using Data.Map.Strict (balanced trees): 1.5 sec
* Bag built using Data.Hashtable: 1.6 sec
* Calling the C code from Haskell: 0.6 sec

All the results above spend a lot of time in pre-processing the input as (potentially) Unicode string. A fairer comparison to the specific benchmarks (both of which only deal with words in English alphabet a-zA-z) would be to use the input as 8-bit ASCII. This is done in the hawc executable, which reads and pre-processes input as Data.ByteString. The improvement in performance in substantial - now:
* Simple list-function algorithm: 2.5 sec
* Edison ternary trie: 1.5 sec
* Non-mutable trie: 0.7 sec
* Mutable trie: 0.9 sec
* Data.Map.Strict: 0.6 sec
* Hashtable: 0.5 sec
* Calling C-code: 0.08 sec
* Hybrid hashmap based bag from MBag module: 0.16 sec

Thus two purely functional algorithms (trie, balanced trees) and two mutable ones (Data.Hashtable, MBag) beat the chained shell commands, and the latter comes fairly close to calling C-code from Haskell.
