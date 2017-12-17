# Word frequency counts
Overview
---------

This toy project tests a few alternative implementations of a programme that counts occurences of words in a text file. Various Haskell versions are tested against the classic simple UNIX shell script and a trie-based version written in C.

Testing with the full text of Tolstoy's War and Peace on late 2014 Macbook Pro 15". The benchmark shell script (pipeline of tr, sort, uniq and sed) executes in 0.7 sec, while a quick-and-dirty trie data structure coded in C completes in 0.04 sec.

The various tested Haskell approaches perform as follows (GHC 8.2.2 and Stack nightly-2017-12-16) - run hwc to test the various methods:

* The same algorithm as the UNIX script using standard list functions: 2.0 sec
* Ternary trie from Data.Edison.Assoc.TernaryTrie: 1.5 sec
* Multiway trie in the Trie library in this project 0.8 sec
* Very simple bag data structure using Data.Map.Strict (balanced trees): 0.5 sec
* Bag built using Data.Hashtable: 0.5 sec
* Bag built using custom hash map from MBag module in this project: 0.4 sec

All the results above are based on updated version (3.6.1) where String type has been replaced with Data.Text, gaining substantial increase in speed. However, there is still overhead from supporting Unicode text, and a fairer comparison to the specific non-Haskell benchmarks (both of which only deal with words in English alphabet a-zA-z) would be to use the input as 8-bit ASCII. This is done in the hawc executable, which reads and pre-processes input as Data.ByteString. This provides faster execution as expected (except for the simple algorithm):

* Simple list-function algorithm: 2.2 sec
* Edison ternary trie: 1.2 sec
* Multiway trie: 0.5 sec
* Data.Map.Strict: 0.5 sec
* Hashtable: 0.4 sec
* Hybrid hashmap based bag from MBag module: 0.13 sec
* Calling C-code from Haskell: 0.06 sec

Thus two purely functional algorithms (trie, balanced trees) and two mutable ones (Data.Hashtable, MBag) beat the chained shell commands, and the latter comes fairly close to calling C-code from Haskell.

Installation
------------
Builds using [Stack](http://docs.haskellstack.org). With Stack installed, run `stack build` in the project root.

Usage
-----
Two executables, hwc and hawc, implement Unicode and ASCII-only versions respectively. To run with stack, say `stack exec h[a]wc lines methods filename`, where lines is the number of top words and their counts to display, methods is a string encoding methods to test (each by one letter, repeated letters will repeat runs, coding available by executing without parameters) and filename is path to the text file. E.g, `stack exec hwc 5 ts wp.txt` to run hwc with 5 lines of output using both Trie and Data.Map.Strict methods on wp.txt.
