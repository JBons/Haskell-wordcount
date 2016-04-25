-- Program comparing various word frequency counter implementations
-- Allows easy modular addition of new methods to compare and benchmark

module Main where

import           Control.Monad      (sequence, when)
import           Data.Char          (isDigit)
import           Data.Map           (fromList, lookup)
import           Data.Maybe         (mapMaybe)
import           GHC.Exts           (sortWith)
import           Prelude            hiding (fromList, lookup, words)
import           System.Environment (getArgs)
import           System.TimeIt      (timeIt)
import           Text.Printf        (printf)
import           WordCounters

-- User interface and main IO action
-- Command line arguments: #top words to show, methods to test, filename
main = do args <- getArgs
          when (invalid args) (fail errMsg)
          let n = read (args !! 0)
          let selector = args !! 1
          let methods = decode selector
          let filename  = args!! 2
          text <- readFile filename
          sequence $ fmap (timeIt . run n text) methods


-- Take first command line parameter and map to methdods to run and
-- descriptive strings to use for each in output
decode :: [Char] -> [(String -> [(String, Int)], String)]
decode = mapMaybe (flip lookup methods) where
    methods = fromList
        [ ('l', (simpleCounts, "Using a simple combination of list methods"))
        , ('t', (trieCounts,   "Counting with non-mutable trie"))
        , ('m', (mTrieCounts,  "Counting with mutable trie"))
        , ('h', (htCounts,     "Counting with hash table"))
        , ('c', (cTrieCounts,  "Counting with fast c-code trie"))
        , ('e', (eCounts,      "Using Data.Edison.Assoc.TernaryTrie"))
        , ('s', (sCounts,      "Counting with Data.Map.Strict")) ]


-- Runs word counting on text using the selected method, printing
-- n highest-frequency words in the string
run :: Int -> String -> (String -> [(String, Int)], String) -> IO ()
run n txt method =  do let results = (fst method) txt
                       putStrLn "\n"
                       putStrLn (snd method)
                       display $ n `mostFrequent` results
                       let wcount = length (words txt)
                       let uwords = length results
                       putStrLn $ "Total words: " ++ show wcount
                       putStrLn $ "Different words: " ++ show uwords
                       putStrLn "----------------------------- \n"


-- From list of pairs take top n sorted based on second members of the pairs
mostFrequent :: Ord b => Int -> [(a, b)] -> [(a, b)]
mostFrequent n = take n . reverse . sortWith snd

--Pretty-print results
display = mapM (putStrLn . disp) where
    disp (x,y) = printf "%15s :   %4d" x y

-- Argument errors
invalid args = length args < 3 || not (all isDigit (head args))
errMsg = "Usage: hwc lines methods filename,\nwhere lines is the number most frequent words and their frequencies to show, methods is a sequence of letters naming the methods to test, and filename is the name of the file containing the text to analyse.\nAvailable methods are: \ns = simple,\nt = trie-based,\nm = mutable trie,\nh = hash table and\nc = trie written in C,\ne = Data.Edison.Assoc.TernaryTrie"
