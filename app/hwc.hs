-- Program comparing various word frequency counter implementations
-- Allows easy modular addition of new methods to compare and benchmark

module Main where

import           Data.Map           (fromList, lookup)
import           Data.Maybe         (mapMaybe)
import           Data.Text          (Text,words)
import           Data.Text.IO       (readFile)
import           Data.List          (sortOn)
import           Prelude            hiding (lookup, words, readFile)
import           System.Environment (getArgs)
import           System.TimeIt      (timeIt)
import           Text.Printf        (printf)
import           Text.Read          (readMaybe)
import           WordCounters


-- User interface and main IO action
-- Command line arguments: #top words to show, methods to test, filename
main :: IO ()
main = do
    args <- getArgs
    case args of
        rawN : selector : filename : _
            | Just n <- readMaybe rawN :: Maybe Integer
            , n >= 0, n <= toInteger (maxBound :: Int) -> do
                text <- readFile filename
                mapM_ (timeIt . run (fromInteger n) text) (decode selector)
        _ -> fail errMsg


-- Decode the method selector into counters and
-- descriptive strings to use for each in output
decode :: [Char] -> [(Text -> [(Text, Int)], String)]
decode = mapMaybe (flip lookup methods) where
    methods = fromList
        [ ('l', (simpleCounts, "Using a simple combination of list methods"))
        , ('t', (trieCounts,   "Counting with non-mutable trie"))
        , ('h', (htCounts,     "Counting with hash table"))
        , ('e', (eCounts,      "Using Data.Edison.Assoc.TernaryTrie"))
        , ('s', (sCounts,      "Counting with Data.Map.Strict"))
        , ('b', (bCounts,      "Using MBags hash-map bag"))]


-- Runs word counting on text using the selected method, printing
-- n highest-frequency words in the string
run :: Int -> Text -> (Text -> [(Text, Int)], String) -> IO ()
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
mostFrequent n = take n . reverse . sortOn snd

--Pretty-print results
display :: [(Text, Int)] -> IO ()
display = mapM_ (putStrLn . disp) where
    disp (x,y) = printf "%15s :   %4d" x y

-- Argument errors
errMsg :: String
errMsg = "Usage: hwc lines methods filename,\nwhere lines is the number most frequent words and their frequencies to show, methods is a sequence of letters naming the methods to test, and filename is the name of the file containing the text to analyse.\nAvailable methods are: \nl = simple list methods,\nt = trie-based,\nb = MBags hash-map,\nh = hash table and\ne = Data.Edison.Assoc.TernaryTrie,\ns = using Data.Map.Strict"
