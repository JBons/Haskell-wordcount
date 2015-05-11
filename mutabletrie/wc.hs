module Main where

import Prelude hiding (words)
import Data.Char hiding (isSpace)
import Data.List (reverse)
import GHC.Exts (sortWith)
import Control.Monad (forM, liftM)
import Control.Monad.ST
import MTrie
import System.Environment
import System.TimeIt
import Text.Printf

-- Expanded definition of "white space" for word splitter
isSpace :: Char -> Bool
isSpace c = not (isLetter c)

-- Own implementation of more accurate word splitter
words :: String -> [String]
words s = let clean = dropWhile isSpace s in
    case clean of
        ""  -> []
        str -> first : (words rest)
            where (first, rest) = span isLetter str 

--Counts frequencies of strings in a list using the ST monad
counts :: [String] -> [(String, Int)] 
counts words = let size = length words in
               runST $ do 
                       lexicon <- empty ::  ST s (Trie s Int)
                       forM words (update byIncrements lexicon) 
                       toList lexicon

byIncrements Nothing   = Just 1
byIncrements (Just n)  = Just (n+1)

--Words of a text together with their frequencies, sorted highest->lowest
wordFreqs :: String ->  [(String,Int)]  
wordFreqs = reverse . sortWith snd . counts . words . (map toLower)

main = timeIt proc --get running time

--Actual main UI task
proc = do args <- getArgs
          let n = (read $ args!! 0) :: Int
          let filename  = args!! 1
          text   <- readFile filename
          let result = wordFreqs text
          display $ take n result
          let wcount = length $ words text
          let uwords = length result
          putStrLn $ "Total words: " ++ (show wcount)
          putStrLn $ "Different words: " ++ (show uwords) 

--Pretty-print the results
display xs = mapM putStrLn $ map disp xs where
    disp (x,y) = printf "%15s :   %4d" x y 
