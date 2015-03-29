module Main where

import Prelude hiding (words)
import Data.Char hiding (isSpace)
import Data.List (reverse)
import GHC.Exts (sortWith)
import Control.Monad (forM, liftM)
import qualified Data.HashTable.IO as H
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

type HashTable k v = H.LinearHashTable k v

--Counts frequencies of strings in a list in the IO monad
counts :: [String] -> IO ( [(String, Int)] )
counts words = let size = length words in
               do lexicon <- H.newSized size :: IO (HashTable String Int)
                  forM words (increment lexicon)
                  H.toList lexicon

--Increment hashtable value for a key or create value 1 for new key
increment ht key = do cur <- H.lookup ht key
                      case cur of
                           Nothing -> H.insert ht key 1
                           Just n  -> H.insert ht key (n+1)

--Words of a text together with their frequencies, sorted highest->lowest
--(result of counts is in IO monad)
wordFreqs :: String -> IO ( [(String,Int)] ) 
wordFreqs = liftM (reverse . (sortWith snd)) . counts . words . (map toLower)

main = timeIt proc --get running time

--Actual main UI task
proc = do args <- getArgs
          let n = (read $ args!! 0) :: Int
          let filename  = args!! 1
          text   <- readFile filename
          result <- wordFreqs text
          display $ take n result
          let wcount = length $ words text
          let uwords = length result
          putStrLn $ "Total words: " ++ (show wcount)
          putStrLn $ "Different words: " ++ (show uwords) 

--Pretty-print the results
display xs = mapM putStrLn $ map disp xs where
    disp (x,y) = printf "%15s :   %4d" x y 
