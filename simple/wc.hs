module Main where

import Prelude hiding (words)
import Data.Char hiding (isSpace)
import Data.List (reverse,sort,group)
import GHC.Exts (sortWith)
import System.Environment
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

--Words of a text together with their frequencies, sorted highest->lowest
wordFreqs :: String ->  [(String,Int)]  
wordFreqs = reverse . sortWith snd . counts . words . (fmap toLower) where
    counts = (map (\seq -> (head seq, length seq))).group.sort
    
--Actual main UI task
main = do args <- getArgs
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
display xs = mapM putStrLn $ fmap disp xs where
    disp (x,y) = printf "%15s :   %4d" x y 
