module WordCounters where

import           Prelude                  hiding (words)

import           Control.Monad            (forM_)
import           Control.Monad.ST         (ST, runST)
import           Data.Char                (isLetter, toLower,ord)
import           Data.List                (group, sort)

import qualified CCounterLib              as CT
import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Linear as HL
import qualified MTrie                    as MT
import qualified Trie                     as T

-- String to lower-case words
prepare = words . map toLower


-- 1. Simplest possible count with generic list functions
simpleCounts :: String -> [(String, Int)]
simpleCounts = map (\seq -> (head seq, length seq)).group.sort.prepare


-- 2. Implements lexicon as Trie to count occurences of the words in a list
trieCounts :: String -> [(String, Int)]
trieCounts = T.toList . buildTrie . prepare  where
    buildTrie = foldl (T.update counting) emptyTrie
    counting Nothing = Just 1
    counting (Just n)  = Just (n+1)
    emptyTrie = T.empty :: T.Trie Char Int


-- 3. Counts frequencies of strings in a list using mutable trie in the ST monad
mTrieCounts :: String -> [(String, Int)]
mTrieCounts string = let size = length words in
               runST $ do
                       lexicon <- MT.empty ::  ST s (MT.Trie s Int)
                       forM_ words (MT.update byIncrements lexicon)
                       MT.toList lexicon
    where
        words = map (\w -> map ord w) (prepare string)
        byIncrements Nothing   = Just 1
        byIncrements (Just n)  = Just (n+1)


--4. Counts frequencies of strings in a list using hash tables in the ST monad
type HashTable s k v = HL.HashTable s k v

htCounts :: String -> [(String, Int)]
htCounts string = let size = length words in
               runST $ do
                       lexicon <- H.newSized size ::  ST s (HashTable s String Int)
                       forM_ words (increment lexicon)
                       H.toList lexicon
    where
        words =prepare string
        --Increment hashtable value for a key or create value 1 for new key
        increment :: HashTable s String Int -> String -> ST s ()
        increment ht key = do cur <- H.lookup ht key
                              case cur of
                                  Nothing -> H.insert ht key 1
                                  Just n  -> H.insert ht key (n+1)

-- 5. Using fast C library through FFI

cTrieCounts :: String -> [(String, Int)]
cTrieCounts = CT.counts


-- Replacement for Prelude words function
-----------------------------------------

-- Definition of "white space" for word splitter
isSpace :: Char -> Bool
isSpace c = not (isLetter c)

-- Split into words cleaning white space - in place of Prelude version
words :: String -> [String]
words s = let clean = dropWhile isSpace s in
    case clean of
        ""  -> []
        str -> first : (words rest)
            where (first, rest) = span isLetter str
