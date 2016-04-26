module WordCounters.ASCII where

import           Prelude                       hiding (words)

import           Control.Monad                 (forM_)
import           Control.Monad.ST              (ST, runST)
import           Data.Char                     (isLetter, toLower)
import           Data.List                     (group, sort)

import qualified CCounterLib                   as CT
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.Edison.Assoc.TernaryTrie as E
import qualified Data.HashTable.Class          as H
import qualified Data.HashTable.ST.Linear      as HL
import qualified Data.Map.Strict               as M
import qualified MBag                          as MB
import qualified MTrie                         as MT
import qualified Trie                          as T

-- Common helper: string to lower-case words
prepare :: C.ByteString -> [String]
prepare = (map C.unpack) . prepare'

prepare' :: C.ByteString -> [C.ByteString]
prepare' = C.words . (B.map conv) where
    conv c
        | c > 96 && c < 123 = c
        | c > 64 && c < 91  = c + 32
        | otherwise = 32


-- 1. Simplest possible count with generic list functions
simpleCounts :: B.ByteString -> [(String, Int)]
simpleCounts = map (\seq -> (head seq, length seq)).group.sort.prepare


-- 2. Implements lexicon as Trie to count occurences of the words in a list
trieCounts :: B.ByteString -> [(String, Int)]
trieCounts = T.toList . buildTrie . prepare  where
    buildTrie = foldl (T.update counting) emptyTrie
    counting Nothing = Just 1
    counting (Just n)  = Just (n+1)
    emptyTrie = T.empty :: T.Trie Char Int


-- 3. Counts frequencies of strings in a list using mutable trie in the ST monad
mTrieCounts :: B.ByteString -> [(String, Int)]
mTrieCounts string = let size = length words in
               runST $ do
                       lexicon <- MT.empty ::  ST s (MT.Trie s Int)
                       forM_ words (MT.update byIncrements lexicon)
                       MT.toList lexicon
    where
        words =prepare string
        byIncrements Nothing   = Just 1
        byIncrements (Just n)  = Just (n+1)


-- 4. Using Data.Map.Strict to count the words
sCounts :: B.ByteString -> [(String, Int)]
sCounts str = f (prepare str) where
    f l = M.toAscList (M.fromListWith (+) (zip l (repeat 1)))

--5. ST monad HashTable in place of Map to count the words
type HashTable s k v = HL.HashTable s k v

htCounts :: B.ByteString -> [(String, Int)]
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

-- 6. Testing StringBag

bCounts :: C.ByteString -> [(String, Int)]
bCounts string =
    let pairs = runST $ do bag <- MB.empty 180000
                           forM_ (prepare' string) (MB.add bag)
                           MB.toList bag
    in map (\(a,b) -> (C.unpack a, b)) pairs

-- 7. Data.Edison.Assoc.TernaryTrie
eCounts :: B.ByteString ->  [(String, Int)]
eCounts str =  E.toSeq $ E.insertSeqWith (+) input E.empty where
    input =zip (prepare str) (repeat 1)

-- 8. Using fast C library through FFI

cTrieCounts :: B.ByteString -> [(String, Int)]
cTrieCounts = CT.counts . C.unpack
