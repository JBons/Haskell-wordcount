module WordCounters where

import           Prelude                       hiding (filter, map, words)

import           Control.Monad                 (forM_)
import           Control.Monad.ST              (ST, runST)
import           Data.Char                     (isLetter)
import           Data.List                     (group, sort)

import qualified CCounterLib                   as CT
import qualified Data.Edison.Assoc.TernaryTrie as E
import qualified Data.HashTable.Class          as H
import qualified Data.HashTable.ST.Linear      as HL
import qualified Data.Map.Strict               as M
import           Data.Text                     (Text, filter, map, pack,
                                                toLower, unpack, words)
import qualified MBag                          as MB
import qualified TTrie                         as T

-- Common helper: string to lower-case words
prepare = words . toLower . map nonLtrToSpace where
    nonLtrToSpace c = if isLetter c then c else ' '


-- 1. Simple count with generic list functions
simpleCounts :: Text -> [(Text, Int)]
simpleCounts = fmap (\seq -> (head seq, length seq)).group.sort.prepare

-- 2. Implements lexicon as Trie to count occurences of the words in a list
trieCounts :: Text -> [(Text, Int)]
trieCounts = T.toList . buildTrie . prepare  where
    buildTrie = foldl (T.update counting) emptyTrie
    counting Nothing = Just 1
    counting (Just n)  = Just (n+1)
    emptyTrie = T.empty :: T.Trie Int

-- 3. Using Data.Map.Strict to count the words
sCounts :: Text -> [(Text, Int)]
sCounts str = f (prepare str) where
    f l = M.toAscList (M.fromListWith (+) (zip l (repeat 1)))

--4. ST monad HashTable in place of Map to count the words
type HashTable s k v = HL.HashTable s k v

htCounts :: Text -> [(Text, Int)]
htCounts string = let size = length words in
               runST $ do
                       lexicon <- H.newSized size ::  ST s (HashTable s Text Int)
                       forM_ words (increment lexicon)
                       H.toList lexicon
    where
        words =prepare string
        --Increment hashtable value for a key or create value 1 for new key
        increment :: HashTable s Text Int -> Text -> ST s ()
        increment ht key = do cur <- H.lookup ht key
                              case cur of
                                  Nothing -> H.insert ht key 1
                                  Just n  -> H.insert ht key (n+1)

-- 5. Data.Edison.Assoc.TernaryTrie
eCounts :: Text ->  [(Text, Int)]
eCounts str =  finish $ E.toSeq $ E.insertSeqWith (+) input E.empty where
    input = zip (fmap unpack (prepare str)) (repeat 1)
    finish = fmap (\(s,i) -> (pack s,i))

-- 6. StringBag
bCounts :: Text -> [(Text, Int)]
bCounts string =
    let ws = prepare string
        l = length (words string)
    in runST $ do bag <- MB.empty (sizeFor l)
                  forM_ ws (MB.add bag)
                  MB.toList bag
    -- Use log TTR of 0.8 as estimate for # unique words, keep table sparse
    where sizeFor l = floor (8 * (fromIntegral l ** 0.8))
