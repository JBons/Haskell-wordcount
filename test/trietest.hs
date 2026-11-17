{-
 
 Automated tests of monad and traversable laws

-}

module Main where

import Trie
import Test.QuickCheck
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as B
import Data.Functor.Identity
import Data.Functor.Compose
import Data.List (sort)
import qualified Data.Text as Text
import System.Exit (exitFailure)
import qualified TTrie
import qualified WordCounters as Unicode
import qualified WordCounters.ASCII as ASCII

monad1 :: [(String, Int)] -> Bool
monad1 = (\t -> (t >>= pure) == t) . fromList

monad2 :: Int -> Fun Int [(String, Int)] -> Bool
monad2 n g = (pure n >>= f) == f n where
    f = fromList . applyFun g

monad3 :: [(String, Int)] -> Fun Int [(String, Int)] -> Fun Int [(String, Int)] -> Bool
monad3 alm alf alg = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g)) where
    m = fromList alm
    f = fromList . applyFun alf
    g = fromList . applyFun alg

traversable1 :: [(String, Int)] -> Bool
traversable1 = (\t -> runIdentity (traverse Identity t) == runIdentity (Identity t)).fromList

-- Law2 in terms of sequenceA
traversable2 :: [(String,[(String, String)])] -> Bool
traversable2 s = getCompose ((sequenceA . fmap Compose) t) == (fmap sequenceA . sequenceA) t  where
    t = fromList (map (\(key,l) -> (key,fromList l)) s) :: TR (TR String)

type TR a = Trie Char a

-- List traversal forms Cartesian products. Bound each level explicitly so
-- testing composition cannot accidentally enumerate enormous result sets.
compositionInputs :: Gen [(String, [(String, String)])]
compositionInputs = do
    outerSize <- chooseInt (0, 3)
    vectorOf outerSize $ do
        key <- resize 2 arbitrary
        innerSize <- chooseInt (0, 2)
        pairs <- vectorOf innerSize ((,) <$> resize 2 arbitrary <*> resize 2 arbitrary)
        pure (key, pairs)

-- Exercise empty keys, Unicode keys and overwriting through both trie APIs.
textTrieAgreement :: [(String, Int)] -> String -> Int -> Bool
textTrieAgreement entries key v =
    TTrie.toList textTrie == [(Text.pack k, n) | (k,n) <- toList stringTrie]
    && TTrie.lookup textTrie (Text.pack key) == Just v
  where
    -- Text replaces surrogate code points; compare the same valid Unicode keys.
    normalise = Text.unpack . Text.pack
    stringTrie = add v (fromList [(normalise k, n) | (k,n) <- entries]) (normalise key)
    textTrie = TTrie.add v (TTrie.fromList [(Text.pack k, n) | (k,n) <- entries]) (Text.pack key)

counterAgreement :: Property
counterAgreement = forAll (listOf (elements "abcXYZ \n\t,.-")) $ \input ->
    let text = Text.pack input
        bytes = B.pack input
        expected = sort (ASCII.sCounts bytes)
        asciiCounters = [ASCII.simpleCounts, ASCII.trieCounts, ASCII.mTrieCounts,
                         ASCII.htCounts, ASCII.eCounts, ASCII.bCounts]
        unicodeCounters = [Unicode.simpleCounts, Unicode.trieCounts,
                           Unicode.htCounts, Unicode.eCounts, Unicode.bCounts]
    in conjoin
        ([sort (counter bytes) === expected | counter <- asciiCounters]
         ++ [sort [(Text.unpack w, n) | (w,n) <- counter text] === expected
             | counter <- unicodeCounters])

testArgs :: Args
testArgs = stdArgs {maxSuccess = 10, maxSize = 10}

main :: IO ()
main = do
    results <- mapM check
        [ ("Monad right identity", property monad1)
        , ("Monad left identity", property monad2)
        , ("Monad associativity", property monad3)
        , ("Traversable identity", property traversable1)
        , ("Traversable composition", forAllShrink compositionInputs shrink traversable2)
        , ("Text/String trie agreement", property textTrieAgreement)
        , ("Word counter agreement", counterAgreement)
        ]
    unless (all isSuccess results) exitFailure
  where
    check (name, prop) = do
        putStrLn name
        quickCheckWithResult testArgs prop
