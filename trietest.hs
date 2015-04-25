{-
 
 Automated tests of monad and traversable laws

-}

module Main where

import Trie
import Test.QuickCheck
import Text.Show.Functions
import Control.Monad.Identity
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Traversable

monad1 :: [(String, Int)] -> Bool
monad1 =  (\t -> (t >>= return) == t).fromList 

monad2 :: Int -> (Int -> [(String, Int)]) -> Bool
monad2 n g =     (return n >>= f) == f n where
    f n = fromList $ g n

monad3 :: [(String, Int)] -> (Int -> [(String, Int)]) -> (Int -> [(String, Int)]) -> Bool
monad3 alm alf alg = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g)) where
    m = fromList alm
    f = fromList . alf
    g = fromList . alg

traversable1 :: [(String, Int)] -> Bool
traversable1 = (\t -> runIdentity (traverse Identity t) == runIdentity (Identity t)).fromList

-- Law2 in terms of sequenceA
traversable2 :: [(String,[(String, String)])] -> Bool
traversable2 s = getCompose ((sequenceA . fmap Compose) t) == (fmap sequenceA . sequenceA) t  where
    t = (fromList $ map (\(s,l) -> (s,fromList l)) s) :: TR (TR [Char])

type TR a = Trie Char a

a = stdArgs {maxSuccess = 10, maxSize = 10} 

main = do
    quickCheckWith a monad1
    quickCheckWith a monad2
    quickCheckWith a monad3

    quickCheckWith a traversable1
    quickCheckWith a traversable2
