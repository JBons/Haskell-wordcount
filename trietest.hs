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

-- Still to do
traversable2 = undefined --traverse (Compose . fmap g . f) == Compose . fmap (traverse g) . traverse f


main = do
    quickCheck monad1
    quickCheck monad2
    quickCheck monad3

    quickCheck traversable1
    --quickCheck traversable2
