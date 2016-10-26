-- Trie data structure for counting words in a string built using "basic"
-- components and data structures.
-- The trie type has been made instance of both Monad and Traversable
-- (and thus also Functor, Applicative and Foldable) as well as Show.
-- A new custom type class Mapping is implemented for finite map behaviours.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE Strict #-} -- Enabled in GHC 8.x but no apparent impact on performance detected

module Trie where

import           Control.Applicative (Applicative, pure, (<$>), (<*>))
import           Control.Monad       hiding (sequence)
import qualified Data.Foldable       as F
import qualified Data.Map.Strict     as M
import           Data.Traversable
import           Prelude             hiding (lookup, sequence)
import           Text.Printf

-- Will use existence of value to mark end of word
data Trie c v = Trie { value :: !(Maybe v), tails :: !(M.Map c (Trie c v)) }

class Ord c => Mapping m c v where
    empty  :: m c v
    lookup :: m c v -> [c] -> Maybe v
    update :: (Maybe v -> Maybe v) -> m c v -> [c]  -> m c v
    delete :: m c v -> [c] -> m c v
    toList :: m c v -> [([c],v)]

instance Ord c => Mapping Trie c v where

    empty = Trie { value = Nothing, tails = M.empty }

    lookup trie []     = value trie
    lookup trie (c:cs) = do
        tail <- M.lookup c (tails trie)
        lookup tail cs
    {-# Specialise lookup :: Trie Char Int -> String -> Maybe Int #-}

    update f trie []     = trie { value = f  $ value trie }
    update f trie (c:cs) = trie { tails = updated } where
        updated = case M.lookup c (tails trie) of
            Just sub -> M.update (\_ -> Just $ update f sub cs) c (tails trie)
            Nothing  -> M.insert c newbranch (tails trie) where
                newbranch = update f empty cs
    {-# Specialise update :: (Maybe Int -> Maybe Int)-> Trie Char Int -> String -> Trie Char Int #-}

    delete = update (const Nothing)

    toList trie = rv ++ concatMap builder (M.toList $ tails trie) where
        rv = case value trie of
            Nothing -> []
            Just v  -> [([],v)]
        builder :: Ord c => (c, Trie c v) -> [([c],v)]
        builder (c,t) = case value t of
                           Nothing -> prefix c $ toList t
                           Just v  -> prefix c $ toList t
                       where prefix c ll = fmap (\(s,v) -> (c:s,v)) ll

add :: Ord c => v -> Trie c v -> [c] -> Trie c v
add value = update (\_ -> Just value )

-- If key is repeated, then later value overrides earlier value
fromList :: Ord c => [([c],v)] -> Trie c v
fromList = foldl (\ t (k,v) -> add v t k ) empty

instance (Ord c, Eq c, Eq v) => Eq (Trie c v) where
    t1 == t2 = toList t1 == toList t2

instance Ord c => Traversable (Trie c) where
   traverse f t = Trie <$> root <*> children  where
       root     = sequenceA( fmap f (value t) )
       children = sequenceA $ M.map (traverse f) (tails t)

instance Ord c => F.Foldable (Trie c) where
    foldMap = foldMapDefault

instance Ord c => Functor (Trie c) where
    fmap = fmapDefault
--
-- Notice that (like with Data.Map et al.) Foldable.toList makes list of only values and unlike
-- Trie.toList does NOT produce an association list (from which the trie can be reconstructed).

instance Ord c => Monad (Trie c) where
    (>>=) t f = let tl = toList t in (fromList.concat) (map g tl) where
       g (k,v) = map (\(a,b) -> (k++a, b)) (toList (f v))
    return v = add v empty []

instance Ord c=> Applicative (Trie c) where
    (<*>) = ap
    pure  = return

instance (Show v, Show [c], Ord c) => Show (Trie c v) where
    show t = summary ++ display (take 15 graph) where
                 graph = toList t
                 summary = printf "Trie with %v key-value pairs, starting with:\n" (length graph)
                 display = concatMap ( \(k,v) ->  printf "%15s :   %4v \n" (show k) (show v) )

size :: Ord c => Trie c v -> Int
size t = 1 + sum (fmap (size.snd) $ M.toList $ tails t)

{- Made Trie into a monad with t >>= f defined as follows:
-
- for each key k in t with corresponding value v, take the keys ks of f v. Form new keys k's = (k ++ ks)
- by concatenating. Replace the key k in t with the keys k's and give them values from (f v).
-}
