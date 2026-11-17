-- Trie data structure for counting words in Data.Text.
-- The trie type has been made instance of both Monad and Traversable
-- (and thus also Functor, Applicative and Foldable) as well as Show.
-- A new custom type class Mapping is implemented for finite map behaviours.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TTrie where

import           Control.Monad       (ap)
import qualified Data.Foldable       as F
import qualified Data.Map.Strict     as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Traversable    (foldMapDefault, fmapDefault)
import           Prelude             hiding (lookup)
import           Text.Printf

-- Will use existence of value to mark end of word
data Trie v = Trie { value :: !(Maybe v), tails :: !(M.Map Char (Trie v)) }

class Mapping m v where
    empty  :: m v
    lookup :: m v -> Text -> Maybe v
    update :: (Maybe v -> Maybe v) -> m v -> Text  -> m v
    delete :: m v -> Text -> m v
    toList :: m v -> [ (Text, v) ]

instance Mapping Trie v where

    empty = Trie { value = Nothing, tails = M.empty }

    lookup trie txt = case T.uncons txt of
        Nothing -> value trie
        Just (c,cs) -> do
            child <- M.lookup c (tails trie)
            lookup child cs

    update f trie txt = case T.uncons txt of
        Nothing -> trie { value = f (value trie) }
        Just (c,cs) -> trie { tails = updated c cs }
      where
        updated c cs = case M.lookup c (tails trie) of
            Just sub -> M.update (\_ -> Just $ update f sub cs) c (tails trie)
            Nothing  -> M.insert c newbranch (tails trie) where
                newbranch = update f empty cs

    delete = update (const Nothing)

    toList trie = rv ++ concatMap builder (M.toList $ tails trie) where
        rv = case value trie of
            Nothing -> []
            Just v  -> [(T.empty, v)]
        builder :: (Char, Trie v) -> [(Text,v)]
        builder (c,t) = [(T.cons c s,v) | (s,v) <- toList t]

add :: v -> Trie v -> Text -> Trie v
add v = update (const (Just v))

-- If key is repeated, then later value overrides earlier value
fromList :: [(Text,v)] -> Trie v
fromList = foldl (\ t (k,v) -> add v t k ) empty

instance Eq v => Eq (Trie v) where
    t1 == t2 = toList t1 == toList t2

instance Traversable Trie where
   traverse f t = Trie <$> root <*> children  where
       root     = traverse f (value t)
       children = traverse (traverse f) (tails t)

instance F.Foldable Trie where
    foldMap = foldMapDefault

instance Functor Trie where
    fmap = fmapDefault
--
-- Notice that (like with Data.Map et al.) Foldable.toList makes list of only values and unlike
-- Trie.toList does NOT produce an association list (from which the trie can be reconstructed).

-- WARNING: This is not a lawful Monad: associativity fails when concatenated
-- keys collide. fromList keeps only the last value at a key, so an intermediate
-- bind can discard a value that the other grouping would still use. Example
-- (with OverloadedStrings for the Text keys):
--   m   = fromList [("", 0), ("a", 1)]
--   f 0 = fromList [("a", 0)]
--   f _ = fromList [("", 1)]
--   g 0 = fromList [("x", 0)]
--   g _ = empty
-- Then toList ((m >>= f) >>= g) == [], but
-- toList (m >>= (\x -> f x >>= g)) == [("ax", 0)].
-- Both identity laws hold under the association-list equality used by Eq.
instance Monad Trie where
    (>>=) t f = fromList (concatMap g (toList t)) where
       g (k,v) = map (\(a,b) -> (T.append k a, b)) (toList (f v))

-- WARNING: Using ap also makes this Applicative unlawful (composition fails).
-- With OverloadedStrings, let u = fromList [("", id), ("a", id)],
--      v = fromList [("", id), ("aa", id)],
--      w = fromList [("", 0), ("a", 1)].
-- lookup (pure (.) <*> u <*> v <*> w) "aa" == Just 0, whereas
-- lookup (u <*> (v <*> w)) "aa" == Just 1.
instance Applicative Trie where
    (<*>) = ap
    pure v = add v empty T.empty

instance Show v => Show (Trie v) where
    show t = display (take 15 graph) where
                 graph = toList t
                 display = concatMap ( \(k,v) ->  printf "%15s :   %4v \n" (show k) (show v) )

size :: Trie v -> Int
size t = 1 + sum (fmap (size.snd) $ M.toList $ tails t)

{- Made Trie  into a monad with t >>= f defined as follows:
-
- for each key k in t with corresponding value v, take the keys ks of f v. Form new keys k's = (k ++ ks)
- by concatenating. Replace the key k in t with the keys k's and give them values from (f v).
-}
