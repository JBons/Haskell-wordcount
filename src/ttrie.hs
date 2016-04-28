-- Trie data structure for counting words in Data.Text.
-- The trie type has been made instance of both Monad and Traversable
-- (and thus also Functor, Applicative and Foldable) as well as Show.
-- A new custom type class Mapping is implemented for finite map behaviours.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE Strict #-} : Try as soon as GHC 8 is out!

module TTrie where

import           Control.Applicative (Applicative, pure, (<$>), (<*>))
import           Control.Monad       hiding (sequence)
import qualified Data.Foldable       as F
import qualified Data.Map.Strict     as M
import           Data.Text           (Text, append, cons, length,head,tail,empty)
import           Data.Traversable
import           Prelude             hiding (lookup,sequence,length,head,tail)
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

    lookup trie txt = if length txt == 0
        then value trie
        else let (c,cs) = (head txt, tail txt)
             in do tail <- M.lookup c (tails trie)
                   lookup tail cs

    update f trie txt = if length txt == 0
        then trie { value = f  $ value trie }
        else let (c,cs) = (head txt, tail txt)
        in trie { tails = updated c cs} where
        updated c cs = case M.lookup c (tails trie) of
            Just sub -> M.update (\_ -> Just $ update f sub cs) c (tails trie)
            Nothing  -> M.insert c newbranch (tails trie) where
                newbranch = update f TTrie.empty cs

    delete = update (const Nothing)

    toList trie = rv ++ concatMap builder (M.toList $ tails trie) where
        rv = case value trie of
            Nothing -> []
            Just v  -> [(Data.Text.empty, v)]
        builder :: (Char, Trie v) -> [(Text,v)]
        builder (c,t) = case value t of
                           Nothing -> prefix c $ toList t
                           Just v  -> prefix c $ toList t
                       where prefix c ll = fmap (\(s,v) -> (c `cons` s,v)) ll

add :: v -> Trie v -> Text -> Trie v
add value = update (const (Just value) )

-- If key is repeated, then later value overrides earlier value
fromList :: [(Text,v)] -> Trie v
fromList = foldl (\ t (k,v) -> add v t k ) TTrie.empty

instance Eq v => Eq (Trie v) where
    t1 == t2 = toList t1 == toList t2

instance Traversable Trie where
   traverse f t = Trie <$> root <*> children  where
       root     = sequenceA( fmap f (value t) )
       children = sequenceA $ M.map (traverse f) (tails t)

instance F.Foldable Trie where
    foldMap = foldMapDefault

instance Functor Trie where
    fmap = fmapDefault
--
-- Notice that (like with Data.Map et al.) Foldable.toList makes list of only values and unlike
-- Trie.toList does NOT produce an association list (from which the trie can be reconstructed).

instance Monad Trie where
    (>>=) t f = let tl = toList t in (fromList.concat) (map g tl) where
       g (k,v) = map (\(a,b) -> (k `append` a, b)) (toList (f v))
    return v = add v TTrie.empty Data.Text.empty

instance Applicative Trie where
    (<*>) = ap
    pure  = return

instance Show v => Show (Trie v) where
    show t = display (take 15 graph) where
                 graph = toList t
--                 summary = printf "Trie with %v key-value pairs, starting with:\n" (Prelude.length graph)
                 display = concatMap ( \(k,v) ->  printf "%15s :   %4v \n" (show k) (show v) )

size :: Trie v -> Int
size t = 1 + sum (fmap (size.snd) $ M.toList $ tails t)

{- Made Trie  into a monad with t >>= f defined as follows:
-
- for each key k in t with corresponding value v, take the keys ks of f v. Form new keys k's = (k ++ ks)
- by concatenating. Replace the key k in t with the keys k's and give them values from (f v).
-}
