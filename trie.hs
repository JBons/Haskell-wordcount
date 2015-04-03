{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Word
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Text.Printf

-- Will use existence of value to mark end of word
data Trie c v = Trie { value :: Maybe v, tails :: M.Map c (Trie c v) }

-- Tries should be:
--  * Foldable
--  * Traversables
-- Monad not clear? Meaning?
-- To handle String tries and ByteString tries

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
    
    update f trie []     = trie { value = f  $ value trie }
    update f trie (c:cs) = trie { tails = updated } where
        updated = case M.lookup c (tails trie) of
            Just sub -> M.update (\_ -> Just $ update f sub cs) c (tails trie)  
            Nothing  -> M.insert c newbranch (tails trie) where
                newbranch = update f empty cs
             
    delete = update (\_ -> Nothing) 

    toList trie = concat $ fmap builder $ M.toList $ tails trie where     
        builder :: Ord c => (c, Trie c v) -> [([c],v)]
        builder (c,t) = case value t of
                           Nothing -> prefix c $ toList t 
                           Just v  -> ([c], v) : (prefix c $ toList t) 
                       where prefix c ll = fmap (\(s,v) -> (c:s,v)) ll   

add :: Ord c => v -> Trie c v -> [c] -> Trie c v
add value = update (\_ -> Just(value)) 

-- If key is repeated, then later value overrides earlier value
fromList :: Ord c => [([c],v)] -> Trie c v
fromList = foldl (\ t (k,v) -> add v t k ) empty 

-- Makes Trie into functor (on the value type)
map :: Ord c => (v -> u) -> Trie c v -> Trie c u
map f t = Trie { value = liftM f $ value t, tails = M.map (Trie.map f) (tails t) }

instance (Show v, Show [c], Ord c) => Show (Trie c v) where
    show t = summary ++ (display $ take 15 graph) where   
                 graph = toList t  
                 summary = printf "Trie with %v key-value pairs, starting with:\n" (length graph)
                 display = concat  . fmap ( \(k,v) ->  printf "%15s :   %4v \n" (show k) (show v) )




