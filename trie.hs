{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Word
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Text.Printf

-- Will use existence of value to mark end of word
data Trie c k v = Trie { value :: Maybe v, tails :: M.Map c (Trie c k v) }

-- Tries should be:
--  * Foldable
--  * Traversables
-- Monad not clear? Meaning?
-- To handle String tries and ByteString tries

class Ord k => Mapping m c k v where 
    empty  :: m c k v
    lookup :: m c k v -> k -> Maybe v
    update :: (Maybe v -> Maybe v) -> m c k v -> k  -> m c k v 
    delete :: m c k v -> k -> m c k v
    toList :: m c k v -> [(k,v)]

instance Mapping Trie Char String v where

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
        builder :: (Char, Trie Char String v) -> [(String,v)]
        builder (c,t) = case value t of
                           Nothing -> prefix c $ toList t 
                           Just v  -> ([c], v) : (prefix c $ toList t) 
                       where prefix c ll = fmap (\(s,v) -> (c:s,v)) ll   

add :: v -> Trie Char String v -> String -> Trie Char String v
add value = update (\_ -> Just(value)) 

-- If key is repeated, then later value overrides earlier value
fromList :: [(String,v)] -> Trie Char String v
fromList = foldl (\ t (k,v) -> add v t k ) empty 

-- Makes Trie into functor (on the value type)
map :: (v -> u) -> Trie Char String v -> Trie Char String u
map f t = Trie { value = liftM f $ value t, tails = M.map (Trie.map f) (tails t) }

instance Show v => Show (Trie Char String v) where
    show t = summary ++ (display $ take 15 graph) where   
                 graph = toList t  
                 summary = printf "Trie with %v key-value pairs, starting with:\n" (length graph)
                 display = concat  . fmap ( \(k,v) ->  printf "%15s :   %4v \n" k (show v) )




