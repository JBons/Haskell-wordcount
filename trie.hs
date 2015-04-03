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

-- Library will be about ASCII Tries
type BTrie v    = Trie B.ByteString Word8 v
type STrie v    = Trie Char String v
--type MSTrie s v = Trie Char String (STRef s v) --THINK!!

-- Tries should be:
--  * Functors
--  * Foldable
--  * Traversables
-- Monad not clear? Meaning?
-- To handle String tries and ByteString tries

class Ord k => Mapping m c k v where 
    empty  :: m c k v
    lookup :: m c k v -> k -> Maybe v
    update :: m c k v -> k -> (Maybe v -> Maybe v) -> m c k v 
    delete :: m c k v -> k -> m c k v
    toList :: m c k v -> [(k,v)]

instance Mapping Trie Char String v where

    empty = Trie { value = Nothing, tails = M.empty }

    lookup trie []     = value trie
    lookup trie (c:cs) = do 
        tail <- M.lookup c (tails trie)
        lookup tail cs    
    
    update trie [] f     = trie { value = f  $ value trie }
    update trie (c:cs) f = trie { tails = updated } where
        updated = case M.lookup c (tails trie) of
            Just sub -> M.update (\_ -> Just $ update sub cs f) c (tails trie)  
            Nothing  -> M.insert c newbranch (tails trie) where
                newbranch = update empty cs f
             
    delete trie key = update trie key (\_ -> Nothing)

    toList trie = concat $ fmap builder $ M.toList $ tails trie where     
        builder :: (Char, Trie Char String v) -> [(String,v)]
        builder (c,t) = case value t of
                           Nothing -> prefix c $ toList t 
                           Just v  -> ([c], v) : (prefix c $ toList t) 
                       where prefix c ll = fmap (\(s,v) -> (c:s,v)) ll   

add :: Trie Char String v -> String -> v -> Trie Char String v
add trie key value = update trie key (\_ -> Just(value))

-- If key is repeated, then later value overrides earlier value
fromList :: [(String,v)] -> Trie Char String v
fromList = foldl (\ t (k,v) -> add t k v) empty 

-- Makes Trie into functor (on the value type)
map :: (v -> u) -> Trie Char String v -> Trie Char String u
map f t = Trie { value = liftM f $ value t, tails = M.map (Trie.map f) (tails t) }

instance Show v => Show (Trie Char String v) where
    show t = summary ++ (display $ take 15 graph) where   
                 graph = toList t  
                 summary = printf "Trie with %v key-value pairs, starting with:\n" (length graph)
                 display = concat  . fmap ( \(k,v) ->  printf "%15s :   %4v \n" k (show v) )




