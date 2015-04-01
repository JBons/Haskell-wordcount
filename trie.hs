{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Word
import qualified Data.ByteString.Char8 as B
import Control.Monad

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
    lookup :: m c k v -> k -> Maybe v
    update :: m c k v -> k -> (Maybe v -> v) -> m c k v 
--    delete :: m c k v -> k -> M k v
--    toList :: m c k v -> ([k,v])
    empty  :: m c k v

instance Mapping Trie Char String v where

    lookup trie []     = value trie
    lookup trie (c:cs) = do 
        tail <- M.lookup c (tails trie)
        lookup tail cs    
    
    update trie [] f     = trie { value = Just (f $ value trie) }
    update trie (c:cs) f = trie { tails = updated } where
        updated = case M.lookup c (tails trie) of
            Just sub -> M.update (\_ -> Just $ update sub cs f) c (tails trie)  
            Nothing  -> M.insert c newbranch (tails trie) where
                newbranch = update empty cs f
             
    empty = Trie { value = Nothing, tails = M.empty } 
