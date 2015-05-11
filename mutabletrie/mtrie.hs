{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module MTrie where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Word
import Control.Monad
import Control.Monad.ST
import qualified Data.HashTable.ST.Linear as HL
import qualified Data.HashTable.Class as H
import Data.Hashable(Hashable)
import Data.STRef
import Text.Printf
import Data.Foldable(foldlM)

--Alias for the Linear (ST) hash table
type HTable s k v = HL.HashTable s k v

-- Will use existence of value to mark end of word 
-- Node value is stored by STRef, child nodes in HTable
data Trie s c v = Trie { value :: STRef s (Maybe v), tails :: HTable s c (Trie s c v) }

class SMapping m s c v where 
    empty  :: ST s (m s c v)
    lookup :: m s c v -> [c] -> ST s (Maybe v)
    update :: (Maybe v -> Maybe v) -> m s c v -> [c] -> ST s () 
    delete :: m s c v -> [c] -> ST s ()
    toList :: m s c v -> ST s [([c],v)]

instance (Eq c, Hashable c) => SMapping Trie s c v where

    -- Implement empty
    empty = do newRef   <- newSTRef Nothing
               newTails <- HL.newSized 50  
               return $ Trie { value = newRef, tails = newTails }

    -- Implement lookup
    lookup trie []     = do
        val <- readSTRef $ value trie
        return val

    lookup trie (c:cs) = do
        match <- H.lookup (tails trie) c
        case match of
            Just tail -> lookup tail cs    
            Nothing   -> return Nothing
    
    -- Implement update
    update f trie [] = do
        let vref = value trie
        val <- readSTRef vref
        modifySTRef' vref f

    update f trie (c:cs) = do
        match <- H.lookup (tails trie) c
        case match of
            Just tail -> update f tail cs
            Nothing   -> do
                newEmpty <- empty
                H.insert (tails trie) c newEmpty
                update f newEmpty cs

    -- Implement delete
    delete = update (\_ -> Nothing) 

    -- Implement toList 
    toList trie = do
        childList <- H.toList $ tails trie
        liftM concat $ mapM builder childList where
        
            builder :: (Eq c, Hashable c) => (c, Trie s c v) -> ST s [([c],v)]
            builder (c,t) = do
                val  <- readSTRef $ value t
                ends <- toList t
                let upd = prefix c ends
                case val of
                    Nothing -> return upd
                    Just v  -> return $ ([c],v) : upd
    
-- Helper for toList: prefixes an element to the first pos in list of tuples
prefix c ll = fmap (\(s,v) -> (c:s,v)) ll   

add :: (Eq c, Hashable c) => v -> Trie s c v -> [c] -> ST s ()
add value = update (\_ -> Just(value)) 

-- If key is repeated, then later value overrides earlier value
fromList :: (Eq c, Hashable c) => [([c],v)] -> ST s (Trie s c v)
fromList pairs = do
    t <- empty
    forM pairs (\ (k,v) -> add v t k ) 
    return t

-- Makes Trie into functor (on the value type) =====TO DO / THINK WHAT IS CORRECT APPROACH
--map :: (Eq c, Hashable c) => (v -> u) -> Trie s c v -> Trie c u
--map f t = Trie { value = liftM f $ value t, tails = M.map (Trie.map f) (tails t) }

{-

instance (Show v, Show [c], Eq c, Hashable c) => Show (Trie s c v) where
    show t = let aux = do graph <- toList t
                          putStr $ (summary graph) ++ (display $ take 15 graph)    
                          return t where
                               summary graph = printf "Trie with %v key-value pairs, starting with:\n" (length graph)
                               display = concat  . fmap ( \(k,v) ->  printf "%15s :   %4v \n" (show k) (show v) )
             in (\_ -> "Printed") aux
-}
