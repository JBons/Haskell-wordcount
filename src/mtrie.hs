
-- New version of mutable trie where child nodes are maintained as a mutable
-- list and not a hash table. Improves performance substantially.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MTrie where

import           Control.Monad    (forM_)
import           Control.Monad.ST (ST)
import           Data.Array.ST    (Ix, STArray, getAssocs, newArray, readArray,
                                   writeArray)
import qualified Data.Map         as M
import           Data.STRef
import           Prelude          hiding (lookup)

-- Uses existence of value to mark end of word
-- Node value is stored by STRef, child nodes in STArray of Maybe Trie
-- We specialise to c = Char, so type signatures change
data Trie s v = Trie { value   :: STRef s (Maybe v),
                         tails :: STArray s Char (Maybe (Trie s v)) }
class SMapping m s v where
    empty  :: ST s (m s v)
    lookup :: m s v -> String -> ST s (Maybe v)
    update :: (Maybe v -> Maybe v) -> m s v -> String -> ST s ()
    delete :: m s v -> String -> ST s ()
    toList :: m s v -> ST s [ (String, v) ]

instance SMapping Trie s v where

    -- Implement empty
    empty = do newRef   <- newSTRef Nothing
               newTails <- newArray ('a','z') Nothing
               return Trie { value = newRef, tails = newTails }

    -- Implement lookup
    lookup trie ""  = readSTRef $ value trie

    lookup trie (c:cs) = do
        match <- readArray (tails trie) c
        case match of
            Just tail -> lookup tail cs
            Nothing   -> return Nothing

    -- Implement update
    update f trie "" = do
        let vref = value trie
        val <- readSTRef vref
        modifySTRef' vref f

    update f trie (c:cs) = do
        match <- readArray (tails trie) c
        case match of
            Just tail -> update f tail cs
            Nothing   -> do
                newEmpty <- empty
                writeArray (tails trie) c (Just newEmpty)
                update f newEmpty cs

    -- Implement delete
    delete = update (const Nothing)

    -- Implement toList
    toList trie = do
        childList <- toAssocList $ tails trie
        fmap concat $ mapM builder childList where

            builder :: (Char, Trie s v) -> ST s [ (String, v) ]
            builder (c,t) = do
                val  <- readSTRef $ value t
                ends <- toList t
                let upd = prefix c ends
                case val of
                    Nothing -> return upd
                    Just v  -> return $ ([c],v) : upd

-- Turn array of (Maybe a) values to list of (ind, a) tuples
toAssocList :: Ix i => STArray s i (Maybe v) -> ST s [ (i,v) ]
toAssocList a = do
    pairs <- getAssocs a
    return [ (ind, v) | (ind, Just v) <- pairs]

-- Helper for toList: prefixes an element to the first pos in list of tuples
prefix c ll = fmap (\(s,v) -> (c:s,v)) ll

add :: v -> Trie s v -> String -> ST s ()
add value = update (\_ -> Just value )

-- If key is repeated, then later value overrides earlier value
fromList :: [ (String, v) ] -> ST s (Trie s v)
fromList pairs = do
    t <- empty
    forM_ pairs (\ (k,v) -> add v t k )
    return t
