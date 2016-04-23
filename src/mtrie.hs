
-- New version of mutable trie where child nodes are maintained as a mutable
-- list and not a hash table. Improves performance substantially.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MTrie where

import           Control.Monad       (forM_)
import           Control.Monad.ST    (ST)
--import           Data.Array.ST       (Ix, STArray, getAssocs, newArray,
--                                      readArray, writeArray)
import qualified Data.Map            as M
import           Data.STRef
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector        as V
import           Prelude             hiding (lookup)
import           Data.Char           (ord,chr)

-- Will use existence of value to mark end of word
-- Node value is stored by STRef, child nodes in STArray of Maybe Trie
-- We specialise to c = Char, so type signatures change
data Trie s v = Trie { value :: STRef s (Maybe v),
                       tails :: MV.MVector s (Maybe (Trie s v)) }
class SMapping m s v where
    empty  :: ST s (m s v)
    --lookup :: m s v -> String -> ST s (Maybe v)
    update :: (Maybe v -> Maybe v) -> m s v -> [Int] -> ST s ()
    update' :: (Maybe v -> Maybe v) -> m s v -> [Int] -> ST s ()
    delete :: m s v -> [Int] -> ST s ()
    toList' :: m s v -> ST s [ ([Int], v) ]
    toList :: m s v -> ST s [ (String, v) ]


instance SMapping Trie s v where

    -- Implement empty
    empty = do newRef   <- newSTRef Nothing
               newTails <- MV.replicate 128 Nothing
               return Trie { value = newRef, tails = newTails }


    toList = (fmap convert) . toList' where
        convert = fmap (\(is,v) -> (fmap chr is,v))
    -- Implement lookup
    --lookup trie ""  = readSTRef $ value trie

    --lookup trie (c:cs) = do
    --    match <- readArray (tails trie) c
    --    case match of
    --        Just tail -> lookup tail cs
    --        Nothing   -> return Nothing

    -- Implement update
    update' f trie [] = do
        let vref = value trie
        val <- readSTRef vref
        modifySTRef' vref f


    update' f trie (n:ns) = do
        match <- MV.read (tails trie) n
        case match of
            Just tail -> update' f tail ns
            Nothing   -> do
                newEmpty <- empty
                MV.write (tails trie) n (Just newEmpty)
                update' f newEmpty ns

    update = update'
    --update f trie str = update' f trie (fmap ord str)

    -- Implement delete
    delete = update (const Nothing)

    -- Implement toList
    toList' trie = do
        childList <- toAssocList $ tails trie
        fmap concat $ mapM builder childList where

            --builder :: (Char, Trie s v) -> ST s [ (String, v) ]
            builder (c,t) = do
                val  <- readSTRef $ value t
                ends <- toList' t
                let upd = prefix c ends
                case val of
                    Nothing -> return upd
                    Just v  -> return $ ([c],v) : upd

getAssocs :: V.MVector s b -> ST s [(Int, b)]
getAssocs v = fmap (\w -> zip [0..] (V.toList w)) (V.freeze v)
-- Turn array of (Maybe a) values to list of (ind, a) tuples
--toAssocList :: Ix i => STArray s i (Maybe v) -> ST s [ (i,v) ]
toAssocList a = do
    pairs <- getAssocs a
    return [ (ind, v) | (ind, Just v) <- pairs]

-- Helper for toList: prefixes an element to the first pos in list of tuples
prefix :: a ->  [([a], t)] -> [([a], t)]
prefix c = fmap (\(s,v) -> (c:s,v))

add :: v -> Trie s v -> [Int] -> ST s ()
add value = update (\_ -> Just value )

-- If key is repeated, then later value overrides earlier value
fromList :: [ ([Int], v) ] -> ST s (Trie s v)
fromList pairs = do
    t <- empty
    forM_ pairs (\ (k,v) -> add v t k )
    return t
