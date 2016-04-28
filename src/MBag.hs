module MBag where

import           Control.Monad.ST
import qualified Data.ByteString             as C
import           Data.Char                   (ord)
import           Data.Hashable               (hash, Hashable)
import qualified Data.Map.Strict             as M
import           Data.STRef                  as ST
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

data StringBag s e = SB { counts :: MU.MVector s Int
                      , els      :: MV.MVector s e
                      , collided :: STRef s (M.Map e Int)
                      , tSize    :: Int }

empty :: Int -> ST s (StringBag s e)
empty n = do   c <- MU.new n
               e <- MV.new n
               m <- ST.newSTRef M.empty
               return SB {counts = c, els = e, collided = m, tSize = n}

add :: (Ord e, Hashable e) =>StringBag s e -> e -> ST s ()
add (SB cs es coll size) str =
    let i = (hash str) `mod` size in
    do c <- MU.read cs i
       case compare c 0 of
           GT -> exOne i c
           EQ -> new i
           LT -> exSeveral
    where exOne i c = do w <- MV.read es i
                         if w == str
                             then MU.write cs i (c+1)
                             else handleNewCollision i
          new i = do MU.write cs i 1
                     MV.write es i str
          exSeveral = do m <- ST.readSTRef coll
                         let m' = M.insertWith (+) str 1 m
                         ST.writeSTRef coll m'
          handleNewCollision i = do m  <- ST.readSTRef coll
                                    ec <- MU.read cs i
                                    ee <- MV.read es i
                                    let m'  = M.insert ee ec m
                                    let m'' = M.insert str 1 m'
                                    ST.writeSTRef coll m''
                                    MU.write cs i (-1)
{-# Specialise add :: StringBag s T.Text -> T.Text -> ST s () #-}
{-# Specialise add :: StringBag s C.ByteString -> C.ByteString -> ST s () #-}
{-# Specialise add :: StringBag s String -> String -> ST s () #-}

toList :: Ord e => StringBag s e -> ST s [(e, Int)]
toList bag = do m <- ST.readSTRef (collided bag)
                let cl = M.toList m

                csu <- U.unsafeFreeze (counts bag)
                let csv = V.convert csu :: V.Vector Int
                es <- V.unsafeFreeze (els bag)
                let rps = V.zip es csv
                let ps = V.filter ((>0).snd) rps
                let l = V.toList ps
                return (l ++ cl)
