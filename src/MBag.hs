module MBag where

import           Control.Monad.ST
import           Data.Bits                   (shiftL)
import           Data.Char                   (ord)
import           Data.List                   (foldl')
import qualified Data.Map.Strict             as M
import           Data.STRef                  as ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU


data StringBag s = SB { counts   :: MU.MVector s Int
                      , els      :: MV.MVector s String
                      , collided :: STRef s (M.Map String Int)
                      , tSize    :: Int }

--empty :: Int -> Int -> MBag s e
empty :: Int -> ST s (StringBag s)
empty n = do   c <- MU.new n
               e <- MV.new n
               m <- ST.newSTRef M.empty
               return SB {counts = c, els = e, collided = m, tSize = n}

add :: StringBag s -> String -> ST s ()
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

toList :: StringBag s -> ST s [(String, Int)]
toList bag = do m <- ST.readSTRef (collided bag)
                let cl = M.toList m

                csu <- U.unsafeFreeze (counts bag)
                let csv = V.convert csu :: V.Vector Int
                es <- V.unsafeFreeze (els bag)
                let rps = V.zip es csv
                let ps = V.filter ((>0).snd) rps
                let l = V.toList ps
                return (l ++ cl)

-- djb2 string hash algorithm
hash ::  String -> Int
hash = foldl' iter 5381 where
    iter hash chr = 33 * hash + ord chr
{-# Inline hash #-}
