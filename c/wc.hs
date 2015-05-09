{- Compilation: gcc -dynamiclib -fPIC -o lib.so counterlib.o
 - ghci lib.so
 - ghc ctwc.hs lib.so
 -} 




{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Prelude hiding (words)
import Data.Char hiding (isSpace)
import Data.List (reverse)
import GHC.Exts (sortWith)
import Control.Monad (forM, liftM)
import System.Environment
import System.TimeIt
import Text.Printf
import GHC.Exts hiding (Word)

import Foreign.C.String
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

data Word = Word { count :: Int
                 , word  :: String }

instance Storable Word where
    peek ptr = do
        let chrP = (castPtr ptr) :: CString
        let intP = (plusPtr ptr 20)   :: Ptr CInt
        cc   <- peek intP
        let count = fromIntegral cc
        word <- peekCString chrP
        return $ Word {count = count, word = word}
    poke ptr w = do return () -- do nothing
    sizeOf _ = 24
    alignment _ = 4

foreign import ccall "counterlib.h counts" c_counts :: CString -> Ptr(Ptr Word) -> Ptr () -> IO CInt

-- COMPILES BUT MISREADS THE DATA
-- Words of a text together with their frequencies, sorted highest->lowest
wordFreqs :: String ->  IO [(String,Int)]  
wordFreqs s = do
    cs   <- newCAString s                            -- Forcing use of 8-bit ASCII strings
    wpa  <- mallocArray 50000 :: IO (Ptr (Ptr Word)) -- Room for return array of pointers to words 
    heap <- mallocBytes(50000 * 24)                  -- Room for the Word structures
    wc <- c_counts cs wpa heap
    let size = fromIntegral wc
    plist <- peekArray size wpa
    wordlist <- mapM peek plist
    let raw = map (\w -> (word w, count w)) wordlist
    let result = reverse $ sortWith snd raw
    return result


main = timeIt proc --get running time

--Actual main UI task
proc = do args <- getArgs
          let n = (read $ args!! 0) :: Int
          let filename  = args!! 1
          text   <- readFile filename
          result <- wordFreqs text
          display $ take n result

--Pretty-print the results
display xs = mapM putStrLn $ fmap disp xs where
    disp (x,y) = printf "%15s :   %4d" x y 

