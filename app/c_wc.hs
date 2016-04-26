{- Build:
 - gcc -dynamiclib -fPIC -O3 -o lib.so counterlib.c
 - ghci lib.so
 - ghc -O2 wc.hs lib.so
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Control.Monad         (forM, liftM)
import           Data.Char             hiding (isSpace)
import           Data.List             (reverse)
import           GHC.Exts              (sortWith)
import           GHC.Exts              hiding (Word)
import           Prelude               hiding (words)
import           System.Environment
import           System.TimeIt
import           Text.Printf

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

data Word = Word { word  :: String,
                   count :: Int    }

instance Storable Word where
    peek ptr = do
        let chrP = (castPtr ptr)    :: CString
        let intP = (plusPtr ptr 20) :: Ptr CInt
        word  <- peekCString chrP
        count <- peek intP
        return $ Word {word = word, count = fromIntegral count}
    poke ptr w = do return () -- do nothing as not needed
    sizeOf _ = 24
    alignment _ = 4

foreign import ccall "counterlib.h counts" c_counts :: CString -> Ptr(Ptr Word) -> Ptr () -> IO CInt

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
