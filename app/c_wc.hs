{- Build:
 - gcc -dynamiclib -fPIC -O3 -o lib.so counterlib.c
 - ghci lib.so
 - ghc -O2 wc.hs lib.so
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Data.List             (sortOn)
import           Control.Exception     (bracket)
import           Prelude               hiding (Word)
import           System.Environment
import           System.TimeIt
import           Text.Printf           (printf)
import           Text.Read             (readMaybe)

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
        text <- peekCString chrP
        n <- peek intP
        pure Word {word = text, count = fromIntegral n}
    poke _ _ = pure () -- do nothing as not needed
    sizeOf _ = 24
    alignment _ = 4

foreign import ccall "counterlib.h counts" c_counts :: CString -> Ptr(Ptr Word) -> Ptr () -> IO CInt
foreign import ccall "counterlib.h counts_error" c_counts_error :: CInt -> IO CString

-- Words of a text together with their frequencies, sorted highest->lowest
-- withCAString retains the original 8-bit, NUL-terminated input convention.
-- bracket releases both output buffers on success and on a C/error exception.
wordFreqs :: String ->  IO [(String,Int)]
wordFreqs s = withCAString s $ \cs ->
    bracket (mallocArray 50000) free $ \wpa ->
    -- MAXUNIQUEWORDS Word records, each 24 bytes with the current C layout.
    bracket (mallocBytes (50000 * 24)) free $ \heap -> do
        wc <- c_counts cs wpa heap
        -- Negative values are CounterError codes, not array lengths.
        if wc < 0
            then do
                message <- c_counts_error wc >>= peekCString
                ioError (userError ("c_wc: " ++ message))
            else do
                plist <- peekArray (fromIntegral wc) wpa
                wordlist <- mapM peek plist
                pure (reverse (sortOn snd [(word w, count w) | w <- wordlist]))

main :: IO ()
main = timeIt proc --get running time

--Actual main UI task
proc :: IO ()
proc = do
    args <- getArgs
    case args of
        rawN : filename : _
            | Just n <- readMaybe rawN :: Maybe Integer
            , n >= 0, n <= toInteger (maxBound :: Int) -> do
                text <- readFile filename
                result <- wordFreqs text
                display (take (fromInteger n) result)
        _ -> fail "Usage: c_wc lines filename"

--Pretty-print the results
display :: [(String, Int)] -> IO ()
display = mapM_ (putStrLn . disp) where
    disp (x,y) = printf "%15s :   %4d" x y
