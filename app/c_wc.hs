{- Build:
 - gcc -dynamiclib -fPIC -O3 -o lib.so counterlib.c
 - ghci lib.so
 - ghc -O2 wc.hs lib.so
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Data.List             (sortOn)
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
    let result = reverse $ sortOn snd raw
    pure result

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
