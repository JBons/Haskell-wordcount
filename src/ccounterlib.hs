-- Interface to a fast C-library counting frequencies of words in a string.
-- Due to the (current) limitations of the C-code, only letters a-z are
-- accepted as parts of words, all else is treated as word-separating white
-- space.

-- The required C-library built and linked by Stack/Cabal.
-- To mandually build the C library and link to a stand-alone Haskell app:
--      gcc -dynamiclib -fPIC -O3 -o lib.so libCounterlib.c
--      ghci lib.so
--      ghc -O2 wc.hs lib.so


{-# LANGUAGE ForeignFunctionInterface #-}

module CCounterLib where

import           Prelude               hiding (Word)

import           Data.ByteString.Char8 (ByteString, useAsCString)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe

-- Type to capture the C Structs returned by the C library function
data Word = Word { word  :: String,
                   count :: Int    }
-- Mapping of the Haskell Word type to the C Struct memory layout
instance Storable Word where
    peek ptr = do
        let chrP = castPtr ptr    :: CString
        let intP = plusPtr ptr 20 :: Ptr CInt
        word  <- peekCString chrP
        count <- peek intP
        return Word {word = word, count = fromIntegral count}
    poke ptr w = undefined -- do nothing as not needed
    sizeOf _ = 24
    alignment _ = 4

-- Specification of the FFI call to the C library
foreign import ccall "libCounterlib.h counts" c_counts :: CString -> Ptr(Ptr Word) -> Ptr () -> IO CInt

-- Build list of words in a text together with their frequencies
-- Using C libabry "Counterlib" via FFI
-- Uses unsafePerformIO to bring the (pure) results back from IO monad.
{-# NOINLINE counts #-}
counts :: ByteString -> [(String,Int)]
counts s = unsafePerformIO $ useAsCString s $ \cs -> do
    -- Allocate for return array of pointers to words
    wpa  <- mallocArray 50000 :: IO (Ptr (Ptr Word))
    -- Allocate for the Word structures
    heap <- mallocBytes(50000 * 24)

    wc <- c_counts cs wpa heap
    let size = fromIntegral wc
    plist <- peekArray size wpa
    wordlist <- mapM peek plist
    let result = map (\w -> (word w, count w)) wordlist
    free heap
    free wpa
    return result
