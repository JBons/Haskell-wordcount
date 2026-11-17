-- Interface to a fast C-library counting frequencies of words in a string.
-- ASCII letters a-z and A-Z form words; upper case is folded to lower case.
-- All other bytes, including embedded NULs, are word separators.

-- The required C-library built and linked by Stack/Cabal.
-- To mandually build the C library and link to a stand-alone Haskell app:
--      gcc -dynamiclib -fPIC -O3 -o lib.so libCounterlib.c
--      ghci lib.so
--      ghc -O2 wc.hs lib.so


{-# LANGUAGE ForeignFunctionInterface #-}

module CCounterLib where

import           Prelude               hiding (Word)

import           Control.Exception     (bracket)
import           Data.ByteString.Char8 (ByteString, useAsCStringLen)
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
-- Mapping of the Haskell Word type to the C Struct memory layout.
-- These offsets/sizes match MAXWORDLENGTH=20 and a 4-byte C int; keep them
-- synchronized with include/libCounterlib.h if the production layout changes.
instance Storable Word where
    peek ptr = do
        let chrP = castPtr ptr    :: CString
        let intP = plusPtr ptr 20 :: Ptr CInt
        text <- peekCString chrP
        n <- peek intP
        pure Word {word = text, count = fromIntegral n}
    poke _ _ = undefined -- Reading only: writing Word records is not supported.
    sizeOf _ = 24
    alignment _ = 4

-- Keep the original NUL-terminated entry point available to existing callers.
foreign import ccall "libCounterlib.h counts" c_counts :: CString -> Ptr(Ptr Word) -> Ptr () -> IO CInt
-- The ByteString wrapper uses its byte length, rather than stopping at a NUL.
foreign import ccall "libCounterlib.h counts_n" c_counts_n :: CString -> CSize -> Ptr (Ptr Word) -> Ptr Word -> IO CInt
-- Error messages are borrowed C string literals and must not be freed.
foreign import ccall "libCounterlib.h counts_error" c_counts_error :: CInt -> IO CString

-- Build list of words in a text together with their frequencies
-- Uses unsafePerformIO to expose counting as a pure function. The C routine
-- owns its mutable state per call; no pointers into that state escape.
-- Capacity/allocation failures raise a descriptive exception, not a partial
-- result. Both output buffers are released even when an exception is raised.
{-# NOINLINE counts #-}
counts :: ByteString -> [(String,Int)]
counts s = unsafePerformIO $ useAsCStringLen s $ \(cs, len) ->
    -- Allocate the pointer array and Word heap required by MAXUNIQUEWORDS.
    -- mallocArray uses the Storable sizes; initialization is done by C.
    bracket (mallocArray 50000) free $ \wpa ->
    bracket (mallocArray 50000) free $ \heap -> do
        wc <- c_counts_n cs (fromIntegral len) wpa heap
        -- A negative status must never be passed to peekArray as a length.
        if wc < 0
            then do
                message <- c_counts_error wc >>= peekCString
                ioError (userError ("CCounterLib.counts: " ++ message))
            else do
                -- Read the records and copy their strings before bracket frees
                -- the caller-owned C buffers. The returned pairs contain no Ptrs.
                plist <- peekArray (fromIntegral wc) wpa
                wordlist <- mapM peek plist
                pure [(word w, count w) | w <- wordlist]
