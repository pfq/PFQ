{-# LANGUAGE ForeignFunctionInterface #-}

module PFq 
    (
        PFqTag,
        open
    ) where

import Control.Monad (when)

import Foreign.Ptr (Ptr, plusPtr, nullPtr, FunPtr)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types

import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr)

newtype PFqTag = PFqTag ()

--- open socket:

open :: Int  --
     -> Int  --
     -> Int  --
     -> IO (ForeignPtr PFqTag)

open caplen offset slots = do
        ptr <- pfq_open (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots)
        when (ptr == nullPtr) $
            ioError $ userError "Can't open pfq socket"
        newForeignPtr ptr (pfq_close ptr) 


-- C function
--
foreign import ccall unsafe pfq_open  :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_close :: Ptr PFqTag -> IO ()

