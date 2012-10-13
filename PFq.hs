{-# LANGUAGE ForeignFunctionInterface #-}

module PFq 
    (
        PFqTag,
        open,
        bind,
        enable,
        readQ,
        NetQueue(..)
    ) where

import Control.Monad (when)

import Foreign.Ptr 
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

import Foreign.Concurrent as C (newForeignPtr) 
import Foreign.ForeignPtr (ForeignPtr)

newtype PFqTag = PFqTag ()

newtype PFqQueue = PFqQueue ()

-- NetQueue:
--

data NetQueue = NetQueue {
                    queuePtr    :: Ptr PFqQueue
                 ,  queueLen    :: CSize
                 ,  queueSlot   :: CSize
                 ,  queueIndex  :: CUInt
                } deriving (Show)

--- open socket:

open :: Int  --
     -> Int  --
     -> Int  --
     -> IO (ForeignPtr PFqTag)


open caplen offset slots = do
        ptr <- pfq_open (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots)
        when (ptr == nullPtr) $
            ioError $ userError "PFq: Can't open socket"
        C.newForeignPtr ptr (pfq_close ptr) 

-- bind:
--

bind :: Ptr PFqTag 
     -> String      -- device name
     -> Int         -- queue index
     -> IO ()

bind hdl name queue = do
    withCString name $ \dev -> do
        rv <- pfq_bind hdl dev (fromIntegral queue) 
        when (rv == -1) $
            ioError $ userError "PFq: Could not bind"
        return ()

-- enable:
--

enable :: Ptr PFqTag -> IO ()
enable hdl = do
             rv <- pfq_enable hdl
             when (rv == -1) $
                ioError $ userError "PFq: Could not enable"
             return ()

-- read:
--

readQ :: Ptr PFqTag -> Int -> IO NetQueue
readQ hdl msec = 
    allocaBytes ((32)) $ \queue -> do
       rv <- pfq_read hdl queue (fromIntegral msec)  
       when (rv == -1) $
            ioError $ userError "PFq: read error"
       qptr <- ((\h -> peekByteOff h 0)) queue
       clen <- ((\h -> peekByteOff h 8)) queue
       css  <- ((\h -> peekByteOff h 16)) queue
       cid  <- ((\h -> peekByteOff h 24)) queue
       return NetQueue { queuePtr   = qptr :: Ptr PFqQueue,
                         queueLen   = clen :: CSize,
                         queueSlot  = css  :: CSize,
                         queueIndex = cid  :: CUInt }
-- C function
--
foreign import ccall unsafe pfq_open   :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_close  :: Ptr PFqTag -> IO ()
foreign import ccall unsafe pfq_enable :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_bind   :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_read   :: Ptr PFqTag -> Ptr NetQueue -> CLong -> IO CInt

