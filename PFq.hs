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
                    queuePtr        :: Ptr PktHdr
                 ,  queueLen        :: Word64
                 ,  queueSlotSize   :: Word64
                 ,  queueIndex      :: Word32
                } deriving (Eq, Show)

-- PktHdr:
--
--- open socket:

data PktHdr = PktHdr {
                hdrSec      ::  Word32,
                hdrNsec     ::  Word32,
                hdrGid      ::  Word32,
                hdrIfIndex  ::  Word32,
                hdrLen      ::  Word16,
                hdrCapLen   ::  Word16,
                hdrTci      ::  Word16,
                hdrHwQueue  ::  Word8,
                hdrCommit   ::  Word8 
              } deriving (Eq, Show)


toPktHdr :: Ptr PktHdr -> IO PktHdr
toPktHdr hdr = do
    sec'  <- ((\h -> peekByteOff h 0)) hdr
    nsec' <- ((\h -> peekByteOff h 4)) hdr
    ifid' <- ((\h -> peekByteOff h 8)) hdr
    gid'  <- ((\h -> peekByteOff h 12)) hdr
    len'  <- ((\h -> peekByteOff h 16)) hdr
    cap'  <- ((\h -> peekByteOff h 18)) hdr
    tci'  <- ((\h -> peekByteOff h 20)) hdr
    hwq'  <- ((\h -> peekByteOff h 22)) hdr
    com'  <- ((\h -> peekByteOff h 23)) hdr
    return PktHdr { 
                    hdrSec      = fromIntegral (sec'  :: CUInt),
                    hdrNsec     = fromIntegral (nsec' :: CUInt),
                    hdrIfIndex  = fromIntegral (ifid' :: CInt),
                    hdrGid      = fromIntegral (gid'  :: CInt),
                    hdrLen      = fromIntegral (len'  :: CUShort),
                    hdrCapLen   = fromIntegral (cap'  :: CUShort),
                    hdrTci      = fromIntegral (tci'  :: CUShort),
                    hdrHwQueue  = fromIntegral (hwq'  :: CUChar),
                    hdrCommit   = fromIntegral (com'  :: CUChar)
                  }
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

