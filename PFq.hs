{-# LANGUAGE ForeignFunctionInterface #-}

module PFq 
    (
        PFqTag,
        open,
        bind,
        enable,
        setTimestamp,
        readQ,
        getHeaders,
        NetQueue(..)
    ) where

import Data.Word

import Control.Monad (liftM, when)

import Foreign.Ptr 
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

import Foreign.Concurrent as C (newForeignPtr) 
import Foreign.ForeignPtr (ForeignPtr)

import Debug.Trace

newtype PFqTag = PFqTag ()

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
    sec'  <- ((\h -> peekByteOff h 0))  hdr
    nsec' <- ((\h -> peekByteOff h 4))  hdr
    ifid' <- ((\h -> peekByteOff h 8))  hdr
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

-- getHeaders: obtain a list of PktHdr from a NetQueue
--

getHeaders :: NetQueue -> IO [PktHdr]
-- getHeaders queue | trace ("getHeaders: slot_size=" ++ show(queueSlotSize queue) ++ " queue_addr:" ++ show (queuePtr queue) ) False = undefined 
getHeaders queue = getHeaders' (queuePtr queue) (queuePtr queue `plusPtr` q_size) (fromIntegral $ queueSlotSize queue)
                    where q_slot = fromIntegral $ queueSlotSize queue 
                          q_len  = fromIntegral $ queueLen queue
                          q_size = q_slot * q_len

getHeaders' :: Ptr PktHdr -> Ptr PktHdr -> Int -> IO [PktHdr]
getHeaders' cur end slotSize 
    | cur == end = return []
    | otherwise  = do
        h <- toPktHdr cur 
        l <- getHeaders' (cur `plusPtr` slotSize) end slotSize 
        return (h:l)

-- open socket:
--
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
        when (rv == -1) 
            (ioError $ userError "PFq: Could not bind")


-- enable:
--
enable :: Ptr PFqTag 
       -> IO ()

enable hdl = do
             rv <- pfq_enable hdl
             when (rv == -1) 
                (ioError $ userError "PFq: Could not enable")

-- readQ:
--
readQ :: Ptr PFqTag 
      -> Int 
      -> IO NetQueue

readQ hdl msec = 
    allocaBytes ((24)) $ \queue -> do
       rv <- pfq_read hdl queue (fromIntegral msec)  
       when (rv == -1) $
            ioError $ userError "PFq: read error"
       qptr <- ((\h -> peekByteOff h 0))  queue
       clen <- ((\h -> peekByteOff h (sizeOf qptr)))  queue
       css  <- ((\h -> peekByteOff h (sizeOf qptr + sizeOf clen))) queue
       cid  <- ((\h -> peekByteOff h (sizeOf qptr + sizeOf clen + sizeOf css))) queue
       let slotSize'= fromIntegral(css :: CSize)
       let slotSize = slotSize' + slotSize' `mod` 8
       return NetQueue { queuePtr       = qptr :: Ptr PktHdr,
                         queueLen       = fromIntegral (clen :: CSize),
                         queueSlotSize  = slotSize,
                         queueIndex     = fromIntegral (cid  :: CUInt) 
                       }

-- toggleTimestamp:
--
setTimestamp :: Ptr PFqTag 
                -> Bool 
                -> IO ()

setTimestamp hdl toggle = do
    let value = if (toggle) then 1 else 0 
    rv <- pfq_set_timestamp hdl value
    when (rv == -1) 
        (ioError $ userError "PFq: Could not set timestamp")

-- C functions from libpfq.c
--

foreign import ccall unsafe pfq_open            :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
-- foreign import ccall unsafe pfq_open_orphan     :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
-- foreign import ccall unsafe pfq_open_group      :: CLong -> CInt -> CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_close           :: Ptr PFqTag -> IO ()
-- 
foreign import ccall unsafe pfq_enable          :: Ptr PFqTag -> IO CInt
-- foreign import ccall unsafe pfq_disable         :: Ptr PFqTag -> IO CInt
-- foreign import ccall unsafe pfq_is_enabled      :: Ptr PFqTag -> IO CInt
-- 
-- foreign import ccall unsafe pfq_set_promisc     :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_set_timestamp   :: Ptr PFqTag -> CInt -> IO CInt
-- foreign import ccall unsafe pfq_get_timestamp   :: Ptr PFqTag -> IO CInt
-- 
-- foreign import ccall unsafe pfq_set_caplen      :: Ptr PFqTag -> CSize -> IO CInt
-- foreign import ccall unsafe pfq_get_caplen      :: Ptr PFqTag -> CSSize
-- 
-- foreign import ccall unsafe pfq_set_offset      :: Ptr PFqTag -> CSize -> IO CInt
-- foreign import ccall unsafe pfq_get_offset      :: Ptr PFqTag -> CSSize
-- 
-- foreign import ccall unsafe pfq_set_slots       :: Ptr PFqTag -> CSize -> IO CInt
-- foreign import ccall unsafe pfq_get_slots       :: Ptr PFqTag -> CSize
-- foreign import ccall unsafe pfq_get_slot_size   :: Ptr PFqTag -> CSize
-- 
foreign import ccall unsafe pfq_bind            :: Ptr PFqTag -> CString -> CInt -> IO CInt
-- foreign import ccall unsafe pfq_bind_group      :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt
-- 
-- foreign import ccall unsafe pfq_unbind          :: Ptr PFqTag -> CString -> CInt -> IO CInt
-- foreign import ccall unsafe pfq_unbind_group    :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt
-- 
-- foreign import ccall unsafe pfq_steering_function :: Ptr PFqTag -> CInt -> CString -> IO CInt
-- foreign import ccall unsafe pfq_group_state       :: Ptr PFqTag -> CInt -> Ptr CChar -> CSize -> IO CInt
-- 
-- foreign import ccall unsafe pfq_poll            :: Ptr PFqTag -> CLong -> IO CInt
-- 
-- foreing import ccall unsafe pfq_id              :: Ptr PFqTag -> IO CInt
-- foreing import ccall unsafe pfq_group_id        :: Ptr PFqTag -> IO CInt
-- 
-- 
foreign import ccall unsafe pfq_read            :: Ptr PFqTag -> Ptr NetQueue -> CLong -> IO CInt


-- extern int pfq_dispatch(pfq_t *q, pfq_handler_t cb, long int microseconds, char *user);
-- extern int pfq_get_stats(pfq_t const *q, struct pfq_stats *stats); 
-- extern int pfq_get_group_stats(pfq_t const *q, int gid, struct pfq_stats *stats); 





