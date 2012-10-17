{-# LINE 1 "PFq.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "PFq.hsc" #-}

module Network.PFq 
    (
        PFqTag,
        open,
        openNoGroup,
        openGroup,
        enable,
        disable,
        getId,
        getGroupId,
        isEnabled,
        bind,
        bindGroup,
        unbind,
        unbindGroup,
        joinGroup,
        leaveGroup,
        setTimestamp,
        getTimestamp,
        setPromisc,
        getCaplen,
        setCaplen,
        getOffset,
        setOffset,
        getSlots,
        setSlots,
        getSlotSize,
        Network.PFq.read,
        dispatch,
        getStats,
        getGroupStats,
        steeringFunction,
        getHeaders,
        getPackets,
        NetQueue(..),
        Callback,
        class_default,
        class_any,
        policy_undefined,
        policy_priv, 
        policy_restricted,
        policy_shared,
        Packet
    ) where

import Data.Word
import Data.Bits

import Foreign.Ptr 
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Concurrent as C (newForeignPtr) 
import Foreign.ForeignPtr (ForeignPtr)

newtype PFqTag = PFqTag ()


{-# LINE 62 "PFq.hsc" #-}

-- NetQueue:
--

data NetQueue = NetQueue {
      queuePtr        :: Ptr PktHdr
   ,  queueLen        :: {-# UNPACK #-} !Word64
   ,  queueSlotSize   :: {-# UNPACK #-} !Word64
   ,  queueIndex      :: {-# UNPACK #-} !Word32
   } deriving (Eq, Show)

-- PktHdr:
--

data PktHdr = PktHdr {
      hdrSec      :: {-# UNPACK #-} !Word32
    , hdrNsec     :: {-# UNPACK #-} !Word32
    , hdrGid      :: {-# UNPACK #-} !Word32
    , hdrIfIndex  :: {-# UNPACK #-} !Word32
    , hdrLen      :: {-# UNPACK #-} !Word16
    , hdrCapLen   :: {-# UNPACK #-} !Word16
    , hdrTci      :: {-# UNPACK #-} !Word16
    , hdrHwQueue  :: {-# UNPACK #-} !Word8
    , hdrCommit   :: {-# UNPACK #-} !Word8 
    } deriving (Eq, Show)

-- Statistics:

data Statistics = Statistics {
      statReceived     :: {-# UNPACK #-} !Word32    -- packets received
    , statLost         :: {-# UNPACK #-} !Word32    -- packets lost 
    , statDropped      :: {-# UNPACK #-} !Word32    -- packets dropped 
    } deriving (Eq, Show)


newtype ClassMask = ClassMask { unClassMask :: CLong }
                        deriving (Eq, Show)

newtype GroupPolicy = GroupPolicy { unGroupPolicy :: CInt }
                        deriving (Eq, Show)

class_default  :: ClassMask
class_default  = ClassMask 1
class_any      :: ClassMask
class_any      = ClassMask 15

{-# LINE 107 "PFq.hsc" #-}

combineClassMasks :: [ClassMask] -> ClassMask
combineClassMasks = ClassMask . foldr ((.|.) . unClassMask) 0

policy_undefined   :: GroupPolicy
policy_undefined   = GroupPolicy 0
policy_priv        :: GroupPolicy
policy_priv        = GroupPolicy 1
policy_restricted  :: GroupPolicy
policy_restricted  = GroupPolicy 2
policy_shared      :: GroupPolicy
policy_shared      = GroupPolicy 3

{-# LINE 117 "PFq.hsc" #-}


--

toPktHdr :: Ptr PktHdr -> IO PktHdr
toPktHdr hdr = do
    _sec  <- ((\h -> peekByteOff h 0))  hdr
    _nsec <- ((\h -> peekByteOff h 4))  hdr
    _ifid <- ((\h -> peekByteOff h 8))  hdr
    _gid  <- ((\h -> peekByteOff h 12)) hdr
    _len  <- ((\h -> peekByteOff h 16)) hdr
    _cap  <- ((\h -> peekByteOff h 18)) hdr
    _tci  <- ((\h -> peekByteOff h 20)) hdr
    _hwq  <- ((\h -> peekByteOff h 22)) hdr
    _com  <- ((\h -> peekByteOff h 23)) hdr
    return PktHdr { 
                    hdrSec      = fromIntegral (_sec  :: CUInt),
                    hdrNsec     = fromIntegral (_nsec :: CUInt),
                    hdrIfIndex  = fromIntegral (_ifid :: CInt),
                    hdrGid      = fromIntegral (_gid  :: CInt),
                    hdrLen      = fromIntegral (_len  :: CUShort),
                    hdrCapLen   = fromIntegral (_cap  :: CUShort),
                    hdrTci      = fromIntegral (_tci  :: CUShort),
                    hdrHwQueue  = fromIntegral (_hwq  :: CUChar),
                    hdrCommit   = fromIntegral (_com  :: CUChar)
                  }

-- type of the callback function passed to 'dispatch' 

type Callback  = PktHdr -> Ptr Word8  -> IO ()

type CPFqCallback = Ptr PktHdr -> Ptr Word8 -> Ptr Word8 -> IO ()
               
-- error handling 
--

throwPFqIf :: Ptr PFqTag -> (a -> Bool) -> a -> IO a
throwPFqIf hdl p v = if p v
    then pfq_error hdl >>= peekCString >>= ioError . userError
    else return v


throwPFqIf_ :: Ptr PFqTag -> (a -> Bool) -> a -> IO ()
throwPFqIf_ hdl p v = throwPFqIf hdl p v >> return ()

type Packet = (PktHdr, Ptr Word8)

-- getHeaders: obtain a list of PktHdr from a NetQueue
--

getHeaders :: NetQueue -> IO [PktHdr]
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


getPackets :: NetQueue -> IO [Packet]
getPackets queue = getPackets' (queuePtr queue) (queuePtr queue `plusPtr` q_size) (fromIntegral $ queueSlotSize queue)
                    where q_slot = fromIntegral $ queueSlotSize queue 
                          q_len  = fromIntegral $ queueLen queue
                          q_size = q_slot * q_len

getPackets' :: Ptr PktHdr -> Ptr PktHdr -> Int -> IO [Packet]
getPackets' cur end slotSize 
    | cur == end = return []
    | otherwise  = do
        h <- toPktHdr cur 
        let p = cur `plusPtr` 24 :: Ptr Word8    
        l <- getPackets' (cur `plusPtr` slotSize) end slotSize 
        return ( (h, p) : l )

-- open:
--

open :: Int  --
     -> Int  --
     -> Int  --
     -> IO (ForeignPtr PFqTag)

open caplen offset slots = do
        pfq_open (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (pfq_close ptr) 

-- openNoGroup:
--
openNoGroup:: Int  --
     -> Int  --
     -> Int  --
     -> IO (ForeignPtr PFqTag)

openNoGroup caplen offset slots = do
        pfq_open_nogroup (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (pfq_close ptr) 


-- openGroup:
--
openGroup :: [ClassMask]  --
          -> GroupPolicy  -- 
          -> Int  --
          -> Int  --
          -> Int  --
          -> IO (ForeignPtr PFqTag)

openGroup ms policy caplen offset slots = do
        pfq_open_group (unClassMask $ combineClassMasks ms) (unGroupPolicy policy) (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (pfq_close ptr) 

-- bind:
--
bind :: Ptr PFqTag 
     -> String      -- device name
     -> Int         -- queue index
     -> IO ()

bind hdl name queue = do
    withCString name $ \dev -> do
        pfq_bind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1) 

-- bindGroup:
--
bindGroup :: Ptr PFqTag
          -> Int         -- group id
          -> String      -- device name
          -> Int         -- queue index
          -> IO ()

bindGroup hdl gid name queue = do
    withCString name $ \dev -> do
        pfq_bind_group hdl (fromIntegral gid) dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1) 


-- unbind:
--
unbind :: Ptr PFqTag 
       -> String      -- device name
       -> Int         -- queue index
       -> IO ()

unbind hdl name queue = do
    withCString name $ \dev -> do
        pfq_unbind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1) 

-- unbindGroup:
--
unbindGroup :: Ptr PFqTag
            -> Int         -- group id
            -> String      -- device name
            -> Int         -- queue index
            -> IO ()

unbindGroup hdl gid name queue = do
    withCString name $ \dev -> do
        pfq_unbind_group hdl (fromIntegral gid) dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1) 

-- joinGroup:
--

joinGroup :: Ptr PFqTag
          -> Int            -- group id
          -> [ClassMask]    -- 
          -> GroupPolicy    -- group policy
          -> IO ()

joinGroup hdl gid ms pol =
    pfq_join_group hdl (fromIntegral gid) (unClassMask $ combineClassMasks ms) (unGroupPolicy pol) 
        >>= throwPFqIf_ hdl (== -1)  

-- leaveGroup:
-- 

leaveGroup :: Ptr PFqTag
           -> Int        -- group id
           -> IO ()

leaveGroup hdl gid =
    pfq_leave_group hdl (fromIntegral gid) 
        >>= throwPFqIf_ hdl (== -1)  

-- getId:
--

getId :: Ptr PFqTag -> IO Int
getId hdl = 
    pfq_id hdl >>= throwPFqIf hdl (== -1) >>= return . fromIntegral 


-- getGroupId:
--

getGroupId :: Ptr PFqTag -> IO Int
getGroupId hdl = 
    pfq_group_id hdl >>= throwPFqIf hdl (== -1) >>= return . fromIntegral


-- enable:
--

enable :: Ptr PFqTag -> IO ()
enable hdl = 
    pfq_enable hdl >>= throwPFqIf_ hdl (== -1)


-- disable:
--

disable :: Ptr PFqTag -> IO ()
disable hdl = pfq_disable hdl >>= throwPFqIf_ hdl (== -1)

-- isEnabled:
--

isEnabled :: Ptr PFqTag -> IO Bool
isEnabled hdl = 
    pfq_is_enabled hdl >>= throwPFqIf hdl (== -1) >>= \v ->
        if (v == 0) then return False else return True


-- setPromisc:
--

setPromisc :: Ptr PFqTag -> String -> Bool -> IO ()
setPromisc hdl name value = 
    withCString name $ \dev -> do
        pfq_set_promisc hdl dev (if value then 1 else 0) >>=
            throwPFqIf_ hdl (== -1)

-- read:
--

read :: Ptr PFqTag 
     -> Int 
     -> IO NetQueue

read hdl msec = 
    allocaBytes ((24)) $ \queue -> do
       pfq_read hdl queue (fromIntegral msec) >>= throwPFqIf_ hdl (== -1) 
       _ptr <- ((\h -> peekByteOff h 0))  queue
       _len <- ((\h -> peekByteOff h (sizeOf _ptr)))  queue
       _css <- ((\h -> peekByteOff h (sizeOf _ptr + sizeOf _len))) queue
       _cid <- ((\h -> peekByteOff h (sizeOf _ptr + sizeOf _len + sizeOf _css))) queue
       let slotSize'= fromIntegral(_css :: CSize)
       let slotSize = slotSize' + slotSize' `mod` 8
       return NetQueue { queuePtr       = _ptr :: Ptr PktHdr,
                         queueLen       = fromIntegral (_len :: CSize),
                         queueSlotSize  = slotSize,
                         queueIndex     = fromIntegral (_cid  :: CUInt) 
                       }
-- setTimestamp:
--

setTimestamp :: Ptr PFqTag -> Bool -> IO ()

setTimestamp hdl toggle = do
    let value = if (toggle) then 1 else 0 
    pfq_set_timestamp hdl value >>= throwPFqIf_ hdl (== -1)


-- getTimestamp:
--
getTimestamp :: Ptr PFqTag -> IO Bool 

getTimestamp hdl = do
    pfq_get_timestamp hdl >>= throwPFqIf hdl (== -1) >>= \v ->
        if (v == 0) then return False else return True


-- setCaplen:
--
setCaplen:: Ptr PFqTag -> Int -> IO ()

setCaplen hdl value = do
    pfq_set_caplen hdl (fromIntegral value) 
        >>= throwPFqIf_ hdl (== -1)


-- getCaplen:
--
getCaplen :: Ptr PFqTag -> IO Int  

getCaplen hdl = 
    pfq_get_caplen hdl >>= throwPFqIf hdl (== -1) 
        >>= return . fromIntegral


-- setOffset:
--
setOffset:: Ptr PFqTag -> Int -> IO ()

setOffset hdl value = 
    pfq_set_offset hdl (fromIntegral value) >>= 
    throwPFqIf_ hdl (== -1)


-- getOffset:
--
getOffset :: Ptr PFqTag -> IO Int  

getOffset hdl = 
    pfq_get_offset hdl >>= throwPFqIf hdl (== -1) >>= 
    return . fromIntegral


-- setSlots:
--
setSlots:: Ptr PFqTag -> Int -> IO ()

setSlots hdl value = 
    pfq_set_slots hdl (fromIntegral value) 
    >>= throwPFqIf_ hdl (== -1)

-- getSlots:
--
getSlots :: Ptr PFqTag -> IO Int  

getSlots hdl = 
    pfq_get_slots hdl >>= throwPFqIf hdl (== -1) 
        >>= return . fromIntegral

-- getSlotSize:
--
getSlotSize :: Ptr PFqTag -> IO Int  

getSlotSize hdl = 
    pfq_get_slot_size hdl >>= throwPFqIf hdl (== -1) 
        >>= return . fromIntegral


-- getStats:
--

getStats :: Ptr PFqTag 
         -> IO Statistics
         
getStats hdl =
    allocaBytes ((sizeOf (0 :: CLong)) * 3) $ \sp -> do
        pfq_get_stats hdl sp >>= throwPFqIf_ hdl (== -1)
        makeStats sp

-- getGroupStats:
--

getGroupStats :: Ptr PFqTag 
              -> Int            -- gid
              -> IO Statistics
         
getGroupStats hdl gid =
    allocaBytes ((sizeOf (0 :: CLong)) * 3) $ \sp -> do
        pfq_get_group_stats hdl (fromIntegral gid) sp >>= throwPFqIf_ hdl (== -1)
        makeStats sp


makeStats :: Ptr a -> IO Statistics
makeStats p = do
        _recv <- ((\ptr -> peekByteOff ptr 0 )) p
        _lost <- ((\ptr -> peekByteOff ptr (sizeOf _recv) )) p
        _drop <- ((\ptr -> peekByteOff ptr (sizeOf _recv + sizeOf _lost))) p
        return Statistics { 
                            statReceived = fromIntegral (_recv :: CLong),
                            statLost     = fromIntegral (_lost :: CLong),
                            statDropped  = fromIntegral (_drop :: CLong)
                          }

-- steeringFunction:
--

steeringFunction :: Ptr PFqTag
                 -> Int     -- group id
                 -> String  -- steering function name
                 -> IO ()

steeringFunction hdl gid name =
    withCString name $ \fname -> do
        pfq_steering_function hdl (fromIntegral gid) fname >>= throwPFqIf_ hdl (== -1) 


-- dispatch:
-- 

dispatch :: Ptr PFqTag  -- packet capture descriptor
         -> Callback    -- ^ packet processing function
         -> Int         -- ^ timeout
         -> IO ()       -- 


dispatch hdl f timeo = do
    cback <- makeCallback f
    ret  <- pfq_dispatch hdl cback (fromIntegral timeo) nullPtr
    freeHaskellFunPtr cback
    throwPFqIf_ hdl (== (-1 :: Integer)) (fromIntegral ret)


makeCallback :: Callback -> IO (FunPtr CPFqCallback)
makeCallback fun = make_callback $ \chdr ptr _ -> do
    hdr <- toPktHdr chdr
    fun hdr ptr


-- C functions from libpfq.c
--

foreign import ccall unsafe pfq_open              :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_nogroup      :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_group        :: CLong -> CInt -> CSize -> CSize -> CSize -> IO (Ptr PFqTag)

foreign import ccall unsafe pfq_close             :: Ptr PFqTag -> IO ()
foreign import ccall unsafe pfq_error             :: Ptr PFqTag -> IO CString

foreign import ccall unsafe pfq_id                :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_group_id          :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_enable            :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_disable           :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_is_enabled        :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_promisc       :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_set_timestamp     :: Ptr PFqTag -> CInt -> IO CInt
foreign import ccall unsafe pfq_get_timestamp     :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_caplen        :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_caplen        :: Ptr PFqTag -> IO CPtrdiff

foreign import ccall unsafe pfq_set_offset        :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_offset        :: Ptr PFqTag -> IO CPtrdiff 

foreign import ccall unsafe pfq_set_slots         :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_slots         :: Ptr PFqTag -> IO CSize
foreign import ccall unsafe pfq_get_slot_size     :: Ptr PFqTag -> IO CSize

foreign import ccall unsafe pfq_bind              :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_bind_group        :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_unbind            :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_unbind_group      :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_join_group        :: Ptr PFqTag -> CInt -> CLong -> CInt -> IO CInt
foreign import ccall unsafe pfq_leave_group       :: Ptr PFqTag -> CInt -> IO CInt

foreign import ccall unsafe pfq_get_stats         :: Ptr PFqTag -> Ptr Statistics -> IO CInt
foreign import ccall unsafe pfq_get_group_stats   :: Ptr PFqTag -> CInt -> Ptr Statistics -> IO CInt

foreign import ccall unsafe pfq_steering_function :: Ptr PFqTag -> CInt -> CString -> IO CInt

foreign import ccall pfq_dispatch                 :: Ptr PFqTag -> FunPtr CPFqCallback -> CLong -> Ptr Word8 -> IO CInt
foreign import ccall "wrapper" make_callback      :: CPFqCallback -> IO (FunPtr CPFqCallback)

foreign import ccall unsafe pfq_read              :: Ptr PFqTag -> Ptr NetQueue -> CLong -> IO CInt

-- TODO
-- foreign import ccall unsafe pfq_group_state    :: Ptr PFqTag -> CInt -> Ptr CChar -> CSize -> IO CInt

