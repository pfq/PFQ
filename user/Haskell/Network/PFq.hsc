--
--    Copyright (c) 2011-2013, Nicola Bonelli
--    All rights reserved.
--
--    Redistribution and use in source and binary forms, with or without
--    modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--    * Neither the name of University of Pisa nor the names of its contributors
--      may be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--    POSSIBILITY OF SUCH DAMAGE.
--
--

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.PFq
    (
        PFqTag,
        open,
        openNoGroup,
        openGroup,
        close,
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
        getMaxlen,
        setMaxlen,
        getOffset,
        setOffset,

        getTxSlots,
        setTxSlots,

        getRxSlots,
        setRxSlots,
        getRxSlotSize,

        Network.PFq.read,
        dispatch,
        getStats,
        getGroupStats,

        getPackets,
        getHeader,
        isPacketReady,
        waitForPacket,

        vlanFiltersEnabled,
        vlanSetFilterId,
        vlanResetFilterId,

        groupFunction,
        putContextFunction,
        getContextFunction,

        bindTx,
        startTxThread,
        stopTxThread,
        wakeupTxThread,
        txQueueFlush,

        inject,
        send,
        send_async,

        -- groupComputation,

        --- data

        NetQueue(..),
        Packet(..),
        Callback,

        class_default,
        class_any,
        policy_undefined,
        policy_priv,
        policy_restricted,
        policy_shared,
    ) where


import Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Unsafe

-- import Data.Maybe
-- import Debug.Trace

import Control.Monad
import Control.Concurrent

import Foreign.Ptr
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Concurrent as C (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr)

-- Placeholders
--

newtype PFqTag = PFqTag ()


#include <pfq/pfq.h>

-- NetQueue:
--

data NetQueue = NetQueue {
      qPtr        :: Ptr PktHdr
   ,  qLen        :: {-# UNPACK #-} !Word64
   ,  qSlotSize   :: {-# UNPACK #-} !Word64
   ,  qIndex      :: {-# UNPACK #-} !Word32
   } deriving (Eq, Show)

-- PktHdr:
--

data PktHdr = PktHdr {
      hData     :: {-# UNPACK #-} !Word64
    , hSec      :: {-# UNPACK #-} !Word32
    , hNsec     :: {-# UNPACK #-} !Word32
    , hIfIndex  :: {-# UNPACK #-} !Word32
    , hGid      :: {-# UNPACK #-} !Word32
    , hLen      :: {-# UNPACK #-} !Word16
    , hCapLen   :: {-# UNPACK #-} !Word16
    , hTci      :: {-# UNPACK #-} !Word16
    , hHwQueue  :: {-# UNPACK #-} !Word8
    , hCommit   :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

-- Statistics:

data Statistics = Statistics {
      sReceived     :: {-# UNPACK #-} !Word32    -- packets received
    , sLost         :: {-# UNPACK #-} !Word32    -- packets lost
    , sDropped      :: {-# UNPACK #-} !Word32    -- packets dropped
    } deriving (Eq, Show)


-- Packet:

data Packet = Packet {
      pHdr   :: Ptr PktHdr
   ,  pData  :: Ptr Word8
   ,  pIndex :: !Word32
   } deriving (Eq, Show)


newtype ClassMask = ClassMask { unClassMask :: CUInt }
                        deriving (Eq, Show)

newtype GroupPolicy = GroupPolicy { unGroupPolicy :: CInt }
                        deriving (Eq, Show)


#{enum ClassMask, ClassMask
    , class_default = Q_CLASS_DEFAULT
    , class_any     = Q_CLASS_ANY
}


combineClassMasks :: [ClassMask] -> ClassMask
combineClassMasks = ClassMask . foldr ((.|.) . unClassMask) 0


#{enum GroupPolicy, GroupPolicy
    , policy_undefined  = Q_GROUP_UNDEFINED
    , policy_priv       = Q_GROUP_PRIVATE
    , policy_restricted = Q_GROUP_RESTRICTED
    , policy_shared     = Q_GROUP_SHARED
}


--

toPktHdr :: Ptr PktHdr -> IO PktHdr
toPktHdr hdr = do
    _data <- (\h -> peekByteOff h 0)  hdr
    _sec  <- (\h -> peekByteOff h 8)  hdr
    _nsec <- (\h -> peekByteOff h 12) hdr
    _ifid <- (\h -> peekByteOff h 16) hdr
    _gid  <- (\h -> peekByteOff h 20) hdr
    _len  <- (\h -> peekByteOff h 24) hdr
    _cap  <- (\h -> peekByteOff h 26) hdr
    _tci  <- (\h -> peekByteOff h 28) hdr
    _hwq  <- (\h -> peekByteOff h 30) hdr
    _com  <- (\h -> peekByteOff h 31) hdr
    return PktHdr {
                    hData     = fromIntegral (_data :: Word64),
                    hSec      = fromIntegral (_sec  :: Word32),
                    hNsec     = fromIntegral (_nsec :: Word32),
                    hIfIndex  = fromIntegral (_ifid :: CInt),
                    hGid      = fromIntegral (_gid  :: CInt),
                    hLen      = fromIntegral (_len  :: CUShort),
                    hCapLen   = fromIntegral (_cap  :: CUShort),
                    hTci      = fromIntegral (_tci  :: CUShort),
                    hHwQueue  = fromIntegral (_hwq  :: CUChar),
                    hCommit   = fromIntegral (_com  :: CUChar)
                  }

-- type of the callback function passed to 'dispatch'

type Callback  = PktHdr -> Ptr Word8  -> IO ()

type CPFqCallback = Ptr Word8 -> Ptr PktHdr -> Ptr Word8 -> IO ()

-- error handling
--

throwPFqIf :: Ptr PFqTag -> (a -> Bool) -> a -> IO a
throwPFqIf hdl p v = if p v
    then pfq_error hdl >>= peekCString >>= ioError . userError
    else return v


throwPFqIf_ :: Ptr PFqTag -> (a -> Bool) -> a -> IO ()
throwPFqIf_ hdl p v = throwPFqIf hdl p v >> return ()


getPackets :: NetQueue -> IO [Packet]
getPackets nq = getPackets' (qIndex nq) (qPtr nq) (qPtr nq `plusPtr` _size) (fromIntegral $ qSlotSize nq)
                    where _slot = fromIntegral $ qSlotSize nq
                          _len  = fromIntegral $ qLen nq
                          _size = _slot * _len


getPackets' :: Word32 -> Ptr PktHdr -> Ptr PktHdr -> Int -> IO [Packet]
getPackets' index cur end slotSize
    | cur == end = return []
    | otherwise  = do
        let h = cur :: Ptr PktHdr
        let p = cur `plusPtr` 24 :: Ptr Word8
        l <- getPackets' index (cur `plusPtr` slotSize) end slotSize
        return ( Packet h p index : l )


isPacketReady :: Packet -> IO Bool
isPacketReady p = do
    !_com  <- ((\h -> peekByteOff h 31)) (pHdr p)
    return ((_com :: CUChar) == (fromIntegral $ pIndex p))

{-# INLINE isPacketReady #-}


waitForPacket :: Packet -> IO ()
waitForPacket p = do
    !ready <- isPacketReady p
    when (not ready) $ yield >> waitForPacket p

{-# INLINE waitForPacket #-}


getHeader :: Packet -> IO PktHdr
getHeader p = waitForPacket p >> toPktHdr (pHdr p)

{-# INLINE getHeader #-}


-- open:
--

open :: Int  --
     -> Int  --
     -> Int  --
     -> IO (ForeignPtr PFqTag)

open caplen offset slots =
        pfq_open (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)


close :: Ptr PFqTag -> IO ()
close hdl =
    pfq_close hdl >>= throwPFqIf_ hdl (== -1)


-- openNoGroup:
--
openNoGroup:: Int  --
     -> Int  --
     -> Int  --
     -> IO (ForeignPtr PFqTag)

openNoGroup caplen offset slots =
        pfq_open_nogroup (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)

-- openGroup:
--
openGroup :: [ClassMask]  --
          -> GroupPolicy  --
          -> Int  --
          -> Int  --
          -> Int  --
          -> IO (ForeignPtr PFqTag)

openGroup ms policy caplen offset slots =
        pfq_open_group (unClassMask $ combineClassMasks ms) (unGroupPolicy policy) (fromIntegral caplen) (fromIntegral offset) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)

-- bind:
--
bind :: Ptr PFqTag
     -> String      -- device name
     -> Int         -- queue index
     -> IO ()

bind hdl name queue =
    withCString name $ \dev ->
        pfq_bind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)

-- bindGroup:
--
bindGroup :: Ptr PFqTag
          -> Int         -- group id
          -> String      -- device name
          -> Int         -- queue index
          -> IO ()

bindGroup hdl gid name queue =
    withCString name $ \dev ->
        pfq_bind_group hdl (fromIntegral gid) dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- unbind:
--
unbind :: Ptr PFqTag
       -> String      -- device name
       -> Int         -- queue index
       -> IO ()

unbind hdl name queue =
    withCString name $ \dev ->
        pfq_unbind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)

-- unbindGroup:
--
unbindGroup :: Ptr PFqTag
            -> Int         -- group id
            -> String      -- device name
            -> Int         -- queue index
            -> IO ()

unbindGroup hdl gid name queue =
    withCString name $ \dev ->
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
    withCString name $ \dev ->
        pfq_set_promisc hdl dev (if value then 1 else 0) >>=
            throwPFqIf_ hdl (== -1)

-- read:
--

read :: Ptr PFqTag
     -> Int
     -> IO NetQueue

read hdl msec =
    allocaBytes (32) $ \queue -> do
       pfq_read hdl queue (fromIntegral msec) >>= throwPFqIf_ hdl (== -1)
       _ptr <- ((\h -> peekByteOff h 0))  queue
       _len <- ((\h -> peekByteOff h (sizeOf _ptr)))  queue
       _css <- ((\h -> peekByteOff h (sizeOf _ptr + sizeOf _len))) queue
       _cid <- ((\h -> peekByteOff h (sizeOf _ptr + sizeOf _len + sizeOf _css))) queue
       let slotSize'= fromIntegral(_css :: CSize)
       let slotSize = slotSize' + slotSize' `mod` 8
       return NetQueue { qPtr       = _ptr :: Ptr PktHdr,
                         qLen       = fromIntegral (_len :: CSize),
                         qSlotSize  = slotSize,
                         qIndex     = fromIntegral (_cid  :: CUInt)
                       }
-- setTimestamp:
--

setTimestamp :: Ptr PFqTag -> Bool -> IO ()

setTimestamp hdl toggle = do
    let value = if (toggle) then 1 else 0
    pfq_timestamp_enable hdl value >>= throwPFqIf_ hdl (== -1)


-- getTimestamp:
--
getTimestamp :: Ptr PFqTag -> IO Bool

getTimestamp hdl =
    pfq_is_timestamp_enabled hdl >>= throwPFqIf hdl (== -1) >>= \v ->
        if (v == 0) then return False else return True


-- setCaplen:
--
setCaplen:: Ptr PFqTag -> Int -> IO ()

setCaplen hdl value =
    pfq_set_caplen hdl (fromIntegral value)
        >>= throwPFqIf_ hdl (== -1)


-- getCaplen:
--
getCaplen :: Ptr PFqTag -> IO Int

getCaplen hdl =
    pfq_get_caplen hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral


-- setMaxlen:
--
setMaxlen:: Ptr PFqTag -> Int -> IO ()

setMaxlen hdl value =
    pfq_set_maxlen hdl (fromIntegral value)
        >>= throwPFqIf_ hdl (== -1)


-- getMaxlen:
--
getMaxlen :: Ptr PFqTag -> IO Int

getMaxlen hdl =
    pfq_get_maxlen hdl >>= throwPFqIf hdl (== -1)
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


-- setRxSlots:
--
setRxSlots:: Ptr PFqTag -> Int -> IO ()

setRxSlots hdl value =
    pfq_set_rx_slots hdl (fromIntegral value)
    >>= throwPFqIf_ hdl (== -1)

-- getRxSlots:
--
getRxSlots :: Ptr PFqTag -> IO Int

getRxSlots hdl =
    pfq_get_rx_slots hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral

-- getRxSlotSize:
--
getRxSlotSize :: Ptr PFqTag -> IO Int

getRxSlotSize hdl =
    pfq_get_rx_slot_size hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral

-- setTxSlots:
--
setTxSlots:: Ptr PFqTag -> Int -> IO ()

setTxSlots hdl value =
    pfq_set_tx_slots hdl (fromIntegral value)
    >>= throwPFqIf_ hdl (== -1)

-- getTxSlots:
--
getTxSlots :: Ptr PFqTag -> IO Int

getTxSlots hdl =
    pfq_get_tx_slots hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral

-- vlanFiltersEnabled:
--

vlanFiltersEnabled :: Ptr PFqTag ->
                      Int        -> -- gid
                      Bool       -> -- toggle
                      IO ()
vlanFiltersEnabled hdl gid value =
    pfq_vlan_filters_enable hdl (fromIntegral gid) (fromIntegral $ if value then 1 else 0)
        >>= throwPFqIf_ hdl (== -1)


-- vlanSetFilterId:
--

vlanSetFilterId :: Ptr PFqTag ->
                   Int        -> -- gid
                   Int        -> -- vlan id
                   IO ()

vlanSetFilterId hdl gid id =
    pfq_vlan_set_filter hdl (fromIntegral gid) (fromIntegral id)
        >>= throwPFqIf_ hdl (== -1)

-- vlanResetFilterId:
--

vlanResetFilterId :: Ptr PFqTag ->
                   Int        -> -- gid
                   Int        -> -- vlan id
                   IO ()

vlanResetFilterId hdl gid id =
    pfq_vlan_reset_filter hdl (fromIntegral gid) (fromIntegral id)
        >>= throwPFqIf_ hdl (== -1)

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
                            sReceived = fromIntegral (_recv :: CLong),
                            sLost     = fromIntegral (_lost :: CLong),
                            sDropped  = fromIntegral (_drop :: CLong)
                          }

-- groupComputation:
--

-- groupComputation :: Ptr PFqTag
--                  -> Int            -- group id
--                  -> Computation a  -- computation from (PFlang)
--                  -> IO ()
--
-- groupComputation hdl gid (Computation _ name ctx) =
--     withCString name $ \fname -> do
--         pfq_set_group_function hdl (fromIntegral gid) fname (fromIntegral 0) >>= throwPFqIf_ hdl (== -1)
--         when (isJust ctx) $ putContextFunction hdl ((\(StorableContext c) -> c) . fromJust $ ctx) gid 0
--
-- groupComputation hdl gid (Composition _ ns cs) =
--     forM_ (zip3 ns cs [0,1..]) $ \(name,ctx,ix) ->
--     withCString name $ \fname -> do
--         pfq_set_group_function hdl (fromIntegral gid) fname ix >>= throwPFqIf_ hdl (== -1)
--         when (isJust ctx) $ putContextFunction hdl ((\(StorableContext c) -> c) . fromJust $ ctx) gid 0


-- groupFunction:

groupFunction :: Ptr PFqTag
                 -> Int     -- group id
                 -> Int     -- index in the computation chain
                 -> String  -- function name(s) in continuation-passing style
                 -> IO ()
groupFunction hdl gid ix name =
    withCString name $ \fname ->
        pfq_set_group_function hdl (fromIntegral gid) fname (fromIntegral ix) >>= throwPFqIf_ hdl (== -1)


-- putContextFunction:

putContextFunction :: (Storable s) =>
                    Ptr PFqTag
                    -> s        -- context
                    -> Int      -- group id
                    -> Int      -- index in the computation chain
                    -> IO ()

putContextFunction hdl ctx gid level = do
    allocaBytes (sizeOf ctx) $ \ptr -> do
        poke ptr ctx
        pfq_set_group_function_context hdl (fromIntegral gid) ptr (fromIntegral $ sizeOf ctx) (fromIntegral level) >>= throwPFqIf_ hdl (== -1)


-- getContextFunction:

getContextFunction :: (Storable s) =>
                    Ptr PFqTag
                    -> Int      -- group id
                    -> Int      -- index in the computation chain
                    -> Int      -- size of the context
                    -> IO s

getContextFunction hdl gid level size = do
    allocaBytes (size) $ \ptr -> do
        pfq_get_group_function_context hdl (fromIntegral gid) ptr (fromIntegral size) (fromIntegral level) >>= throwPFqIf_ hdl (== -1)
        peek ptr

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
makeCallback fun = make_callback $ \_ hdr ptr -> toPktHdr hdr >>= flip fun ptr

-- Transmission:
--

bindTx :: Ptr PFqTag
       -> String      -- device name
       -> Int         -- queue index
       -> IO ()

bindTx hdl name queue =
    withCString name $ \dev ->
        pfq_bind_tx hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


startTxThread :: Ptr PFqTag
              -> Int
              -> IO ()

startTxThread hdl node = do
    pfq_start_tx_thread hdl (fromIntegral node) >>= throwPFqIf_ hdl (== -1)


stopTxThread :: Ptr PFqTag
             -> IO ()
stopTxThread hdl = do
    pfq_stop_tx_thread hdl >>= throwPFqIf_ hdl (== -1)


wakeupTxThread :: Ptr PFqTag
               -> IO ()
wakeupTxThread hdl = do
    pfq_wakeup_tx_thread hdl >>= throwPFqIf_ hdl (== -1)


txQueueFlush :: Ptr PFqTag
             -> IO ()
txQueueFlush hdl = do
    pfq_tx_queue_flush hdl >>= throwPFqIf_ hdl (== -1)


inject :: Ptr PFqTag
       -> C.ByteString
       -> IO ()
inject hdl xs = do
    unsafeUseAsCStringLen xs $ \(p, l) ->
        pfq_inject hdl p (fromIntegral l) >>= throwPFqIf_ hdl (== -1)


send :: Ptr PFqTag
     -> C.ByteString
     -> IO ()
send hdl xs = do
    unsafeUseAsCStringLen xs $ \(p, l) ->
        pfq_send hdl p (fromIntegral l) >>= throwPFqIf_ hdl (== -1)


send_async :: Ptr PFqTag
           -> C.ByteString
           -> IO ()
send_async hdl xs = do
    unsafeUseAsCStringLen xs $ \(p, l) ->
        pfq_send_async hdl p (fromIntegral l) >>= throwPFqIf_ hdl (== -1)


-- C functions from libpfq
--

foreign import ccall unsafe pfq_open              :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_nogroup      :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_group        :: CUInt -> CInt -> CSize -> CSize -> CSize -> IO (Ptr PFqTag)

foreign import ccall unsafe pfq_close             :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_error             :: Ptr PFqTag -> IO CString

foreign import ccall unsafe pfq_id                :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_group_id          :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_enable            :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_disable           :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_is_enabled        :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_promisc       :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_timestamp_enable  :: Ptr PFqTag -> CInt -> IO CInt
foreign import ccall unsafe pfq_is_timestamp_enabled :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_caplen        :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_caplen        :: Ptr PFqTag -> IO CPtrdiff

foreign import ccall unsafe pfq_set_maxlen        :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_maxlen        :: Ptr PFqTag -> IO CPtrdiff

foreign import ccall unsafe pfq_set_offset        :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_offset        :: Ptr PFqTag -> IO CPtrdiff

foreign import ccall unsafe pfq_set_tx_slots      :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_tx_slots      :: Ptr PFqTag -> IO CSize

foreign import ccall unsafe pfq_set_rx_slots      :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_rx_slots      :: Ptr PFqTag -> IO CSize
foreign import ccall unsafe pfq_get_rx_slot_size  :: Ptr PFqTag -> IO CSize

foreign import ccall unsafe pfq_bind              :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_bind_group        :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_unbind            :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_unbind_group      :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_join_group        :: Ptr PFqTag -> CInt -> CUInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_leave_group       :: Ptr PFqTag -> CInt -> IO CInt

foreign import ccall unsafe pfq_get_stats         :: Ptr PFqTag -> Ptr Statistics -> IO CInt
foreign import ccall unsafe pfq_get_group_stats   :: Ptr PFqTag -> CInt -> Ptr Statistics -> IO CInt

foreign import ccall unsafe pfq_set_group_function :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_reset_group        :: Ptr PFqTag -> CInt -> IO CInt

-- Note: Ptr a is void *

foreign import ccall unsafe pfq_set_group_function_context :: Ptr PFqTag -> CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall unsafe pfq_get_group_function_context :: Ptr PFqTag -> CInt -> Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall pfq_dispatch                 :: Ptr PFqTag -> FunPtr CPFqCallback -> CLong -> Ptr Word8 -> IO CInt
foreign import ccall "wrapper" make_callback      :: CPFqCallback -> IO (FunPtr CPFqCallback)

foreign import ccall unsafe pfq_read              :: Ptr PFqTag -> Ptr NetQueue -> CLong -> IO CInt

foreign import ccall unsafe pfq_vlan_filters_enable  :: Ptr PFqTag -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_vlan_set_filter      :: Ptr PFqTag -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_vlan_reset_filter    :: Ptr PFqTag -> CInt -> CInt -> IO CInt

-- Tx APIs

foreign import ccall unsafe pfq_bind_tx             :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_start_tx_thread     :: Ptr PFqTag -> CInt -> IO CInt
foreign import ccall unsafe pfq_stop_tx_thread      :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_wakeup_tx_thread    :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_tx_queue_flush      :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_inject              :: Ptr PFqTag -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe pfq_send                :: Ptr PFqTag -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe pfq_send_async          :: Ptr PFqTag -> Ptr CChar -> CSize -> IO CInt

-- Missing

-- extern int pfq_get_groups_mask(pfq_t const *q, unsigned long *_mask);
-- extern int pfq_group_fprog(pfq_t *q, int gid, struct sock_fprog *);
-- extern int pfq_group_fprog_reset(pfq_t *q, int gid);


