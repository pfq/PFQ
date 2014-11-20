
--    Copyright (c) 2011-14, Nicola Bonelli <nicola@pfq.io>
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
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

------------------------------------------------------------------------------
-- |
--  Module      : Network.PFq
--  Copyright   : Nicola Bonelli (c) 2014
--  License     : GPL
--  Maintainer  : nicola@pfq.io
--  Stability   : experimental
--  Portability : non-portable
--
-- The 'Network.PFq' module is a low level binding to the
-- functions in @libpfq@.  See <https://github.com/pfq/PFQ/wiki> for more
-- information.
--
------------------------------------------------------------------------------


module Network.PFq
    (
        -- * Types

        PFqTag,
        Statistics(..),
        NetQueue(..),
        Packet(..),
        PktHdr(..),
        Callback,

        ClassMask(..),

        class_default       ,
        class_user_plane    ,
        class_control_plane ,
        class_control       ,
        class_any           ,

        GroupPolicy(..),
        policy_undefined,
        policy_priv,
        policy_restricted,
        policy_shared,

        PFqConstant(..),

        -- * Socket and Groups

        open,
        open',
        openDefault,
        openNoGroup,
        openNoGroup',
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

        egressBind,
        egressUnbind,

        joinGroup,
        leaveGroup,
        setTimestamp,
        getTimestamp,

        setPromisc,

        -- * Packet capture

        Network.PFq.read,
        dispatch,

        getPackets,
        getPacketHeader,
        isPacketReady,
        waitForPacket,

        getCaplen,
        setCaplen,

        getRxSlots,
        setRxSlots,
        getRxSlotSize,

        vlanFiltersEnabled,
        vlanSetFilterId,
        vlanResetFilterId,

        -- * Packet transmission

        bindTx,
        startTxThread,
        stopTxThread,
        wakeupTxThread,
        txQueueFlush,

        inject,
        send,
        sendAsync,

        getTxSlots,
        setTxSlots,

        getMaxlen,
        setMaxlen,

        -- * Q-lang

        groupComputation,
        groupComputationFromString,

        -- * Statistics and counters

        getStats,
        getGroupStats,
        getGroupCounters,

    ) where


import Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Unsafe
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SV

-- import Data.Maybe
-- import Debug.Trace

import Control.Monad
import Control.Concurrent

import Foreign.Ptr
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Concurrent as C (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr)

import Network.PFq.Lang


-- |Packet capture handle.
newtype PFqTag = PFqTag ()

#include <pfq/pfq.h>

-- |Capture Queue handle.
data NetQueue = NetQueue {
      qPtr        :: Ptr PktHdr                 -- ^ pointer to the memory mapped queue
   ,  qLen        :: {-# UNPACK #-} !Word64     -- ^ queue length
   ,  qSlotSize   :: {-# UNPACK #-} !Word64     -- ^ size of a slot = pfq header + packet
   ,  qIndex      :: {-# UNPACK #-} !Word32     -- ^ index of the queue
   } deriving (Eq, Show)

-- |PFq packet header.
data PktHdr = PktHdr {
      hData     :: {-# UNPACK #-} !Word64       -- ^ opaque 64-bits mark
    , hSec      :: {-# UNPACK #-} !Word32       -- ^ timestamp (seconds)
    , hNsec     :: {-# UNPACK #-} !Word32       -- ^ timestamp (nanoseconds)
    , hIfIndex  :: {-# UNPACK #-} !Word32       -- ^ interface index
    , hGid      :: {-# UNPACK #-} !Word32       -- ^ group id
    , hLen      :: {-# UNPACK #-} !Word16       -- ^ packet length (wire size)
    , hCapLen   :: {-# UNPACK #-} !Word16       -- ^ capture length
    , hTci      :: {-# UNPACK #-} !Word16       -- ^ vlan tci
    , hHwQueue  :: {-# UNPACK #-} !Word8        -- ^ hardware queue index
    , hCommit   :: {-# UNPACK #-} !Word8        -- ^ commit bit
    } deriving (Eq, Show)

-- |PFq statistics.
data Statistics = Statistics {
      sReceived   ::  Integer  -- ^ packets received
    , sLost       ::  Integer  -- ^ packets lost
    , sDropped    ::  Integer  -- ^ packets dropped
    , sSent       ::  Integer  -- ^ packets sent
    , sDiscard    ::  Integer  -- ^ packets discarded
    , sForward    ::  Integer  -- ^ packets forwarded to devices
    , sKernel     ::  Integer  -- ^ packets forwarded to kernel
    } deriving (Eq, Show)

-- |PFq counters.
data Counters = Counters {
      counter     ::  [Integer] -- ^ per-group counter
    } deriving (Eq, Show)


-- |Descriptor of the packet.
data Packet = Packet {
      pHdr   :: Ptr PktHdr      -- ^ pointer to pfq packet header
   ,  pData  :: Ptr Word8       -- ^ pointer to the packet data
   ,  pIndex :: !Word32         -- ^ index of the queue
   } deriving (Eq, Show)


-- |ClassMask type.
newtype ClassMask = ClassMask { getClassMask :: CULong }
                        deriving (Eq, Show)

-- |Group policy type.
newtype GroupPolicy = GroupPolicy { getGroupPolicy :: CInt }
                        deriving (Eq, Show)

-- |Async policy type.
newtype AsyncPolicy = AsyncPolicy { getAsyncPolicy :: CInt }
                        deriving (Eq, Show)

-- |Generic pfq constant.
newtype PFqConstant = PFqConstant { getConstant :: Int }
                        deriving (Eq, Show)


#{enum ClassMask, ClassMask
    , class_default       = Q_CLASS_DEFAULT
    , class_user_plane    = Q_CLASS_USER_PLANE
    , class_control_plane = Q_CLASS_CONTROL_PLANE
    , class_control       = Q_CLASS_CONTROL
    , class_any           = Q_CLASS_ANY
}


#{enum GroupPolicy, GroupPolicy
    , policy_undefined  = Q_POLICY_GROUP_UNDEFINED
    , policy_priv       = Q_POLICY_GROUP_PRIVATE
    , policy_restricted = Q_POLICY_GROUP_RESTRICTED
    , policy_shared     = Q_POLICY_GROUP_SHARED
}


#{enum PFqConstant, PFqConstant
    , any_device           = Q_ANY_DEVICE
    , any_queue            = Q_ANY_QUEUE
    , any_group            = Q_ANY_GROUP
    , group_max_counters   = Q_MAX_COUNTERS
    , group_fun_descr_size = sizeof(struct pfq_functional_descr)
}


#{enum AsyncPolicy, AsyncPolicy
    , tx_deferred          = Q_TX_ASYNC_DEFERRED
    , tx_threaded          = Q_TX_ASYNC_THREADED
}


-- | Utility function which folds a list of ClassMask.
combineClassMasks :: [ClassMask] -> ClassMask
combineClassMasks = ClassMask . foldr ((.|.) . getClassMask) 0


toPktHdr :: Ptr PktHdr
         -> IO PktHdr
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

-- | The type of the callback function passed to 'dispatch'.
type Callback = PktHdr -> Ptr Word8  -> IO ()


type CPFqCallback = Ptr Word8 -> Ptr PktHdr -> Ptr Word8 -> IO ()

-- Error handling
--

throwPFqIf :: Ptr PFqTag
           -> (a -> Bool)
           -> a
           -> IO a
throwPFqIf hdl p v = if p v
    then pfq_error hdl >>= peekCString >>= ioError . userError
    else return v


throwPFqIf_ :: Ptr PFqTag
            -> (a -> Bool)
            -> a
            -> IO ()
throwPFqIf_ hdl p v = void (throwPFqIf hdl p v)


-- |Return the list of 'Packet' stored in the 'NetQueue'.
getPackets :: NetQueue
           -> IO [Packet]
getPackets nq = getPackets' (qIndex nq) (qPtr nq) (qPtr nq `plusPtr` _size) (fromIntegral $ qSlotSize nq)
                    where _slot = fromIntegral $ qSlotSize nq
                          _len  = fromIntegral $ qLen nq
                          _size = _slot * _len

getPackets' :: Word32
            -> Ptr PktHdr
            -> Ptr PktHdr
            -> Int
            -> IO [Packet]
getPackets' index cur end slotSize
    | cur == end = return []
    | otherwise  = do
        let h = cur :: Ptr PktHdr
        let p = cur `plusPtr` 24 :: Ptr Word8
        l <- getPackets' index (cur `plusPtr` slotSize) end slotSize
        return ( Packet h p index : l )


-- |Check whether the 'Packet' is ready or not.

isPacketReady :: Packet -> IO Bool
isPacketReady p = do
    !_com  <- (\h -> peekByteOff h 31) (pHdr p)
    return ((_com :: CUChar) == fromIntegral (pIndex p))

{-# INLINE isPacketReady #-}


-- |Wait until the 'Packet' is ready.
waitForPacket :: Packet
              -> IO ()
waitForPacket p = do
    !ready <- isPacketReady p
    unless ready $ yield >> waitForPacket p

{-# INLINE waitForPacket #-}

-- |Return the 'PktHdr' of the given the 'Packet'.
getPacketHeader :: Packet
          -> IO PktHdr
getPacketHeader p = waitForPacket p >> toPktHdr (pHdr p)

{-# INLINE getPacketHeader #-}

-- |Open the socket.
--
-- The default values are used; no group is joined or created.
-- The socket open is suitable for egress sockets.

openDefault :: IO (ForeignPtr PFqTag)
openDefault = pfq_open_default  >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open a socket and join a new private group.
--
-- The default values for class mask and group policy are 'class_default' and
-- 'policy_priv', respectively.

open :: Int  -- ^ caplen
     -> Int  -- ^ number of rx slots
     -> IO (ForeignPtr PFqTag)
open caplen slots =
        pfq_open (fromIntegral caplen) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open a socket and join a new private group.
--
-- The default values for class mask and group policy are 'class_default' and
-- 'policy_priv', respectively.

open' :: Int  -- ^ caplen
      -> Int  -- ^ number of rx slots
      -> Int  -- ^ maxlen
      -> Int  -- ^ number of tx slots
      -> IO (ForeignPtr PFqTag)
open' caplen rx_slots maxlen tx_slots =
        pfq_open_ (fromIntegral caplen) (fromIntegral rx_slots) (fromIntegral maxlen) (fromIntegral tx_slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open a socket. No group is joined or created.
--
-- Groups can later be joined by means of the 'joinGroup' function.

openNoGroup :: Int  -- ^ caplen
            -> Int  -- ^ number of rx slots
            -> IO (ForeignPtr PFqTag)
openNoGroup caplen slots =
        pfq_open_nogroup (fromIntegral caplen) (fromIntegral slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)

-- |Open a socket. No group is joined or created.
--
-- Groups can later be joined by means of the 'joinGroup' function.

openNoGroup' :: Int  -- ^ caplen
             -> Int  -- ^ number of rx slots
             -> Int  -- ^ maxlen
             -> Int  -- ^ number of tx slots
             -> IO (ForeignPtr PFqTag)
openNoGroup' caplen rx_slots maxlen tx_slots =
        pfq_open_nogroup_ (fromIntegral caplen) (fromIntegral rx_slots) (fromIntegral maxlen) (fromIntegral tx_slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)

-- |Open the socket and create a new group with the specified class and policy.
--
-- All the possible parameters are specifiable.

openGroup :: [ClassMask]  -- ^ list of ClassMask (e.g., [class_default])
          -> GroupPolicy  -- ^ policy for the group
          -> Int          -- ^ caplen
          -> Int          -- ^ number of rx slots
          -> Int          -- ^ maxlen
          -> Int          -- ^ number of tx slots
          -> IO (ForeignPtr PFqTag)
openGroup ms policy caplen rx_slots maxlen tx_slots =
        pfq_open_group (getClassMask $ combineClassMasks ms) (getGroupPolicy policy)
            (fromIntegral caplen) (fromIntegral rx_slots)
            (fromIntegral maxlen) (fromIntegral tx_slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)

-- |Close the socket.

close :: Ptr PFqTag
      -> IO ()
close hdl =
    pfq_close hdl >>= throwPFqIf_ hdl (== -1)


-- |Bind the main group of the socket to the given device/queue.

bind :: Ptr PFqTag
     -> String      -- ^ device name
     -> Int         -- ^ queue index (-1 means any queue)
     -> IO ()
bind hdl name queue =
    withCString name $ \dev ->
        pfq_bind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- |Unbind the main group of the socket from the given device/queue.

unbind :: Ptr PFqTag
       -> String      -- ^ device name
       -> Int         -- ^ queue index
       -> IO ()
unbind hdl name queue =
    withCString name $ \dev ->
        pfq_unbind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- |Bind the group to the given device/queue.

bindGroup :: Ptr PFqTag
          -> Int         -- ^ group id
          -> String      -- ^ device name
          -> Int         -- ^ queue index
          -> IO ()
bindGroup hdl gid name queue =
    withCString name $ \dev ->
        pfq_bind_group hdl (fromIntegral gid) dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- |Unbind the group from the given device/queue.

unbindGroup :: Ptr PFqTag
            -> Int         -- ^ group id
            -> String      -- ^ device name
            -> Int         -- ^ queue index
            -> IO ()
unbindGroup hdl gid name queue =
    withCString name $ \dev ->
        pfq_unbind_group hdl (fromIntegral gid) dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)

-- | Mark the socket as egress and bind it to the given device/queue.
--
-- The egress socket will be used within the capture group as forwarder.

egressBind :: Ptr PFqTag
       -> String      -- device name
       -> Int         -- queue index
       -> IO ()
egressBind hdl name queue =
    withCString name $ \dev ->
        pfq_egress_bind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)

-- | Unmark the socket as egress.

egressUnbind :: Ptr PFqTag -> IO ()
egressUnbind hdl =
    pfq_egress_unbind hdl >>= throwPFqIf_ hdl (== -1)

-- |Join the group with the given class mask and group policy.

joinGroup :: Ptr PFqTag
          -> Int            -- ^ group id
          -> [ClassMask]    -- ^ class mask list
          -> GroupPolicy    -- ^ group policy
          -> IO ()
joinGroup hdl gid ms pol =
    pfq_join_group hdl (fromIntegral gid) (getClassMask $ combineClassMasks ms) (getGroupPolicy pol)
        >>= throwPFqIf_ hdl (== -1)


-- |Leave the group.

leaveGroup :: Ptr PFqTag
           -> Int        -- ^ group id
           -> IO ()
leaveGroup hdl gid =
    pfq_leave_group hdl (fromIntegral gid)
        >>= throwPFqIf_ hdl (== -1)


-- |Return the id of the socket.

getId :: Ptr PFqTag
      -> IO Int
getId hdl =
    pfq_id hdl >>= throwPFqIf hdl (== -1) >>= return . fromIntegral


-- |Return the group id of the socket.
--

getGroupId :: Ptr PFqTag
           -> IO Int
getGroupId hdl =
    pfq_group_id hdl >>= throwPFqIf hdl (== -1) >>= return . fromIntegral


-- |Enable the socket for packet capture.

enable :: Ptr PFqTag
       -> IO ()
enable hdl =
    pfq_enable hdl >>= throwPFqIf_ hdl (== -1)


-- |Disable the socket for packet capture.

disable :: Ptr PFqTag
        -> IO ()
disable hdl = pfq_disable hdl >>= throwPFqIf_ hdl (== -1)


-- |Check whether the packet capture is enabled.

isEnabled :: Ptr PFqTag
          -> IO Bool
isEnabled hdl =
    pfq_is_enabled hdl >>= throwPFqIf hdl (== -1) >>= \v ->
        return $ v /= 0


-- |Set the promiscuous mode for the given interface.
--

setPromisc :: Ptr PFqTag
           -> String    -- ^ device name
           -> Bool      -- ^ toggle: true is on, false is off.
           -> IO ()
setPromisc hdl name value =
    withCString name $ \dev ->
        pfq_set_promisc hdl dev (if value then 1 else 0) >>=
            throwPFqIf_ hdl (== -1)

-- |Read packets in place.
--
-- Wait for packets and return a 'NetQueue' descriptor.
-- The timeout is specified in microseconds.

read :: Ptr PFqTag
     -> Int         -- ^ timeout (msec)
     -> IO NetQueue
read hdl msec =
    allocaBytes 32 $ \queue -> do
       pfq_read hdl queue (fromIntegral msec) >>= throwPFqIf_ hdl (== -1)
       _ptr <- (\h -> peekByteOff h 0)  queue
       _len <- (\h -> peekByteOff h (sizeOf _ptr))  queue
       _css <- (\h -> peekByteOff h (sizeOf _ptr + sizeOf _len)) queue
       _cid <- (\h -> peekByteOff h (sizeOf _ptr + sizeOf _len + sizeOf _css)) queue
       let slotSize'= fromIntegral(_css :: CSize)
       let slotSize = slotSize' + slotSize' `mod` 8
       return NetQueue { qPtr       = _ptr :: Ptr PktHdr,
                         qLen       = fromIntegral (_len :: CSize),
                         qSlotSize  = slotSize,
                         qIndex     = fromIntegral (_cid  :: CUInt)
                       }

-- |Set the timestamping for packets.

setTimestamp :: Ptr PFqTag
             -> Bool        -- ^ toggle: true is on, false is off.
             -> IO ()
setTimestamp hdl toggle = do
    let value = if toggle then 1 else 0
    pfq_timestamp_enable hdl value >>= throwPFqIf_ hdl (== -1)


-- |Check whether the timestamping for packets is enabled.

getTimestamp :: Ptr PFqTag
             -> IO Bool
getTimestamp hdl =
    pfq_is_timestamp_enabled hdl >>= throwPFqIf hdl (== -1) >>= \v ->
        return $ v /= 0


-- |Specify the capture length of packets, in bytes.
--
-- Capture length must be set before the socket is enabled for capture.

setCaplen :: Ptr PFqTag
          -> Int        -- ^ caplen (bytes)
          -> IO ()
setCaplen hdl value =
    pfq_set_caplen hdl (fromIntegral value)
        >>= throwPFqIf_ hdl (== -1)


-- |Return the capture length of packets, in bytes.

getCaplen :: Ptr PFqTag
          -> IO Int
getCaplen hdl =
    pfq_get_caplen hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral


-- |Specify the max transmission length of packets, in bytes.

setMaxlen :: Ptr PFqTag
          -> Int        -- ^ maxlen (bytes)
          -> IO ()
setMaxlen hdl value =
    pfq_set_maxlen hdl (fromIntegral value)
        >>= throwPFqIf_ hdl (== -1)


-- |Return the max transmission length of packets, in bytes.

getMaxlen :: Ptr PFqTag
          -> IO Int
getMaxlen hdl =
    pfq_get_maxlen hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral

-- |Specify the length of the RX queue, in number of packets./
--
-- The number of RX slots can't exceed the max value specified by
-- the rx_queue_slot kernel module parameter.

setRxSlots :: Ptr PFqTag
           -> Int   -- ^ number of rx slots
           -> IO ()
setRxSlots hdl value =
    pfq_set_rx_slots hdl (fromIntegral value)
    >>= throwPFqIf_ hdl (== -1)

-- |Return the length of the RX queue, in number of packets.

getRxSlots :: Ptr PFqTag
           -> IO Int
getRxSlots hdl =
    pfq_get_rx_slots hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral


-- |Return the length of a RX slot, in bytes.

getRxSlotSize :: Ptr PFqTag
              -> IO Int
getRxSlotSize hdl =
    pfq_get_rx_slot_size hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral


-- |Specify the length of the TX queue, in number of packets.
--
-- The number of TX slots can't exceed the max value specified by
-- the tx_queue_slot kernel module parameter.

setTxSlots :: Ptr PFqTag
           -> Int       -- ^ numer of tx slots
           -> IO ()
setTxSlots hdl value =
    pfq_set_tx_slots hdl (fromIntegral value)
    >>= throwPFqIf_ hdl (== -1)


-- |Return the length of the TX queue, in number of packets.

getTxSlots :: Ptr PFqTag
           -> IO Int
getTxSlots hdl =
    pfq_get_tx_slots hdl >>= throwPFqIf hdl (== -1)
        >>= return . fromIntegral


-- |Set the vlan filtering for the given group.

vlanFiltersEnabled :: Ptr PFqTag
                   -> Int        -- ^ group id
                   -> Bool       -- ^ toggle: true is on, false is off.
                   -> IO ()
vlanFiltersEnabled hdl gid value =
    pfq_vlan_filters_enable hdl (fromIntegral gid) (fromIntegral $ if value then 1 else 0 :: Int)
        >>= throwPFqIf_ hdl (== -1)


-- |Specify a capture filter for the given group and vlan id.
--

vlanSetFilterId :: Ptr PFqTag
                -> Int        -- ^ group id
                -> Int        -- ^ vlan id
                -> IO ()
vlanSetFilterId hdl gid vid =
    pfq_vlan_set_filter hdl (fromIntegral gid) (fromIntegral vid)
        >>= throwPFqIf_ hdl (== -1)

-- |Reset the vlan filter.

vlanResetFilterId :: Ptr PFqTag
                  -> Int        -- ^ group id
                  -> Int        -- ^ vlan id
                  -> IO ()
vlanResetFilterId hdl gid vid =
    pfq_vlan_reset_filter hdl (fromIntegral gid) (fromIntegral vid)
        >>= throwPFqIf_ hdl (== -1)

-- |Return the socket statistics.

getStats :: Ptr PFqTag
         -> IO Statistics
getStats hdl =
    allocaBytes (sizeOf (undefined :: CLong) * 7) $ \sp -> do
        pfq_get_stats hdl sp >>= throwPFqIf_ hdl (== -1)
        makeStats sp

-- |Return the statistics of the given group.

getGroupStats :: Ptr PFqTag
              -> Int            -- ^ group id
              -> IO Statistics
getGroupStats hdl gid =
    allocaBytes (sizeOf (undefined :: CLong) * 7) $ \sp -> do
        pfq_get_group_stats hdl (fromIntegral gid) sp >>= throwPFqIf_ hdl (== -1)
        makeStats sp


makeStats :: Ptr a
          -> IO Statistics
makeStats p = do
    _recv <- (\ptr -> peekByteOff ptr (sizeOf (undefined :: CLong) * 0)) p
    _lost <- (\ptr -> peekByteOff ptr (sizeOf (undefined :: CLong) * 1)) p
    _drop <- (\ptr -> peekByteOff ptr (sizeOf (undefined :: CLong) * 2)) p
    _sent <- (\ptr -> peekByteOff ptr (sizeOf (undefined :: CLong) * 3)) p
    _disc <- (\ptr -> peekByteOff ptr (sizeOf (undefined :: CLong) * 4)) p
    _frwd <- (\ptr -> peekByteOff ptr (sizeOf (undefined :: CLong) * 5)) p
    _kern <- (\ptr -> peekByteOff ptr (sizeOf (undefined :: CLong) * 6)) p
    return Statistics {
                            sReceived = fromIntegral (_recv :: CULong),
                            sLost     = fromIntegral (_lost :: CULong),
                            sDropped  = fromIntegral (_drop :: CULong),
                            sSent     = fromIntegral (_sent :: CULong),
                            sDiscard  = fromIntegral (_disc :: CULong),
                            sForward  = fromIntegral (_frwd :: CULong),
                            sKernel   = fromIntegral (_kern :: CULong)
                      }

-- |Return the set of counters of the given group.

getGroupCounters :: Ptr PFqTag
                 -> Int            -- ^ group id
                 -> IO Counters
getGroupCounters hdl gid =
    allocaBytes (sizeOf (undefined :: CLong) * getConstant group_max_counters) $ \sp -> do
        pfq_get_group_counters hdl (fromIntegral gid) sp >>= throwPFqIf_ hdl (== -1)
        makeCounters sp


makeCounters :: Ptr a
             -> IO Counters
makeCounters ptr = do
    cs <- forM [0.. getConstant group_max_counters - 1] $ \ n -> peekByteOff ptr (sizeOf (undefined :: CULong) * n)
    return $ Counters $ map fromIntegral (cs :: [CULong])


padArguments :: [Argument] -> [Argument]
padArguments xs = xs ++ replicate (4 - length xs) ArgNull


withSingleArg :: Argument
              -> ((IntPtr, CSize, CSize) -> IO a)
              -> IO a
withSingleArg arg callback =
    case arg of
        ArgNull                    -> callback (ptrToIntPtr nullPtr, 0                      ,  0)
        ArgFun i                   -> callback (ptrToIntPtr nullPtr, fromIntegral i         , -1)
        ArgString s                -> withCString s $ \s' -> callback (ptrToIntPtr s', 0    , -1)
        ArgVector xs               -> let vec = SV.pack xs in SV.withStartPtr vec $ \ ptr len -> callback (ptrToIntPtr ptr, fromIntegral $ sizeOf (head xs), fromIntegral len)
        ArgData v -> alloca $ \ptr -> poke ptr v >> callback (ptrToIntPtr ptr, fromIntegral $ sizeOf v, -1)


type MarshalFunctionDescr = (CString, [(IntPtr, CSize, CSize)], CSize)

withFunDescr :: FunctionDescr
             -> (MarshalFunctionDescr -> IO a)
             -> IO a
withFunDescr (FunctionDescr symbol args next) callback =
    withCString symbol $ \ symbol' ->
        withMany withSingleArg (padArguments args) $ \marArgs ->
            callback (symbol', marArgs, fromIntegral next)


fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

data StorableFunDescr = StorableFunDescr CString [(IntPtr,CSize,CSize)] CSize

instance Storable StorableFunDescr where
        sizeOf _    = getConstant group_fun_descr_size
        alignment _ = alignment (undefined :: CSize)
        poke ptr (StorableFunDescr symbol args next) = do
            pokeByteOff ptr (off 0)  symbol
            forM_ [0..3] $ \x -> do
                pokeByteOff ptr (off (x*3 + 1)) (fst3 $ args !! x)
                pokeByteOff ptr (off (x*3 + 2)) (snd3 $ args !! x)
                pokeByteOff ptr (off (x*3 + 3)) (trd3 $ args !! x)
            pokeByteOff ptr (off 13) next
         where
            off n = sizeOf nullPtr * n


-- |Specify a functional computation for the given group.
--
-- The functional computation is specified as a PFq-Lang expression.
--

groupComputation :: Ptr PFqTag
                 -> Int                                     -- ^ group id
                 -> Function (SkBuff -> Action SkBuff)      -- ^ expression (PFq-Lang)
                 -> IO ()

groupComputation hdl gid comp = do
    let (descrList, _) = serialize comp 0
    allocaBytes (sizeOf (undefined :: CSize) * 2 + getConstant group_fun_descr_size * length descrList) $ \ ptr -> do
        pokeByteOff ptr 0 (fromIntegral (length descrList) :: CSize)     -- size
        pokeByteOff ptr (sizeOf(undefined :: CSize)) (0 :: CSize)        -- entry_point: always the first one!
        withMany withFunDescr descrList $ \marshList -> do
            let offset n = sizeOf(undefined :: CSize) * 2 + getConstant group_fun_descr_size * n
            forM_ (zip [0..] marshList) $ \(n, (symbol, ps, next)) ->
                pokeByteOff ptr (offset n)
                    (StorableFunDescr symbol ps (fromIntegral next))
            pfq_set_group_computation hdl (fromIntegral gid) ptr >>= throwPFqIf_ hdl (== -1)


-- |Specify a simple functional computation for the given group, from String.
--
-- This function is experimental and is limited to simple functional computations.
-- Only the composition of monadic functions without binding arguments are supported.

groupComputationFromString :: Ptr PFqTag
                           -> Int       -- ^ group id
                           -> String    -- ^ simple expression (PFq-Lang)
                           -> IO ()

groupComputationFromString hdl gid comp =
    withCString comp $ \ptr ->
            pfq_set_group_computation_from_string hdl (fromIntegral gid) ptr >>= throwPFqIf_ hdl (== -1)


-- |Collect and process packets.
--
-- This function is passed a function 'Callback' which is called on each packet.


dispatch :: Ptr PFqTag
         -> Callback    -- ^ packet processing function
         -> Int         -- ^ timeout (msec)
         -> IO ()       --
dispatch hdl f timeo = do
    cback <- makeCallback f
    ret  <- pfq_dispatch hdl cback (fromIntegral timeo) nullPtr
    freeHaskellFunPtr cback
    throwPFqIf_ hdl (== (-1 :: Integer)) (fromIntegral ret)


makeCallback :: Callback
             -> IO (FunPtr CPFqCallback)
makeCallback fun = make_callback $ \_ hdr ptr -> toPktHdr hdr >>= flip fun ptr


-- |Bind the socket for transmission to the given device name and queue.
--
-- A socket for transmission can be bound to a given device/queue at time.

bindTx :: Ptr PFqTag
       -> String      -- device name
       -> Int         -- queue index
       -> IO ()
bindTx hdl name queue =
    withCString name $ \dev ->
        pfq_bind_tx hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)

-- |Start the TX kernel thread for transmission.

startTxThread :: Ptr PFqTag
              -> Int        -- ^ node
              -> IO ()
startTxThread hdl node =
    pfq_start_tx_thread hdl (fromIntegral node) >>= throwPFqIf_ hdl (== -1)


-- |Stop the TX kernel thread.

stopTxThread :: Ptr PFqTag
             -> IO ()
stopTxThread hdl =
    pfq_stop_tx_thread hdl >>= throwPFqIf_ hdl (== -1)

-- |Wakeup the TX kernel thread.
--
-- Wake up the TX kernel thread which transmits the packets in the Tx queue.
-- The kernel thread must be already started.

wakeupTxThread :: Ptr PFqTag
               -> IO ()
wakeupTxThread hdl =
    pfq_wakeup_tx_thread hdl >>= throwPFqIf_ hdl (== -1)


-- |Flush the TX queue, in the user context of the calling thread.
--
-- To invoke this function, no kernel thread is required.

txQueueFlush :: Ptr PFqTag
             -> IO ()
txQueueFlush hdl =
    pfq_tx_queue_flush hdl >>= throwPFqIf_ hdl (== -1)


-- |Schedule the packet for transmission.
--
-- The packet is copied into the TX queue and sent when
-- the 'txQueueFlush' or the 'wakeupTxThread' function is invoked.

inject :: Ptr PFqTag
       -> C.ByteString  -- ^ bytes of packet
       -> IO Bool
inject hdl xs =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        liftM (> 0) $ pfq_inject hdl p (fromIntegral l) >>= throwPFqIf hdl (== -1)


-- |Transmit the packet passed.
--
-- The packet is copied into the TX queue and sent immediately.

send :: Ptr PFqTag
     -> C.ByteString    -- ^ bytes of packet
     -> IO Bool
send hdl xs =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        liftM (>0) $ pfq_send hdl p (fromIntegral l) >>= throwPFqIf hdl (== -1)


-- |Store the packet and possibly transmit the packets in the queue, asynchronously.
--
-- The transmission is invoked by the kernel thread, every n packets enqueued.

sendAsync :: Ptr PFqTag
          -> C.ByteString  -- ^ bytes of packet
          -> Int           -- ^ length of the batch
          -> AsyncPolicy
          -> IO Bool
sendAsync hdl xs blen apol =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        liftM (>0) $ pfq_send_async hdl p (fromIntegral l) (fromIntegral blen) (getAsyncPolicy apol) >>= throwPFqIf hdl (== -1)


-- C functions from libpfq
--

foreign import ccall unsafe pfq_open_default        :: IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open                :: CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_               :: CSize -> CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_nogroup        :: CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_nogroup_       :: CSize -> CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_group          :: CULong -> CInt  -> CSize -> CSize -> CSize -> CSize -> IO (Ptr PFqTag)

foreign import ccall unsafe pfq_close               :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_error               :: Ptr PFqTag -> IO CString

foreign import ccall unsafe pfq_id                  :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_group_id            :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_enable              :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_disable             :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_is_enabled          :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_promisc         :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_timestamp_enable    :: Ptr PFqTag -> CInt -> IO CInt
foreign import ccall unsafe pfq_is_timestamp_enabled :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_caplen          :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_caplen          :: Ptr PFqTag -> IO CPtrdiff

foreign import ccall unsafe pfq_set_maxlen          :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_maxlen          :: Ptr PFqTag -> IO CPtrdiff

foreign import ccall unsafe pfq_set_tx_slots        :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_tx_slots        :: Ptr PFqTag -> IO CSize

foreign import ccall unsafe pfq_set_rx_slots        :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_rx_slots        :: Ptr PFqTag -> IO CSize
foreign import ccall unsafe pfq_get_rx_slot_size    :: Ptr PFqTag -> IO CSize

foreign import ccall unsafe pfq_bind                :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_bind_group          :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_unbind              :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_unbind_group        :: Ptr PFqTag -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_egress_bind         :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_egress_unbind       :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_join_group          :: Ptr PFqTag -> CInt -> CULong -> CInt -> IO CInt
foreign import ccall unsafe pfq_leave_group         :: Ptr PFqTag -> CInt -> IO CInt

foreign import ccall unsafe pfq_get_stats           :: Ptr PFqTag -> Ptr Statistics -> IO CInt
foreign import ccall unsafe pfq_get_group_stats     :: Ptr PFqTag -> CInt -> Ptr Statistics -> IO CInt
foreign import ccall unsafe pfq_get_group_counters  :: Ptr PFqTag -> CInt -> Ptr Counters -> IO CInt

foreign import ccall unsafe pfq_set_group_computation :: Ptr PFqTag -> CInt -> Ptr a -> IO CInt
foreign import ccall unsafe pfq_set_group_computation_from_string :: Ptr PFqTag -> CInt -> CString -> IO CInt

foreign import ccall pfq_dispatch                   :: Ptr PFqTag -> FunPtr CPFqCallback -> CLong -> Ptr Word8 -> IO CInt
foreign import ccall "wrapper" make_callback        :: CPFqCallback -> IO (FunPtr CPFqCallback)

foreign import ccall unsafe pfq_read                :: Ptr PFqTag -> Ptr NetQueue -> CLong -> IO CInt

foreign import ccall unsafe pfq_vlan_filters_enable :: Ptr PFqTag -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_vlan_set_filter     :: Ptr PFqTag -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_vlan_reset_filter   :: Ptr PFqTag -> CInt -> CInt -> IO CInt

foreign import ccall unsafe pfq_bind_tx             :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_start_tx_thread     :: Ptr PFqTag -> CInt -> IO CInt
foreign import ccall unsafe pfq_stop_tx_thread      :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_wakeup_tx_thread    :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_tx_queue_flush      :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_inject              :: Ptr PFqTag -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe pfq_send                :: Ptr PFqTag -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe pfq_send_async          :: Ptr PFqTag -> Ptr CChar -> CSize -> CSize -> CInt -> IO CInt

-- extern int pfq_get_groups_mask(pfq_t const *q, unsigned long *_mask);
-- extern int pfq_group_fprog(pfq_t *q, int gid, struct sock_fprog *);
-- extern int pfq_group_fprog_reset(pfq_t *q, int gid);

