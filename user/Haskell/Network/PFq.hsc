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

------------------------------------------------------------------------------
-- |
--  Module      : Network.PFq
--  Copyright   : Nicola Bonelli (c) 2012-2015
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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
        any_device,
        any_queue,
        any_group,
        version,

        SocketParams(..),
        defaultSocketParams,

        -- * Socket and Groups

        open,
        openNoGroup,
        openGroup,
        openParam,
        close,

        enable,
        disable,
        getId,
        getGroupId,
        isEnabled,

        -- * Socket control

        joinGroup,
        leaveGroup,

        bind,
        bindGroup,
        unbind,
        unbindGroup,
        egressBind,
        egressUnbind,

        -- * Socket parameters

        timestampingEnable,
        isTimestampingEnabled,

        setWeight,
        getWeight,

        setPromisc,

        getCaplen,
        setCaplen,

        getRxSlots,
        setRxSlots,
        getRxSlotSize,

        getTxSlots,
        setTxSlots,

        getMaxlen,

        -- * Packet capture

        Network.PFq.read,
        dispatch,

        getPackets,
        getPacketHeader,
        isPacketReady,
        waitForPacket,

        VlanTag(..),

        vlan_untag,
        vlan_anytag,

        vlanFiltersEnable,
        vlanSetFilter,
        vlanResetFilter,

        -- * Packet transmission

        send,
        sendTo,
        sendAsync,
        sendAt,

        txQueue,

        bindTx,
        unbindTx,

        -- * pfq-lang

        setGroupComputation,
        setGroupComputationFromString,

        -- * Statistics and counters

        getStats,
        getGroupStats,
        getGroupCounters,

    ) where


import Data.Word
import Data.Bits
import Data.Monoid
import Data.List (intercalate)

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Unsafe
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SV

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
import System.Clock

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
      hMark     :: {-# UNPACK #-} !Word32       -- ^ skb 32-bits mark
    , hState    :: {-# UNPACK #-} !Word32       -- ^ opaque 32-bits state
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

-- |SocketParams data type.
--
-- To be passed as argument to 'openParam' function.

data SocketParams = SocketParams
    {   parCaplen     :: Int            -- ^ capture len
    ,   parRxSlots    :: Int            -- ^ socket Rx queue length
    ,   parTxSlots    :: Int            -- ^ socket Tx queue length
    ,   parPolicy     :: GroupPolicy    -- ^ default group policy: policy_undefined means no group
    ,   parClass      :: ClassMask      -- ^ socket class
    } deriving (Eq, Show)


-- |Default values of Socket parameters.

defaultSocketParams :: SocketParams
defaultSocketParams =
    SocketParams
    {   parCaplen  = 1514
    ,   parRxSlots = 4096
    ,   parTxSlots = 4096
    ,   parPolicy  = policy_priv
    ,   parClass   = class_default
    }


-- |ClassMask type.
newtype ClassMask = ClassMask { getClassMask :: CULong }
                        deriving (Eq, Show, Read)

-- |Monoid instance
instance Monoid ClassMask where
    mempty = ClassMask 0
    ClassMask a `mappend` ClassMask b = ClassMask (a .|. b)


-- |Group policy type.
newtype GroupPolicy = GroupPolicy { getGroupPolicy :: CInt }
                        deriving (Eq, Show, Read)

-- |Async policy type.
newtype AsyncPolicy = AsyncPolicy { getAsyncPolicy :: CInt }
                        deriving (Eq, Show, Read)

-- |Generic pfq constant.
newtype PFqConstant = PFqConstant { getConstant :: Int }
                        deriving (Eq, Show, Read)

-- |Vlan tag.
newtype VlanTag = VlanTag { getVid:: CInt }
                    deriving (Eq, Show, Read)

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
    , no_kthread           = Q_NO_KTHREAD
    , group_max_counters   = Q_MAX_COUNTERS
    , version_code         = PFQ_VERSION_CODE
    , major_version        = PFQ_MAJOR(PFQ_VERSION_CODE)
    , minor_version        = PFQ_MINOR(PFQ_VERSION_CODE)
    , patchlevel_version   = PFQ_PATCHLEVEL(PFQ_VERSION_CODE)
}

#{enum VlanTag, VlanTag
    , vlan_untag           = Q_VLAN_UNTAG
    , vlan_anytag          = Q_VLAN_ANYTAG
}


version :: String
version = #{const_str PFQ_VERSION_STRING }


toPktHdr :: Ptr PktHdr
         -> IO PktHdr
toPktHdr hdr = do
    _mark  <- (\h -> peekByteOff h 0)  hdr
    _state <- (\h -> peekByteOff h 4)  hdr
    _sec   <- (\h -> peekByteOff h 8)  hdr
    _nsec  <- (\h -> peekByteOff h 12) hdr
    _ifidx <- (\h -> peekByteOff h 16) hdr
    _gid   <- (\h -> peekByteOff h 20) hdr
    _len   <- (\h -> peekByteOff h 24) hdr
    _cap   <- (\h -> peekByteOff h 26) hdr
    _tci   <- (\h -> peekByteOff h 28) hdr
    _hwq   <- (\h -> peekByteOff h 30) hdr
    _com   <- (\h -> peekByteOff h 31) hdr
    return PktHdr {
                    hMark     = fromIntegral (_mark :: Word32),
                    hState    = fromIntegral (_state:: Word32),
                    hSec      = fromIntegral (_sec  :: Word32),
                    hNsec     = fromIntegral (_nsec :: Word32),
                    hIfIndex  = fromIntegral (_ifidx:: CInt),
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
        let p = cur `plusPtr` #{size struct pfq_pkthdr} :: Ptr Word8
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

-- |Return the 'PktHdr' of the given 'Packet'.
getPacketHeader :: Packet
          -> IO PktHdr
getPacketHeader p = waitForPacket p >> toPktHdr (pHdr p)

{-# INLINE getPacketHeader #-}


-- |Open a socket and create a new private group.
--
-- The default values for class mask and group policy are 'class_default' and
-- 'policy_priv', respectively.


open  :: Int  -- ^ caplen
      -> Int  -- ^ number of Rx slots
      -> Int  -- ^ number of Tx slots
      -> IO (ForeignPtr PFqTag)
open  caplen rx_slots tx_slots =
        pfq_open (fromIntegral caplen) (fromIntegral rx_slots) (fromIntegral tx_slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open a socket. No group is joined or created.
--
-- Groups can later be joined by means of 'joinGroup' function.

openNoGroup  :: Int  -- ^ caplen
             -> Int  -- ^ number of Rx slots
             -> Int  -- ^ number of Tx slots
             -> IO (ForeignPtr PFqTag)
openNoGroup  caplen rx_slots tx_slots =
        pfq_open_nogroup (fromIntegral caplen) (fromIntegral rx_slots) (fromIntegral tx_slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open the socket and create a new group with the specified parameters.
--
-- If group_policy is 'policy_undefined' no gorup is joined or created.

openGroup :: ClassMask    -- ^ ClassMask (e.g., class_default `mappend` class_control_plane)
          -> GroupPolicy  -- ^ policy for the group
          -> Int          -- ^ caplen
          -> Int          -- ^ number of Rx slots
          -> Int          -- ^ number of Tx slots
          -> IO (ForeignPtr PFqTag)
openGroup ms policy caplen rx_slots tx_slots =
        pfq_open_group (getClassMask ms) (getGroupPolicy policy)
            (fromIntegral caplen) (fromIntegral rx_slots)
            (fromIntegral tx_slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open the socket with 'SocketParams'.
--
-- Default values are defined as 'defaultSocketParams'.

openParam :: SocketParams  -- ^ parameters
          -> IO (ForeignPtr PFqTag)
openParam  SocketParams
          {   parCaplen     = caplen
          ,   parRxSlots    = rx_slots
          ,   parTxSlots    = tx_slots
          ,   parPolicy     = policy
          ,   parClass      = cmask
          } =
          pfq_open_group (getClassMask cmask) (getGroupPolicy policy) (fromIntegral caplen) (fromIntegral rx_slots) (fromIntegral tx_slots) >>=
            throwPFqIf nullPtr (== nullPtr) >>= \ptr ->
                C.newForeignPtr ptr (void $ pfq_close ptr)

-- |Close the socket.
--
-- Release the shared memory, stop kernel threads.

close :: Ptr PFqTag
      -> IO ()
close hdl =
    pfq_close hdl >>= throwPFqIf_ hdl (== -1)


-- |Return the id of the socket.

getId :: Ptr PFqTag
      -> IO Int
getId hdl =
    liftM fromIntegral (pfq_id hdl >>= throwPFqIf hdl (== -1))


-- |Return the group-id of the socket.

getGroupId :: Ptr PFqTag
           -> IO Int
getGroupId hdl =
    liftM fromIntegral (pfq_group_id hdl >>= throwPFqIf hdl (== -1))


-- |Enable the socket for packets capture and transmission.
--
-- Allocate the shared memory for socket queues, possibly using
-- the Linux HugePages support.
-- If the enviroment variable PFQ_HUGEPAGES is set to 0 (or
-- PFQ_NO_HUGEPAGES is defined) standard 4K pages are used.

enable :: Ptr PFqTag
       -> IO ()
enable hdl =
    pfq_enable hdl >>= throwPFqIf_ hdl (== -1)


-- |Disable the socket.
--
-- Release the shared memory, stop kernel threads.

disable :: Ptr PFqTag
        -> IO ()
disable hdl = pfq_disable hdl >>= throwPFqIf_ hdl (== -1)


-- |Check whether the socket is enabled.

isEnabled :: Ptr PFqTag
          -> IO Bool
isEnabled hdl =
    pfq_is_enabled hdl >>= throwPFqIf hdl (== -1) >>= \v ->
        return $ v /= 0


-- |Enable/disable timestamping for packets.

timestampingEnable :: Ptr PFqTag
                   -> Bool        -- ^ toggle: True is on, False off.
                   -> IO ()
timestampingEnable hdl toggle = do
    let value = if toggle then 1 else 0
    pfq_timestamping_enable hdl value >>= throwPFqIf_ hdl (== -1)

-- |Check whether timestamping for packets is enabled.

isTimestampingEnabled :: Ptr PFqTag
                      -> IO Bool
isTimestampingEnabled hdl =
    pfq_is_timestamping_enabled hdl >>= throwPFqIf hdl (== -1) >>= \v ->
        return $ v /= 0


-- |Set the weight of the socket for the steering phase.

setWeight :: Ptr PFqTag
          -> Int    -- ^ weight of socket (valid range is [1,16))
          -> IO ()
setWeight hdl value =
    pfq_set_weight hdl (fromIntegral value) >>= throwPFqIf_ hdl (== -1)


-- |Return the weight of the socket.

getWeight :: Ptr PFqTag
          -> IO Int
getWeight hdl =
    liftM fromIntegral (pfq_get_weight hdl >>= throwPFqIf hdl (== -1))


-- |Specify the capture length of packets, in bytes.
--
-- Capture length must be set before the socket is enabled.

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
    liftM fromIntegral (pfq_get_caplen hdl >>= throwPFqIf hdl (== -1))


-- |Return the max transmission length of packets, in bytes.

getMaxlen :: Ptr PFqTag
          -> IO Int
getMaxlen hdl =
    liftM fromIntegral (pfq_get_maxlen hdl >>= throwPFqIf hdl (== -1))


-- |Specify the length of the Rx queue, in number of packets.

setRxSlots :: Ptr PFqTag
           -> Int   -- ^ number of Rx slots
           -> IO ()
setRxSlots hdl value =
    pfq_set_rx_slots hdl (fromIntegral value)
    >>= throwPFqIf_ hdl (== -1)


-- |Return the length of the Rx queue, in number of packets.

getRxSlots :: Ptr PFqTag
           -> IO Int
getRxSlots hdl =
    liftM fromIntegral (pfq_get_rx_slots hdl >>= throwPFqIf hdl (== -1))


-- |Return the length of a Rx slot, in bytes.

getRxSlotSize :: Ptr PFqTag
              -> IO Int
getRxSlotSize hdl =
    liftM fromIntegral (pfq_get_rx_slot_size hdl >>= throwPFqIf hdl (== -1))


-- |Specify the length of the Tx queue, in number of packets.

setTxSlots :: Ptr PFqTag
           -> Int       -- ^ number of Tx slots
           -> IO ()
setTxSlots hdl value =
    pfq_set_tx_slots hdl (fromIntegral value)
    >>= throwPFqIf_ hdl (== -1)


-- |Return the length of the Tx queue, in number of packets.

getTxSlots :: Ptr PFqTag
           -> IO Int
getTxSlots hdl =
    liftM fromIntegral (pfq_get_tx_slots hdl >>= throwPFqIf hdl (== -1))


-- |Bind the main group of the socket to the given device/queue.

bind :: Ptr PFqTag
     -> String      -- ^ device name
     -> Int         -- ^ queue index (or any_queue constant)
     -> IO ()
bind hdl name queue =
    withCString name $ \dev ->
        pfq_bind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- |Unbind the main group of the socket from the given device/queue.

unbind :: Ptr PFqTag
       -> String      -- ^ device name
       -> Int         -- ^ queue index (or any_queue constant)
       -> IO ()
unbind hdl name queue =
    withCString name $ \dev ->
        pfq_unbind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- |Bind the group to the given device/queue.

bindGroup :: Ptr PFqTag
          -> Int         -- ^ group id
          -> String      -- ^ device name
          -> Int         -- ^ queue index (or any_queue constant)
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


-- | Set the socket as egress and bind it to the given device/queue.
--
-- The egress socket is be used by groups as network forwarder.

egressBind :: Ptr PFqTag
           -> String      -- device name
           -> Int         -- queue index
           -> IO ()
egressBind hdl name queue =
    withCString name $ \dev ->
        pfq_egress_bind hdl dev (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- | Unset the socket as egress.

egressUnbind :: Ptr PFqTag
             -> IO ()
egressUnbind hdl =
    pfq_egress_unbind hdl >>= throwPFqIf_ hdl (== -1)


-- |Bind the socket for transmission to the given device name and queue.
--

bindTx :: Ptr PFqTag
       -> String      -- device name
       -> Int         -- hw queue index
       -> Int         -- kthread id (number)
       -> IO ()
bindTx hdl name queue kthread =
    withCString name $ \dev ->
        pfq_bind_tx hdl dev (fromIntegral queue) (fromIntegral kthread) >>= throwPFqIf_ hdl (== -1)


-- |Unbind the socket for transmission.
--
-- Unbind the socket for transmission from any device/queue.

unbindTx :: Ptr PFqTag
         -> IO ()
unbindTx hdl =
    pfq_unbind_tx hdl >>= throwPFqIf_ hdl (== -1)


-- |Join the group with the given class mask and group policy.

joinGroup :: Ptr PFqTag
          -> Int            -- ^ group id
          -> ClassMask      -- ^ class mask
          -> GroupPolicy    -- ^ group policy
          -> IO ()
joinGroup hdl gid ms pol =
    pfq_join_group hdl (fromIntegral gid) (getClassMask ms) (getGroupPolicy pol)
        >>= throwPFqIf_ hdl (== -1)


-- |Leave the group specified by the group id.

leaveGroup :: Ptr PFqTag
           -> Int        -- ^ group id
           -> IO ()
leaveGroup hdl gid =
    pfq_leave_group hdl (fromIntegral gid)
        >>= throwPFqIf_ hdl (== -1)


-- |Set the promiscuous mode for the given interface.
--

setPromisc :: Ptr PFqTag
           -> String    -- ^ device name
           -> Bool      -- ^ toggle: True is on, False off.
           -> IO ()
setPromisc hdl name value =
    withCString name $ \dev ->
        pfq_set_promisc hdl dev (if value then 1 else 0) >>=
            throwPFqIf_ hdl (== -1)

-- |Read packets in place.
--
-- Wait for packets and return a 'NetQueue' descriptor.
--
-- The memory of the socket queue is reset at the next read.
-- A timeout is specified in microseconds.

read :: Ptr PFqTag
     -> Int         -- ^ timeout (msec)
     -> IO NetQueue
read hdl msec =
    allocaBytes #{size struct pfq_net_queue} $ \queue -> do
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


-- |Enable/disable vlan filtering for the given group.

vlanFiltersEnable :: Ptr PFqTag
                  -> Int        -- ^ group id
                  -> Bool       -- ^ toggle: True is on, False off.
                  -> IO ()
vlanFiltersEnable hdl gid value =
    pfq_vlan_filters_enable hdl (fromIntegral gid) (fromIntegral $ if value then 1 else 0 :: Int)
        >>= throwPFqIf_ hdl (== -1)


-- |Specify a capture vlan filter for the given group.
--
-- In addition to standard vlan ids, valid ids are also 'vlan_untag' and 'vlan_anytag'.

vlanSetFilter :: Ptr PFqTag
              -> Int        -- ^ group id
              -> VlanTag    -- ^ vlan id
              -> IO ()
vlanSetFilter hdl gid vid =
    pfq_vlan_set_filter hdl (fromIntegral gid) (fromIntegral $ getVid vid)
        >>= throwPFqIf_ hdl (== -1)


-- |Reset the vlan filter for the given group.

vlanResetFilter :: Ptr PFqTag
                -> Int        -- ^ group id
                -> VlanTag    -- ^ vlan id
                -> IO ()
vlanResetFilter hdl gid vid =
    pfq_vlan_reset_filter hdl (fromIntegral gid) (fromIntegral $ getVid vid)
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
    return Statistics
           { sReceived = fromIntegral (_recv :: CULong)
           , sLost     = fromIntegral (_lost :: CULong)
           , sDropped  = fromIntegral (_drop :: CULong)
           , sSent     = fromIntegral (_sent :: CULong)
           , sDiscard  = fromIntegral (_disc :: CULong)
           , sForward  = fromIntegral (_frwd :: CULong)
           , sKernel   = fromIntegral (_kern :: CULong)
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


padArguments :: Int -> [Argument] -> [Argument]
padArguments n xs = xs ++ replicate (n - length xs) ArgNull

withSingleArg :: Argument
              -> ((IntPtr, CSize, CSize) -> IO a)
              -> IO a
withSingleArg arg callback =
    case arg of
        ArgNull       -> callback (ptrToIntPtr nullPtr, 0                      ,  0)
        ArgFunPtr i   -> callback (ptrToIntPtr nullPtr, fromIntegral i         , -1)
        ArgString s   -> withCString s $ \ ptr -> callback (ptrToIntPtr ptr, 0 , -1)
        ArgVector xs  -> let vec = SV.pack xs in SV.withStartPtr vec $ \ ptr len -> callback (ptrToIntPtr ptr, fromIntegral $ sizeOf (head xs), fromIntegral len)
        ArgSVector xs -> let s = intercalate "\x1e" xs in withCString s $ \ ptr -> callback (ptrToIntPtr ptr, 0, fromIntegral (length xs))
        ArgData v     -> alloca $ \ptr -> poke ptr v >> callback (ptrToIntPtr ptr, fromIntegral $ sizeOf v, -1)


type MarshalFunctionDescr = (CString, [(IntPtr, CSize, CSize)], CSize)

withFunDescr :: FunctionDescr
             -> (MarshalFunctionDescr -> IO a)
             -> IO a
withFunDescr (FunctionDescr symbol args _ next) callback =
    withCString symbol $ \ symbol' ->
        withMany withSingleArg (padArguments 8 args) $ \marArgs ->
            callback (symbol', marArgs, fromIntegral next)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

data StorableFunDescr = StorableFunDescr CString [(IntPtr,CSize,CSize)] CSize

instance Storable StorableFunDescr where
        sizeOf _    = #{size struct pfq_functional_descr}
        alignment _ = alignment (undefined :: CSize)
        peek        = undefined
        poke ptr (StorableFunDescr symbol args next) = do
            pokeByteOff ptr (off 0)  symbol
            forM_ [0..(maxNargs-1)] $ \x -> do
                pokeByteOff ptr (off (x*3 + 1)) (fst3 $ args !! x)
                pokeByteOff ptr (off (x*3 + 2)) (snd3 $ args !! x)
                pokeByteOff ptr (off (x*3 + 3)) (trd3 $ args !! x)
            pokeByteOff ptr (off (1 + maxNargs *3)) next
         where
            off n = sizeOf nullPtr * n
            maxNargs = 8

-- |Specify a functional computation for the given group.
--
-- The functional computation is specified as a pfq-lang expression.
--

setGroupComputation :: Ptr PFqTag
                    -> Int                                     -- ^ group id
                    -> Function (SkBuff -> Action SkBuff)      -- ^ expression (PFq-Lang)
                    -> IO ()

setGroupComputation hdl gid comp = do
    let (descrList, _) = serialize comp 0
    allocaBytes (sizeOf (undefined :: CSize) * 2 + #{size struct pfq_functional_descr} * length descrList) $ \ ptr -> do
        pokeByteOff ptr 0 (fromIntegral (length descrList) :: CSize)     -- size
        pokeByteOff ptr (sizeOf(undefined :: CSize)) (0 :: CSize)        -- entry_point: always the first one!
        withMany withFunDescr descrList $ \marshList -> do
            let offset n = sizeOf(undefined :: CSize) * 2 + #{size struct pfq_functional_descr} * n
            forM_ (zip [0..] marshList) $ \(n, (symbol, parms, next)) ->
                pokeByteOff ptr (offset n)
                    (StorableFunDescr symbol parms (fromIntegral next))
            pfq_set_group_computation hdl (fromIntegral gid) ptr >>= throwPFqIf_ hdl (== -1)


-- |Specify a simple functional computation for the given group, from String.
--
-- This ability is limited to simple pfq-lang functional computations.
-- Only the composition of monadic functions without arguments are currently supported.

setGroupComputationFromString :: Ptr PFqTag
                              -> Int       -- ^ group id
                              -> String    -- ^ simple expression (PFq-Lang)
                              -> IO ()

setGroupComputationFromString hdl gid comp =
    withCString comp $ \ptr ->
            pfq_set_group_computation_from_string hdl (fromIntegral gid) ptr >>= throwPFqIf_ hdl (== -1)


-- |Flush the Tx queue(s)
--
-- Transmit the packets in the Tx queues of the socket.

txQueue :: Ptr PFqTag
        -> Int     -- queue index (0 is valid queue)
        -> IO ()
txQueue hdl queue =
    pfq_tx_queue hdl (fromIntegral queue) >>= throwPFqIf_ hdl (== -1)


-- |Store the packet and transmit the packets in the queue.
--
-- The queue is flushed every fhint packets.
-- Requires the socket is bound for transmission to a net device and queue.
-- See 'bindTx'.

send :: Ptr PFqTag
     -> C.ByteString  -- ^ bytes of packet
     -> Int           -- ^ copies
     -> Int           -- ^ fhint
     -> IO Bool
send hdl xs copies fhint =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        liftM (> 0) $ pfq_send_raw hdl
                        p
                        (fromIntegral l)
                        0
                        0
                        0
                        (fromIntegral fhint)
                        (fromIntegral copies)
                        0
                        (fromIntegral $ getConstant any_queue)


-- |Store the packet and transmit the packets in the queue.
--
-- The queue is flushed every fhint packets.

sendTo :: Ptr PFqTag
       -> C.ByteString  -- ^ bytes of packet
       -> Int           -- ^ ifindex
       -> Int           -- ^ qindex
       -> Int           -- ^ copies
       -> Int           -- ^ fhint
       -> IO Bool
sendTo hdl xs ifindex qindex copies fhint =
    unsafeUseAsCStringLen xs $ \(p, l) ->
    liftM (> 0) $ pfq_send_raw hdl
                    p
                    (fromIntegral l)
                    (fromIntegral ifindex)
                    (fromIntegral qindex)
                    0
                    (fromIntegral fhint)
                    (fromIntegral copies)
                    0
                    (fromIntegral $ getConstant any_queue)


-- |Transmit the packet asynchronously.
--
-- The transmission is handled by PFQ kernel threads.
-- Requires the socket is bound for transmission to one (or multiple) PFQ kernel threads.
-- See 'bindTx'.

sendAsync :: Ptr PFqTag
          -> C.ByteString  -- ^ bytes of packet
          -> Int           -- ^ copies
          -> IO Bool
sendAsync hdl xs copies =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        liftM (> 0) $ pfq_send_raw hdl
                        p
                        (fromIntegral l)
                        0
                        0
                        0
                        0
                        (fromIntegral copies)
                        1
                        (fromIntegral $ getConstant any_queue)


-- |Transmit the packet asynchronously.
--
-- The transmission takes place asynchronously at the given Timespec time.
-- Requires the socket is bound for transmission to one (or multiple) PFQ kernel threads.
-- See 'bindTx'.

sendAt :: Ptr PFqTag
       -> C.ByteString  -- ^ bytes of packet
       -> TimeSpec      -- ^ active timestamp
       -> Int           -- ^ copies
       -> IO Bool
sendAt hdl xs ts copies =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        liftM (> 0) $ pfq_send_raw hdl
                        p
                        (fromIntegral l)
                        0
                        0
                        (fromIntegral (fromIntegral (sec ts) * (1000000000 :: Integer) + fromIntegral (nsec ts)))
                        0
                        (fromIntegral copies)
                        1
                        (fromIntegral $ getConstant any_queue)


-- C functions from libpfq
--

foreign import ccall unsafe pfq_open                :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_nogroup        :: CSize -> CSize -> CSize -> IO (Ptr PFqTag)
foreign import ccall unsafe pfq_open_group          :: CULong -> CInt  -> CSize -> CSize -> CSize -> IO (Ptr PFqTag)

foreign import ccall unsafe pfq_close               :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_error               :: Ptr PFqTag -> IO CString

foreign import ccall unsafe pfq_id                  :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_group_id            :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_enable              :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_disable             :: Ptr PFqTag -> IO CInt
foreign import ccall unsafe pfq_is_enabled          :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_promisc         :: Ptr PFqTag -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_timestamping_enable     :: Ptr PFqTag -> CInt -> IO CInt
foreign import ccall unsafe pfq_is_timestamping_enabled :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_set_caplen          :: Ptr PFqTag -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_caplen          :: Ptr PFqTag -> IO CPtrdiff

foreign import ccall unsafe pfq_set_weight          :: Ptr PFqTag -> CInt -> IO CInt
foreign import ccall unsafe pfq_get_weight          :: Ptr PFqTag -> IO CInt

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

foreign import ccall unsafe pfq_bind_tx             :: Ptr PFqTag -> CString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_unbind_tx           :: Ptr PFqTag -> IO CInt

foreign import ccall unsafe pfq_send_raw            :: Ptr PFqTag -> Ptr CChar -> CSize -> CInt -> CInt -> CULLong -> CSize -> CUInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe pfq_tx_queue            :: Ptr PFqTag -> CInt -> IO CInt

