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
--  Module      : Network.PFQ
--  Copyright   : Nicola Bonelli (c) 2012-2015-16
--  License     : GPL
--  Maintainer  : nicola@pfq.io
--  Stability   : experimental
--  Portability : non-portable
--
-- The 'Network.PFQ' module is a low level binding to the
-- functions in @libpfq@.  See <https://github.com/pfq/PFQ/wiki> for more
-- information.
--
------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.PFQ
    (  -- * Handle and pointers

       PfqHandle()
    ,  PfqHandlePtr
    ,  withPfq

       -- * Types

    ,  Statistics(..)
    ,  NetQueue(..)
    ,  Packet(..)
    ,  PktHdr(..)
    ,  Callback
    ,  ClassMask(..)
    ,  class_default
    ,  class_user_plane
    ,  class_control_plane
    ,  class_control
    ,  class_any
    ,  GroupPolicy(..)
    ,  policy_undefined
    ,  policy_priv
    ,  policy_restricted
    ,  policy_shared
    ,  Constant(..)
    ,  any_device
    ,  any_queue
    ,  any_group
    ,  any_kthread
    ,  no_kthread
    ,  version_code
    ,  major_version
    ,  minor_version
    ,  patchlevel_version
    ,  version
    ,  SocketParams(..)
    ,  defaultSocketParams

    ,  FlowKey(..)
    ,  key_5tuple
    ,  key_3tuple

    , key_eth_type
    , key_eth_src
    , key_eth_dst
    , key_ip_src
    , key_ip_dst
    , key_ip_proto
    , key_ip_ecn
    , key_ip_dscp
    , key_src_port
    , key_dst_port
    , key_icmp_type
    , key_icmp_code

    -- * Socket and Groups

    ,  open
    ,  openNoGroup
    ,  openGroup
    ,  openParam
    ,  close

    ,  enable
    ,  disable
    ,  getId
    ,  getGroupId
    ,  isEnabled

       -- * Socket control

    ,  joinGroup
    ,  leaveGroup

    ,  bind
    ,  bindGroup
    ,  unbind
    ,  unbindGroup
    ,  egressBind
    ,  egressUnbind

       -- * Socket parameters

    ,  timestampingEnable
    ,  isTimestampingEnabled

    ,  setWeight
    ,  getWeight

    ,  setPromisc

    ,  getCaplen
    ,  setCaplen

    ,  getRxSlots
    ,  setRxSlots
    ,  getRxSlotSize

    ,  getTxSlots
    ,  setTxSlots
    ,  getXmitlen
    ,  setXmitlen

       -- * Packet capture

    ,  Network.PFQ.read
    ,  dispatch

    ,  getPackets
    ,  getPacketHeader
    ,  isPacketReady
    ,  waitForPacket

    ,  VlanTag(..)
    ,  vlan_untag
    ,  vlan_anytag
    ,  vlanFiltersEnable
    ,  vlanSetFilter
    ,  vlanResetFilter

       -- * Packet transmission

    ,  send
    ,  sendAsync

    ,  syncQueue

    ,  bindTx
    ,  unbindTx

       -- * pfq-lang

    ,  setGroupComputation
    ,  setGroupComputationFromString
    ,  setGroupComputationFromFile
    ,  setGroupComputationFromDescr
    ,  setGroupComputationFromJSON

       -- * Statistics and counters

    ,  getStats
    ,  getGroupStats
    ,  getGroupCounters

    ) where


import Data.Aeson
import Data.Word
import Data.Bits
import Data.Maybe (fromJust)

-- #if __GLASGOW_HASKELL__ < 710
-- import Data.Monoid
-- #else
-- import Data.Monoid()
-- #endif

import Data.Monoid((<>))

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as BL (pack)

import Data.ByteString.Unsafe
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SV
import qualified Foreign.Storable.Newtype as Store

import Control.Monad
import Control.Concurrent

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Concurrent as C (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)

import Network.PFQ.Lang

-- import System.Clock
import System.Process(readProcess)

-- |Packet capture handle.

newtype PfqHandle = PfqHandle (ForeignPtr PfqHandle)

type PfqHandlePtr = Ptr PfqHandle


#include <pfq/pfq.h>

-- |Capture Queue handle.

data NetQueue = NetQueue {
      qPtr      :: Ptr PktHdr               -- ^ pointer to the memory mapped queue
   ,  qLen      :: {-# UNPACK #-} !Word64   -- ^ queue length
   ,  qSlotSize :: {-# UNPACK #-} !Word64   -- ^ size of a slot = pfq header + packet
   ,  qIndex    :: {-# UNPACK #-} !Word32   -- ^ index of the queue
   } deriving (Eq, Show)

-- |PFQ packet header.

data PktHdr = PktHdr {
      hSec      :: {-# UNPACK #-} !Word32   -- ^ timestamp (seconds)
    , hNsec     :: {-# UNPACK #-} !Word32   -- ^ timestamp (nanoseconds)
    , hCapLen   :: {-# UNPACK #-} !Word16   -- ^ capture length
    , hLen      :: {-# UNPACK #-} !Word16   -- ^ packet length (wire size)
    , hIfIndex  :: {-# UNPACK #-} !Word32   -- ^ interface index
    , hMark     :: {-# UNPACK #-} !Word32   -- ^ skb 32-bits mark
    , hState    :: {-# UNPACK #-} !Word32   -- ^ opaque 32-bits state
    , hTci      :: {-# UNPACK #-} !Word16   -- ^ vlan tci
    , hHwQueue  :: {-# UNPACK #-} !Word16    -- ^ hardware queue index
    , hCommit   :: {-# UNPACK #-} !Word32   -- ^ commit bit
    } deriving (Eq, Show)

-- |PFQ statistics.

data Statistics = Statistics {
      sReceived   ::  Integer               -- ^ packets received
    , sLost       ::  Integer               -- ^ packets lost
    , sDropped    ::  Integer               -- ^ packets dropped
    , sSent       ::  Integer               -- ^ packets sent
    , sDiscard    ::  Integer               -- ^ packets discarded
    , sFailure    ::  Integer               -- ^ packets Tx failure
    , sForward    ::  Integer               -- ^ packets forwarded to devices
    , sKernel     ::  Integer               -- ^ packets forwarded to kernel
    } deriving (Eq, Show)

-- |PFQ counters.

newtype Counters = Counters {
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

data SocketParams = SocketParams {
        parCaplen     :: Int            -- ^ capture len
    ,   parRxSlots    :: Int            -- ^ socket Rx queue length
    ,   parXmitLen    :: Int            -- ^ xmit len
    ,   parTxSlots    :: Int            -- ^ socket Tx queue length
    ,   parPolicy     :: GroupPolicy    -- ^ default group policy: policy_undefined means no group
    ,   parClass      :: ClassMask      -- ^ socket class
    } deriving (Eq, Show)


-- |Default values of Socket parameters.

defaultSocketParams :: SocketParams
defaultSocketParams = SocketParams {
        parCaplen  = 1520
    ,   parRxSlots = 4096
    ,   parXmitLen = 1520
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
newtype Constant = Constant { getConstant :: Int }
    deriving (Eq, Show, Read)

-- |Vlan tag.
newtype VlanTag = VlanTag { getVid:: CInt }
    deriving (Eq, Show, Read)

-- |Generic pfq flow-key constant.
newtype FlowKey = FlowKey { getFlowKey:: Word64 }
    deriving (Eq, Show, Read, Bits)


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


#{enum Constant, Constant
    , any_device           = Q_ANY_DEVICE
    , any_queue            = Q_ANY_QUEUE
    , any_group            = Q_ANY_GROUP
    , any_kthread          = Q_ANY_KTHREAD
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

#{enum FlowKey, FlowKey
  , key_eth_type           = Q_KEY_ETH_TYPE
  , key_eth_src            = Q_KEY_ETH_SRC
  , key_eth_dst            = Q_KEY_ETH_DST
  , key_ip_src             = Q_KEY_IP_SRC
  , key_ip_dst             = Q_KEY_IP_DST
  , key_ip_proto           = Q_KEY_IP_PROTO
  , key_ip_ecn             = Q_KEY_IP_ECN
  , key_ip_dscp            = Q_KEY_IP_DSCP
  , key_src_port           = Q_KEY_SRC_PORT
  , key_dst_port           = Q_KEY_DST_PORT
  , key_icmp_type          = Q_KEY_ICMP_TYPE
  , key_icmp_code          = Q_KEY_ICMP_CODE
}

instance Monoid FlowKey where
    mempty = FlowKey 0
    FlowKey a `mappend` FlowKey b = FlowKey (a .|. b)

instance Storable FlowKey where
    sizeOf    = Store.sizeOf getFlowKey
    alignment = Store.alignment getFlowKey
    peek      = Store.peek FlowKey
    poke      = Store.poke getFlowKey


key_5tuple  = key_ip_src <> key_ip_dst <> key_src_port <> key_dst_port <> key_ip_proto
key_3tuple  = key_ip_src <> key_ip_dst <> key_ip_proto


version :: String
version = #{const_str PFQ_VERSION_STRING }


toPktHdr :: Ptr PktHdr -> IO PktHdr
toPktHdr hdr =
    PktHdr <$> #{peek struct pfq_pkthdr, tstamp.tv.sec }  hdr
           <*> #{peek struct pfq_pkthdr, tstamp.tv.nsec}  hdr
           <*> #{peek struct pfq_pkthdr, caplen}          hdr
           <*> #{peek struct pfq_pkthdr, len}             hdr
           <*> #{peek struct pfq_pkthdr, info.ifindex}    hdr
           <*> #{peek struct pfq_pkthdr, info.data.mark}  hdr
           <*> #{peek struct pfq_pkthdr, info.data.state} hdr
           <*> #{peek struct pfq_pkthdr, info.vlan.tci}   hdr
           <*> #{peek struct pfq_pkthdr, info.queue}      hdr
           <*> #{peek struct pfq_pkthdr, info.commit}     hdr

-- | The type of the callback function passed to 'dispatch'.

type Callback = PktHdr -> Ptr Word8  -> IO ()

type CPfqCallback = Ptr Word8 -> Ptr PktHdr -> Ptr Word8 -> IO ()

-- Handle
--

{-# INLINE withPfq #-}
withPfq :: PfqHandle -> (PfqHandlePtr -> IO a) -> IO a
withPfq (PfqHandle hdl) = withForeignPtr hdl


-- Error handling
--

throwPfqIf :: PfqHandlePtr
           -> (a -> Bool)
           -> a
           -> IO a
throwPfqIf hdl p v = if p v
    then pfq_error hdl >>= peekCString >>= ioError . userError
    else return v


throwPfqIf_ :: PfqHandlePtr
            -> (a -> Bool)
            -> a
            -> IO ()
throwPfqIf_ hdl p v = void (throwPfqIf hdl p v)


-- |Return the list of 'Packet' stored in the 'NetQueue'.
getPackets :: NetQueue
           -> IO [Packet]
getPackets nq =
    getPackets' (qIndex nq) (qPtr nq) (qPtr nq `plusPtr` _size) (fromIntegral $ qSlotSize nq)
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
    !_com  <- pHdr p `peekByteOff` (#{size struct pfq_pkthdr}-1)
    return ((_com :: CUChar) == fromIntegral (pIndex p))

{-# INLINE isPacketReady #-}


-- |Wait until the 'Packet' is ready.
waitForPacket :: Packet -> IO ()
waitForPacket p = do
    !ready <- isPacketReady p
    unless ready $ yield >> waitForPacket p

{-# INLINE waitForPacket #-}

-- |Return the 'PktHdr' of the given 'Packet'.
getPacketHeader :: Packet -> IO PktHdr
getPacketHeader p = waitForPacket p >> toPktHdr (pHdr p)


{-# INLINE getPacketHeader #-}


-- |Open a socket and create a new private group.
--
-- The default values for class mask and group policy are 'class_default' and
-- 'policy_priv', respectively.


open  :: Int  -- ^ caplen
      -> Int  -- ^ number of Rx slots
      -> Int  -- ^ xmitlen
      -> Int  -- ^ number of Tx slots
      -> IO PfqHandle
open  caplen rx_slots xmitlen tx_slots =
    pfq_open (fromIntegral caplen) (fromIntegral rx_slots) (fromIntegral xmitlen) (fromIntegral tx_slots) >>=
            throwPfqIf nullPtr (== nullPtr) >>= \ptr ->
                PfqHandle <$> C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open a socket. No group is joined or created.
--
-- Groups can later be joined by means of 'joinGroup' function.

openNoGroup  :: Int  -- ^ caplen
             -> Int  -- ^ number of Rx slots
             -> Int  -- ^ xmitlen
             -> Int  -- ^ number of Tx slots
             -> IO PfqHandle
openNoGroup  caplen rx_slots xmitlen tx_slots =
    pfq_open_nogroup (fromIntegral caplen) (fromIntegral rx_slots) (fromIntegral xmitlen) (fromIntegral tx_slots) >>=
            throwPfqIf nullPtr (== nullPtr) >>= \ptr ->
                PfqHandle <$> C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open the socket and create a new group with the specified parameters.
--
-- If group_policy is 'policy_undefined' no gorup is joined or created.

openGroup :: ClassMask    -- ^ ClassMask (e.g., class_default `mappend` class_control_plane)
          -> GroupPolicy  -- ^ policy for the group
          -> Int          -- ^ caplen
          -> Int          -- ^ number of Rx slots
          -> Int          -- ^ xmitlen
          -> Int          -- ^ number of Tx slots
          -> IO PfqHandle
openGroup ms policy caplen rx_slots xmitlen tx_slots =
        pfq_open_group (getClassMask ms) (getGroupPolicy policy)
                       (fromIntegral caplen) (fromIntegral rx_slots)
                       (fromIntegral xmitlen) (fromIntegral tx_slots) >>=
            throwPfqIf nullPtr (== nullPtr) >>= \ptr ->
                PfqHandle <$> C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Open the socket with 'SocketParams'.
--
-- Default values are defined as 'defaultSocketParams'.

openParam :: SocketParams  -- ^ parameters
          -> IO PfqHandle
openParam  SocketParams{..} =
    pfq_open_group (getClassMask parClass)
                   (getGroupPolicy parPolicy)
                   (fromIntegral parCaplen)
                   (fromIntegral parRxSlots)
                   (fromIntegral parXmitLen)
                   (fromIntegral parTxSlots) >>=
        throwPfqIf nullPtr (== nullPtr) >>= \ptr ->
            PfqHandle <$> C.newForeignPtr ptr (void $ pfq_close ptr)


-- |Close the socket.
--
-- Release the shared memory, stop kernel threads.

close :: PfqHandlePtr
      -> IO ()
close hdl =
    pfq_close hdl >>= throwPfqIf_ hdl (== -1)


-- |Return the id of the socket.

getId :: PfqHandlePtr
      -> IO Int
getId hdl =
    fmap fromIntegral (pfq_id hdl >>= throwPfqIf hdl (== -1))


-- |Return the group-id of the socket.

getGroupId :: PfqHandlePtr
           -> IO Int
getGroupId hdl =
    fmap fromIntegral (pfq_group_id hdl >>= throwPfqIf hdl (== -1))


-- |Enable the socket for packets capture and transmission.
--
-- Allocate the shared memory for socket queues, possibly using
-- the Linux HugePages support.
-- If the enviroment variable PFQ_HUGEPAGES is set to 0 (or
-- PFQ_NO_HUGEPAGES is defined) standard 4K pages are used.

enable :: PfqHandlePtr
       -> IO ()
enable hdl =
    pfq_enable hdl >>= throwPfqIf_ hdl (== -1)


-- |Disable the socket.
--
-- Release the shared memory, stop kernel threads.

disable :: PfqHandlePtr
        -> IO ()
disable hdl = pfq_disable hdl >>= throwPfqIf_ hdl (== -1)


-- |Check whether the socket is enabled.

isEnabled :: PfqHandlePtr
          -> IO Bool
isEnabled hdl =
    pfq_is_enabled hdl >>= throwPfqIf hdl (== -1) >>= \v ->
        return $ v /= 0


-- |Enable/disable timestamping for packets.

timestampingEnable :: PfqHandlePtr
                   -> Bool        -- ^ toggle: True is on, False off.
                   -> IO ()
timestampingEnable hdl toggle = do
    let value = if toggle then 1 else 0
    pfq_timestamping_enable hdl value >>= throwPfqIf_ hdl (== -1)

-- |Check whether timestamping for packets is enabled.

isTimestampingEnabled :: PfqHandlePtr
                      -> IO Bool
isTimestampingEnabled hdl =
    pfq_is_timestamping_enabled hdl >>= throwPfqIf hdl (== -1) >>= \v ->
        return $ v /= 0


-- |Set the weight of the socket for the steering phase.

setWeight :: PfqHandlePtr
          -> Int    -- ^ weight of socket (valid range is [1,16))
          -> IO ()
setWeight hdl value =
    pfq_set_weight hdl (fromIntegral value) >>= throwPfqIf_ hdl (== -1)


-- |Return the weight of the socket.

getWeight :: PfqHandlePtr
          -> IO Int
getWeight hdl =
    fmap fromIntegral (pfq_get_weight hdl >>= throwPfqIf hdl (== -1))


-- |Specify the capture length of packets, in bytes.
--
-- Capture length must be set before the socket is enabled.

setCaplen :: PfqHandlePtr
          -> Int        -- ^ caplen (bytes)
          -> IO ()
setCaplen hdl value =
    pfq_set_caplen hdl (fromIntegral value)
        >>= throwPfqIf_ hdl (== -1)


-- |Return the capture length of packets, in bytes.

getCaplen :: PfqHandlePtr
          -> IO Int
getCaplen hdl =
    fmap fromIntegral (pfq_get_caplen hdl >>= throwPfqIf hdl (== -1))


-- |Return the max transmission length of packets, in bytes.

getXmitlen :: PfqHandlePtr
          -> IO Int
getXmitlen hdl =
    fmap fromIntegral (pfq_get_xmitlen hdl >>= throwPfqIf hdl (== -1))

-- |Specify the transmission length of packets, in bytes.
--
-- Transmission length must be set before the socket is enabled.

setXmitlen :: PfqHandlePtr
           -> Int        -- ^ xmitlen (bytes)
           -> IO ()
setXmitlen hdl value =
    pfq_set_xmitlen hdl (fromIntegral value)
        >>= throwPfqIf_ hdl (== -1)


-- |Specify the length of the Rx queue, in number of packets.

setRxSlots :: PfqHandlePtr
           -> Int   -- ^ number of Rx slots
           -> IO ()
setRxSlots hdl value =
    pfq_set_rx_slots hdl (fromIntegral value)
    >>= throwPfqIf_ hdl (== -1)


-- |Return the length of the Rx queue, in number of packets.

getRxSlots :: PfqHandlePtr
           -> IO Int
getRxSlots hdl =
    fmap fromIntegral (pfq_get_rx_slots hdl >>= throwPfqIf hdl (== -1))


-- |Return the length of a Rx slot, in bytes.

getRxSlotSize :: PfqHandlePtr
              -> IO Int
getRxSlotSize hdl =
    fmap fromIntegral (pfq_get_rx_slot_size hdl >>= throwPfqIf hdl (== -1))


-- |Specify the length of the Tx queue, in number of packets.

setTxSlots :: PfqHandlePtr
           -> Int       -- ^ number of Tx slots
           -> IO ()
setTxSlots hdl value =
    pfq_set_tx_slots hdl (fromIntegral value)
        >>= throwPfqIf_ hdl (== -1)


-- |Return the length of the Tx queue, in number of packets.

getTxSlots :: PfqHandlePtr
           -> IO Int
getTxSlots hdl =
    fmap fromIntegral (pfq_get_tx_slots hdl >>= throwPfqIf hdl (== -1))


-- |Bind the main group of the socket to the given device/queue.

bind :: PfqHandlePtr
     -> String      -- ^ device name
     -> Int         -- ^ queue index (or any_queue constant)
     -> IO ()
bind hdl name queue =
    withCString name $ \dev ->
        pfq_bind hdl dev (fromIntegral queue) >>= throwPfqIf_ hdl (== -1)


-- |Unbind the main group of the socket from the given device/queue.

unbind :: PfqHandlePtr
       -> String      -- ^ device name
       -> Int         -- ^ queue index (or any_queue constant)
       -> IO ()
unbind hdl name queue =
    withCString name $ \dev ->
        pfq_unbind hdl dev (fromIntegral queue) >>= throwPfqIf_ hdl (== -1)


-- |Bind the group to the given device/queue.

bindGroup :: PfqHandlePtr
          -> Int         -- ^ group id
          -> String      -- ^ device name
          -> Int         -- ^ queue index (or any_queue constant)
          -> IO ()
bindGroup hdl gid name queue =
    withCString name $ \dev ->
        pfq_bind_group hdl (fromIntegral gid) dev (fromIntegral queue) >>= throwPfqIf_ hdl (== -1)


-- |Unbind the group from the given device/queue.

unbindGroup :: PfqHandlePtr
            -> Int         -- ^ group id
            -> String      -- ^ device name
            -> Int         -- ^ queue index
            -> IO ()
unbindGroup hdl gid name queue =
    withCString name $ \dev ->
        pfq_unbind_group hdl (fromIntegral gid) dev (fromIntegral queue) >>= throwPfqIf_ hdl (== -1)


-- | Set the socket as egress and bind it to the given device/queue.
--
-- The egress socket is be used by groups as network forwarder.

egressBind :: PfqHandlePtr
           -> String      -- ^ device name
           -> Int         -- ^ queue index
           -> IO ()
egressBind hdl name queue =
    withCString name $ \dev ->
        pfq_egress_bind hdl dev (fromIntegral queue) >>= throwPfqIf_ hdl (== -1)


-- | Unset the socket as egress.

egressUnbind :: PfqHandlePtr
             -> IO ()
egressUnbind hdl =
    pfq_egress_unbind hdl >>= throwPfqIf_ hdl (== -1)


-- |Bind the socket for transmission to the given device name and queue.
--

bindTx :: PfqHandlePtr
       -> String      -- ^ device name
       -> Int         -- ^ hw queue index
       -> Int         -- ^ PFQ thread id (number)
       -> IO ()
bindTx hdl name queue kthread =
    withCString name $ \dev ->
        pfq_bind_tx hdl dev (fromIntegral queue) (fromIntegral kthread) >>= throwPfqIf_ hdl (== -1)


-- |Unbind the socket for transmission.
--
-- Unbind the socket for transmission from any device/queue.

unbindTx :: PfqHandlePtr
         -> IO ()
unbindTx hdl =
    pfq_unbind_tx hdl >>= throwPfqIf_ hdl (== -1)


-- |Join the group with the given class mask and group policy.

joinGroup :: PfqHandlePtr
          -> Int            -- ^ group id
          -> ClassMask      -- ^ class mask
          -> GroupPolicy    -- ^ group policy
          -> IO ()
joinGroup hdl gid ms pol =
    pfq_join_group hdl (fromIntegral gid) (getClassMask ms) (getGroupPolicy pol)
        >>= throwPfqIf_ hdl (== -1)


-- |Leave the group specified by the group id.

leaveGroup :: PfqHandlePtr
           -> Int        -- ^ group id
           -> IO ()
leaveGroup hdl gid =
    pfq_leave_group hdl (fromIntegral gid)
        >>= throwPfqIf_ hdl (== -1)


-- |Set the promiscuous mode for the given interface.
--

setPromisc :: PfqHandlePtr
           -> String    -- ^ device name
           -> Bool      -- ^ toggle: True is on, False off.
           -> IO ()
setPromisc hdl name value =
    withCString name $ \dev ->
        pfq_set_promisc hdl dev (if value then 1 else 0) >>=
            throwPfqIf_ hdl (== -1)

-- |Read packets in place.
--
-- Wait for packets and return a 'NetQueue' descriptor.
--
-- The memory of the socket queue is reset at the next read.
-- A timeout is specified in microseconds.

read :: PfqHandlePtr
     -> Int         -- ^ timeout (msec)
     -> IO NetQueue
read hdl msec =
    allocaBytes #{size struct pfq_net_queue} $ \qptr -> do
       pfq_read hdl qptr (fromIntegral msec) >>= throwPfqIf_ hdl (== -1)
       NetQueue <$> #{peek struct pfq_net_queue, queue} qptr
                <*> fmap fromIntegral (#{peek struct pfq_net_queue, len} qptr       :: IO CSize)
                <*> fmap fromIntegral (#{peek struct pfq_net_queue, slot_size} qptr :: IO CSize)
                <*> fmap fromIntegral (#{peek struct pfq_net_queue, index} qptr     :: IO CUInt)

-- |Collect and process packets.
--
-- This function is passed a function 'Callback' which is called on each packet.

dispatch :: PfqHandlePtr
         -> Callback    -- ^ packet processing function
         -> Int         -- ^ timeout (msec)
         -> IO ()       --
dispatch hdl f timeo = do
    cback <- makeCallback f
    ret  <- pfq_dispatch hdl cback (fromIntegral timeo) nullPtr
    freeHaskellFunPtr cback
    throwPfqIf_ hdl (== (-1 :: Integer)) (fromIntegral ret)


makeCallback :: Callback
             -> IO (FunPtr CPfqCallback)
makeCallback fun = make_callback $ \_ hdr ptr -> toPktHdr hdr >>= flip fun ptr


-- |Enable/disable vlan filtering for the given group.

vlanFiltersEnable :: PfqHandlePtr
                  -> Int        -- ^ group id
                  -> Bool       -- ^ toggle: True is on, False off.
                  -> IO ()
vlanFiltersEnable hdl gid value =
    pfq_vlan_filters_enable hdl (fromIntegral gid) (fromIntegral $ if value then 1 else 0 :: Int)
        >>= throwPfqIf_ hdl (== -1)


-- |Specify a capture vlan filter for the given group.
--
-- In addition to standard vlan ids, valid ids are also 'vlan_untag' and 'vlan_anytag'.

vlanSetFilter :: PfqHandlePtr
              -> Int        -- ^ group id
              -> VlanTag    -- ^ vlan id
              -> IO ()
vlanSetFilter hdl gid vid =
    pfq_vlan_set_filter hdl (fromIntegral gid) (fromIntegral $ getVid vid)
        >>= throwPfqIf_ hdl (== -1)


-- |Reset the vlan filter for the given group.

vlanResetFilter :: PfqHandlePtr
                -> Int        -- ^ group id
                -> VlanTag    -- ^ vlan id
                -> IO ()
vlanResetFilter hdl gid vid =
    pfq_vlan_reset_filter hdl (fromIntegral gid) (fromIntegral $ getVid vid)
        >>= throwPfqIf_ hdl (== -1)


-- |Return the socket statistics.

getStats :: PfqHandlePtr
         -> IO Statistics
getStats hdl =
    allocaBytes #{size struct pfq_stats} $ \sp -> do
        pfq_get_stats hdl sp >>= throwPfqIf_ hdl (== -1)
        makeStats sp


-- |Return the statistics of the given group.

getGroupStats :: PfqHandlePtr
              -> Int            -- ^ group id
              -> IO Statistics
getGroupStats hdl gid =
    allocaBytes #{size struct pfq_stats} $ \sp -> do
        pfq_get_group_stats hdl (fromIntegral gid) sp >>= throwPfqIf_ hdl (== -1)
        makeStats sp


makeStats :: Ptr a
          -> IO Statistics
makeStats p =
    Statistics <$> fmap fromIntegral (#{peek struct pfq_stats, recv} p :: IO CULong)
               <*> fmap fromIntegral (#{peek struct pfq_stats, lost} p :: IO CULong)
               <*> fmap fromIntegral (#{peek struct pfq_stats, drop} p :: IO CULong)
               <*> fmap fromIntegral (#{peek struct pfq_stats, sent} p :: IO CULong)
               <*> fmap fromIntegral (#{peek struct pfq_stats, disc} p :: IO CULong)
               <*> fmap fromIntegral (#{peek struct pfq_stats, fail} p :: IO CULong)
               <*> fmap fromIntegral (#{peek struct pfq_stats, frwd} p :: IO CULong)
               <*> fmap fromIntegral (#{peek struct pfq_stats, kern} p :: IO CULong)

-- |Return the set of counters of the given group.

getGroupCounters :: PfqHandlePtr
                 -> Int            -- ^ group id
                 -> IO Counters
getGroupCounters hdl gid =
    allocaBytes #{size struct pfq_counters} $ \sp -> do
        pfq_get_group_counters hdl (fromIntegral gid) sp >>= throwPfqIf_ hdl (== -1)
        makeCounters sp


makeCounters :: Ptr a
             -> IO Counters
makeCounters ptr = do
    cs <- forM [0.. getConstant group_max_counters - 1] $ \ n -> peekByteOff ptr (sizeOf (undefined :: CULong) * n)
    return $ Counters $ map fromIntegral (cs :: [CULong])


padArguments :: Int
             -> [Argument]
             -> [Argument]
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
        ArgStrings xs -> withMany withCString xs $ \xs' -> let vec = SV.pack xs' in SV.withStartPtr vec $ \ ptr len -> callback (ptrToIntPtr ptr, 0, fromIntegral len)
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
        sizeOf _    = #{size struct pfq_lang_functional_descr}
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

setGroupComputation :: PfqHandlePtr
                    -> Int                                     -- ^ group id
                    -> Function (Qbuff -> Action Qbuff)      -- ^ expression (pfq-Lang)
                    -> IO ()

setGroupComputation hdl gid comp =
    setGroupComputationFromDescr hdl gid (fst $ serialize comp 0)


-- |Specify a functional computation for the given group, as pfq-lang program from file.
--

setGroupComputationFromFile :: PfqHandlePtr
                            -> Int       -- ^ group id
                            -> FilePath  -- ^ pfq-lang file
                            -> IO ()

setGroupComputationFromFile hdl gid file =
  readFile file >>= \str -> length str `seq` setGroupComputationFromString hdl gid str

-- |Specify a functional computation for the given group, as pfq-lang program from string.
--

setGroupComputationFromString :: PfqHandlePtr
                              -> Int       -- ^ group id
                              -> String    -- ^ pfq-lang expression
                              -> IO ()

setGroupComputationFromString hdl gid comp =
  readProcess "pfq-lang" ["--json"] comp >>= setGroupComputationFromJSON hdl gid



-- |Specify a functional computation for the given group, from JSON description.
--

setGroupComputationFromJSON :: PfqHandlePtr
                            -> Int       -- ^ group id
                            -> String    -- ^ decode (json) :: [FunctionDesc]
                            -> IO ()

setGroupComputationFromJSON hdl gid comp =
    setGroupComputationFromDescr hdl gid (fromJust $ decode (BL.pack comp))


-- |Specify a functional computation for the given group.
--
-- The functional computation is specified as a list of FuncitonDescr.
--

setGroupComputationFromDescr :: PfqHandlePtr
                             -> Int                  -- ^ group id
                             -> [FunctionDescr]      -- ^ expression (pfq-lang)
                             -> IO ()
setGroupComputationFromDescr hdl gid descr =
    allocaBytes (sizeOf (undefined :: CSize) * 2 + #{size struct pfq_lang_functional_descr} * length descr) $ \ ptr -> do
        pokeByteOff ptr 0 (fromIntegral (length descr) :: CSize)     -- size
        pokeByteOff ptr (sizeOf(undefined :: CSize)) (0 :: CSize)    -- entry_point: always the first one!
        withMany withFunDescr descr $ \marshList -> do
            let mkOffset n = sizeOf(undefined :: CSize) * 2 + #{size struct pfq_lang_functional_descr} * n
            forM_ (zip [0..] marshList) $ \(n, (symbol, parms, next)) ->
                pokeByteOff ptr (mkOffset n)
                    (StorableFunDescr symbol parms (fromIntegral next))
            pfq_set_group_computation hdl (fromIntegral gid) ptr >>= throwPfqIf_ hdl (== -1)


-- |Sync the Tx queue(s)
--
-- Transmit the packets in the Tx queues of the socket.

syncQueue :: PfqHandlePtr
          -> Int     -- ^ queue index (0 is sync tx queue)
          -> IO ()
syncQueue hdl queue =
    pfq_sync_queue hdl (fromIntegral queue) >>= throwPfqIf_ hdl (== -1)


-- |Store the packet and transmit the packets in the queue.
--
-- The queue is flushed every sync packets.
-- Requires the socket is bound for transmission to a net device and queue.
-- See 'bindTx'.

send :: PfqHandlePtr
     -> C.ByteString  -- ^ bytes of packet
     -> Int           -- ^ copies
     -> Int           -- ^ sync
     -> IO Bool
send hdl xs copies sync =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        fmap (> 0) $ pfq_send hdl
                        p
                        (fromIntegral l)
                        (fromIntegral copies)
                        (fromIntegral sync) >>= throwPfqIf hdl (== -1)


-- |Transmit the packet asynchronously.
--
-- The transmission is handled by PFQ kernel threads.
-- Requires the socket is bound for transmission to one (or multiple) PFQ kernel threads.
-- See 'bindTx'.

sendAsync :: PfqHandlePtr
          -> C.ByteString  -- ^ bytes of packet
          -> Int           -- ^ copies
          -> IO Bool
sendAsync hdl xs copies =
    unsafeUseAsCStringLen xs $ \(p, l) ->
        fmap (> 0) $ pfq_send_raw hdl
                        p
                        (fromIntegral l)
                        0
                        (fromIntegral copies)
                        (fromIntegral $ getConstant any_queue) >>= throwPfqIf hdl (== -1)


-- C functions from libpfq
--

foreign import ccall unsafe pfq_open                :: CSize -> CSize -> CSize -> CSize -> IO PfqHandlePtr
foreign import ccall unsafe pfq_open_nogroup        :: CSize -> CSize -> CSize -> CSize -> IO PfqHandlePtr
foreign import ccall unsafe pfq_open_group          :: CULong -> CInt  -> CSize -> CSize -> CSize -> CSize -> IO PfqHandlePtr

foreign import ccall unsafe pfq_close               :: PfqHandlePtr -> IO CInt
foreign import ccall unsafe pfq_error               :: PfqHandlePtr -> IO CString

foreign import ccall unsafe pfq_id                  :: PfqHandlePtr -> IO CInt
foreign import ccall unsafe pfq_group_id            :: PfqHandlePtr -> IO CInt

foreign import ccall unsafe pfq_enable              :: PfqHandlePtr -> IO CInt
foreign import ccall unsafe pfq_disable             :: PfqHandlePtr -> IO CInt
foreign import ccall unsafe pfq_is_enabled          :: PfqHandlePtr -> IO CInt

foreign import ccall unsafe pfq_set_promisc         :: PfqHandlePtr -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_timestamping_enable     :: PfqHandlePtr -> CInt -> IO CInt
foreign import ccall unsafe pfq_is_timestamping_enabled :: PfqHandlePtr -> IO CInt

foreign import ccall unsafe pfq_set_caplen          :: PfqHandlePtr -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_caplen          :: PfqHandlePtr -> IO CPtrdiff

foreign import ccall unsafe pfq_set_weight          :: PfqHandlePtr -> CInt -> IO CInt
foreign import ccall unsafe pfq_get_weight          :: PfqHandlePtr -> IO CInt

foreign import ccall unsafe pfq_set_xmitlen         :: PfqHandlePtr -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_xmitlen         :: PfqHandlePtr -> IO CPtrdiff

foreign import ccall unsafe pfq_set_tx_slots        :: PfqHandlePtr -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_tx_slots        :: PfqHandlePtr -> IO CSize

foreign import ccall unsafe pfq_set_rx_slots        :: PfqHandlePtr -> CSize -> IO CInt
foreign import ccall unsafe pfq_get_rx_slots        :: PfqHandlePtr -> IO CSize
foreign import ccall unsafe pfq_get_rx_slot_size    :: PfqHandlePtr -> IO CSize

foreign import ccall unsafe pfq_bind                :: PfqHandlePtr -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_bind_group          :: PfqHandlePtr -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_unbind              :: PfqHandlePtr -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_unbind_group        :: PfqHandlePtr -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe pfq_egress_bind         :: PfqHandlePtr -> CString -> CInt -> IO CInt
foreign import ccall unsafe pfq_egress_unbind       :: PfqHandlePtr -> IO CInt

foreign import ccall unsafe pfq_join_group          :: PfqHandlePtr -> CInt -> CULong -> CInt -> IO CInt
foreign import ccall unsafe pfq_leave_group         :: PfqHandlePtr -> CInt -> IO CInt

foreign import ccall unsafe pfq_get_stats           :: PfqHandlePtr -> Ptr Statistics -> IO CInt
foreign import ccall unsafe pfq_get_group_stats     :: PfqHandlePtr -> CInt -> Ptr Statistics -> IO CInt
foreign import ccall unsafe pfq_get_group_counters  :: PfqHandlePtr -> CInt -> Ptr Counters -> IO CInt

foreign import ccall unsafe pfq_set_group_computation :: PfqHandlePtr -> CInt -> Ptr a -> IO CInt
foreign import ccall unsafe pfq_set_group_computation_from_string :: PfqHandlePtr -> CInt -> CString -> IO CInt
foreign import ccall unsafe pfq_set_group_computation_from_file   :: PfqHandlePtr -> CInt -> CString -> IO CInt

foreign import ccall pfq_dispatch                   :: PfqHandlePtr -> FunPtr CPfqCallback -> CLong -> Ptr Word8 -> IO CInt
foreign import ccall "wrapper" make_callback        :: CPfqCallback -> IO (FunPtr CPfqCallback)

foreign import ccall unsafe pfq_read                :: PfqHandlePtr -> Ptr NetQueue -> CLong -> IO CInt

foreign import ccall unsafe pfq_vlan_filters_enable :: PfqHandlePtr -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_vlan_set_filter     :: PfqHandlePtr -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_vlan_reset_filter   :: PfqHandlePtr -> CInt -> CInt -> IO CInt

foreign import ccall unsafe pfq_bind_tx             :: PfqHandlePtr -> CString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe pfq_unbind_tx           :: PfqHandlePtr -> IO CInt

foreign import ccall unsafe pfq_send                :: PfqHandlePtr -> Ptr CChar -> CSize -> CSize -> CUInt -> IO CInt
foreign import ccall unsafe pfq_send_raw            :: PfqHandlePtr -> Ptr CChar -> CSize -> CULLong -> CUInt -> CInt -> IO CInt

foreign import ccall unsafe pfq_sync_queue          :: PfqHandlePtr -> CInt -> IO CInt

