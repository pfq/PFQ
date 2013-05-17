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

{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
     
module Network.PFqLang
    (
        StorableContext(..),
        Computation(..)
    ) where

import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Cont
import Control.Concurrent.STM
import Foreign.Storable


-- Placeholder types: sk_buff and Action, defined in the kernel
--

newtype SkBuff = SkBuff ()

newtype Action = Action ()

-- Placeholder for in-kernel monads:
--

type Monitor action state = ContT action (StateT state STM)


-- prototype for in-kernel Monitor style functions...
--

comp_type :: (SkBuff, Action) -> Monitor Action a (SkBuff,Action)  
comp_type (skb,a) = return (skb,a)


-- Packet Function computation
--

data StorableContext = forall c. (Storable c) => StorableContext c

data Computation a = Computation a String (Maybe StorableContext) |
                     Composition a [String] [Maybe StorableContext]


instance Show (Computation a) where
    show (Computation _  n _) = show n
    show (Composition _ ns _) = show ns


-- operator: >->
--
    
(>->) :: (Monad m) => Computation (a -> m b) -> Computation (b -> m c) -> Computation (a -> m c) 
(Computation f1 n1 c1) >-> (Computation f2 n2 c2) = Composition (f1 >=> f2) [n1, n2] [c1, c2]  
(Composition f1 ns cs) >-> (Computation f2 n2 c2) = Composition (f1 >=> f2) (ns ++ [n2]) (cs ++ [c2])  
(Computation f1 n1 c1) >-> (Composition f2 n2 c2) = Composition (f1 >=> f2) (n1 : n2) (c1 : c2)  
(Composition f1 n1 c1) >-> (Composition f2 n2 c2) = Composition (f1 >=> f2) (n1 ++ n2) (c1 ++ c2)  


-- Predefined in-kernel computations
--

steer_mac    = Computation comp_type "steer-mac"     Nothing  
steer_vlan   = Computation comp_type "steer-vlan-id" Nothing  
steer_ipv4   = Computation comp_type "steer-ipv4"    Nothing  
steer_ipv6   = Computation comp_type "steer-ipv6"    Nothing  
steer_flow   = Computation comp_type "steer-flow"    Nothing  
steer_rtp    = Computation comp_type "steer-rtp"     Nothing  
                                    
clone        = Computation comp_type "clone"         Nothing  
broadcast    = Computation comp_type "broadcast"     Nothing  
                                    
vlan         = Computation comp_type "vlan"          Nothing  
ipv4         = Computation comp_type "ipv4"          Nothing  
udp          = Computation comp_type "udp"           Nothing  
tcp          = Computation comp_type "tcp"           Nothing  
flow         = Computation comp_type "flow"          Nothing  
                                    
strict_vlan  = Computation comp_type "strict-vlan"   Nothing  
strict_ipv4  = Computation comp_type "strict-ipv4"   Nothing  
strict_udp   = Computation comp_type "strict-udp"    Nothing  
strict_tcp   = Computation comp_type "strict-tcp"    Nothing  
strict_flow  = Computation comp_type "strict-flow"   Nothing  
                                    
neg          = Computation comp_type "neg"           Nothing  
par          = Computation comp_type "par"           Nothing  

-- ctx       = Computation comp_type "example"       -- udp >-> (ctx 0) >-> steer-ipv4

