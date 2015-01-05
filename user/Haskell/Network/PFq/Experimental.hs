--    Copyright (c) 2011-14, Nicola Bonelli
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

{-# LANGUAGE ImpredicativeTypes #-}

module Network.PFq.Experimental
    (
        -- * Experimental Functions
        -- | This set of experimental functions may be subject to changes in future releases

        dummy        ,
        dummy_vector ,
        dummy_string ,
        dummy_strings,

        crc16      ,

        class'     ,
        deliver    ,

    ) where


import Network.PFq.Lang
import Foreign.C.Types

-- Experimental in-kernel computations

-- | Specify the class mask for the given packet.

class'  = MFunction1 "class"   :: CInt -> NetFunction

deliver = MFunction1 "deliver" :: CInt -> NetFunction

dummy = MFunction1 "dummy" :: CInt -> NetFunction


dummy_vector  :: [CInt] -> NetFunction
dummy_vector xs  = MFunction1 "dummy_vector" xs

dummy_string :: String -> NetFunction
dummy_string xs  = MFunction1 "dummy_string" xs

dummy_strings :: [String] -> NetFunction
dummy_strings xs  = MFunction1 "dummy_strings" xs


crc16 = MFunction "crc16" :: NetFunction



