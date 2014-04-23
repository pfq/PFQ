--
--    Copyright (c) 2014 Nicola Bonelli
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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

import Control.Monad
import Control.Exception
import Test.QuickCheck

data Action a where
        Drop  :: forall a.   Action a
        Pass  :: forall a.   a -> Action a
        Copy  :: forall a.   a -> Action a
        Steer :: forall a b. b -> a -> Action a
        Class :: forall a b. b -> a -> Action a

instance Monad Action where

        return = Pass

        (Drop)      >>=  _  = Drop
        (Pass x)    >>=  k  = k x

        (Copy x)    >>=  k  = case k x of
                                  (Pass x) -> Copy x
                                  _        -> k x

        (Steer b x) >>=  k  = case k x of
                                  (Pass x) -> Steer b x
                                  _        -> k x

        (Class b x) >>=  k  = case k x of
                                  (Pass x) -> Class b x
                                  _        -> k x


instance Eq (Action a) where
        Drop == Drop = True
        (Pass x1) == (Pass x2) = True
        (Copy x1) == (Copy x2) = True
        (Steer _ x1) == (Steer _ x2) = True
        (Class _ x1) == (Class _ x2) = True
        _ == _ = False

-- simple monadic functions
--

drop' :: a -> Action a
drop' _ = Drop

newtype Skb = Skb ()
skb = Skb ()

main = do

    quickCheck ((return skb >>= drop' ) == Drop)
    quickCheck ((return skb >>= Pass ) == Pass skb)
    quickCheck ((return skb >>= Copy ) == Copy skb)
    quickCheck ((return skb >>= Steer 1) == Steer 1 skb)
    quickCheck ((return skb >>= Class 1) == Class 1 skb)

    quickCheck (( Drop        >>= return)  == Drop)
    quickCheck (( Pass skb    >>= return ) == Pass skb)
    quickCheck (( Copy skb    >>= return ) == Copy skb)
    quickCheck (( Steer 1 skb >>= return ) == Steer 1 skb)
    quickCheck (( Class 1 skb >>= return ) == Class 1 skb)

    quickCheck ((return skb >>= drop' >>= Pass   ) == (return skb >>= (\x -> drop' x >>= Pass )))
    quickCheck ((return skb >>= drop' >>= Copy   ) == (return skb >>= (\x -> drop' x >>= Copy )))
    quickCheck ((return skb >>= drop' >>= drop'  ) == (return skb >>= (\x -> drop' x >>= drop' )))
    quickCheck ((return skb >>= drop' >>= Class 1) == (return skb >>= (\x -> drop' x >>= Class 1)))
    quickCheck ((return skb >>= drop' >>= Steer 1) == (return skb >>= (\x -> drop' x >>= Steer 1)))

    quickCheck ((return skb >>= Pass >>= Pass   ) == (return skb >>= (\x -> Pass x >>= Pass   )))
    quickCheck ((return skb >>= Pass >>= Copy   ) == (return skb >>= (\x -> Pass x >>= Copy   )))
    quickCheck ((return skb >>= Pass >>= drop'  ) == (return skb >>= (\x -> Pass x >>= drop'  )))
    quickCheck ((return skb >>= Pass >>= Class 1) == (return skb >>= (\x -> Pass x >>= Class 1)))
    quickCheck ((return skb >>= Pass >>= Steer 1) == (return skb >>= (\x -> Pass x >>= Steer 1)))

    quickCheck ((return skb >>= Copy >>= Pass   ) == (return skb >>= (\x -> Copy x >>= Pass   )))
    quickCheck ((return skb >>= Copy >>= Copy   ) == (return skb >>= (\x -> Copy x >>= Copy   )))
    quickCheck ((return skb >>= Copy >>= drop'  ) == (return skb >>= (\x -> Copy x >>= drop'  )))
    quickCheck ((return skb >>= Copy >>= Class 1) == (return skb >>= (\x -> Copy x >>= Class 1)))
    quickCheck ((return skb >>= Copy >>= Steer 1) == (return skb >>= (\x -> Copy x >>= Steer 1)))

    quickCheck ((return skb >>= Steer 1 >>= Pass   ) == (return skb >>= (\x -> Steer 1 x >>= Pass   )))
    quickCheck ((return skb >>= Steer 1 >>= Copy   ) == (return skb >>= (\x -> Steer 1 x >>= Copy   )))
    quickCheck ((return skb >>= Steer 1 >>= drop'  ) == (return skb >>= (\x -> Steer 1 x >>= drop'  )))
    quickCheck ((return skb >>= Steer 1 >>= Class 1) == (return skb >>= (\x -> Steer 1 x >>= Class 1)))
    quickCheck ((return skb >>= Steer 1 >>= Steer 1) == (return skb >>= (\x -> Steer 1 x >>= Steer 1)))

    quickCheck ((return skb >>= Class 1 >>= Pass   ) == (return skb >>= (\x -> Class 1 x >>= Pass   )))
    quickCheck ((return skb >>= Class 1 >>= Copy   ) == (return skb >>= (\x -> Class 1 x >>= Copy   )))
    quickCheck ((return skb >>= Class 1 >>= drop'  ) == (return skb >>= (\x -> Class 1 x >>= drop'  )))
    quickCheck ((return skb >>= Class 1 >>= Class 1) == (return skb >>= (\x -> Class 1 x >>= Class 1)))
    quickCheck ((return skb >>= Class 1 >>= Steer 1) == (return skb >>= (\x -> Class 1 x >>= Steer 1)))

