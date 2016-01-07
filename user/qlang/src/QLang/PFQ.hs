-- Copyright (c) 2015 Nicola Bonelli <nicola@pfq.io>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

{-# LANGUAGE TupleSections #-}

module QLang.PFQ where

import Language.Haskell.Interpreter
import Network.PFQ.Lang as Q
import Network.PFQ as Q

import Control.Monad.Trans.Reader
import Control.Monad()
import Options

import Data.Maybe
import Foreign.ForeignPtr


load :: Q.Function (SkBuff -> Action SkBuff) -> OptionT IO String
load comp = do
  gid' <- return . fromJust . gid  =<< ask
  lift $ Q.openNoGroup 64 4096 4096 >>= \fp ->
              withForeignPtr fp $ \ctrl -> do
                Q.joinGroup ctrl gid' class_control policy_shared
                Q.setGroupComputation ctrl gid' comp
                return $ "PFQ: computation loaded for gid " ++ show gid' ++ "."


