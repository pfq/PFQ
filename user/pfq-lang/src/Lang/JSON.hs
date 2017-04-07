-- Copyright (c) 2015-16 Nicola Bonelli <nicola@pfq.io>
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

module Lang.JSON
(
  compile
) where

import Network.PFQ.Lang as Q

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C
import Options


compile :: (Monad m) => Q.Function (Qbuff -> Action Qbuff) -> OptionT m String
-- compile comp = return (C.unpack $ A.encode comp)
compile comp = return (C.unpack $ A.encode $ fst (Q.serialize comp 0))


