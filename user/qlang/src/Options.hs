--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import Control.Monad.Trans.Reader
import Data.Maybe
import Data.Data

type OptionT = ReaderT Options

data Options = Options
    -- Pattern:
    {   output              :: Maybe String
    -- IR (intermediate representation):
    ,   json                :: Bool
    ,   fdescr              :: Bool
    -- Generic
    ,   verb                :: Int
    ,   files               :: [FilePath]
    } deriving (Data, Typeable, Show)

