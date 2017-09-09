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

module Options
( OptionT
, Options(..)
, whenLevel
) where

import Control.Monad.Reader
import Data.Data

type OptionT = ReaderT Options

data Options = Options
    -- Generic:
    {   output              :: Maybe String
    ,   modules             :: [String]
    -- IR (intermediate representation):
    ,   json                :: Bool
    ,   fdescr              :: Bool
    ,   gid                 :: Maybe Int
    -- other
    ,   verb                :: Int
    ,   ver                 :: Bool
    ,   file                :: Maybe FilePath
    } deriving (Data, Typeable, Show)



whenLevel :: (Monad m) => Int -> OptionT m () ->  OptionT m ()
whenLevel level run = ask >>= \opt -> when (verb opt >= level) run


