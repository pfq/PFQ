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

module CmdLine where

import System.Console.CmdArgs
import Data.Version (showVersion)

import Options
import Paths_qlang

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
          {     output = Nothing &= typ "FILE"   &= help "write output to FILE"
          ,     modules = []     &= typ "FILE"   &= help "specify additional modules to import" &= name "import"
          ,     json = False     &= groupname "IR:" &=help "Format output as json object" &= explicit &= name "json"
          ,     fdescr = False   &= help "Format output as list of function descriptors" &= explicit &= name "fdescr"
          ,     verb = 0         &= help "Control verbosity level (0..3)" &= explicit &= name "verbosity"
          ,     files = []       &= args
          } &= summary ("qlang " ++ showVersion version)  &= program "qlang"

