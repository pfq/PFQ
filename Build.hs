--
-- Copyright (c) 2014 Nicola Bonelli <nicola@pfq.io>
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

import Development.SimpleBuilder

import System.Environment
import Control.Monad


script :: Script
script =
    [
        Configure "pfq.ko"       *>>  into "kernel/" [],
        Build     "pfq.ko"       *>>  into "kernel/" ["make -j" ++ show numberOfPhyCores ],
        Install   "pfq.ko"       *>>  into "kernel/" ["make install"] .|. [Build "pfq.ko"],
        Clean     "pfq.ko"       *>>  into "kernel/" ["make clean"],

        Configure "pfq-clib"     *>>  into "user/C/" ["cmake ."],
        Build     "pfq-clib"     *>>  into "user/C/" ["make"]         .|. [Configure "pfq-clib"],
        Install   "pfq-clib"     *>>  into "user/C/" ["make install"] .|. [Build "pfq-clib"],
        Clean     "pfq-clib"     *>>  into "user/C/" ["make clean"],

        Configure "pfq-cpplib"   *>>  into "user/C++/pfq/" [],
        Build     "pfq-cpplib"   *>>  into "user/C++/pfq/" [],
        Install   "pfq-cpplib"   *>>  into "user/C++/pfq/" ["make install"],
        Clean     "pfq-cpplib"   *>>  into "user/C++/pfq/" [],

        Configure "pfq-haskell-lib"  *>>  into "user/Haskell/" [cabalConfigure] .|. [Install "pfq.ko", Install "pfq-clib"],
        Build     "pfq-haskell-lib"  *>>  into "user/Haskell/" [cabalBuild]     .|. [Configure "pfq-haskell-lib"],
        Install   "pfq-haskell-lib"  *>>  into "user/Haskell/" [cabalInstall]   .|. [Build "pfq-haskell-lib"],
        Clean     "pfq-haskell-lib"  *>>  into "user/Haskell/" [cabalClean],

        Configure "pfq-counters-hs" *>>  into "user/Haskell/pfq-counters-hs/" [cabalConfigure] .|. [Install   "pfq-haskell-lib"],
        Build     "pfq-counters-hs" *>>  into "user/Haskell/pfq-counters-hs/" [cabalBuild]     .|. [Configure "pfq-counters-hs"],
        Install   "pfq-counters-hs" *>>  into "user/Haskell/pfq-counters-hs/" [cabalInstall]   .|. [Build     "pfq-counters-hs"],
        Clean     "pfq-counters-hs" *>>  into "user/Haskell/pfq-counters-hs/" [cabalClean],

        Configure "irq-affinity" *>>  into "script/irq-affinity/" [cabalConfigure] .|. [Install   "pfq-haskell-lib"],
        Build     "irq-affinity" *>>  into "script/irq-affinity/" [cabalBuild]     .|. [Configure "irq-affinity"],
        Install   "irq-affinity" *>>  into "script/irq-affinity/" [cabalInstall]   .|. [Build     "irq-affinity"],
        Clean     "irq-affinity" *>>  into "script/irq-affinity/" [cabalClean],

        Configure "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalConfigure] .|. [Install   "pfq-haskell-lib"],
        Build     "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalBuild]     .|. [Configure "pfq-omatic"],
        Install   "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalInstall]   .|. [Build     "pfq-omatic"],
        Clean     "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalClean],

        Configure "pfq-load"     *>>  into "script/pfq-load/" [cabalConfigure] .|. [Install   "irq-affinity"],
        Build     "pfq-load"     *>>  into "script/pfq-load/" [cabalBuild]     .|. [Configure "pfq-load"],
        Install   "pfq-load"     *>>  into "script/pfq-load/" [cabalInstall]   .|. [Build     "pfq-load"],
        Clean     "pfq-load"     *>>  into "script/pfq-load/" [cabalClean],

        Configure "pfqd"        *>>  into "user/pfqd/" [cabalConfigure] .|. [Install   "pfq-haskell-lib", Install "pfq.ko"],
        Build     "pfqd"        *>>  into "user/pfqd/" [cabalBuild]     .|. [Configure "pfqd"],
        Install   "pfqd"        *>>  into "user/pfqd/" [cabalInstall]   .|. [Build     "pfqd"],
        Clean     "pfqd"        *>>  into "user/pfqd/" [cabalClean],

        Configure "test"        *>>  into "user/test/" ["cmake ."]      .|. [Install "pfq-clib", Install "pfq-cpplib"],
        Build     "test"        *>>  into "user/test/" ["make -j" ++ show numberOfPhyCores]  .|. [Configure "test"],
        Install   "test"        *>>  into "user/test/" [ ]              .|. [Build "test"],
        Clean     "test"        *>>  into "user/test/" ["make clean"],

        Configure "tool"        *>>  into "user/tool/" ["cmake ."]      .|. [Build "pfq-clib"],
        Build     "tool"        *>>  into "user/tool/" ["make -j" ++ show numberOfPhyCores] .|. [Configure "tool"],
        Install   "tool"        *>>  into "user/tool/" [ ]              .|. [Build "tool"],
        Clean     "tool"        *>>  into "user/tool/" ["make clean"]
   ]


main = getArgs >>= simpleBuilder script

