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

        Configure "C-lib"        *>>  into "user/C/" ["cmake ."],
        Build     "C-lib"        *>>  into "user/C/" ["make"]         .|. [Configure "C-lib"],
        Install   "C-lib"        *>>  into "user/C/" ["make install"] .|. [Build "C-lib"],
        Clean     "C-lib"        *>>  into "user/C/" ["make clean"],

        Configure "C++-lib"      *>>  into "user/C++/" [],
        Build     "C++-lib"      *>>  into "user/C++/" [],
        Install   "C++-lib"      *>>  into "user/C++/" ["make install"],
        Clean     "C++-lib"      *>>  into "user/C++/" [],

        Configure "Haskell-lib"  *>>  into "user/Haskell/" [cabalConfigure] .|. [Install "pfq.ko", Install "C-lib"],
        Build     "Haskell-lib"  *>>  into "user/Haskell/" [cabalBuild]     .|. [Configure "Haskell-lib"],
        Install   "Haskell-lib"  *>>  into "user/Haskell/" [cabalInstall]   .|. [Build "Haskell-lib"],
        Clean     "Haskell-lib"  *>>  into "user/Haskell/" [cabalClean],

        Configure "pfq-counters" *>>  into "user/Haskell/pfq-counters/" [cabalConfigure] .|. [Install   "Haskell-lib"],
        Build     "pfq-counters" *>>  into "user/Haskell/pfq-counters/" [cabalBuild]     .|. [Configure "pfq-counters"],
        Install   "pfq-counters" *>>  into "user/Haskell/pfq-counters/" [cabalInstall]   .|. [Build     "pfq-counters"],
        Clean     "pfq-counters" *>>  into "user/Haskell/pfq-counters/" [cabalClean],

        Configure "irq-affinity" *>>  into "script/irq-affinity/" [cabalConfigure] .|. [Install   "Haskell-lib"],
        Build     "irq-affinity" *>>  into "script/irq-affinity/" [cabalBuild]     .|. [Configure "irq-affinity"],
        Install   "irq-affinity" *>>  into "script/irq-affinity/" [cabalInstall]   .|. [Build     "irq-affinity"],
        Clean     "irq-affinity" *>>  into "script/irq-affinity/" [cabalClean],

        Configure "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalConfigure] .|. [Install   "Haskell-lib"],
        Build     "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalBuild]     .|. [Configure "pfq-omatic"],
        Install   "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalInstall]   .|. [Build     "pfq-omatic"],
        Clean     "pfq-omatic"   *>>  into "script/pfq-omatic/" [cabalClean],

        Configure "pfq-load"     *>>  into "script/pfq-load/" [cabalConfigure] .|. [Install   "irq-affinity"],
        Build     "pfq-load"     *>>  into "script/pfq-load/" [cabalBuild]     .|. [Configure "pfq-load"],
        Install   "pfq-load"     *>>  into "script/pfq-load/" [cabalInstall]   .|. [Build     "pfq-load"],
        Clean     "pfq-load"     *>>  into "script/pfq-load/" [cabalClean],

        Configure "C/C++-test"   *>>  into "user/test/" ["cmake ."]      .|. [Build "C-lib"],
        Build     "C/C++-test"   *>>  into "user/test/" ["make -j" ++ show numberOfPhyCores]  .|. [Configure "C/C++-test"],
        Install   "C/C++-test"   *>>  into "user/test/" [ ],
        Clean     "C/C++-test"   *>>  into "user/test/" ["make clean"],

        Configure "C/C++-tools"  *>>  into "user/tool/" ["cmake ."]      .|. [Build "C-lib"],
        Build     "C/C++-tools"  *>>  into "user/tool/" ["make -j" ++ show numberOfPhyCores] .|. [Configure "C/C++-tools"],
        Install   "C/C++-tools"  *>>  into "user/tool/" [ ],
        Clean     "C/C++-tools"  *>>  into "user/tool/" ["make clean"]
   ]


main = getArgs >>= simpleBuilder script

