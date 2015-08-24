--
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

{-# LANGUAGE OverloadedStrings #-}

import Development.SimpleBuilder

import System.Environment
import Control.Monad


script :: Script
script =
    [
        Configure "pfq.ko"       *>>  into "kernel/" [],
        Build     "pfq.ko"       *>>  into "kernel/" [ make ],
        Install   "pfq.ko"       *>>  into "kernel/" [ make_install ] .|. [ Build "pfq.ko" ],
        Clean     "pfq.ko"       *>>  into "kernel/" [ make_clean ],

        Configure "pfq-clib"     *>>  into "user/C/" [ cmake ],
        Build     "pfq-clib"     *>>  into "user/C/" [ make ]         .|. [ Configure "pfq-clib" ],
        Install   "pfq-clib"     *>>  into "user/C/" [ make_install ] .|. [ Build "pfq-clib" ],
        Clean     "pfq-clib"     *>>  into "user/C/" [ make_clean ],

        Configure "pfq-cpplib"   *>>  into "user/C++/pfq/" [],
        Build     "pfq-cpplib"   *>>  into "user/C++/pfq/" [],
        Install   "pfq-cpplib"   *>>  into "user/C++/pfq/" [ make_install ],
        Clean     "pfq-cpplib"   *>>  into "user/C++/pfq/" [],

        Configure "pfq-haskell-lib"  *>>  into "user/Haskell/" [ cabalConfigure ] .|. [ Install "pfq.ko", Install "pfq-clib" ],
        Build     "pfq-haskell-lib"  *>>  into "user/Haskell/" [ cabalBuild ]     .|. [ Configure "pfq-haskell-lib" ],
        Install   "pfq-haskell-lib"  *>>  into "user/Haskell/" [ cabalInstall ]   .|. [ Build "pfq-haskell-lib" ],
        Clean     "pfq-haskell-lib"  *>>  into "user/Haskell/" [ cabalClean ],

        Configure "pfq-hcounters" *>>  into "user/Haskell/pfq-hcounters/" [ cabalConfigure ] .|. [ Install   "pfq-haskell-lib" ],
        Build     "pfq-hcounters" *>>  into "user/Haskell/pfq-hcounters/" [ cabalBuild ]     .|. [ Configure "pfq-hcounters" ],
        Install   "pfq-hcounters" *>>  into "user/Haskell/pfq-hcounters/" [ cabalInstall ]   .|. [ Build     "pfq-hcounters" ],
        Clean     "pfq-hcounters" *>>  into "user/Haskell/pfq-hcounters/" [ cabalClean ],

        Configure "irq-affinity" *>>  into "user/irq-affinity/" [ cabalConfigure ] .|. [ Install   "pfq-haskell-lib" ],
        Build     "irq-affinity" *>>  into "user/irq-affinity/" [ cabalBuild ]     .|. [ Configure "irq-affinity" ],
        Install   "irq-affinity" *>>  into "user/irq-affinity/" [ cabalInstall ]   .|. [ Build     "irq-affinity" ],
        Clean     "irq-affinity" *>>  into "user/irq-affinity/" [ cabalClean ],

        Configure "pfq-omatic"   *>>  into "user/pfq-omatic/" [ cabalConfigure ] .|. [ Install   "pfq-haskell-lib" ],
        Build     "pfq-omatic"   *>>  into "user/pfq-omatic/" [ cabalBuild ]     .|. [ Configure "pfq-omatic" ],
        Install   "pfq-omatic"   *>>  into "user/pfq-omatic/" [ cabalInstall ]   .|. [ Build     "pfq-omatic" ],
        Clean     "pfq-omatic"   *>>  into "user/pfq-omatic/" [ cabalClean ],

        Configure "pfq-load"     *>>  into "user/pfq-load/" [ cabalConfigure ] .|. [ Install   "irq-affinity" ],
        Build     "pfq-load"     *>>  into "user/pfq-load/" [ cabalBuild ]     .|. [ Configure "pfq-load" ],
        Install   "pfq-load"     *>>  into "user/pfq-load/" [ cabalInstall ]   .|. [ Build     "pfq-load" ],
        Clean     "pfq-load"     *>>  into "user/pfq-load/" [ cabalClean ],

        Configure "pfq-stress"  *>>  into "user/pfq-stress/" [ cabalConfigure ] .|. [ Install   "irq-affinity", Install "pfq-load" ],
        Build     "pfq-stress"  *>>  into "user/pfq-stress/" [ cabalBuild ]     .|. [ Configure "pfq-stress" ],
        Install   "pfq-stress"  *>>  into "user/pfq-stress/" [ cabalInstall ]   .|. [ Build     "pfq-stress" ],
        Clean     "pfq-stress"  *>>  into "user/pfq-stress/" [ cabalClean ],

        Configure "pfqd"        *>>  into "user/pfqd/" [ cabalConfigure ] .|. [ Install   "pfq-haskell-lib", Install "pfq.ko" ],
        Build     "pfqd"        *>>  into "user/pfqd/" [ cabalBuild ]     .|. [ Configure "pfqd" ],
        Install   "pfqd"        *>>  into "user/pfqd/" [ cabalInstall ]   .|. [ Build     "pfqd" ],
        Clean     "pfqd"        *>>  into "user/pfqd/" [ cabalClean ],

        Configure "test"        *>>  into "user/test/" [ cmake ]        .|. [ Install "pfq-clib", Install "pfq-cpplib" ],
        Build     "test"        *>>  into "user/test/" [ make ]         .|. [ Configure "test" ],
        Install   "test"        *>>  into "user/test/" [ ]              .|. [ Build "test" ],
        Clean     "test"        *>>  into "user/test/" [ make_clean ],

        Configure "tool"        *>>  into "user/tool/" [ cmake ]        .|. [ Build "pfq-clib" ],
        Build     "tool"        *>>  into "user/tool/" [ make ]         .|. [ Configure "tool" ],
        Install   "tool"        *>>  into "user/tool/" [ make_install ] .|. [ Build "tool" ],
        Clean     "tool"        *>>  into "user/tool/" [ make_clean ]
   ]


main = getArgs >>= simpleBuilder script

