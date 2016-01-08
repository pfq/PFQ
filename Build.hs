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

import Control.Monad(when)


script :: BuilderScript
script = do

    -- PFQ kernel module

    Configure "pfq.ko"      *>>  into "kernel/"  $  empty
    Build     "pfq.ko"      *>>  into "kernel/"  $  make         `requires` [Configure "pfq.ko"]
    Install   "pfq.ko"      *>>  into "kernel/"  $  make_install `requires` [Build "pfq.ko"]
    Clean     "pfq.ko"      *>>  into "kernel/"  $  make_clean
    DistClean "pfq.ko"      *>>  into "kernel/"  $  make_clean

    -- PFQ C library

    Configure "pfq-clib"    *>>  into "user/C/"  $  cmake
    Build     "pfq-clib"    *>>  into "user/C/"  $  make         `requires` [Configure "pfq-clib"]
    Install   "pfq-clib"    *>>  into "user/C/"  $  make_install `requires` [Build "pfq-clib"] >> ldconfig
    Clean     "pfq-clib"    *>>  into "user/C/"  $  make_clean
    DistClean "pfq-clib"    *>>  into "user/C/"  $  cmake_distclean

    -- PFQ C++ library

    Configure "pfq-cpplib"  *>>  into "user/C++/pfq/" $ empty
    Build     "pfq-cpplib"  *>>  into "user/C++/pfq/" $ empty
    Install   "pfq-cpplib"  *>>  into "user/C++/pfq/" $ make_install `requires` [Install "pfq-clib", Install "pfq.ko"]
    Clean     "pfq-cpplib"  *>>  into "user/C++/pfq/" $ empty


    -- PFQ Haskell library

    Configure "pfq-haskell-lib" *>>  into "user/Haskell/"   $ cabalConfigure    `requires`  [Install "pfq-clib"]
    Build     "pfq-haskell-lib" *>>  into "user/Haskell/"   $ cabalBuild        `requires`  [Install "pfq.ko", Configure "pfq-haskell-lib"]
    Install   "pfq-haskell-lib" *>>  into "user/Haskell/"   $ cabalInstall      `requires`  [Build "pfq-haskell-lib"]
    Clean     "pfq-haskell-lib" *>>  into "user/Haskell/"   $ cabalClean
    DistClean "pfq-haskell-lib" *>>  into "user/Haskell/"   $ cabalDistClean


    -- PFQ pcap library 1.7.4

    Configure "pfq-pcap-1.7.4"  *>>  into "user/libpcap/libpcap-1.7.4/"  $ do cmd "autoconf" `requires` [ Install "pfq-clib" ]
                                                                              cmd "./configure --enable-pfq"
    Build     "pfq-pcap-1.7.4"  *>>  into "user/libpcap/libpcap-1.7.4/"  $ make              `requires` [ Install "pfq.ko", Configure "pfq-pcap-1.7.4" ]
    Install   "pfq-pcap-1.7.4"  *>>  into "user/libpcap/libpcap-1.7.4/"  $ empty             `requires` [ Build "pfq-pcap-1.7.4" ]
    Clean     "pfq-pcap-1.7.4"  *>>  into "user/libpcap/libpcap-1.7.4/"  $ make_clean
    DistClean "pfq-pcap-1.7.4"  *>>  into "user/libpcap/libpcap-1.7.4/"  $ make_distclean

    -- PFQ hcounters (exmaple)

    Configure "pfq-hcounters"   *>>  into "user/pfq-hcounters/" $ cabalConfigure    `requires`  [Install   "pfq-haskell-lib"]
    Build     "pfq-hcounters"   *>>  into "user/pfq-hcounters/" $ cabalBuild        `requires`  [Configure "pfq-hcounters"  ]
    Install   "pfq-hcounters"   *>>  into "user/pfq-hcounters/" $ cabalInstall      `requires`  [Build     "pfq-hcounters"  ]
    Clean     "pfq-hcounters"   *>>  into "user/pfq-hcounters/" $ cabalClean
    DistClean "pfq-hcounters"   *>>  into "user/pfq-hcounters/" $ cabalDistClean


    -- PFQ htest (misc tests)

    Configure "pfq-htest"       *>>  into "user/pfq-htest/"     $ cabalConfigure    `requires` [Install   "pfq-haskell-lib"]
    Build     "pfq-htest"       *>>  into "user/pfq-htest/"     $ cabalBuild        `requires` [Configure "pfq-htest"      ]
    Install   "pfq-htest"       *>>  into "user/pfq-htest/"     $ empty             `requires` [Build     "pfq-htest"      ]
    Clean     "pfq-htest"       *>>  into "user/pfq-htest/"     $ cabalClean
    DistClean "pfq-htest"       *>>  into "user/pfq-htest/"     $ cabalDistClean


    -- qlang compiler:

    Configure "qlang"   *>>  into "user/qlang/" $ cabalConfigure    `requires`  [Install   "pfq-haskell-lib"]
    Build     "qlang"   *>>  into "user/qlang/" $ cabalBuild        `requires`  [Configure "qlang"  ]
    Install   "qlang"   *>>  into "user/qlang/" $ cabalInstall      `requires`  [Build     "qlang"  ]
    Clean     "qlang"   *>>  into "user/qlang/" $ cabalClean
    DistClean "qlang"   *>>  into "user/qlang/" $ cabalDistClean


    -- PFQ user tools

    Configure "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalConfigure    `requires` [Install   "pfq-haskell-lib"]
    Build     "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalBuild        `requires` [Configure "irq-affinity"   ]
    Install   "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalInstall      `requires` [Build     "irq-affinity"   ]
    Clean     "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalClean
    DistClean "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalDistClean

    Configure "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalConfigure    `requires` [Install   "pfq-haskell-lib"]
    Build     "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalBuild        `requires` [Configure "pfq-omatic"     ]
    Install   "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalInstall      `requires` [Build     "pfq-omatic"     ]
    Clean     "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalClean
    DistClean "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalDistClean

    Configure "pfq-load"        *>>  into "user/pfq-load/"      $ cabalConfigure
    Build     "pfq-load"        *>>  into "user/pfq-load/"      $ cabalBuild        `requires` [Install "irq-affinity", Configure "pfq-load"]
    Install   "pfq-load"        *>>  into "user/pfq-load/"      $ cabalInstall      `requires` [Build   "pfq-load"]
    Clean     "pfq-load"        *>>  into "user/pfq-load/"      $ cabalClean
    DistClean "pfq-load"        *>>  into "user/pfq-load/"      $ cabalDistClean

    Configure "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalConfigure
    Build     "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalBuild        `requires` [Install "irq-affinity", Install "pfq-load", Configure "pfq-stress"]
    Install   "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalInstall      `requires` [Build   "pfq-stress"]
    Clean     "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalClean
    DistClean "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalDistClean

    Configure "pfqd"            *>>  into "user/pfqd/"          $ cabalConfigure    `requires` [Install "pfq-haskell-lib"]
    Build     "pfqd"            *>>  into "user/pfqd/"          $ cabalBuild        `requires` [Install "pfq.ko", Configure "pfqd"]
    Install   "pfqd"            *>>  into "user/pfqd/"          $ cabalInstall      `requires` [Build   "pfqd"]
    Clean     "pfqd"            *>>  into "user/pfqd/"          $ cabalClean
    DistClean "pfqd"            *>>  into "user/pfqd/"          $ cabalDistClean

    Configure "tests"           *>>  into "user/test/"          $ cmake
    Build     "tests"           *>>  into "user/test/"          $ make              `requires` [Install "pfq-clib", Install "pfq-cpplib", Configure "tests"]
    Install   "tests"           *>>  into "user/test/"          $ empty             `requires` [Build   "tests"]
    Clean     "tests"           *>>  into "user/test/"          $ make_clean
    DistClean "tests"           *>>  into "user/test/"          $ cmake_distclean

    Configure "tools"           *>>  into "user/tool/"          $ cmake
    Build     "tools"           *>>  into "user/tool/"          $ make              `requires` [Install "pfq-clib", Configure "tools"]
    Install   "tools"           *>>  into "user/tool/"          $ make_install      `requires` [Build   "tools"]
    Clean     "tools"           *>>  into "user/tool/"          $ make_clean
    DistClean "tools"           *>>  into "user/tool/"          $ cmake_distclean


main = simpleBuilder script =<< getArgs

