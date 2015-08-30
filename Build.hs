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

import Control.Monad.Writer.Lazy

script :: BuilderScript
script = do

    -- PFQ kernel module

    Configure "pfq.ko"      *>>  into "kernel/"     empty
    Build     "pfq.ko"      *>>  into "kernel/"     make
    Install   "pfq.ko"      *>>  into "kernel/"  $  make_install `requires` [Build "pfq.ko", Configure "pfq.ko"]
    Clean     "pfq.ko"      *>>  into "kernel/"     make_clean


    -- PFQ C library

    Configure "pfq-clib"    *>>  into "user/C/"        cmake
    Build     "pfq-clib"    *>>  into "user/C/"   $    make         `requires` [Configure "pfq-clib"]
    Install   "pfq-clib"    *>>  into "user/C/"   $ do make_install `requires` [Build "pfq-clib"    ]
                                                       ldconfig
    Clean     "pfq-clib"    *>>  into "user/C/"        make_clean


    -- PFQ C++ library

    Configure "pfq-cpplib"  *>>  into "user/C++/pfq/" empty
    Build     "pfq-cpplib"  *>>  into "user/C++/pfq/" empty
    Install   "pfq-cpplib"  *>>  into "user/C++/pfq/" make_install
    Clean     "pfq-cpplib"  *>>  into "user/C++/pfq/" empty


    -- PFQ Haskell library

    Configure "pfq-haskell-lib" *>>  into "user/Haskell/"       $ cabalConfigureUser    `requires`  [Install "pfq.ko" , Install "pfq-clib"]
    Build     "pfq-haskell-lib" *>>  into "user/Haskell/"       $ cabalBuild            `requires`  [Configure "pfq-haskell-lib"]
    Install   "pfq-haskell-lib" *>>  into "user/Haskell/"       $ cabalInstall          `requires`  [Build "pfq-haskell-lib"    ]
    Clean     "pfq-haskell-lib" *>>  into "user/Haskell/"       $ cabalClean


    -- PFQ pcap library

    -- Configure "pfq-pcap"        *>>  into "user/libpcap/libpcap-1.3.0/"  $ cmd "./configure --enable-pfq" `requires` [ Install "pfq.ko", Install "pfq-clib" ]
    -- Build     "pfq-pcap"        *>>  into "user/libpcap/libpcap-1.3.0/"  $ make                           `requires` [ Configure "pfq-pcap" ]
    -- Install   "pfq-pcap"        *>>  into "user/libpcap/libpcap-1.3.0/"  $ empty                          `requires` [ Build "pfq-pcap" ]
    -- Clean     "pfq-pcap"        *>>  into "user/libpcap/libpcap-1.3.0/"  $ make_clean


    -- PFQ hcounters (exmaple)

    Configure "pfq-hcounters"   *>>  into "user/Haskell/pfq-hcounters/"  $ cabalConfigureUser   `requires`  [Install   "pfq-haskell-lib"]
    Build     "pfq-hcounters"   *>>  into "user/Haskell/pfq-hcounters/"  $ cabalBuild           `requires`  [Configure "pfq-hcounters"  ]
    Install   "pfq-hcounters"   *>>  into "user/Haskell/pfq-hcounters/"  $ cabalInstall         `requires`  [Build     "pfq-hcounters"  ]
    Clean     "pfq-hcounters"   *>>  into "user/Haskell/pfq-hcounters/"  $ cabalClean


    -- PFQ htest (misc tests)

    Configure "pfq-htest"       *>>  into "user/Haskell/pfq-htest/"     $ cabalConfigureUser    `requires` [Install   "pfq-haskell-lib"]
    Build     "pfq-htest"       *>>  into "user/Haskell/pfq-htest/"     $ cabalBuild            `requires` [Configure "pfq-htest"      ]
    Install   "pfq-htest"       *>>  into "user/Haskell/pfq-htest/"     $ empty                 `requires` [Build     "pfq-htest"      ]
    Clean     "pfq-htest"       *>>  into "user/Haskell/pfq-htest/"     $ cabalClean


    -- PFQ user tools

    Configure "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalConfigureUser    `requires` [Install   "pfq-haskell-lib"]
    Build     "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalBuild            `requires` [Configure "irq-affinity"   ]
    Install   "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalInstall          `requires` [Build     "irq-affinity"   ]
    Clean     "irq-affinity"    *>>  into "user/irq-affinity/"  $ cabalClean

    Configure "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalConfigureUser    `requires` [Install   "pfq-haskell-lib"]
    Build     "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalBuild            `requires` [Configure "pfq-omatic"     ]
    Install   "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalInstall          `requires` [Build     "pfq-omatic"     ]
    Clean     "pfq-omatic"      *>>  into "user/pfq-omatic/"    $ cabalClean

    Configure "pfq-load"        *>>  into "user/pfq-load/"      $ cabalConfigureUser    `requires` [Install   "irq-affinity"   ]
    Build     "pfq-load"        *>>  into "user/pfq-load/"      $ cabalBuild            `requires` [Configure "pfq-load"       ]
    Install   "pfq-load"        *>>  into "user/pfq-load/"      $ cabalInstall          `requires` [Build     "pfq-load"       ]
    Clean     "pfq-load"        *>>  into "user/pfq-load/"      $ cabalClean

    Configure "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalConfigureUser    `requires` [Install "irq-affinity", Install "pfq-load"]
    Build     "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalBuild            `requires` [Configure "pfq-stress"]
    Install   "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalInstall          `requires` [Build     "pfq-stress"]
    Clean     "pfq-stress"      *>>  into "user/pfq-stress/"    $ cabalClean

    Configure "pfqd"            *>>  into "user/pfqd/"          $ cabalConfigureUser    `requires` [Install   "pfq-haskell-lib", Install "pfq.ko"]
    Build     "pfqd"            *>>  into "user/pfqd/"          $ cabalBuild            `requires` [Configure "pfqd"]
    Install   "pfqd"            *>>  into "user/pfqd/"          $ cabalInstall          `requires` [Build     "pfqd"]
    Clean     "pfqd"            *>>  into "user/pfqd/"          $ cabalClean

    Configure "tests"           *>>  into "user/test/"          $ cmake                 `requires` [Install "pfq-clib", Install "pfq-cpplib"]
    Build     "tests"           *>>  into "user/test/"          $ make                  `requires` [Configure "tests"]
    Install   "tests"           *>>  into "user/test/"          $ empty                 `requires` [Build "tests"    ]
    Clean     "tests"           *>>  into "user/test/"          $ make_clean

    Configure "tools"           *>>  into "user/tool/"          $ cmake                 `requires` [Build "pfq-clib"]
    Build     "tools"           *>>  into "user/tool/"          $ make                  `requires` [Configure "tools"]
    Install   "tools"           *>>  into "user/tool/"          $ make_install          `requires` [Build "tools"    ]
    Clean     "tools"           *>>  into "user/tool/"          $ make_clean


main = simpleBuilder script =<< getArgs

