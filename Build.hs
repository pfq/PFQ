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


options = defaultOptions { stack = False }


script :: BuilderScript
script = do

    -- PFQ kernel module

    objective "pfq.ko" "kernel/" $ do

        config    $ empty
        build     $ make
        install   $ make_install `req` buildOf "pfq.ko"
        clean     $ make_clean
        distclean $ make_clean

   -- PFQ C library

    objective "pfq-clib" "user/C/" $ do

       config     $ cmake
       build      $ make         `req` configOf "pfq-clib"
       install    $ make_install `req` buildOf "pfq-clib" >> ldconfig
       clean      $ make_clean
       distclean  $ cmake_distclean

    -- PFQ C++ library

    objective "pfq-cpplib" "user/C++/pfq/" $ do
        config    $ empty
        build     $ empty
        install   $ make_install `req` installOf "pfq-clib" `req` installOf "pfq.ko"
        clean     $ empty

     -- PFQ Haskell library

    objective "pfq-haskell-lib" "user/Haskell/" $ do
        config    $ cabalConfigure  `req` installOf "pfq-clib"
        build     $ cabalBuild      `req` installOf "pfq.ko" `req` configOf "pfq-haskell-lib"
        install   $ cabalInstall    `req` buildOf "pfq-haskell-lib"
        clean     $ cabalClean
        distclean $ cabalDistClean

     -- PFQ pcap library 1.8.1

    objective "pfq-pcap" "user/libpcap/libpcap-1.8.1-fanout/" $ do
        config    $ cmd "autoconf" `req` installOf "pfq-clib" >> configure
        build     $ make           `req` installOf "pfq.ko" `req` configOf "pfq-pcap"
        install   $ empty          `req` buildOf "pfq-pcap"
        clean     $ make_clean     `req` configOf "pfq-pcap"
        distclean $ make_distclean

     -- PFQ hcounters (exmaple)

    objective "pfq-hcounters" "user/pfq-hcounters/" $ do
        config    $ cabalConfigure  `req` installOf   "pfq-haskell-lib"
        build     $ cabalBuild      `req` configOf    "pfq-hcounters"
        install   $ cabalInstall    `req` buildOf     "pfq-hcounters"
        clean     $ cabalClean
        distclean $ cabalDistClean

     -- pfq-lang compiler:

    objective "pfq-lang" "user/pfq-lang/" $ do
        config    $ cabalConfigure  `req` installOf   "pfq-haskell-lib"
        build     $ cabalBuild      `req` configOf    "pfq-lang"
        install   $ cabalInstall    `req` buildOf     "pfq-lang"
        clean     $ cabalClean
        distclean $ cabalDistClean


     -- PFQ user tools

    objective "pfq-affinity" "user/pfq-affinity/" $ do
        config    $ cabalConfigure    `req` installOf "pfq-haskell-lib"
        build     $ cabalBuild        `req` configOf  "pfq-affinity"
        install   $ cabalInstall      `req` buildOf   "pfq-affinity"
        clean     $ cabalClean
        distclean $ cabalDistClean

    objective "pfq-omatic" "user/pfq-omatic/" $ do
        config    $ cabalConfigure    `req` installOf "pfq-haskell-lib"
        build     $ cabalBuild        `req` configOf  "pfq-omatic"
        install   $ cabalInstall      `req` buildOf   "pfq-omatic"
        clean     $ cabalClean
        distclean $ cabalDistClean

    objective "pfq-load" "user/pfq-load/" $ do
        config    $ cabalConfigure
        build     $ cabalBuild        `req` installOf "pfq-affinity" `req` configOf "pfq-load"
        install   $ cabalInstall      `req` buildOf   "pfq-load"
        clean     $ cabalClean
        distclean $ cabalDistClean

    objective "pfq-stress" "user/pfq-stress/" $ do
        config    $ cabalConfigure
        build     $ cabalBuild        `reqs` [installOf "pfq-affinity", installOf "pfq-load", configOf "pfq-stress"]
        install   $ cabalInstall      `req` buildOf "pfq-stress"
        clean     $ cabalClean
        distclean $ cabalDistClean

    objective "pfqd" "user/pfqd/" $ do
        config    $ cabalConfigure    `req` installOf "pfq-haskell-lib"
        build     $ cabalBuild        `req` installOf "pfq.ko" `req` configOf "pfqd"
        install   $ cabalInstall      `req` buildOf   "pfqd"
        clean     $ cabalClean
        distclean $ cabalDistClean

    objective "regression" "user/regression/C/" $ do
        config    $ cmake
        build     $ make              `reqs` [installOf "pfq-clib", installOf "pfq-cpplib", configOf "regression"]
        install   $ empty             `req`  buildOf   "regression"
        clean     $ make_clean
        distclean $ cmake_distclean

     -- PFQ htest (misc tests)

    objective "h-regression" "user/regression/Haskell/" $ do
        config    $ cabalConfigure  `req` installOf "pfq-haskell-lib"
        build     $ cabalBuild      `req` configOf  "h-regression"
        install   $ empty           `req` buildOf   "h-regression"
        clean     $ cabalClean
        distclean $ cabalDistClean

    objective "tools" "user/tool/" $ do
        config    $ cmake
        build     $ make             `reqs` [installOf "pfq-clib", configOf "tools"]
        install   $ make_install     `req`  buildOf "tools"
        clean     $ make_clean
        distclean $ cmake_distclean


main = simpleBuilder script options =<< getArgs


