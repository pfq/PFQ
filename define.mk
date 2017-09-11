#
# Copyright (c) 2017 Nicola Bonelli <nicola@pfq.io>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#


ifdef BUILD_TYPE
define cmake-configure =
		cmake -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) .
endef
else
define cmake-configure =
		cmake .
endef
endif

define cmake-distclean =
    rm -rf install_manifest.txt
    rm -rf cmake.depends
    rm -rf cmake.chek_depends
    rm -rf CMakeCache.txt
    rm -rf *.cmake
    rm -rf CMakeFiles
    rm -rf Makefile
endef

#
# cabal
#

define cabal-configure =
		runhaskell Setup configure --user
endef
define cabal-build =
		runhaskell Setup build
endef
define cabal-install =
		runhaskell Setup install
endef
define cabal-clean =
		runhaskell Setup clean
endef
define cabal-distclean =
		rm -rf dist
endef

#
# stack
#

ifeq ($(JOBS),)
		stack_parallel=
else
		stack_parallel=--ghc-options -j$(JOBS)
endif

define stack-configure =
		stack setup
endef
define stack-build =
		stack build $(stack_parallel)
endef
define stack-install =
		stack install $(stack_parallel)
endef
define stack-clean =
		stack clean
endef
define stack-distclean =
		rm -rf .stack-work
endef


ifeq ($(HASKELL_BUILD),stack)

define haskell-configure =
		$(stack-configure)
endef
define haskell-build =
		$(stack-build)
endef
define haskell-clean =
		$(stack-clean)
endef
define haskell-install =
		$(stack-install)
endef
define haskell-distclean =
		$(stack-distclean)
endef

else

define haskell-configure =
		$(cabal-configure)
endef
define haskell-build =
		$(cabal-build)
endef
define haskell-clean =
		$(cabal-clean)
endef
define haskell-install =
		$(cabal-install)
endef
define haskell-distclean =
		$(cabal-distclean)
endef

endif


