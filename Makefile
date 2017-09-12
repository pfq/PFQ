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

# https://stackoverflow.com/questions/5303553/gnu-make-extracting-argument-to-j-within-makefile
#

MAKE_PID := $(shell echo $$PPID)
JOB_FLAG := $(filter -j%, $(subst -j ,-j,$(shell ps T | grep "^\s*$(MAKE_PID).*$(MAKE)")))
JOBS     := $(subst -j,,$(JOB_FLAG))

HASKELL_BUILD = stack
BUILD_TYPE = Release

include define.mk

.ONESHELL:
.NOTPARALLEL:

.PHONY: pfq pfq_install pfq_clean \
		clib_conf clib clib_install clib_clean clib_distclean \
		cpplib_install \
		pcap_conf pcap pcap_install pcap_clean pcap_distclean \
		tools_conf tools tools_install tools_clean tools_distclean \
		hlib_conf hlib hlib_install hlib_clean hlib_distclean \
		pfq_cgression pfq_cgression_conf pfq_cgression_install pfq_cgression_clean pfq_cgression_distclean \
		pfq_hgression pfq_hgression_conf pfq_hgression_install pfq_hgression_clean pfq_hgression_distclean \
		pfq_stress pfq_stress_conf pfq_stress_install pfq_stress_clean pfq_stress_distclean \
		pfq_affinity pfq_affinity_conf pfq_affinity_install pfq_affinity_clean pfq_affinity_distclean \
		pfq_omatic pfq_omatic_conf pfq_omatic_install pfq_omatic_clean pfq_omatic_distclean \
		pfq_load pfq_load_conf pfq_load_install pfq_load_clean pfq_load_distclean \
		pfq_lang pfq_lang_conf pfq_lang_install pfq_lang_clean pfq_lang_distclean \
		pfq_hcounters pfq_hcounters_conf pfq_hcounters_install pfq_hcounters_clean pfq_hcounters_distclean \
		pfqd pfqd_conf pfqd_install pfqd_clean pfqd_distclean

all: pfq clib tools hlib \
	 pfq_lang pfq_affinity pfq_load pfq_hcounters pfq_omatic pfqd \
     pfq_cgression pfq_hregression pfq_stress

install: pfq_install clib_install cpplib_install tools_install hlib_install \
		 pfq_lang_install pfq_affinity_install pfq_load_install pfq_hcounters_install pfq_omatic_install pfqd_install

clean: pfq_clean clib_clean tools_clean hlib_clean pfq_lang_clean pfq_affinity_clean pfq_affinity_clean \
       pfq_load_clean pfq_hcounters_clean pfq_omatic_clean pfqd_clean \
	   pfq_cgression_clean pfq_hregression_clean  pfq_stress_clean

distclean: pfq_distclean clib_distclean hlib_distclean tools_distclean hlib_distclean \
		   pfq_lang_distclean pfq_affinity_distclean pfq_load_distclean pfq_hcounters_distclean pfq_omatic_distclean pfqd_distclean \
		   pfq_cgression_distclean pfq_hregression_distclean pfq_stress_distclean

#
# PFQ kernel module...
#

pfq:
		@cd kernel
		$(MAKE)

pfq_install: pfq
		@cd kernel
		$(MAKE) install

pfq_clean:
		@cd kernel
		$(MAKE) clean

pfq_distclean: pfq_clean

#
# PFQ C library...
#

CLIB_PATH="user/lib/C"

user/lib/C/Makefile:
		@cd $(CLIB_PATH)
		$(cmake-configure)

clib_conf: user/lib/C/Makefile

clib:   clib_conf
		@cd $(CLIB_PATH)
		$(MAKE)

clib_install: clib
		@cd $(CLIB_PATH)
		$(MAKE) install

clib_clean: clib_conf
		@cd $(CLIB_PATH)
		$(MAKE) clean

clib_distclean: clib_clean
		@cd $(CLIB_PATH)
		$(cmake-distclean)

#
# PFQ C++ library...
#

CPPLIB_PATH="user/lib/C++/pfq"
cpplib_install: pfq_install clib_install
		@cd $(CPPLIB_PATH)
		$(MAKE) install

#
# PFQ Haskell library
#

HLIB_PATH="user/lib/Haskell/"
hlib_conf: clib_install
		@cd $(HLIB_PATH)
		$(haskell-configure)

hlib: hlib_conf pfq_install
		@cd $(HLIB_PATH)
		$(haskell-build)

hlib_install: hlib
		@cd $(HLIB_PATH)
		$(haskell-install)

hlib_clean:
		@cd $(HLIB_PATH)
		$(haskell-clean)

hlib_distclean:
		@cd $(HLIB_PATH)
		$(haskell-distclean)

#
# PFQ pcap library...
#

PCAP_PATH="user/lib/libpcap-1.8.1-fanout/"
pcap_conf:
		@cd $(PCAP_PATH)
		autoconf
		./configure

pcap: pcap_conf pfq_install clib_install
		@cd $(PCAP_PATH)
		$(MAKE)

pcap_install: pcap
		@cd $(PCAP_PATH)
		$(MAKE) install

pcap_clean: pcap_conf
		@cd $(PCAP_PATH)
		$(MAKE) clean

pcap_distclean: pcap_clean
		@cd $(PCAP_PATH)
		$(MAKE) distclean
#
# PFQ tools...
#

TOOL_PATH="user/pfq-tools/"
user/pfq-tools/Makefile:
		@cd $(TOOL_PATH)
		$(cmake-configure)

tools_conf: user/pfq-tools/Makefile

tools:  clib_install tools_conf
		@cd $(TOOL_PATH)
		$(MAKE)

tools_install: tools
		@cd $(TOOL_PATH)
		$(MAKE) install

tools_clean: tools_conf
		@cd $(TOOL_PATH)
		$(MAKE) clean

tools_distclean: tools_clean
		@cd $(TOOL_PATH)
		$(cmake-distclean)

#
# pfq-lang compiler
#

PFQ_LANG_PATH="user/pfq-lang/"
pfq_lang_conf: hlib_install
		@cd $(PFQ_LANG_PATH)
		$(haskell-configure)

pfq_lang: pfq_lang_conf
		@cd $(PFQ_LANG_PATH)
		$(haskell-build)

pfq_lang_install: pfq_lang
		@cd $(PFQ_LANG_PATH)
		$(haskell-install)

pfq_lang_clean:
		@cd $(PFQ_LANG_PATH)
		$(haskell-clean)

pfq_lang_distclean:
		@cd $(PFQ_LANG_PATH)
		$(haskell-distclean)

#
# pfq-affinity
#

PFQ_AFFINITY_PATH="user/pfq-affinity/"
pfq_affinity_conf: hlib_install
		@cd $(PFQ_AFFINITY_PATH)
		$(haskell-configure)

pfq_affinity: pfq_affinity_conf
		@cd $(PFQ_AFFINITY_PATH)
		$(haskell-build)

pfq_affinity_install: pfq_affinity
		@cd $(PFQ_AFFINITY_PATH)
		$(haskell-install)

pfq_affinity_clean:
		@cd $(PFQ_AFFINITY_PATH)
		$(haskell-clean)

pfq_affinity_distclean:
		@cd $(PFQ_AFFINITY_PATH)
		$(haskell-distclean)

#
# pfq-load
#

PFQ_LOAD_PATH="user/pfq-load/"
pfq_load_conf: hlib_install
		@cd $(PFQ_LOAD_PATH)
		$(haskell-configure)

pfq_load: pfq_affinity_install pfq_load_conf
		@cd $(PFQ_LOAD_PATH)
		$(haskell-build)

pfq_load_install: pfq_load
		@cd $(PFQ_LOAD_PATH)
		$(haskell-install)

pfq_load_clean:
		@cd $(PFQ_LOAD_PATH)
		$(haskell-clean)

pfq_load_distclean:
		@cd $(PFQ_LOAD_PATH)
		$(haskell-distclean)

#
# pfq-omatic
#

PFQ_OMATIC_PATH="user/pfq-omatic/"
pfq_omatic_conf: hlib_install
		@cd $(PFQ_OMATIC_PATH)
		$(haskell-configure)

pfq_omatic: pfq_omatic_conf
		@cd $(PFQ_OMATIC_PATH)
		$(haskell-build)

pfq_omatic_install: pfq_omatic
		@cd $(PFQ_OMATIC_PATH)
		$(haskell-install)

pfq_omatic_clean:
		@cd $(PFQ_OMATIC_PATH)
		$(haskell-clean)

pfq_omatic_distclean:
		@cd $(PFQ_OMATIC_PATH)
		$(haskell-distclean)

#
# pfqd
#

PFQD_PATH="user/pfqd/"
pfqd_conf: hlib_install
		@cd $(PFQD_PATH)
		$(haskell-configure)

pfqd: pfq_install pfqd_conf
		@cd $(PFQD_PATH)
		$(haskell-build)

pfqd_install: pfqd
		@cd $(PFQD_PATH)
		$(haskell-install)

pfqd_clean:
		@cd $(PFQD_PATH)
		$(haskell-clean)

pfqd_distclean:
		@cd $(PFQD_PATH)
		$(haskell-distclean)

#
# pfq-hcounters
#

PFQ_HCOUNTERS_PATH="user/pfq-hcounters/"
pfq_hcounters_conf: hlib_install
		@cd $(PFQ_HCOUNTERS_PATH)
		$(haskell-configure)

pfq_hcounters: pfq_hcounters_conf
		@cd $(PFQ_HCOUNTERS_PATH)
		$(haskell-build)

pfq_hcounters_install: pfq_hcounters
		@cd $(PFQ_HCOUNTERS_PATH)
		$(haskell-install)

pfq_hcounters_clean:
		@cd $(PFQ_HCOUNTERS_PATH)
		$(haskell-clean)

pfq_hcounters_distclean:
		@cd $(PFQ_HCOUNTERS_PATH)
		$(haskell-distclean)

#
# C regression...
#

PFQ_CREGRESSION_PATH="user/pfq-regression/C"
user/pfq-regression/C/Makefile:
		@cd $(PFQ_CREGRESSION_PATH)
		$(cmake-configure)

pfq_cgression_conf: user/pfq-regression/C/Makefile

pfq_cgression: clib_install pfq_cgression_conf
		@cd $(PFQ_CREGRESSION_PATH)
		$(MAKE)

pfq_cgression_install: pfq_cgression
		@cd $(PFQ_CREGRESSION_PATH)
		$(MAKE) install

pfq_cgression_clean: pfq_cgression_conf
		@cd $(PFQ_CREGRESSION_PATH)
		$(MAKE) clean

pfq_cgression_distclean: pfq_cgression_conf
		@cd $(PFQ_CREGRESSION_PATH)
		$(cmake-distclean)

#
# Haskell regressions...
#

PFQ_HREGRESSION_PATH="user/pfq-regression/Haskell/"
pfq_hregression_conf: hlib_install
		@cd $(PFQ_HREGRESSION_PATH)
		$(haskell-configure)

pfq_hregression: pfq_hregression_conf
		@cd $(PFQ_HREGRESSION_PATH)
		$(haskell-build)

pfq_hregression_install: pfq_hregression
		@cd $(PFQ_HREGRESSION_PATH)
		$(haskell-install)

pfq_hregression_clean:
		@cd $(PFQ_HREGRESSION_PATH)
		$(haskell-clean)

pfq_hregression_distclean:
		@cd $(PFQ_HREGRESSION_PATH)
		$(haskell-distclean)

#
# pfq-stress...
#

PFQ_STRESS_PATH="user/pfq-stress/"
pfq_stress_conf: hlib_install
		@cd $(PFQ_STRESS_PATH)
		$(haskell-configure)

pfq_stress: pfq_affinity_install pfq_load_install pfq_stress_conf
		@cd $(PFQ_STRESS_PATH)
		$(haskell-build)

pfq_stress_install: pfq_stress
		@cd $(PFQ_STRESS_PATH)
		$(haskell-install)

pfq_stress_clean:
		@cd $(PFQ_STRESS_PATH)
		$(haskell-clean)

pfq_stress_distclean:
		@cd $(PFQ_STRESS_PATH)
		$(haskell-distclean)

