# (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
#

GHCFLAGS= --make -W -O2 -threaded -with-rtsopts="-N"

INSTDIR=/usr/local

LIBS= -lpfq

HC=ghc

HSC=hsc2hs

.PHONY: all clean

all: Network/PFq.hs pfq-counters test-read test-bloom test-dispatch test-send test-lang

Network/PFq.hs:
		$(HSC) Network/PFq.hsc

test-read: Network/PFq.hs test-read.hs
		$(HC) $(GHCFLAGS) $(LIBS) test-read.hs -o $@

test-bloom: Network/PFq.hs test-bloom.hs
		$(HC) $(GHCFLAGS) $(LIBS) test-bloom.hs -o $@

test-send: Network/PFq.hs test-send.hs
		$(HC) $(GHCFLAGS) $(LIBS) test-send.hs -o $@

pfq-counters: Network/PFq.hs pfq-counters.hs
		$(HC) $(GHCFLAGS) $(LIBS) pfq-counters.hs -o $@

test-dispatch: Network/PFq.hs test-dispatch.hs
		$(HC) $(GHCFLAGS) $(LIBS) test-dispatch.hs -o $@

test-lang: test-lang.hs
		$(HC) $(GHCFLAGS) $(LIBS) test-lang.hs -o $@

install: all
		cp pfq-counters  ${INSTDIR}/bin/

clean:
	   @rm -f pfq-counters test-read test-bloom test-send test-dispatch test-lang
	   @rm -f *.o *.hi Network/*.o Network/*.hi Network/PFq.hs Network/PFq_stub.h
	   @rm -f Network/PFq/*.o Network/PFq/*.hi
