# (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
#

GHCFLAGS= --make -O2 -Wall 

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: irq-affinity pfq-omatic

irq-affinity: irq-affinity.hs
		$(HC) $(GHCFLAGS) $< -o $@

pfq-omatic: pfq-omatic.hs
		$(HC) $(GHCFLAGS) $< -o $@

install: all
		@mkdir -p ${INSTDIR}/include/pfq 
		cp irq-affinity  ${INSTDIR}/bin/
		cp pfq-omatic ${INSTDIR}/bin/
        
clean:
	   @rm -f irq-affinity pfq-omatic
	   @rm -f *.o *.hi
