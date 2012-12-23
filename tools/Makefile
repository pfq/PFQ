# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

GHCFLAGS= --make -fno-warn-unused-matches -O2 

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: irq-affinity

irq-affinity: irq-affinity.hs
		$(HC) $(GHCFLAGS) $< -o $@

install: all
		@mkdir -p ${INSTDIR}/include/pfq 
		cp pfq_kcompat.h  ${INSTDIR}/include/pfq/
		cp irq-affinity  ${INSTDIR}/bin/
		cp pfq\'omatic.sh ${INSTDIR}/bin/
        
clean:
	   @rm -f irq-affinity 
	   @rm -f *.o *.hi
