# PFQ Makefile (c) 2011 Nicola Bonelli <nicola.bonelli@cnit.it>
#

SUBDIRS := kernel user

all:
	@for i in $(SUBDIRS); do \
	echo "make all in $$i..."; \
	(cd $$i; $(MAKE) all); done

clean:
	@for i in $(SUBDIRS); do \
	echo "make clean in $$i..."; \
	(cd $$i; $(MAKE) clean); done

install:
	@for i in $(SUBDIRS); do \
	echo "make install in $$i..."; \
	(cd $$i; $(MAKE) install); done

