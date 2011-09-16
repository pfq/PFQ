#
# (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>
#

TARGET = pfq

EXTRA_CFLAGS = -I$(PWD) 
EXTRA_CFLAGS += -DEXPORT_SYMTAB 

obj-m := $(TARGET).o 

pfq-objs := pf_q.o pf_q-devmap.o mpdb-queue.o

ifeq (,$(BUILD_KERNEL))
BUILD_KERNEL=$(shell uname -r)
endif

# set the install path

INSTDIR := $(DESTDIR)/lib/modules/$(BUILD_KERNEL)/kernel/net/pf_q
TARGETDIR := /usr/src/$(BUILD_KERNEL)/include/linux/

all: Makefile pf_q.c linux/pf_q.h
	make -C /lib/modules/$(BUILD_KERNEL)/build SUBDIRS=$(PWD) modules

clean:
	make -C /lib/modules/$(BUILD_KERNEL)/build SUBDIRS=$(PWD) clean
	\rm -f *~ Module.symvers  Module.markers  modules.order 

install:
	mkdir -p $(INSTDIR)
	cp *.ko $(INSTDIR)
	cp linux/pf_q.h $(DESTDIR)/usr/include/linux
	@if test -d ${TARGETDIR}; then \
		cp linux/pf_q.h ${TARGETDIR}; \
	fi
ifeq (,$(DESTDIR))
	/sbin/depmod $(BUILD_KERNEL)
endif
