# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

TARGET = pfq

EXTRA_CFLAGS += -I$(obj)
EXTRA_CFLAGS += -Ofast -march=native
EXTRA_CFLAGS += -DEXPORT_SYMTAB
EXTRA_CFLAGS += -DPFQ_USE_SKB_LINEARIZE
#EXTRA_CFLAGS += -DPFQ_USE_SKB_RECYCLE
#EXTRA_CFLAGS += -DPFQ_TX_PROFILE
#EXTRA_CFLAGS += -DPFQ_USE_SKB_RECYCLE_STAT
#EXTRA_CFLAGS += -DDEBUG

obj-m := $(TARGET).o

pfq-objs := pf_q.o pf_q-sockopt.o pf_q-global.o pf_q-devmap.o pf_q-queue.o pf_q-sock.o pf_q-memory.o pf_q-group.o \
		    pf_q-functional.o pf_q-mpdb-queue.o pf_q-prefetch.o pf_q-bpf.o pf_q-vlan.o \
		    pf_q-thread.o pf_q-transmit.o functional/default.o

KERNELVERSION := $(shell uname -r)

INSTDIR := $(DESTDIR)/lib/modules/${KERNELVERSION}/kernel/net/pfq

all:
		make -C /lib/modules/${KERNELVERSION}/build M=$(PWD) modules

clean:
		make -C /lib/modules/${KERNELVERSION}/build M=$(PWD) clean

install:
		mkdir -p $(INSTDIR)
		cp ${TARGET}.ko $(INSTDIR)
		cp linux/pf_q.h $(DESTDIR)/usr/include/linux
		cp linux/pf_q-fun.h $(DESTDIR)/usr/include/linux
		cp linux/pf_q-kcompat.h   $(DESTDIR)/usr/include/linux
		mkdir -p /usr/local/include/pfq/
		cp Module.symvers $(INSTDIR)

ifeq (,$(DESTDIR))
		-/sbin/depmod -a ${KERNELVERSION}
endif
