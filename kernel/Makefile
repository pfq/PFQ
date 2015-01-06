# (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
#

TARGET = pfq

EXTRA_CFLAGS += -I$(obj)
EXTRA_CFLAGS += -O3 -march=native
EXTRA_CFLAGS += -DEXPORT_SYMTAB

EXTRA_CFLAGS += -DPFQ_USE_SKB_LINEARIZE
EXTRA_CFLAGS += -DPFQ_USE_SKB_RECYCLE
EXTRA_CFLAGS += -DPFQ_USE_EXTENDED_PROC
EXTRA_CFLAGS += -DPFQ_USE_XMIT_MORE

EXTRA_CFLAGS += -DPFQ_DEBUG
EXTRA_CFLAGS += -DDEBUG

# profiling...
#
#EXTRA_CFLAGS += -DPFQ_LANG_PROFILE
#EXTRA_CFLAGS += -DPFQ_RX_PROFILE
#EXTRA_CFLAGS += -DPFQ_TX_PROFILE
#

obj-m := $(TARGET).o

pfq-objs := pf_q.o pf_q-sockopt.o pf_q-global.o pf_q-proc.o pf_q-devmap.o pf_q-sock.o pf_q-shmem.o pf_q-memory.o pf_q-group.o \
		    pf_q-endpoint.o pf_q-symtable.o pf_q-engine.o pf_q-shared-queue.o pf_q-percpu.o pf_q-bpf.o pf_q-vlan.o \
		    pf_q-thread.o pf_q-transmit.o pf_q-signature.o pf_q-GC.o pf_q-printk.o \
		    functional/filter.o functional/steering.o functional/forward.o \
		    functional/predicate.o functional/combinator.o functional/conditional.o \
		    functional/property.o functional/bloom.o functional/vlan.o functional/misc.o functional/dummy.o

KERNELVERSION := $(shell uname -r)

INSTDIR := $(DESTDIR)/lib/modules/${KERNELVERSION}/kernel/net/pfq

all:
		$(MAKE) -C /lib/modules/${KERNELVERSION}/build M=$(PWD) modules

clean:
		$(MAKE) -C /lib/modules/${KERNELVERSION}/build M=$(PWD) clean

install:
		mkdir -p $(INSTDIR)
		cp ${TARGET}.ko $(INSTDIR)
		cp linux/pf_q.h $(DESTDIR)/usr/include/linux
		cp linux/pf_q-kcompat.h   $(DESTDIR)/usr/include/linux
		mkdir -p /usr/local/include/pfq/
		cp Module.symvers $(INSTDIR)

ifeq (,$(DESTDIR))
		-/sbin/depmod -a ${KERNELVERSION}
endif
