# (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
#

TARGET = pfq

EXTRA_CFLAGS += -I$(obj)
EXTRA_CFLAGS += -O3 -march=native -Wno-attributes
EXTRA_CFLAGS += -DEXPORT_SYMTAB

EXTRA_CFLAGS += -DPFQ_USE_SKB_LINEARIZE
EXTRA_CFLAGS += -DPFQ_USE_SKB_POOL

#EXTRA_CFLAGS += -DPFQ_DEBUG
EXTRA_CFLAGS += -DDEBUG

# profiling...
#
#EXTRA_CFLAGS += -DPFQ_LANG_PROFILE
#EXTRA_CFLAGS += -DPFQ_RX_PROFILE
#EXTRA_CFLAGS += -DPFQ_TX_PROFILE
#

obj-m := $(TARGET).o

pfq-objs := pf_q.o pf_q-sockopt.o pf_q-global.o pf_q-proc.o pf_q-devmap.o pf_q-sock.o pf_q-shmem.o pf_q-memory.o pf_q-pool.o \
			pf_q-group.o pf_q-stats.o pf_q-endpoint.o pf_q-shared-queue.o pf_q-percpu.o pf_q-bpf.o pf_q-vlan.o \
		    pf_q-thread.o pf_q-receive.o pf_q-transmit.o pf_q-netdev.o pf_q-printk.o \
		    lang/engine.o lang/GC.o lang/signature.o lang/symtable.o lang/printk.o \
		    lang/filter.o lang/steering.o lang/forward.o \
		    lang/predicate.o lang/combinator.o lang/conditional.o \
		    lang/property.o lang/bloom.o lang/vlan.o lang/misc.o lang/dummy.o

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
