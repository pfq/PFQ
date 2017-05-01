#
# (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
#

TARGET = pfq

EXTRA_CFLAGS += -I$(obj)
EXTRA_CFLAGS += -g -O3 -march=native -fomit-frame-pointer -Wno-attributes
EXTRA_CFLAGS += -DEXPORT_SYMTAB

EXTRA_CFLAGS += -DPFQ_USE_PREFETCH
EXTRA_CFLAGS += -DPFQ_USE_SKB_POOL
EXTRA_CFLAGS += -DPFQ_USE_EXTRA_COUNTERS

#EXTRA_CFLAGS += -DPFQ_DEBUG
#EXTRA_CFLAGS += -DDEBUG


obj-m := $(TARGET).o

pfq-y := pf_q.o pfq/proc.o pfq/shmem.o pfq/memory.o pfq/pool.o pfq/bpf.o pfq/vlan.o \
				pfq/sock.o pfq/thread.o pfq/netdev.o pfq/global.o \
		 		pfq/param.o pfq/timer.o pfq/io.o pfq/percpu.o pfq/qbuff.o \
		 		pfq/sockopt.o pfq/queue.o pfq/global.o pfq/percpu.o pfq/devmap.o \
		 		pfq/sock.o pfq/group.o pfq/endpoint.o pfq/stats.o pfq/printk.o \
		 		lang/engine.o lang/signature.o lang/symtable.o \
		 		lang/filter.o lang/steering.o lang/forward.o \
		 		lang/predicate.o lang/combinator.o lang/control.o \
		 		lang/property.o lang/bloom.o lang/vlan.o lang/misc.o \
		 		lang/dummy.o

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
