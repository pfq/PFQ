#
# (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
#

TARGET = pfq

EXTRA_CFLAGS += -I$(obj)
EXTRA_CFLAGS += -g -O3 -march=native -mtune=native -Wno-attributes
EXTRA_CFLAGS += -DEXPORT_SYMTAB

EXTRA_CFLAGS += -DPFQ_USE_PREFETCH
EXTRA_CFLAGS += -DPFQ_USE_SKB_POOL

#EXTRA_CFLAGS += -DPFQ_USE_POLL
#EXTRA_CFLAGS += -DPFQ_DEBUG
#EXTRA_CFLAGS += -DDEBUG


obj-m := $(TARGET).o

pfq-y := pf_q.o pfq/proc.o pfq/shmem.o pfq/memory.o pfq/pool.o pfq/bpf.o pfq/vlan.o \
				pfq/sock.o pfq/thread.o pfq/netdev.o pfq/global.o \
		 		pfq/param.o pfq/timer.o pfq/io.o pfq/percpu.o pfq/qbuff.o \
		 		core/core.o core/sockopt.o core/queue.o core/GC.o core/global.o core/percpu.o core/devmap.o \
		 		core/sock.o core/group.o core/endpoint.o core/stats.o core/printk.o \
		 		core/lang/engine.o core/lang/signature.o core/lang/symtable.o \
		 		core/lang/filter.o core/lang/steering.o core/lang/forward.o \
		 		core/lang/predicate.o core/lang/combinator.o core/lang/control.o \
		 		core/lang/property.o core/lang/bloom.o core/lang/vlan.o core/lang/misc.o \
		 		core/lang/dummy.o

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
