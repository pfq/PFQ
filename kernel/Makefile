# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

TARGET = pfq

EXTRA_CFLAGS += -I$(obj) 
EXTRA_CFLAGS += -DEXPORT_SYMTAB  
EXTRA_CFLAGS += -DPFQ_USE_SKB_LINEARIZE -DPFQ_USE_VLAN_UNTAGGING
EXTRA_CFLAGS += -Ofast -march=native 
#EXTRA_CFLAGS += -DDEBUG

obj-m := $(TARGET).o 

pfq-objs := pf_q.o pf_q-devmap.o pf_q-group.o pf_q-vlan.o pf_q-steer.o pf_q-steer-default.o  pf_q-mpdb-queue.o pf_q-bpf.o 

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
		cp linux/pf_q-steering.h $(DESTDIR)/usr/include/linux
		mkdir -p /usr/local/include/pfq/
		cp Module.symvers $(INSTDIR)

ifeq (,$(DESTDIR))
		-/sbin/depmod -a ${KERNELVERSION}
endif
