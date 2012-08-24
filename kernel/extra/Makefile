# (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>
#


TARGET = pfq-dummy

EXTRA_CFLAGS += -I/usr/include/
EXTRA_CFLAGS += -Ofast -march=native 

KERNELVERSION := $(shell uname -r)
KBUILD_EXTRA_SYMBOLS := /lib/modules/${KERNELVERSION}/kernel/net/pfq/Module.symvers

obj-m := $(TARGET).o 


all:
		make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
		make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean

