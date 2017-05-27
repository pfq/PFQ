# (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
#


TARGET = RTP

EXTRA_CFLAGS += -I/usr/include/ -I$(src)/../../
EXTRA_CFLAGS += -Ofast -march=native -Wno-attributes

KERNELVERSION := $(shell uname -r)
KBUILD_EXTRA_SYMBOLS := /lib/modules/${KERNELVERSION}/kernel/net/pfq/Module.symvers

obj-m := $(TARGET).o


all:
		make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
		make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean

