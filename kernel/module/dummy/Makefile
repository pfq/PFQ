# (C) 2011-12 Nicola Bonelli <nicola@pfq.io>
#


TARGET = dummy

EXTRA_CFLAGS += -I$(src)/../../ -Wno-attributes

obj-m := $(TARGET).o

all:
		make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
		make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean

