
INSTDIR := /usr/include/pfq

all:
	cd perf && cmake . && make

clean:
	cd perf && cmake . && make clean

install:
	mkdir -p ${INSTDIR}
	cp c++/pfq.hpp ${INSTDIR}

