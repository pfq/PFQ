
INSTDIR := /usr/include/pfq

all:
	cd C    && cmake . && make
	cd perf && cmake . && make
	cd test && cmake . && make

clean:
	cd C    && cmake . && make clean
	cd perf && cmake . && make clean
	cd test && cmake . && make clean

install:
	mkdir -p ${INSTDIR}
	cp C++/pfq.hpp ${INSTDIR}

