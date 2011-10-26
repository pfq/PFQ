PFQ: a Linux kernel module for packet capturing on multi-core architectures
===========================================================================

Introduction
------------

PFQ is capturing engine designed for the Linux kernel that allows efficient 
packet capturing, in-kernel aggregation and packet steering across sockets. 

PFQ is highly optimized for multi-core architecture, as well as for network 
device supporting multiple hardware queues. It works both with vanilla 
drivers and with pfq-aware drivers, that is drivers optimized according to
the PF\_RING philosophy.

The package provides the source code of the kernel module, an optimized driver
for Intel 82599 NICs based on ixgbe-3.4.24 source code, a C++11 user-space 
library and a set of diagnostic tools.

Contributors
------------

Nicola Bonelli <nicola.bonelli@cnit.it>
Andrea Di Pietro <andrea.dipietro@for.unipi.it>

More information are available [here][1].


[1]: http://netgroup.iet.unipi.it/software/pfq/
