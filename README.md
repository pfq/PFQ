PFQ: a Linux kernel module for packet capturing on multi-core architectures
===========================================================================

Introduction
------------

PFQ is a capturing engine designed for the Linux kernel that allows efficient 
packet capturing, in-kernel aggregation, functional processing and packet steering 
across sockets. 

PFQ is highly optimized for multi-core architecture, as well as for network 
devices supporting multiple hardware queues. It works with vanilla 
drivers and provides a script (pfq-omatic) that allows to recompiles them 
to obtain accelerated versions.

The package provides the source code of the kernel module, user-space 
libraries for C, C++11 and Haskell languages and a set of diagnostic tools.


Author
------

Nicola Bonelli <nicola.bonelli@cnit.it>  

Contributors
------------

Andrea Di Pietro <andrea.dipietro@for.unipi.it>  
Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>  
Gregorio Procissi <g.procissi@iet.unipi.it>  

More information are available [here][1].


[1]: http://netgroup.iet.unipi.it/software/pfq/
