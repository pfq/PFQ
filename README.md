PFQ: a Linux kernel module for packet capturing on multi-core architectures
===========================================================================

Introduction
------------

PFQ is a capturing engine designed for the Linux kernel that allows efficient 
packet capturing, in-kernel aggregation, functional processing and packet steering 
across sockets. 

PFQ is highly optimized for multi-core architecture, as well as for network 
devices supporting multiple hardware queues. It works with any device
driver (the ones shipped with the Linux Kernel) and provides a script (pfq-omatic) 
that patches and compiles source codes on-the-fly to obtain accelerated drivers.

The package provides the source code of the PFQ kernel module, user-space 
libraries for C, C++11 and Haskell languages and a set of diagnostic tools.


Author
------

Nicola Bonelli <nicola.bonelli@cnit.it>  

Contributors
------------

Andrea Di Pietro <andrea.dipietro@for.unipi.it>  
Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>  
Gregorio Procissi <g.procissi@iet.unipi.it>  

HomePages
---------

PFQ home-page is [pfq.github.io/PFQ][1]. Additional information are available [here][2].


[1]: http://pfq.github.io/PFQ
[2]: http://netgroup.iet.unipi.it/software/pfq/
