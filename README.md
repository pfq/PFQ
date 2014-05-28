PFQ v3.0 
========

Introduction
------------

PFQ is a network monitoring framework designed for the Linux operating system 
that allows efficient packet capturing, in-kernel functional processing and packet 
steering across sockets. 

PFQ is highly optimized for multi-core architecture, as well as for network 
devices equipped with multiple hardware queues. It works with any device
driver (the ones shipped with the Linux Kernel) and provides a script designed 
to obtain accelerated versions of the drivers from their source codes.

The package provides the source code of the PFQ kernel module, user-space 
libraries for C, C++11 and Haskell languages and a set of diagnostic tools.

Features
--------

* 10-Gbit Line-rate (14,8Mpps) with Intel ixgbe vanilla driver.
* Socket groups allow for concurrent monitoring of multi-threaded applications.
* Per-group packet steering through randomized hashing algorithms or deterministic classifications.
* Per-group Berkeley filters and per-group VLAN filters.
* Extensible framework for in-kernel functional monitoring: Q-Lang. 
* User-space native bindings for C, C++11 and Haskell languages.
* pfq-omatic, a script that allows to compile drivers and get the accelerated versions.
* Accelerated pcap library.

Publication
-----------

We received the Best-Paper-Award at PAM2012 in Vienna for the paper _"PFQ: a Novel Engine for Multi-Gigabit Packet Capturing With Multi-Core Commodity Hardware"_":
http://tma2012.ftw.at/papers/PAM2012paper12.pdf

Author
------

Nicola Bonelli <nicola.bonelli@cnit.it>  

Contributors
------------

Andrea Di Pietro <andrea.dipietro@for.unipi.it>  

Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>  

Gregorio Procissi <g.procissi@iet.unipi.it>

Luca Abeni <abeni@dit.unitn.it>


HomePages
---------

PFQ home-page is [pfq.github.io/PFQ][1]. Additional information are available [here][2].


[1]: http://pfq.github.io/PFQ
[2]: http://netgroup.iet.unipi.it/software/pfq/
