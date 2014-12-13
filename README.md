PFQ v4.0 
========

Introduction
------------

PFQ is a network monitoring framework designed for the Linux operating system 
that allows efficient packet capturing, in-kernel functional processing and packet 
steering across sockets. 

PFQ is highly optimized for multi-core architecture, as well as for network 
devices equipped with multiple hardware queues. It works with any device
driver and provides a script designed to obtain accelerated versions of the drivers 
from their source codes.

The framework enables the development of high-performance networking applications 
with different languages (i.e. C, C++ and Haskell). In addition, a new functional language 
designed for software defined monitoring and networking is included: PFQ/lang.

The package provides the source code of the PFQ kernel module, user-space libraries 
for C, C++11 and Haskell languages, PFQ/lang implemented as eDSL and a set of diagnostic tools.

Features
--------

* 10-Gbit Line-rate (14,8Mpps) with Intel ixgbe vanilla driver.
* Transparent support of multiple kernel threads for packets transmission.
* Socket groups allow for concurrent monitoring of multi-threaded applications.
* Per-group packet steering through randomized hashing algorithms or deterministic classifications.
* Per-group Berkeley filters and per-group VLAN filters.
* Extensible framework for in-kernel functional monitoring: PFQ-Lang. 
* User-space native bindings for C, C++11 and Haskell languages.
* pfq-omatic, a script that allows to compile drivers and get the accelerated versions.
* Accelerated pcap library for both packet capture and transmission.

Publications
------------

* _"PFQ: a Novel Engine for Multi-Gigabit Packet Capturing With Multi-Core Commodity Hardware"_: Best-Paper-Award at PAM2012 in Vienna http://tma2012.ftw.at/papers/PAM2012paper12.pdf
* _"A Purely Functional Approach to Packet Processing"_: ANCS 2014 Conference (October 2014, Marina del Rey) 

Author
------

Nicola Bonelli <nicola@pfq.io>  

Contributors
------------

Andrea Di Pietro <andrea.dipietro@for.unipi.it>  

Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>  

Gregorio Procissi <g.procissi@iet.unipi.it>

Luca Abeni <abeni@dit.unitn.it>


HomePages
---------

PFQ home-page is [www.pfq.io][1]. Additional information are available at [netgroup/pfq][2].


[1]: http://www.pfq.io
[2]: http://netgroup.iet.unipi.it/software/pfq/
