PFQ v5.2 
========

[![Join the chat at https://gitter.im/pfq/PFQ](https://badges.gitter.im/pfq/PFQ.svg)](https://gitter.im/pfq/PFQ?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Introduction
------------

PFQ is a functional networking framework designed for the Linux operating system 
that allows efficient packets capture/transmission (10G and beyond), in-kernel 
functional processing and packets steering across sockets/end-points.

PFQ is highly optimized for multi-core architecture, as well as for network devices 
equipped with multiple hardware queues. It works with any network device driver and 
provides a script designed to obtain accelerated versions starting from source 
code.

The framework enables the development of high-performance networking applications with 
different programming languages: C, C++ and Haskell. In addition, a pure functional 
language designed for early stages in-kernel applications is included: pfq-lang.

pfq-lang is inspired by Haskell and allows the creation of small applications that run in 
kernel space, on top of network device drivers. Through pfq-lang it is possible to build
efficient bridges, port mirrors, simple firewalls, network balancers and so forth.

The package provides the source code of the PFQ kernel module, user-space libraries for C, 
C++11-14 and Haskell language, an implementation of pfq-lang as eDSL for both  C++11-14 and 
Haskell, and a set of diagnostic tools.

Features
--------

* Full lock-free architecture.
* Preallocated pools of socket buffers.
* Compliant with a plethora of network devices drivers.
* Rx and Tx line-rate on 10G links (14,8 Mpps), on-top-of Intel ixgbe _vanilla_ drivers.
* Transparent support of kernel threads for asynchronous packets transmission.
* Asynchronous transmission with active timestamping (which determines when to transmit a packet).
* Concurrent monitoring of multiple multi-threaded applications by means of groups of sockets.
* Per-group packet steering through randomized hashing or deterministic classification.
* Per-group Berkeley and VLAN filters.
* Functional engine for in-kernel packet processing with **pfq-lang**.
* User-space libraries for C, C++11-14 and Haskell languages.
* pfq-lang eDLS for C++11-14 and Haskell language.
* Accelerated pcap library (line-speed tested with [captop][3]).
* **pfqd** daemon used to parallelize multiple instances of pcap legacy applications.
* **pfq-omatic**, a script designed to accelerate vanilla drivers.
* I/O user<->kernel shared-memory on top of **HugePages**.

Publications
------------

* _"PFQ: a Novel Engine for Multi-Gigabit Packet Capturing With Multi-Core Commodity Hardware"_: Best-Paper-Award at PAM2012 in Vienna http://tma2012.ftw.at/papers/PAM2012paper12.pdf
* _"A Purely Functional Approach to Packet Processing"_: ANCS 2014 Conference (October 2014, Marina del Rey) 
* _"Network Traffic Processing with PFQ"_: JSAC-SI-MT/IEEE journal Special Issue on Measuring and Troubleshooting the Internet (April 2016) 

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
[3]: https://github.com/awgn/captop
