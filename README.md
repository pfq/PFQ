PFQ v6.0 
========

[![Stability](https://img.shields.io/badge/stability-experimental-red.svg)](http://github.com/badges/stability-badges)
[![Gitter](https://badges.gitter.im/PFQ/pfq.svg)](https://gitter.im/PFQ/pfq?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Introduction
------------

PFQ is a functional network framework designed for the Linux operating system 
that allows efficient packets capture/transmission (10G, 40G and beyond), in-kernel
functional processing and packets steering across sockets/end-points.

PFQ is highly optimized for multi-core architecture, as well as for network devices 
equipped with multiple hardware queues. It works with any NIC and provides a script 
designed to compile accelerated versions of device drivers starting from the source 
code.

PFQ enables the development of high-performance networking applications with 
different programming languages: C, C++ and Haskell. In addition, a pure functional 
language designed for early stages in-kernel packet processing is included: pfq-lang.

pfq-lang is inspired by Haskell and allows the define of small applications that run
on top of network device drivers. Through pfq-lang it is possible to build efficient
bridges, port mirrors, simple firewalls, network balancers and so forth.

The package provides the source code of the PFQ kernel module, user-space libraries for C, 
C++11-14 and Haskell language, an implementation of pfq-lang as eDSL for C++11-14 and
Haskell, an experimental pfq-lang compiler and a set of diagnostic tools.


Features
--------

* Data-path with full lock-free architecture.
* Preallocated pools of socket buffers.
* Compliant with a plethora of network devices drivers.
* Rx and Tx line-rate on 10-Gbit links (14,8 Mpps), tested with Intel ixgbe _vanilla_ drivers.
* Transparent support of kernel threads for asynchronous packets transmission.
* Transmission with active timestamping.
* Groups of sockets which enable concurrent monitoring of multiple multi-threaded applications.
* Per-group packet steering through randomized hashing or deterministic classification.
* Per-group Berkeley and VLAN filters.
* User-space libraries for C, C++11-14 and Haskell language.
* Functional engine for in-kernel packet processing with **pfq-lang**.
* pfq-lang eDLS for C++11-14 and Haskell language.
* **pfq-lang** compiler used to parse and compile pfq-lang programs.
* Accelerated pcap library for legacy applications (line-speed tested with [captop][2]).
* I/O user<->kernel memory-mapped communications allocated on top of HugePages.
* **pfqd** daemon used to configure and parallelize (pcap) legacy applications.
* **pfq-omatic** script that automatically accelerates vanilla drivers.


Publications
------------

* _"PFQ: a Novel Engine for Multi-Gigabit Packet Capturing With Multi-Core Commodity Hardware"_: Best-Paper-Award at PAM2012 in Vienna http://tma2012.ftw.at/papers/PAM2012paper12.pdf
* _"A Purely Functional Approach to Packet Processing"_: ANCS 2014 Conference (October 2014, Marina del Rey) 
* _"Network Traffic Processing with PFQ"_: JSAC-SI-MT/IEEE journal Special Issue on Measuring and Troubleshooting the Internet (March 2016) 
* _"Enabling Packet Fan--Out in the libpcap Library for Parallel Traffic Processing"_: Network Traffic Measurement and Analysis Conference (TMA 2017)
* _"A Pipeline Functional Language for Stateful Packet Processing"_: IEEE International Workshop on NEtwork Accelerated FunctIOns (NEAF-IO '17)

Invited talks
-------------

* _"Functional Network Programming"_ at Tyrrhenian International Workshop on Digital Communication - (Sep. 2016)
* _"Software Accelerations for Network Applications"_ at NetV IRISA / Technicolor Workshop on Network Virtualization (Feb. 2017)

Author
------

Nicola Bonelli <nicola@pfq.io>  


Contributors (in chronological order)
-------------------------------------

Andrea Di Pietro <andrea.dipietro@for.unipi.it>  

Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>  

Gregorio Procissi <g.procissi@iet.unipi.it>

Giacomo Volpi <volpi.gia@gmail.com>

Luca Abeni <abeni@dit.unitn.it>

Tolysz <tolysz@gmail.com>

LSB <leebutterman@gmail.com>

Andrey Korolyov <andrey@xdel.ru>

MrClick <valerio.click@gmx.com>

Paul Emmerich <emmericp@net.in.tum.de>

Bach Le <bach@bullno1.com>

Marian Jancar <jancar.marian@gmail.com>

nizq <ni.zhiqiang@gmail.com>

Giuseppe Sucameli <brush.tyler@gmail.com>

Sergio Borghese <s.borghese@netresults.it>


HomePages
---------

PFQ home-page is [www.pfq.io][1]


[1]: http://www.pfq.io
[2]: https://github.com/awgn/captop
