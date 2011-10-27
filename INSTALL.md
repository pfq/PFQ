PFQ: a Linux kernel module for packet capturing on multi-core architectures
===========================================================================

Requirements
------------


To install PFQ, the following requisites must be present:

* A 64-bit Linux operating system (the support for 32-bit architectures 
  is not currently available) 

* A modern Linux kernel: suggested Linux 2.6.39 or higher.  

* Kernel headers installed, required to compile kernel modules for your kernel.  

* A working gcc compiler, usually the one used to compile the kernel.  

* A working g++ compiler, g++-4.5 or higher, required to compile the user-space tools.  

* CMake and make tool


Directories 
----------- 


The package is organized as follow: 

.  
├── AUTHORS   
├── driver   
│   ├── ixgbe-3.3.9/  
│   │   ├── COPYING  
│   │   └── ...  
│   └── ixgbe-3.4.24/  
│       ├── COPYING  
│       └── ...  
├── INSTALL  
├── kernel  
│   ├── linux  
│   │   └── pf_q.h  
│   ├── Makefile  
│   └── ...  
├── README.md  
└── user  
    ├── c++  
    │   └── pfq.hpp  
    └── perf  
        ├── CMakeFiles  
        └── ...  


* The directory driver/ contains optimized drivers for PFQ (pfq-aware drivers).

* The directory kernel/ contains the source code of PFQ, along with the pf_q.h 
  header used by user-space applications.

* The directory user/c++ contains an optimized C++11 inline library used to write 
  user-space applications.

* The directory user/perf includes some tools and examples.  


PFQ module
----------

To compile PFQ kernel module:

\# cd kernel && make && make install


PFQ tools
---------

To compile the userland tools shipped with PFQ:
 
\# cd user/perf

\# cmake .

\# make


Notes
-----

In order to obtain the maximum performance from PFQ you have to configure your system
and design your application properly.

Interrupt affinity is indeed a crucial step required for both NAPI context and user-space 
application. A bad setup may impact on the performance dramatically.

Multiple-queue NICs, like Intel 82599 for instance, allow a simple interrupt affinity 
by means of the script set_irq_affinity.sh. Such a setup is sufficient in most cases.

In addition to this, when configuring PFQ please bear in mind the following notes. 


* The load balancing can be enabled on Intel 82599 by means of RSS or VMDQ. Other vendors 
  may provide different technologies.

* A good choice setup is to distribute interrupts among cores. The more core you have, 
  the best performance you obtain from PFQ. We have tested it with 12 cores with great 
  satisfaction :-) -- 13Mpps collected at user-space with 5% of CPUs, on top of a 2.66 Ghz 
  6-cores Xeon processor and Intel 82599 NIC.

* For a single capturing thread, a reserved core gives the best performance. Although the
  PFQ library is written in C++11, the new standard has no means to setup the affinity for a 
  std::thread. To do this you have to go native and use the non-posix system call 
  pthread_setaffinity_np.

* Flow Control of the NIC may slow down the link. Use ethtool to disable it.


