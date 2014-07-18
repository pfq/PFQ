PFQ
===========================================================================

Requirements
------------


To install PFQ, the following requisites must be present:

* A 32/64-bit Linux operating system (Intel/AMD architectures are currently supported).

* A modern Linux kernel: suggested Linux 3.0 or higher.  

* Kernel headers installed, required to compile modules for your kernel.  

* A working gcc compiler, usually the one used to compile the kernel.  

* A working g++ compiler, g++-4.7 or higher, required to compile the user-space tools and applications.  

* Haskell-platform: to compile user-space tools.

* CMake and make tool.


Directories 
----------- 


The package is organized as follow: 

    .
    |-- kernel
    |   |-- functional
    |   |   |-- dummy
    |   |   `-- rtp
    |   `-- linux
    |-- script
    `-- user
        |-- C
        |-- C++
        |-- extra
        |-- Haskell
        |   `-- Network
        |-- test
        `-- tool

* The directory kernel/ contains the source code of PFQ, along with some
  headers used by user-space applications.

* The directory user/C contains the user-space library used to write user-space
  applications.

* The directory user/C++ contains the C++11 inline library used to write 
  user-space applications.

* The directory user/Haskell contains the FFI library used to write user-space applications.

* The directory user/test and user/tools includes some tools and examples.  


PFQ module
----------

To compile PFQ kernel module:

\# cd kernel && make && make install


PFQ test and tools
------------------

To compile the userland tools shipped with PFQ:
 
\# cd user/test

\# cmake .

\# make


Notes
-----

In order to obtain the maximum performance from PFQ you have to configure your system
and design your application properly.

Interrupt affinity is indeed a crucial step required for both NAPI context and user-space application to work at the best condition. A bad setup may dramatically impact on the performance.

Multiple-queue NICs, like Intel 82599 for instance, allow a simple interrupt affinity 
by means of the script set_irq_affinity.sh. Such a setup is sufficient in most cases.

In addition to this, when configuring PFQ please bear in mind the following notes. 

* The load balancing can be enabled on Intel 82599 by means of RSS or VMDQ. Other vendors may provide different technologies.

* A good choice setup is to distribute interrupts among cores. The more core you have the best performance you obtain from PFQ. We have tested it with 12 cores with great satisfaction :-) -- 14.8Mpps collected at user-space with 3 CPUs, on top of a 2.66 Ghz 6-cores Xeon processor and Intel 82599 NIC.

* For a single capturing thread, a reserved core gives it the best performance. Although the PFQ library is written in C++11, the new standard has no means to setup the affinity for a std::thread. To do this you have to go native and use the non-posix system call pthread_setaffinity_np.

* Flow Control of the NIC may slow down the link. Use ethtool to disable it.


