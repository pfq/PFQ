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


The framework is organized as follow: 

    .
    ├── Development
    ├── docs
    ├── kernel
    │   ├── functional
    │   ├── linux
    │   └── module
    │       └── RTP
    ├── misc
    │   ├── Fanout
    │   └── signature
    ├── script
    │   ├── irq-affinity
    │   ├── pfq-load
    │   └── pfq-omatic
    └── user
        ├── C
        ├── C++
        │   └── pfq
        │       └── lang
        ├── common
        ├── Haskell
        │   ├── Network
        │   │   └── PFq
        │   ├── pfq-counters
        │   └── test
        │       └── Network -> ../Network
        ├── pfqd
        │   ├── example
        │   └── src
        ├── test
        └── tool


* The directory kernel/ contains the source code of PFQ, along with some
  headers used by user-space applications.

* The directory user/C contains the user-space C library used by user-space
  applications.

* The directory user/C++ contains the C++11 inline library used by 
  C++ user-space applications.

* The directory user/Haskell contains the FFI library used by Haskell user-space applications.

* The directory user/test and user/tools include some tools and examples.  


Satisfy Library Dependencies
----------------------------

Before installing the framework, ensure the following Haskell libraries are installed:

* filepath
* directory
* unix
* process
* daemons
* network
* cmdargs
* hslogger
* ansi-terminal
* storable-tuple
* storablevector
* data-default
* semigroups
* mtl
* regex-posix
* bytestring
* split

You can use the cabal tool to install them. More information on cabal are available at: [https://www.haskell.org/cabal/download.html](https://www.haskell.org/cabal/download.html).

In addition, we suggest to update cabal to the most recent version with:

`cabal install cabal-install`

Please ensure you have ~/cabal/bin in your PATH.

Build the software
------------------

2. From the base directory launch the following command:

`runhaskell Build.hs install`

The command will configure, build and install the PFQ framework satisfying the dependencies and the correct build order for the various components. 

Alternatively, you can specify the list of components you want to build from the command line. The following command shows the targets available:

`runhaskell Build.hs show`

Example:

`runhaskell Build.hs install pfq.ko pfqd`

Software Components
-------------------

The following components are currently part of the framework:

* pfq.ko
* pfq-clib
* pfq-cpplib
* pfq-haskell-lib
* irq-affinity
* pfq-counters
* pfq-omatic
* pfq-load
* pfqd
* C/C++-test
* C/C++-tools


Notes
-----

For PFQ, in order to obtain the maximum performance you have to configure your system
and design your application properly.

In particular, interrupt affinity is a crucial step required for both NAPI context and 
user-space application to work at the best condition.  A good setup dramatically impacts on the performance.

Multiple-queue NICs, like Intel 82599 for instance, allow to setup interrupt affinity.
In most case the script set_irq_affinity.sh is sufficient. For more advanced setup, we suggest
to use irq-affinity Haskell script shipped with the framework.

In addition to this, when configuring PFQ bear in mind the following notes. 

* The load balancing can be enabled on Intel 82599 by means of RSS or VMDQ. Other vendors may provide different technologies.

* A good setup distributes interrupts among cores. In general, the more core you use the best performance you obtain. 
  We are able to capture 14.8Mpps at user-space with a single thread and 3 hardware queues, running on top of a 2.66 Ghz 6-cores Xeon processor and Intel 82599 NIC.

* For a single capturing thread, a reserved core gives it the best performance. 

* Flow Control of the NIC may slow down the link. Use ethtool to disable it.


