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

* GHC Glasgow Haskell Compiler 7.8 or higher.

* Alex and happy tool.

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

* The directory user/C++ contains the C++11-14 library used by 
  C++ user-space applications.

* The directory user/Haskell contains the FFI library used by Haskell user-space applications.

* The directory user/test and user/tools include some tools and examples.  


Satisfy Library Dependencies
----------------------------

Before installing the framework, ensure to have the required tools installed.

pfq-framework depends on the following Haskell libraries:

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


Build the software
------------------

1. To install libraries dependencies, from the base directory launch the following command:

`cabal install --only-dep`


2. To build and install the framework:

`runhaskell Build.hs install`

The command configures, builds and installs the PFQ framework satisfying the dependencies and the correct order of the various build. 

3. Alternatively, you can specify the list of components you want to build from the command line. The following command shows the targets available:

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


