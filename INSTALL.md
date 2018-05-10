PFQ
===

The following Guide is also available at [Getting-Starte-Guide](https://github.com/pfq/PFQ/wiki/Getting-Started-Guide).

# Table of Contents

This guide covers the following topics:

1. What is PFQ.
2. HW and SW Requirements.
3. Haskell and Linux Distributions.
4. Obtaining source codes.
5. Satisfy library dependencies.
6. Build the software.
7. Software components. 

## What is PFQ

PFQ is a multi-language network framework designed for the Linux Operating System and the Linux Kernel 3.x (or higher). It is highly optimized for multi-core processors, 
as well as for network devices equipped with multiple hardware queues (i.e. Intel 82599 10G).

PFQ consists in a Linux Kernel module, user-space libraries, for the C, C++11/14, Haskell languages and native support for accelerated libpcap library 
with programmable fanout.


## HW and SW Requirements

* A 32/64-bit Linux operating system (Intel/AMD architectures are supported).
* Linux Kernel 3.5 or higher.
* Kernel headers, required to compile modules for your kernel.
* A gcc compiler, the one used to compile the kernel in use.
* A g++ compiler (g++-4.8/clang-3.4 or higher), for user-space tools and libraries.
* The Haskell stack tool [haskell-stack](https://docs.haskellstack.org/en/stable/README/) 
* Alex and happy tool.
* CMake, make, autoconf.
* Flex and bison.


### Haskell and Linux Distributions (PIE)

We recommend you to install GHC compiler following the instructions at [Stackage](http://www.stackage.org/install) site.

If you plan to install the Haskell platform from [Haskell-Platform](https://www.haskell.org/platform/), you may enconter compilation problems if you have
a system with PIE (position independent executables) enabled by default (such as Ubuntu, Debian etc).

If it is your case, you have to edit the GHC settings file at:

`/usr/local/haskell/ghc-___/lib/ghc-___/settings`

and change the `compiler supports -no-pie` flag from "NO" to "YES".


## Obtaining Source Codes

Clone the source codes from the GitHub repository with the following command:
 
`git clone https://github.com/pfq/PFQ.git`


## Build the software

* To build and install the framework:

`stack Build.hs --build-type=Release install`

The command configures, compiles and installs PFQ framework satisfying the dependencies and the correct order for building the components.

* Alternatively, you can specify the list of components you want to build from the command line. The following command shows the targets available:

```
stack Build.hs show

targets:
    pfq.ko
    pfq-clib
    pfq-cpplib
    pfq-haskell-lib
    pfq-pcap-1.8.1-fanout
    pfq-hcounters
    pfq-lang
    pfq-affinity
    pfq-omatic
    pfq-load
    pfq-stress
    pfqd
    regression
    h-regression
    tools
```

For example, to install pfq.ko and pfqd:

`stack Build.hs -j3 --build-type=Release install pfq.ko pfqd`


## Software Components

The following components are currently part of the framework:

* pfq.ko
* pfq-clib
* pfq-cpplib
* pfq-haskell-lib
* pfq-pcap-1.8.1-fanout
* pfq-hcounters
* pfq-lang
* pfq-affinity
* pfq-omatic
* pfq-load
* pfq-stress
* pfqd
* regression
* h-regression
* tools

