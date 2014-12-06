PFQ + libpcap 
=============

Introduction
------------

PFQ is a network monitoring framework designed for the Linux operating system 
that allows efficient packet capturing, in-kernel functional processing and packet 
steering across sockets.

This version of pcap library is intended to support the PFQ framework, thus allowing 
legacy applications to exploit the capture acceleration of PFQ, and at the 
same time to take advantage of PFQ/lang computations to filter and dispatch packets
across pcap sockets.

The pcap library interface is *unchanged*. Additional data (e.g. pfq group) is passed 
to the library as environment variables, while sniffing from multiple devices is possible
by specifying their name in colon-separated fashion.

The greatest benefits are achieved with the cooperation of pfqd, a user-space daemon used
to manage groups and in-kernel computations.


Features
--------

* 10-Gbit Line-rate (14,8Mpps) with tcpdump.
* Parallel sessions of legacy applications through PFQ/lang computations.
* Per-group in-kernel BPF (JIT compiled filters included).
* Fully compliant with PFQ/lang and pfqd.


Details
-------

This implementation of pcap library is extended to support PFQ sockets. By default
the library makes use of AF_PACKET sockets. Only only when a special device name 
is provided the PFQ acceleration takes place.

The syntax of the device name is the following:

```
pfq[/config_file]:[device[:device[:device..]]]
```

Additional PFQ parameters are passed to the library in different ways, by means of
environment variables or a configuration file.


Environment variables
---------------------

The following variables specify additional PFQ parameters not allowed in pcap APIs:


Variable          | Meaning
------------------| --------------------------------------------------
PFQ_GROUP         | Specify the PFQ group for the process.
PFQ_CAPLEN        | Override the snaplen value.
PFQ_RX_SLOTS      | Define the RX queue length of the socket.   
PFQ_TX_SLOTS      | Define the TX queue length of the socket.   
PFQ_TX_QUEUE      | Set the TX HW queue passed to the driver.
PFQ_TX_NODE       | Set the core for the TX kthread (optional)
PFQ_TX_BATCH      | Set the transmission batch length.
PFQ_COMPUTATION   | Set the PFQ/lang computation for the group.


Configuration Files
-------------------

In addition to the environmnet variables it is also possible to specify
a configuration file on per-socket basis. This solve the problem of passing
different values to multiple pcap devices in multi-threaded applications.

The path of the configuration file is passed to the library with the following syntax:

```
pfq/config_file:[device[:device[:device..]]]
```

The configuration is based on the simple key-value grammar.

```
# PFQ configuration file

group  = 11
caplen = 64

rx_slots = 131072

tx_node  = 1

computation = ip >-> steer_flow
```

Examples
--------

Single tcpdump session:

```
PFQ_GROUP=42 tcpdump -n -i pfq:eth0:eth1
```

tcpdump using `pfq-pcap.conf` configuration file:

```
tcpdump -n -i pfq/pfq-pcap.conf:eth0:eth1
```

Load balancing TCP/UDP flows:

```
# master process set computation and binding to devices...

PFQ_GROUP=42 PFQ_COMPUTATION=steer_flow tcpdump -n -i pfq:eth0:eth1

# additional instances specify only the PFQ_GROUP...

PFQ_GORUP=42 tcpdump -n -i pfq
PFQ_GORUP=42 tcpdump -n -i pfq
PFQ_GORUP=42 tcpdump -n -i pfq
...
```

Authors
-------

Nicola Bonelli <nicola@pfq.io>  
Giacomo Volpi <volpi.gia@gmail.com>


HomePage
--------

PFQ home-page is [www.pfq.io][1]. 


[1]: http://www.pfq.io
