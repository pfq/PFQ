PFQ/pcap 
========

Introduction
------------

This version of pcap library is intended to support the PFQ framework, thus allowing 
legacy applications to exploit the acceleration of capture/transmission of PFQ, and at the 
same time to take advantage of pfq-lang computations to filter and dispatch packets
across pcap sockets of the same group.

The pcap library interface is *unchanged*. Additional data (e.g. pfq group) is passed 
to the library as environment variables (or via configuration file), and sniffing 
from multiple devices is also possible by specifying their name in colon-separated fashion.


Features
--------

* 10-Gbit Line-rate (14,8Mpps) tested with tcpdump.
* Parallel sessions of legacy applications through pfq-lang computations.
* Per-group in-kernel BPF (JIT compiled filters included).
* Fully compliant with pfq-lang and pfqd.


Details
-------

This implementation of pcap library is extended to support PFQ sockets. By default
the library makes use of AF\_PACKET sockets. Only only when a special device name 
is provided (prefixed with the string pfq) the PFQ acceleration takes place. 

For applications that do not allow arbitrary names for devices, it is possible 
to trigger the PFQ acceleration by specifying one of the environment variables
described in the table below.

The syntax of the device name is the following:

```
pfq[/config_file]:[device[:device[:device..]]]
```

Additional PFQ parameters are passed to the library by means of environment variables 
or configuration file.


Environment variables
---------------------

The following table summarize the PFQ parameters that cannot be passed through the 
standard pcap APIs.

If not specified otherwise, the default values are assumed. 

(> 1Mpps) column reports suggested values for very high packet rates, 
e.g. 10G links with short packets. 

note: PFQ\_TX\_QUEUE, PFQ\_TX\_THREAD, and PFQ\_VLAN are specified as comma separated list.


Variable          |    Default    |  > 1Mpps  | Meaning
------------------|---------------|-----------|--------------------------------------------
PFQ\_CONFIG       |               |           | Specify the PFQ/pcap config file
PFQ\_GROUP        |  a free one   |           | Specify the PFQ group for the process
PFQ\_CAPLEN       | pcap snapshot |           | Override the snaplen value for capture
PFQ\_RX\_SLOTS    |    4096       |  131072   | Define the RX queue length of the socket   
PFQ\_TX\_SLOTS    |    4096       |   8192    | Define the TX queue length of the socket   
PFQ\_TX\_FHINT    |      1        | 16..512   | Hint used to flush the transmission queue
PFQ\_TX\_QUEUE    | empty list    |e.g. 0,1,2 | Set the TX HW queue passed to the driver
PFQ\_TX\_THREAD   | empty list    |e.g. 0,1,2 | Set the index of the PFQ TX threads (optional)
PFQ\_LANG         |    null       |           | Set the pfq-lang computation for the group
PFQ\_VLAN         | empty list    |           | Set the pfq vlan filter list for the group


Configuration Files
-------------------

In addition to environment variables, it is also possible to specify PFQ parameters with 
a configuration file, on per-socket basis. This solves the problem of passing different values 
to multiple pcap socket in multi-threaded applications.

The path of the configuration file is passed to the library along with the device name:

```
pfq/config_file:[device[:device[:device..]]]
```
Note that the character / is used as separator. To specify an absolute path, e.g. /etc/pfq.conf, 
use the //, as in:


```
pfq//etc/pfq.conf:eth0:eth1
```

The configuration file is based on a simple key-value grammar.

```
# PFQ configuration 

group  = 11
caplen = 64

rx_slots = 131072

tx_thread = 0,1
tx_queue  = 0,1

qlang = main = ip >-> steer_flow
```

It is possible to specify multi-line pfq-lang computations, by prefixing each
line with qlang =:


```
qlang =
qlang = main = ip >-> 
qlang =             tcp
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

PFQ_GROUP=42 PFQ_LANG="main = steer_flow" tcpdump -n -i pfq:eth0:eth1

# additional instances specify only the PFQ_GROUP...

PFQ_GROUP=42 tcpdump -n -i pfq
PFQ_GROUP=42 tcpdump -n -i pfq
PFQ_GROUP=42 tcpdump -n -i pfq
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
