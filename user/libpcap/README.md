PFQ/pcap 
========

Introduction
------------

This version of pcap library is intended to support the PFQ framework, thus allowing 
legacy applications to exploit the acceleration of packet capture/transmission of PFQ, 
and at the same time to take advantage of pfq-lang computations to filter and dispatch 
packets across sockets of the same group.

The pcap library interface is *unchanged*. Additional data (e.g. pfq group) is passed 
to the library as environment variables or specified via configuration file. Sniffing 
from multiple devices is also possible by specifying their name in colon-separated fashion.


Features
--------

* 10-Gbit Line-rate (14,8Mpps) tested with tcpdump and [captop][2].
* Parallel sessions of legacy applications throughout pfq-lang filtering and steering.
* Per-group in-kernel BPF (JIT compiled filtersincluded).


Details
-------

This implementation of pcap library is extended to support the PFQ socket. By default
the library makes use of AF\_PACKET sockets and only when the device name is prefixed 
with the string "pfq" the PFQ acceleration takes place. 

For applications that do not allow arbitrary names for devices, it is possible 
to trigger the PFQ acceleration by specifying one of the environment variables
described in the table below.

The syntax of the device name is the following:

```
pfq[/config_file]:[device[:device[:device..]]]
```

Environment variables
---------------------

The table summarize the PFQ parameters that cannot be passed through the 
standard pcap APIs.

If not specified otherwise, the default values are assumed. 

(> 1Mpps) column reports suggested values for very high packet rates, 
e.g. 10G links with short packets. 


Variable             |    Default    |  > 1Mpps  | Meaning
---------------------|---------------|-----------|--------------------------------------------
PFQ\_CONFIG          |               |           | Specify the PFQ/pcap config file
PFQ\_GROUP           |  a free one   |           | Specify the PFQ group for the process
PFQ\_CAPLEN          | pcap snapshot |           | Override the snaplen value for capture
PFQ\_RX\_SLOTS       |    4096       |  131072   | Define the RX queue length of the socket
PFQ\_TX\_SLOTS       |    4096       |   8192    | Define the TX queue length of the socket
PFQ\_TX\_FLUSH\_HINT |      1        | 16..512   | Hint used to flush the transmission queue
PFQ\_TX\_HW\_QUEUE   | empty list    |e.g. 0,1,2 | Set the TX HW queue passed to the driver
PFQ\_TX\_IDX\_THREAD | empty list    |e.g. 0,1,2 | Set the index of the PFQ TX kernel threads (optional)
PFQ\_LANG\_SRC       |    null       |           | Load the pfq-lang computation from source file
PFQ\_LANG\_LIT       |    null       |           | Set the pfq-lang computation from the env. variable
PFQ\_VLAN            | empty list    |           | Set the pfq vlan id filter list for the group


Note: PFQ\_TX\_HW\_QUEUE, PFQ\_TX\_IDX\_THREAD, and PFQ\_VLAN are specified as comma separated list.

Note(2): PFQ\_LANG\_SRC specifies the name of the pfq-lang source code to load, whereas PFQ\_LANG\_LIT
directly contains the pfq-lang computation.


Configuration File
------------------

In addition to the environment variables, it is possible to specify PFQ parameters with 
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

In addition, it is possible to directly embed a pfq-lang program by prefixing each line with > 
(this is known as Haskell bird style). 

```
# PFQ configuration 

group  = 11
caplen = 64

rx_slots = 131072

tx_thread = 0,1
tx_queue  = 0,1

> 
>  main = do 
>           ip
>           tcp
> 
```

It is also possible to specify an external pfq-lang file using the keyword lang (as in the following examples):

```
lang = /etc/group-lang.hs
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

or

```
PFQ_CONFIG=/pfq-pcap.conf tcpdump -n -i pfq:eth0:eth1
```

Load balancing TCP/UDP flows:

```
# master process sets computation and binding to devices...

PFQ_GROUP=42 PFQ_LANG_LIT="main = steer_flow" tcpdump -n -i pfq:eth0:eth1

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
[2]: https://github.com/awgn/captop

