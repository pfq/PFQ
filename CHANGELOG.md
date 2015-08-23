PFQ 3.0
-------

 * New functional engine embedded within the kernel module.
 * eDSL PFQ-lang implemented for both C++11 and Haskell languages. 
 * Improved stability, random crashes fixed.
 * Improved pfq-omatic script.
 * Minor bugs fixed.


PFQ 3.1
-------
 * New lightweight Garbage Collector for sk_buff (GC).
 * New experimental functions: (lazy) forward, forwardIO, bridge, tap, tee.
 * Added pfq-bridge tool.
 * Minor bugs fixed.


PFQ 3.2
-------
 * In Q-Lang, support for vectors (of Storable and pod) added.


PFQ 3.3
-------
 * Functional argument serialization updated.
 * Q-lang experimental functions: vlan_id, vlan_id_filter, steer_field,
   bloom, bloom_src, bloom_dst, bloom_filter, bloom_src_filter, 
   bloom_dst_filter, bloom_calc_m, bloom_calc_n, bloom_calc_p.
 * Simple proc added in /proc/net/pfq.
 * Code cleanup and minor bugs fixed.
 * Tools and tests updated.


PFQ 3.4
-------
 * PFQ daemon released.


PFQ 3.5
-------
 * Few bugs and kernel panic fixed.


PFQ 3.6
-------
 * Group control access refactoring.
 * Policies enhanced by socket ownership.
 * BPF extended to newer kernels.


PFQ 3.7
-------
 * pfq-load utility added.


PFQ 3.8
-------
 * Aggressive polling added to libraries.
 * Performance improvements.
 * pfq-load utility improved.
 * Bug fixes.


PFQ 4.0
-------
 * Transparent support of kernel transmission threads.
 * Linux Kernel 3.18 xmit_more bit ready.
 * New APIs for transmission of packets.
 * Shared memory on top of HugePages.
 * Switched to LTS 1.0 Haskell (Stackage).
 * Accelerated pcap library. 
 * Bug fixes.


PFQ 4.1
-------
 * New SPSC double-buffered variadic Tx queue.
 * Active timestamping for transmission.
 * Improved performance and stability.
 * Various bug fixes.


PFQ 4.2
-------
 * Pool of socket buffers improved.


PFQ 4.3
-------
 * Possible regression fixed in group policies.
 * Internal refactoring of socket groups.
 * Miscellaneous improvements.
 * Bug fixes.


PFQ 4.4
-------
 * Improved stability of packet transmission.
 * Bug fixes.


PFQ 4.5
-------
 * Steering enhanced with configurable weights.
 * HugePages support enabled by default. 
 * New binding format for user tools.
 * API+internals of pkt transmission improved.
 * Stats of memory pool improved.
 * Support for i40e driver added.
 * User space tools general improved.
 * New declarative syntax of pfqd config.
 * Performance improvements.
 * Major regressions fixed.
 * Code cleanup.
 * Bugs fixed.

