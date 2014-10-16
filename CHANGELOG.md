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

