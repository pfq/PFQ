/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * The full GNU General Public License is included in this distribution in
 * the file called "COPYING".
 *
 ****************************************************************/

#include "inline.h"

struct pfq_function_descr inline_functions[] = {

        { "conditional",	INLINE_FUN_ADDR(conditional) 		},
	{ "mark", 		INLINE_FUN_ADDR(mark) 			},
        { "when",       	INLINE_FUN_ADDR(when) 			},
        { "unless",     	INLINE_FUN_ADDR(unless) 		},

        { "id",			INLINE_FUN_ADDR(id)       		},
        { "ip",         	INLINE_FUN_ADDR(filter_ip)       	},
        { "ip6",        	INLINE_FUN_ADDR(filter_ip6)      	},
        { "udp",        	INLINE_FUN_ADDR(filter_udp)      	},
        { "tcp",        	INLINE_FUN_ADDR(filter_tcp)      	},
        { "icmp",       	INLINE_FUN_ADDR(filter_icmp)     	},
        { "udp6",        	INLINE_FUN_ADDR(filter_udp6)      	},
        { "tcp6",        	INLINE_FUN_ADDR(filter_tcp6)      	},
        { "icmp6",       	INLINE_FUN_ADDR(filter_icmp6)     	},
        { "flow",       	INLINE_FUN_ADDR(filter_flow)     	},
        { "vlan",       	INLINE_FUN_ADDR(filter_vlan)     	},

        { "drop",       	INLINE_FUN_ADDR(forward_drop) 		},
        { "broadcast",  	INLINE_FUN_ADDR(forward_broadcast)   	},
        { "kernel",     	INLINE_FUN_ADDR(forward_kernel) 	},

        { NULL, NULL}};


