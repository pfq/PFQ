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

        { "conditional",	INLINE_FUN_ADDR(conditional) 	, FUN_ACTION | FUN_WITH_PREDICATE 	},
        { "when",       	INLINE_FUN_ADDR(when) 		, FUN_ACTION | FUN_WITH_PREDICATE	},
        { "unless",     	INLINE_FUN_ADDR(unless) 	, FUN_ACTION | FUN_WITH_PREDICATE	},

        { "id",			INLINE_FUN_ADDR(id)           	, FUN_ACTION  	},
        { "ip",         	INLINE_FUN_ADDR(filter_ip)    	, FUN_ACTION   	},
        { "ip6",        	INLINE_FUN_ADDR(filter_ip6)   	, FUN_ACTION   	},
        { "udp",        	INLINE_FUN_ADDR(filter_udp)   	, FUN_ACTION   	},
        { "tcp",        	INLINE_FUN_ADDR(filter_tcp)   	, FUN_ACTION   	},
        { "icmp",       	INLINE_FUN_ADDR(filter_icmp)  	, FUN_ACTION   	},
        { "udp6",        	INLINE_FUN_ADDR(filter_udp6)  	, FUN_ACTION    },
        { "tcp6",        	INLINE_FUN_ADDR(filter_tcp6)  	, FUN_ACTION   	},
        { "icmp6",       	INLINE_FUN_ADDR(filter_icmp6) 	, FUN_ACTION    },
        { "flow",       	INLINE_FUN_ADDR(filter_flow)  	, FUN_ACTION   	},
        { "vlan",       	INLINE_FUN_ADDR(filter_vlan)  	, FUN_ACTION   	},

        { "drop",       	INLINE_FUN_ADDR(forward_drop) 	  , FUN_ACTION  },
        { "broadcast",  	INLINE_FUN_ADDR(forward_broadcast), FUN_ACTION  },
        { "kernel",     	INLINE_FUN_ADDR(forward_kernel)   , FUN_ACTION  },
        { "class",		INLINE_FUN_ADDR(forward_class) 	  , FUN_ACTION | FUN_WITH_ARG },
	{ "mark", 		INLINE_FUN_ADDR(mark) 		  , FUN_ACTION | FUN_WITH_ARG },

        { NULL, NULL}};


