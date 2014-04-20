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

        { "conditional",(void *)INLINE_conditional 	},
	{ "mark", 	(void *)INLINE_mark 		},
        { "when",       (void *)INLINE_when 		},
        { "unless",     (void *)INLINE_unless 		},

        { "id",		(void *)INLINE_id       	},
        { "ip",         (void *)INLINE_filter_ip       	},
        { "ip6",        (void *)INLINE_filter_ip6      	},
        { "udp",        (void *)INLINE_filter_udp      	},
        { "tcp",        (void *)INLINE_filter_tcp      	},
        { "icmp",       (void *)INLINE_filter_icmp     	},
        { "flow",       (void *)INLINE_filter_flow     	},
        { "vlan",       (void *)INLINE_filter_vlan     	},

        { "drop",       (void *)INLINE_forward_drop 	   },
        { "broadcast",  (void *)INLINE_forward_broadcast   },
        { "kernel",     (void *)INLINE_forward_kernel 	   },

        { NULL, NULL}};


