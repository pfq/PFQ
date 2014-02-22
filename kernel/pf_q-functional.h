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

#ifndef _PF_Q_STEER_H_
#define _PF_Q_STEER_H_

#include <linux/skbuff.h>
#include <linux/pf_q.h>
#include <linux/pf_q-fun.h>


extern void pfq_function_factory_init(void);
extern void pfq_function_factory_free(void);

extern int  pfq_register_function(const char *module, const char *name, sk_function_t fun);
extern int  pfq_unregister_function(const char *module, const char *name);

extern sk_function_t pfq_get_function(const char *name);

sk_function_t pfq_get_function(const char *name);

#endif /* _PF_Q_STEER_H_ */
