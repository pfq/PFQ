/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#ifndef __PF_Q_SIGNATURE__
#define __PF_Q_SIGNATURE__

#include "string-view.h"

typedef int bool;
static bool false = 0;
static bool true  = 1;


string_view_t pfq_signature_head(string_view_t str);
string_view_t pfq_signature_tail(string_view_t str);

string_view_t pfq_signature_simplify(string_view_t sig);
string_view_t pfq_signature_bind(string_view_t sig, int n);
string_view_t pfq_signature_arg(string_view_t sig, int n);

int  pfq_signature_arity(string_view_t sig);
bool pfq_signature_equal(string_view_t a, string_view_t b);
bool pfq_signature_is_function(string_view_t sig);

#endif /* __PF_Q_SIGNATURE__ */
