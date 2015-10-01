/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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


#ifndef PFQ_LANG_SIGNATURE_H
#define PFQ_LANG_SIGNATURE_H

#include <lang/string-view.h>

string_view_t pfq_lang_signature_simplify(string_view_t sig);
string_view_t pfq_lang_signature_bind(string_view_t sig, int n);
string_view_t pfq_lang_signature_arg(string_view_t sig, int n);
string_view_t pfq_lang_signature_remove_extent(string_view_t str);

int    pfq_lang_signature_redundant_brackets(string_view_t sig);
int    pfq_lang_signature_arity(string_view_t sig);
bool   pfq_lang_signature_equal(string_view_t a, string_view_t b);
bool   pfq_lang_signature_is_function(string_view_t sig);

ptrdiff_t pfq_lang_signature_sizeof(string_view_t str);

bool   pfq_lang_signature_check(string_view_t sig);
bool   pfq_lang_signature_type_check(string_view_t t);


#endif /* PFQ_LANG_SIGNATURE_H */
