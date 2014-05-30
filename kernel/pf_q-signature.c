/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/ctype.h>
#include <linux/limits.h>

#include "pf_q-signature.h"

int
pfq_signature_redundant_brackets(string_view_t str)
{
       	int red = INT_MAX, nest = 0;
	int bracket = 0;
        const char *p;

	for(p = str.begin; p != str.end; p++)
	{
	       	switch(*p) {
		case '(': {
			bracket++;
			nest = max(nest, bracket);
		} break;

		case ')': {
			--bracket;
		} break;

		case '>': {
                	red = min(red, bracket);
		} break;

		}
	}

	return red != INT_MAX ? red : nest;
}


string_view_t
pfq_signature_simplify(string_view_t str)
{
	int red = pfq_signature_redundant_brackets(str);
	int n;

	for(n = 0; n < red; n++)
	{
        	while (*str.begin++ != '(')
        	{ }

        	while (str.end != str.begin &&
        		*--str.end != ')')
        	{ }
	}

	return string_view_trim(str);
}


string_view_t
pfq_signature_bind(string_view_t str, int n)
{
	string_view_t s = pfq_signature_simplify(str);

 	int state = 0, bracket = 0, arity = 0;

        const char * p = s.begin;

 	for(; p != s.end && arity < n; ++p)
 	{
         	switch (state)
 		{
 		case 0: {
               		if (*p == '-') {
               			state = 1;
               			continue;
 			}
 			if (*p == '(') {
                         	bracket++;
                         	state = 2;
                         	continue;
 			}
 		} break;
 		case 1: {
                   	if(*p == '>') {
                         	arity++;
                         	state = 0;
                         	continue;
 			}

 			return null_view();
 		} break;
 		case 2: {
 			if (*p == '(') {
 				bracket++;
                         	continue;
 			}
 			if (*p == ')') {
                         	if (--bracket == 0)
                         		state = 0;
 			}

 		} break;

 		}
 	}

	s.begin = p;

	return pfq_signature_simplify(s);
}


int
pfq_signature_arity(string_view_t str)
{
	string_view_t s = pfq_signature_simplify(str);

 	int state = 0, bracket = 0, arity = 0;

        const char * p = s.begin;

 	for(; p != s.end; ++p)
 	{
         	switch (state)
 		{
 		case 0: {
               		if (*p == '-') {
               			state = 1;
               			continue;
 			}
 			if (*p == '(') {
                         	bracket++;
                         	state = 2;
                         	continue;
 			}
 		} break;
 		case 1: {
                   	if(*p == '>') {
                         	arity++;
                         	state = 0;
                         	continue;
 			}
 			return -1;
 		} break;
 		case 2: {
 			if (*p == '(') {
 				bracket++;
                         	continue;
 			}
 			if (*p == ')') {
                         	bracket--;
                         	if (bracket == 0)
                         		state = 0;
 			}

 		} break;

 		}
 	}

 	if (p == s.begin)
 		return -1;

 	return arity;
}


static inline const char *
skip_white_space(const char *p, bool *done)
{
	while (isspace(*p)) {
		*done = true;
		p++;
	}
	return p;
}

bool
pfq_signature_equal(string_view_t sig_a, string_view_t sig_b)
{
	bool eq = true;
	int arity, n;

	arity = pfq_signature_arity(sig_a);

	if (arity != pfq_signature_arity(sig_b))
		return false;

	sig_a = pfq_signature_simplify(sig_a);
	sig_b = pfq_signature_simplify(sig_b);

	if (arity == 0) {

        	const char *a = sig_a.begin;
        	const char *b = sig_b.begin;

		while (a != sig_a.end && b != sig_b.end)
		{
			bool sa = false, sb = false;

			a = skip_white_space(a, &sa);
			b = skip_white_space(b, &sb);

			if (sa != sb)
				return false;

			if (a == sig_a.end || b == sig_b.end)
			    	break;

			if (*a != *b)
				return false;

			a++, b++;
		}

		return a == sig_a.end && b == sig_b.end;
	}

	for(n = 0; n < arity; n++)
	{
        	eq &= pfq_signature_equal( pfq_signature_arg(sig_a, n),
        				   pfq_signature_arg(sig_b, n));

        	if (!eq)
        		return false;
	}

	return eq;
}


bool
pfq_signature_is_function(string_view_t sig)
{
	return pfq_signature_arity(sig) > 0;
}


string_view_t
pfq_signature_arg(string_view_t s, int index)
{
	string_view_t s1 = pfq_signature_bind(s,  index);

        const char * p = s1.begin;
        int bracket = 0;

 	for(; p != s1.end; ++p)
 	{
		if (bracket == 0) {
			if (*p == '-')
				break;
			if (*p == '(') {
				bracket++;
				continue;
			}
		} else {
 			if (*p == '(') {
 				bracket++;
 				continue;
			}
			if (*p == ')') {
				--bracket;
				continue;
			}
		}
	}

	s.begin = s1.begin;
 	s.end   = p;

	return pfq_signature_simplify(s);
}

