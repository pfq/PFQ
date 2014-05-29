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

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "pf_q-signature.h"

int
pfq_signature_redundant_brackets(string_view_t str)
{
	int red = 0;

	for(;string_view_length(str) != 0;)
	{
		int bracket = 0;
		const char *p;

		str = string_view_trim(str);

		for(p = str.start; p != str.end; p++)
		{
			switch (*p)
			{
			case '(': bracket++;
				  continue;
			case ')': bracket--;
				  continue;
			default:
				if (*p != '-' &&
				    *p != '>' &&
				    !isspace(*p) &&
				    bracket == 0)
					return red;
			}
		}

		red++;

		if (bracket != 0)
			return -1;

		if (*str.start == '(')
		{
			str.start++;
			str.end--;
		}
	}

	return red;
}


string_view_t
pfq_signature_simplify(string_view_t str)
{
	int red = pfq_signature_redundant_brackets(str);
	int n;

	for(n = 0; n < red; n++)
	{
        	while (*str.start++ != '(')
        	{ }

        	while (str.end != str.start &&
        		*--str.end != ')')
        	{ }
	}

	str = string_view_trim(str);

	if (string_view_length(str) >= 2) {
		if (*str.start == '-')
			str.start += 2;
	}

	if (string_view_length(str) >= 2) {
        	if (*(str.end - 1) == '>')
        		str.end -= 2;
	}

	return string_view_trim(str);
}


string_view_t
pfq_signature_bind(string_view_t str, int n)
{
	string_view_t s = pfq_signature_simplify(str);

 	int state = 0, bracket = 0, arity = 0;

        const char * p = s.start;

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
                         	bracket--;
                         	if (bracket == 0)
                         		state = 0;
 			}

 		} break;

 		}
 	}

	s.start = p;
	return pfq_signature_simplify(s);
}


int
pfq_signature_arity(string_view_t str)
{
	string_view_t s = pfq_signature_simplify(str);

 	int state = 0, bracket = 0, arity = 0;

        const char * p = s.start;

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

 	if (p == s.start)
 		return -1;

 	return arity;
}


static inline const char *
skip_white_space(const char *p)
{
	while (isspace(*p))
		p++;
	return p;
}

bool
pfq_signature_equal(string_view_t _a, string_view_t _b)
{
	string_view_t sig_a = pfq_signature_simplify(_a);
	string_view_t sig_b = pfq_signature_simplify(_b);

        const char * a = sig_a.start, * b = sig_b.start;

	while (a != sig_a.end && b != sig_b.end)
	{
		a = skip_white_space(a);
		b = skip_white_space(b);

		if (*a != *b)
        		return false;
		a++;
		b++;
	}

	return a == sig_a.end && b == sig_b.end;
}


bool
pfq_signature_is_function(string_view_t sig)
{
	return pfq_signature_arity(sig) > 0;
}


string_view_t
pfq_signature_arg(string_view_t s, int index)
{
	string_view_t s1 = pfq_signature_bind(s, index);
	string_view_t s2 = pfq_signature_bind(s1,1);

	s.start = s1.start;
 	s.end   = s2.start;

	return pfq_signature_simplify(s);
}

