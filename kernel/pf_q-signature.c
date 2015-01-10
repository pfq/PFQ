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

#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/ctype.h>
#include <linux/limits.h>

#include <pf_q-signature.h>

#ifdef __KERNEL
static
#endif
int
count_outmost_brackets(string_view_t str)
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


#ifdef __KERNEL
static
#endif
const char *
find_next_arrow(string_view_t str)
{
 	int state = 0, bracket = 0;

        const char * p = str.begin;

 	for(; p != str.end; ++p)
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
                   	if(*p == '>')
                   		return p;
			return NULL;
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

	return NULL;
}


static string_view_t
signature_head(string_view_t str)
{
	string_view_t head = str;

	const char * p = find_next_arrow(str);
	if (p != NULL)
		head.end = p-1;

	return head;
}



static string_view_t
signature_tail(string_view_t str)
{
	string_view_t tail = str;

	const char * p = find_next_arrow(str);
	if (p == NULL)
		return string_view();

	tail.begin = p+1;
	return tail;
}


string_view_t
pfq_signature_remove_extent(string_view_t str)
{
	string_view_t ret = pfq_signature_simplify(str);

	if (string_view_at(ret, 0) == '(' &&
	    string_view_at(ret, string_view_length(ret)-1) == ')') {
		return ret;
	}

	if (string_view_at(ret, 0) == '[' &&
	    string_view_at(ret, string_view_length(ret)-1) == ']') {
        	ret.begin +=1;
        	ret.end -=1;
        	return pfq_signature_simplify(ret);
	}

	if (pfq_signature_arity(ret) > 0)
		return ret;

	return pfq_signature_simplify(string_view_chr(ret, ' '));
}


string_view_t
pfq_signature_simplify(string_view_t str)
{
	int red = count_outmost_brackets(str);
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

int
pfq_signature_arity(string_view_t str)
{
	int __signature_arity(string_view_t s)
	{
		string_view_t str  = pfq_signature_simplify(s);
		string_view_t head = signature_head(str);
		string_view_t tail = signature_tail(str);

		if (!string_view_empty(tail))
                	return 1 + __signature_arity(tail);

		if (string_view_empty(head))
			return 0;

		return 1;
	}

	return -1 + __signature_arity(str);
}

string_view_t
pfq_signature_bind(string_view_t str, int n)
{
	string_view_t __signature_bind(string_view_t s, int stop)
	{
		string_view_t str  = pfq_signature_simplify(s),
			      tail;

		if (stop == n)
			return str;

		tail = signature_tail(str);

		if (!string_view_empty(tail))
                	return __signature_bind(tail, stop + 1);

		return tail;
	}

	return  pfq_signature_simplify(__signature_bind(str, 0));
}

string_view_t
pfq_signature_arg(string_view_t str, int index)
{
	string_view_t __signature_arg(string_view_t s, int stop)
	{
		string_view_t str  = pfq_signature_simplify(s);

		string_view_t head = signature_head(str);
		string_view_t tail = signature_tail(str);

		if (stop == index)
			return head;

		if (!string_view_empty(tail))
                	return __signature_arg(tail, stop + 1);

		return tail;
	}

	return  pfq_signature_simplify(__signature_arg(str, 0));
}


size_t
pfq_signature_sizeof(string_view_t str)
{
	if (!string_view_compare(str, "Bool"))
		return sizeof(bool);
	if (!string_view_compare(str, "CChar"))
		return sizeof(char);
	if (!string_view_compare(str, "CUChar"))
		return sizeof(unsigned char);
	if (!string_view_compare(str, "CInt"))
		return sizeof(int);
	if (!string_view_compare(str, "CUnt"))
		return sizeof(unsigned int);
	if (!string_view_compare(str, "CShort"))
		return sizeof(short);
	if (!string_view_compare(str, "CUShort"))
		return sizeof(unsigned short);
	if (!string_view_compare(str, "CLong"))
		return sizeof(long);
	if (!string_view_compare(str, "CULong"))
		return sizeof(unsigned long);
	if (!string_view_compare(str, "CLLong"))
		return sizeof(long long);
	if (!string_view_compare(str, "CULLong"))
		return sizeof(unsigned long long);
	if (!string_view_compare(str, "CDouble"))
		return sizeof(double);
	if (!string_view_compare(str, "CSize"))
		return sizeof(size_t);
	if (!string_view_compare(str, "Word8"))
		return sizeof(uint8_t);
	if (!string_view_compare(str, "Word16"))
		return sizeof(uint16_t);
	if (!string_view_compare(str, "Word32"))
		return sizeof(uint32_t);
	if (!string_view_compare(str, "Word64"))
		return sizeof(uint64_t);
	return -1;
}


static inline const char *
skip_white_space(const char *p, const char *end)
{
	while (isspace(*p) && p != end)
		p++;
	return p;
}


#ifdef __KERNEL
static
#endif
bool
compare_argument(string_view_t a, string_view_t b)
{
	string_view_t str_a = pfq_signature_simplify(a);
	string_view_t str_b = pfq_signature_simplify(b);

	const char *ap = str_a.begin;
	const char *bp = str_b.begin;

 	while (ap != str_a.end && bp != str_b.end)
 	{
 		if (*ap != *bp)
 			return false;

		if (isspace(*ap)) {
 			ap = skip_white_space(ap, str_a.end);
 			bp = skip_white_space(bp, str_b.end);
		}
		else
 			ap++, bp++;
 	}

	return ap == str_a.end && bp == str_b.end;
}


bool
pfq_signature_equal(string_view_t sig_a, string_view_t sig_b)
{
	bool __signature_equal(string_view_t a, string_view_t b)
	{
		string_view_t str_a = pfq_signature_simplify(a),
			      str_b = pfq_signature_simplify(b);

		string_view_t head_a = signature_head(str_a),
			      tail_a = signature_tail(str_a),
			      head_b = signature_head(str_b),
			      tail_b = signature_tail(str_b);

		if ((string_view_empty(tail_a) && !string_view_empty(tail_b)) ||
		    (!string_view_empty(tail_a) && string_view_empty(tail_b)))
		    	return false;

		if (!(pfq_signature_arity(head_a) == 0 ?
			       compare_argument(head_a, head_b) : pfq_signature_equal(head_a, head_b)))
			return false;

		if (string_view_empty(tail_a) && string_view_empty(tail_b))
			return true;

 		return __signature_equal(tail_a, tail_b);
	}

	return __signature_equal(sig_a, sig_b);
}

bool
pfq_signature_is_function(string_view_t sig)
{
	return pfq_signature_arity(sig) > 0;
}

