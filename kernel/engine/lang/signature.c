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

#ifndef __KERNEL__
#include <kcompat.h>
#endif

#include <engine/lang/signature.h>
#include <engine/lang/types.h>


int count_outmost_brackets(string_view_t str);
const char * find_next_arrow(string_view_t str);
bool compare_argument(string_view_t a, string_view_t b);

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
pfq_lang_signature_remove_extent(string_view_t str)
{
	string_view_t ret = pfq_lang_signature_simplify(str);

	if (string_view_at(ret, 0) == '(' &&
	    string_view_at(ret, string_view_length(ret)-1) == ')') {
		return ret;
	}

	if (string_view_at(ret, 0) == '[' &&
	    string_view_at(ret, string_view_length(ret)-1) == ']') {
		ret.begin +=1;
		ret.end -=1;
		return pfq_lang_signature_simplify(ret);
	}

	if (pfq_lang_signature_arity(ret) > 0)
		return ret;

	return pfq_lang_signature_simplify(string_view_chr(ret, ' '));
}


string_view_t
pfq_lang_signature_simplify(string_view_t str)
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

static
int __signature_arity(string_view_t s)
{
	string_view_t str  = pfq_lang_signature_simplify(s);
	string_view_t head = signature_head(str);
	string_view_t tail = signature_tail(str);

	if (!string_view_empty(tail))
		return 1 + __signature_arity(tail);

	if (string_view_empty(head))
		return 0;

	return 1;
}

int
pfq_lang_signature_arity(string_view_t str)
{
	return -1 + __signature_arity(str);
}

static
string_view_t __signature_bind(string_view_t s, int stop, int n)
{
	string_view_t str  = pfq_lang_signature_simplify(s),
		      tail;

	if (stop == n)
		return str;

	tail = signature_tail(str);

	if (!string_view_empty(tail))
		return __signature_bind(tail, stop + 1, n);

	return tail;
}

string_view_t
pfq_lang_signature_bind(string_view_t str, int n)
{
	return  pfq_lang_signature_simplify(__signature_bind(str, 0, n));
}


static string_view_t
__signature_arg(string_view_t s, int stop, int index)
{
	string_view_t str  = pfq_lang_signature_simplify(s);
	string_view_t head = signature_head(str);
	string_view_t tail = signature_tail(str);

	if (stop == index)
		return head;

	if (!string_view_empty(tail))
		return __signature_arg(tail, stop + 1, index);

	return tail;
}

string_view_t
pfq_lang_signature_arg(string_view_t str, int index)
{
	return  pfq_lang_signature_simplify(__signature_arg(str, 0, index));
}


static struct
{
	const char *symb;
	size_t size;

} sizeof_table[] =
{
	{.symb = "Bool",    .size = sizeof(char)},
	{.symb = "CChar",   .size = sizeof(char)},
	{.symb = "CUChar",  .size = sizeof(unsigned char)},
	{.symb = "CInt",    .size = sizeof(int)},
	{.symb = "CUnt",    .size = sizeof(unsigned int)},
	{.symb = "CShort",  .size = sizeof(short)},
	{.symb = "CUShort", .size = sizeof(unsigned short)},
	{.symb = "CLong",   .size = sizeof(long)},
	{.symb = "CULong",  .size = sizeof(unsigned long)},
	{.symb = "CLLong",  .size = sizeof(long long)},
	{.symb = "CULLong", .size = sizeof(unsigned long long)},
	{.symb = "CDouble", .size = sizeof(double)},
	{.symb = "CSize",   .size = sizeof(size_t)},
	{.symb = "Word8",   .size = sizeof(uint8_t)},
	{.symb = "Word16",  .size = sizeof(uint16_t)},
	{.symb = "Word32",  .size = sizeof(uint32_t)},
	{.symb = "Word64",  .size = sizeof(uint64_t)},
	{.symb = "CIDR",    .size = sizeof(struct CIDR)},
	{.symb = "String",  .size = 0},
	{.symb = "Action",  .size = 0},
	{.symb = "SkBuff",  .size = 0}
};


ptrdiff_t
pfq_lang_signature_sizeof(string_view_t str)
{
	size_t n;
	for(n = 0; n < sizeof(sizeof_table)/sizeof(sizeof_table[0]); n++)
	{
		if (!string_view_compare(str, sizeof_table[n].symb))
			return sizeof_table[n].size;
	}
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
	string_view_t str_a = pfq_lang_signature_simplify(a);
	string_view_t str_b = pfq_lang_signature_simplify(b);

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
		else {
			ap++, bp++;
		}
	}

	return ap == str_a.end && bp == str_b.end;
}


bool
pfq_lang_signature_equal(string_view_t a, string_view_t b)
{
	string_view_t str_a = pfq_lang_signature_simplify(a),
		      str_b = pfq_lang_signature_simplify(b);

	string_view_t head_a = signature_head(str_a),
		      tail_a = signature_tail(str_a),
		      head_b = signature_head(str_b),
		      tail_b = signature_tail(str_b);

	if ((string_view_empty(tail_a) && !string_view_empty(tail_b)) ||
	    (!string_view_empty(tail_a) && string_view_empty(tail_b)))
		return false;

	if (!(pfq_lang_signature_arity(head_a) == 0 ?
		       compare_argument(head_a, head_b) : pfq_lang_signature_equal(head_a, head_b)))
		return false;

	if (string_view_empty(tail_a) && string_view_empty(tail_b))
		return true;

	return pfq_lang_signature_equal(tail_a, tail_b);
}


bool
pfq_lang_signature_is_function(string_view_t sig)
{
	return pfq_lang_signature_arity(sig) > 0;
}


bool
pfq_lang_signature_type_check(string_view_t type)
{
	for(; !string_view_empty(type);)
	{
		if (pfq_lang_signature_sizeof(type) != -1 || islower(string_view_at(type, 0)))
			return true;

		type = pfq_lang_signature_remove_extent(type);
	}

	return false;
}


bool
pfq_lang_signature_check(string_view_t sig)
{
	int n, size = pfq_lang_signature_arity(sig);
	bool ret = true;

	for(n = 0; n <= size && ret; n++)
	{
		string_view_t arg = pfq_lang_signature_arg(sig, n);
		ret = pfq_lang_signature_arity(sig) > 0 ? pfq_lang_signature_check(arg) : pfq_lang_signature_type_check(arg);
	}

	return ret;
}

