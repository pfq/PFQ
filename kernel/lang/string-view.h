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

#ifndef PFQ_LANG_STRING_VIEW_H
#define PFQ_LANG_STRING_VIEW_H

#ifdef __KERNEL__

#include <pragma/diagnostic_push>
#include <linux/string.h>
#include <linux/slab.h>
#include <linux/ctype.h>
#include <pragma/diagnostic_pop>

#else
#include <kcompat.h>
#endif

#define SVIEW_FMT	"%.*s"
#define SVIEW_ARG(x)	(int)string_view_length(x), x.begin


typedef struct
{
	const char *begin;
	const char *end;

} string_view_t;


static inline string_view_t
string_view(void)
{
	string_view_t s = { NULL, NULL };
	return s;
}


static inline string_view_t
make_string_view(const char *str)
{
	string_view_t s = { str, str + strlen(str) };
	return s;
}


static inline bool
string_view_empty(string_view_t str)
{
	return (str.end - str.begin) == 0;
}


static inline size_t
string_view_length(string_view_t str)
{
	return str.end - str.begin;
}


static inline char
string_view_at(string_view_t str, size_t at)
{
	if (at < string_view_length(str))
		return *(str.begin+at);
	return '\0';
}


static inline string_view_t
string_view_chr(string_view_t str, int c)
{
	string_view_t ret;
	const char * p;

	for(p = str.begin; p != str.end && *p != c; ++p)
	{ }

	ret.begin = p;
	ret.end = str.end;
	return ret;
}


static inline string_view_t
string_view_trim(string_view_t str)
{
	if (!str.begin || !str.end)
		return str;
	while ( isspace(*str.begin) &&
		str.begin != str.end )
		str.begin++;

	while ( str.end != str.begin &&
		isspace(*(str.end-1)) )
		str.end--;

	return str;
}


static inline char *
string_view_to_string(string_view_t str)
{
	size_t len = str.end - str.begin;

#ifdef __KERNEL__
	char * ret = kmalloc(len + 1, GFP_KERNEL);
#else
	char * ret = malloc(len + 1);
#endif
	if (ret) {
		strncpy(ret, str.begin, len);
		ret [len] = '\0';
	}
	return ret;
}


static inline int
string_view_compare(string_view_t str, const char *rhs)
{
	const char * l = str.begin;
	const char * r = rhs;

	for(; l != str.end && *l == *r; ++l, ++r)
	{ }

	if (l == str.end) {
		return *r == '\0' ? 0 : -1;
	}
	else {
		return ((*(unsigned char *)l < *(unsigned char *)r) ? -1 : +1);
	}
}


static inline int
string_view_sprintf(char *buffer, string_view_t str)
{
	return sprintf(buffer, SVIEW_FMT, SVIEW_ARG(str));
}


static inline int
string_view_snprintf(char *buffer, size_t s, string_view_t str)
{
	return snprintf(buffer, s, SVIEW_FMT, SVIEW_ARG(str));
}


#ifndef __KERNEL__
static inline int
string_view_puts(string_view_t str)
{
	return printf(SVIEW_FMT, SVIEW_ARG(str));
}

static inline int
string_view_fputs(FILE *stream, string_view_t str)
{
	return fprintf(stream, SVIEW_FMT, SVIEW_ARG(str));
}
#endif


#endif /* PFQ_LANG_STRING_VIEW_H */
