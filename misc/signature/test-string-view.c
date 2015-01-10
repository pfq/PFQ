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

#include "pf_q-string-view.h"

#include <assert.h>

int main()
{
	char buffer0[64];
	char buffer1[64];
	char buffer2[64];
	char buffer3[64];

	string_view_t s0 = make_string_view("");
	string_view_t s1 = make_string_view(" ");
	string_view_t s2 = make_string_view("(CInt )");
	string_view_t s3 = make_string_view("  (CInt )  ");

	string_view_sprintf(buffer0, string_view_trim(s0));
	string_view_sprintf(buffer1, string_view_trim(s1));
	string_view_sprintf(buffer2, string_view_trim(s2));
	string_view_sprintf(buffer3, string_view_trim(s3));

	printf("'%s'\n", buffer0);
	printf("'%s'\n", buffer1);
	printf("'%s'\n", buffer2);
	printf("'%s'\n", buffer3);

	printf("'" SVIEW_FMT "'\n", SVIEW_ARG(string_view_trim(s0)));
	printf("'" SVIEW_FMT "'\n", SVIEW_ARG(string_view_trim(s1)));
	printf("'" SVIEW_FMT "'\n", SVIEW_ARG(string_view_trim(s2)));
	printf("'" SVIEW_FMT "'\n", SVIEW_ARG(string_view_trim(s3)));


	assert( string_view_compare(string_view(), "")   == 0);
	assert( string_view_compare(make_string_view(""), "") == 0);

	assert( string_view_compare(string_view(), "1")   != 0);
	assert( string_view_compare(make_string_view(""), "1") != 0);

	assert( string_view_compare(make_string_view("test"), "") != 0);
	assert( string_view_compare(make_string_view("test"), "1") != 0);
	assert( string_view_compare(make_string_view("test"), "12") != 0);
	assert( string_view_compare(make_string_view("test"), "123") != 0);
	assert( string_view_compare(make_string_view("test"), "1234") != 0);
	assert( string_view_compare(make_string_view("test"), "12345") != 0);

	assert( string_view_compare(make_string_view("test"), "") != 0);
	assert( string_view_compare(make_string_view("test"), "t") != 0);
	assert( string_view_compare(make_string_view("test"), "te") != 0);
	assert( string_view_compare(make_string_view("test"), "tes") != 0);
	assert( string_view_compare(make_string_view("test"), "test") == 0);
	assert( string_view_compare(make_string_view("test"), "test!") != 0);

}
