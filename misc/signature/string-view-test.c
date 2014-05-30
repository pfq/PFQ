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

#include "string-view.h"

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

}
