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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <kcompat.h>
#include <lang/signature.h>

extern int compare_argument(string_view_t sig1, string_view_t sig2);
extern int count_outmost_brackets(string_view_t sig);
extern const char * find_next_arrow(string_view_t str);

int main()
{
	{
		string_view_t f0 =  string_view();
		string_view_t f1 =  make_string_view("");
		string_view_t f2 =  make_string_view("CInt");
		string_view_t f3 =  make_string_view("  CInt   ");
		string_view_t f4 =  make_string_view("CInt   ");
		string_view_t f5 =  make_string_view("   CInt   ");
		string_view_t f6 =  make_string_view("Action SkBuff");
		string_view_t f7 =  make_string_view("   Action   SkBuff");
		string_view_t f8 =  make_string_view("   Action   SkBuff    ");
		string_view_t f9 =  make_string_view("Action   SkBuff    ");

		assert(compare_argument(f0, f0) == true);
		assert(compare_argument(f1, f0) == true);
		assert(compare_argument(f0, f1) == true);

		assert(compare_argument(f2, f2) == true);
		assert(compare_argument(f2, f3) == true);
		assert(compare_argument(f2, f4) == true);
		assert(compare_argument(f2, f5) == true);

		assert(compare_argument(f3, f3) == true);
		assert(compare_argument(f3, f4) == true);
		assert(compare_argument(f3, f5) == true);

		assert(compare_argument(f4, f4) == true);
		assert(compare_argument(f4, f5) == true);

		assert(compare_argument(f5, f5) == true);

		assert(compare_argument(f6, f6) == true);
		assert(compare_argument(f6, f7) == true);
		assert(compare_argument(f6, f8) == true);
		assert(compare_argument(f6, f9) == true);

		assert(compare_argument(f7, f7) == true);
		assert(compare_argument(f7, f8) == true);
		assert(compare_argument(f7, f9) == true);

		assert(compare_argument(f8, f8) == true);
		assert(compare_argument(f8, f9) == true);

		assert(compare_argument(f9, f9) == true);

		assert(compare_argument(f0, f2) == false);
		assert(compare_argument(f0, f3) == false);
		assert(compare_argument(f0, f4) == false);
		assert(compare_argument(f0, f5) == false);
		assert(compare_argument(f0, f6) == false);
		assert(compare_argument(f0, f7) == false);
		assert(compare_argument(f0, f8) == false);
		assert(compare_argument(f0, f9) == false);

	}

	{
		string_view_t f0 =  string_view();
		string_view_t f1 =  make_string_view("");
		string_view_t f2 =  make_string_view("CInt");
		string_view_t f3 =  make_string_view("  CInt   ");
		string_view_t f4 =  make_string_view("CInt   ");
		string_view_t f5 =  make_string_view("   CInt   ");
		string_view_t f6 =  make_string_view("Action SkBuff");
		string_view_t f7 =  make_string_view("   Action   SkBuff");
		string_view_t f8 =  make_string_view("   Action   SkBuff    ");
		string_view_t f9 =  make_string_view("Action   SkBuff    ");

		string_view_t f10 =  string_view();
		string_view_t f11 =  make_string_view("");
		string_view_t f12 =  make_string_view("CInt");
		string_view_t f13 =  make_string_view("  CInt  -> CInt  ");
		string_view_t f14 =  make_string_view("CInt -> (CInt)  ");
		string_view_t f15 =  make_string_view("   CInt   ");
		string_view_t f16 =  make_string_view("Action SkBuff -> ( (CInt ) )");
		string_view_t f17 =  make_string_view("   Action   SkBuff");
		string_view_t f18 =  make_string_view("   Action   SkBuff    ");
		string_view_t f19 =  make_string_view("Action   SkBuff    ");

		assert(count_outmost_brackets(f0) == 0);
		assert(count_outmost_brackets(f1) == 0);
		assert(count_outmost_brackets(f2) == 0);
		assert(count_outmost_brackets(f3) == 0);
		assert(count_outmost_brackets(f4) == 0);
		assert(count_outmost_brackets(f5) == 0);
		assert(count_outmost_brackets(f6) == 0);
		assert(count_outmost_brackets(f7) == 0);
		assert(count_outmost_brackets(f8) == 0);
		assert(count_outmost_brackets(f9) == 0);

		assert(count_outmost_brackets(f10) == 0);
		assert(count_outmost_brackets(f11) == 0);
		assert(count_outmost_brackets(f12) == 0);
		assert(count_outmost_brackets(f13) == 0);
		assert(count_outmost_brackets(f14) == 0);
		assert(count_outmost_brackets(f15) == 0);
		assert(count_outmost_brackets(f16) == 0);
		assert(count_outmost_brackets(f17) == 0);
		assert(count_outmost_brackets(f18) == 0);
		assert(count_outmost_brackets(f19) == 0);

	}

	{
		string_view_t g1 =  make_string_view("()");
		string_view_t g2 =  make_string_view("(CInt)");
		string_view_t g3 =  make_string_view("(  CInt   )");
		string_view_t g4 =  make_string_view("(CInt  )");
		string_view_t g5 =  make_string_view("(   CInt )  ");
		string_view_t g6 =  make_string_view("(Action SkBuff)");
		string_view_t g7 =  make_string_view("(  Action   SkBuff)");
		string_view_t g8 =  make_string_view("(   Action   SkBuff    )");
		string_view_t g9 =  make_string_view("(Action   SkBuff    )");

		assert(count_outmost_brackets(g1) == 1);
		assert(count_outmost_brackets(g2) == 1);
		assert(count_outmost_brackets(g3) == 1);
		assert(count_outmost_brackets(g4) == 1);
		assert(count_outmost_brackets(g5) == 1);
		assert(count_outmost_brackets(g6) == 1);
		assert(count_outmost_brackets(g7) == 1);
		assert(count_outmost_brackets(g8) == 1);
		assert(count_outmost_brackets(g9) == 1);
	}

	{
		string_view_t g1 =  make_string_view("(CInt) -> Bool");
		string_view_t g2 =  make_string_view("(  CInt   )-> Char");
		string_view_t g3 =  make_string_view("(CInt  ) ->Char");
		string_view_t g4 =  make_string_view("(   CInt )  ->(Char)");
		string_view_t g5 =  make_string_view("(Action SkBuff) ->   ( (Char))");
		string_view_t g6 =  make_string_view("(  Action   SkBuff)-> Char");
		string_view_t g7 =  make_string_view("(   Action   SkBuff    ) ->(Char)");
		string_view_t g8 =  make_string_view("(Action   SkBuff    )    -> ((  Char ))");

		assert(count_outmost_brackets(g1) == 0);
		assert(count_outmost_brackets(g2) == 0);
		assert(count_outmost_brackets(g3) == 0);
		assert(count_outmost_brackets(g4) == 0);
		assert(count_outmost_brackets(g5) == 0);
		assert(count_outmost_brackets(g6) == 0);
		assert(count_outmost_brackets(g7) == 0);
		assert(count_outmost_brackets(g8) == 0);
	}


	{
		string_view_t g1 =  make_string_view("((CInt) -> Bool)");
		string_view_t g2 =  make_string_view("((  CInt   )-> Char)");
		string_view_t g3 =  make_string_view("((CInt  ) ->Char  )");
		string_view_t g4 =  make_string_view("((   CInt )  ->(Char))");
		string_view_t g5 =  make_string_view("((Action SkBuff) ->   ( (Char)))");
		string_view_t g6 =  make_string_view("((  Action   SkBuff)-> Char)");
		string_view_t g7 =  make_string_view("((   Action   SkBuff    ) ->(Char))");
		string_view_t g8 =  make_string_view("((Action   SkBuff    )    -> ((  Char )))");

		assert(count_outmost_brackets(g1) == 1);
		assert(count_outmost_brackets(g2) == 1);
		assert(count_outmost_brackets(g3) == 1);
		assert(count_outmost_brackets(g4) == 1);
		assert(count_outmost_brackets(g5) == 1);
		assert(count_outmost_brackets(g6) == 1);
		assert(count_outmost_brackets(g7) == 1);
		assert(count_outmost_brackets(g8) == 1);
	}

	{
		string_view_t g1 =  make_string_view("(((CInt) -> Bool))");
		string_view_t g2 =  make_string_view("( ((  CInt   )-> Char) )");
		string_view_t g3 =  make_string_view("(  ((CInt  ) ->Char  ))");
		string_view_t g4 =  make_string_view("(( (   CInt )  ->(Char)) )");
		string_view_t g5 =  make_string_view("( ((Action SkBuff) ->   ( (Char))))");
		string_view_t g6 =  make_string_view("(((  Action   SkBuff)-> Char))");
		string_view_t g7 =  make_string_view("((  (   Action   SkBuff    ) ->(Char)) )");
		string_view_t g8 =  make_string_view("( ((Action   SkBuff    )    -> ((  Char ) )) )");

		assert(count_outmost_brackets(g1) == 2);
		assert(count_outmost_brackets(g2) == 2);
		assert(count_outmost_brackets(g3) == 2);
		assert(count_outmost_brackets(g4) == 2);
		assert(count_outmost_brackets(g5) == 2);
		assert(count_outmost_brackets(g6) == 2);
		assert(count_outmost_brackets(g7) == 2);
		assert(count_outmost_brackets(g8) == 2);
	}

	{
		string_view_t f0 =  string_view();
		string_view_t f1 =  make_string_view("");
		string_view_t f2 =  make_string_view("CInt");
		string_view_t f3 =  make_string_view("  CInt   ");
		string_view_t f4 =  make_string_view("CInt   ");
		string_view_t f5 =  make_string_view("   CInt   ");
		string_view_t f6 =  make_string_view("Action SkBuff");
		string_view_t f7 =  make_string_view("   Action   SkBuff");
		string_view_t f8 =  make_string_view("   Action   SkBuff    ");
		string_view_t f9 =  make_string_view("Action   SkBuff    ");
		string_view_t f10 =  make_string_view("(CInt) -> Bool");
		string_view_t f11 =  make_string_view("(  CInt   )-> Char");
		string_view_t f12 =  make_string_view("(CInt  ) ->Char");
		string_view_t f13 =  make_string_view("(   CInt )  ->(Char)");
		string_view_t f14 =  make_string_view("(Action SkBuff) ->   ( (Char))");
		string_view_t f15 =  make_string_view("(  Action   SkBuff)-> Char");
		string_view_t f16 =  make_string_view("(   Action   SkBuff    ) ->(Char)");
		string_view_t f17 =  make_string_view("(Action   SkBuff    )    -> ((  Char ))");


		assert(find_next_arrow(f0) == NULL);
		assert(find_next_arrow(f1) == NULL);
		assert(find_next_arrow(f2) == NULL);
		assert(find_next_arrow(f3) == NULL);
		assert(find_next_arrow(f4) == NULL);
		assert(find_next_arrow(f5) == NULL);
		assert(find_next_arrow(f6) == NULL);
		assert(find_next_arrow(f7) == NULL);
		assert(find_next_arrow(f8) == NULL);
		assert(find_next_arrow(f9) == NULL);
		assert(find_next_arrow(f10) != NULL);
		assert(find_next_arrow(f11) != NULL);
		assert(find_next_arrow(f12) != NULL);
		assert(find_next_arrow(f13) != NULL);
		assert(find_next_arrow(f14) != NULL);
		assert(find_next_arrow(f15) != NULL);
		assert(find_next_arrow(f16) != NULL);
		assert(find_next_arrow(f17) != NULL);
	}

	printf("All test passed.\n");
	return 0;
}
