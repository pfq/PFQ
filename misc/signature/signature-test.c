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

#include "pf_q-signature.h"

int main()
{
	string_view_t f0 =  make_string_view("");
	string_view_t f1 =  make_string_view("  CInt");
	string_view_t f2 =  make_string_view("   CInt - Error");
	string_view_t f3 =  make_string_view("  CInt -> Bool   ");
	string_view_t f4 =  make_string_view("    CInt -> ( CInt-> CShort ) -> SkBuff");

	string_view_t f5  = make_string_view( "()");
	string_view_t f6  = make_string_view( "(CInt)");
	string_view_t f7  = make_string_view( "(CInt - Error)");
	string_view_t f8  = make_string_view( "(CInt -> Bool)   ");
	string_view_t f9  = make_string_view( "(CInt -> ( CInt-> CShort ) -> SkBuff)    ");
	string_view_t f10 = make_string_view( "(Int -> (CInt-> CShort) ) -> SkBuff  ");
	string_view_t f11 = make_string_view( "  ((CInt -> ( CInt-> CShort )) -> CInt -> SkBuff)");
	string_view_t f12 = make_string_view( "(  ((CInt -> ( CInt-> CShort )) -> CInt -> SkBuff) )");
	string_view_t f13 = make_string_view("(    CInt -> ( CInt-> CShort ) -> SkBuff -> Action SkBuff )");
	string_view_t f14 = make_string_view("(    Action SkBuff )");

  	string_view_t f15 = make_string_view("CInt -> (String) -> ((Maybe   SkBuff )) -> (Action SkBuff)  ");

  	string_view_t f16 = make_string_view("CInt -> (Maybe   SkBuff -> (Action SkBuff )  )  ");


	assert( pfq_signature_is_function(f0) == false );
	assert( pfq_signature_is_function(f1) == false );
	assert( pfq_signature_is_function(f2) == false );
	assert( pfq_signature_is_function(f3) == true  );
	assert( pfq_signature_is_function(f4) == true  );
	assert( pfq_signature_is_function(f5) == false );
	assert( pfq_signature_is_function(f6) == false );
	assert( pfq_signature_is_function(f7) == false );
	assert( pfq_signature_is_function(f8) == true  );
	assert( pfq_signature_is_function(f9) == true  );
	assert( pfq_signature_is_function(f10) == true );
	assert( pfq_signature_is_function(f11) == true );
	assert( pfq_signature_is_function(f12) == true );
	assert( pfq_signature_is_function(f13) == true );
	assert( pfq_signature_is_function(f14) == false );
	assert( pfq_signature_is_function(f15) == true );
	assert( pfq_signature_is_function(f16) == true );

	assert( pfq_signature_arity(f0) == -1 );
	assert( pfq_signature_arity(f1) == 0  );
	assert( pfq_signature_arity(f2) == 0 );
	assert( pfq_signature_arity(f3) == 1  );
	assert( pfq_signature_arity(f4) == 2  );
	assert( pfq_signature_arity(f5) == -1  );
	assert( pfq_signature_arity(f6) == 0  );
	assert( pfq_signature_arity(f7) == 0  );
	assert( pfq_signature_arity(f8) == 1  );
	assert( pfq_signature_arity(f9) == 2  );
	assert( pfq_signature_arity(f10) == 1  );
	assert( pfq_signature_arity(f11) == 2  );
	assert( pfq_signature_arity(f12) == 2  );
	assert( pfq_signature_arity(f13) == 3  );
	assert( pfq_signature_arity(f14) == 0  );
	assert( pfq_signature_arity(f15) == 3  );
	assert( pfq_signature_arity(f16) == 2  );


	assert( count_outmost_brackets(f0)  == 0 );
	assert( count_outmost_brackets(f1)  == 0 );
	assert( count_outmost_brackets(f2)  == 0 );
	assert( count_outmost_brackets(f3)  == 0 );
	assert( count_outmost_brackets(f4)  == 0 );
	assert( count_outmost_brackets(f5)  == 1 );
	assert( count_outmost_brackets(f6)  == 1 );
	assert( count_outmost_brackets(f7)  == 1 );
	assert( count_outmost_brackets(f8)  == 1 );
	assert( count_outmost_brackets(f9)  == 1 );
	assert( count_outmost_brackets(f10) == 0 );
	assert( count_outmost_brackets(f11) == 1 );
	assert( count_outmost_brackets(f12) == 2 );
	assert( count_outmost_brackets(f13) == 1 );
	assert( count_outmost_brackets(f14) == 1 );
	assert( count_outmost_brackets(f15) == 0 );
	assert( count_outmost_brackets(f15) == 0 );
	assert( count_outmost_brackets(f16) == 0 );

	string_view_t c0  = pfq_signature_simplify(f0);
	string_view_t c1  = pfq_signature_simplify(f1);
	string_view_t c2  = pfq_signature_simplify(f2);
	string_view_t c3  = pfq_signature_simplify(f3);
	string_view_t c4  = pfq_signature_simplify(f4);
	string_view_t c5  = pfq_signature_simplify(f5);
	string_view_t c6  = pfq_signature_simplify(f6);
	string_view_t c7  = pfq_signature_simplify(f7);
	string_view_t c8  = pfq_signature_simplify(f8);
	string_view_t c9  = pfq_signature_simplify(f9);
	string_view_t c10 = pfq_signature_simplify(f10);
	string_view_t c11 = pfq_signature_simplify(f11);
	string_view_t c12 = pfq_signature_simplify(f12);
	string_view_t c13 = pfq_signature_simplify(f13);
	string_view_t c14 = pfq_signature_simplify(f14);
	string_view_t c15 = pfq_signature_simplify(f15);
	string_view_t c16 = pfq_signature_simplify(f16);

	printf("f0: "); string_view_put(c0);    putchar('\n');
	printf("f1: "); string_view_put(c1);    putchar('\n');
	printf("f2: "); string_view_put(c2);    putchar('\n');
	printf("f3: "); string_view_put(c3);    putchar('\n');
	printf("f4: "); string_view_put(c4);    putchar('\n');
	printf("f5: "); string_view_put(c5);    putchar('\n');
	printf("f6: "); string_view_put(c6);    putchar('\n');
	printf("f7: "); string_view_put(c7);    putchar('\n');
	printf("f8: "); string_view_put(c8);    putchar('\n');
	printf("f9: "); string_view_put(c9);    putchar('\n');
	printf("f10: "); string_view_put(c10);  putchar('\n');
	printf("f11: "); string_view_put(c11);  putchar('\n');
	printf("f12: "); string_view_put(c12);  putchar('\n');
	printf("f13: "); string_view_put(c13);  putchar('\n');
	printf("f14: "); string_view_put(c14);  putchar('\n');
	printf("f15: "); string_view_put(c15);  putchar('\n');
	printf("f16: "); string_view_put(c16);  putchar('\n');

	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("CInt")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("(CInt)")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("  (CInt)")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("  (CInt)   ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("(CInt)   ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("(  CInt)")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("(  CInt   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("(CInt   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("   (CInt   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("   (CInt   )    ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("(CInt   )    ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("  (  CInt   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("  (  CInt   )   ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt"), make_string_view("(  CInt   )   ")) == true );

	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("CInt -> SkBuff")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("(CInt -> SkBuff)")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("  (CInt -> SkBuff)")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("  (CInt -> SkBuff)   ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("(CInt -> SkBuff)   ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("(  CInt -> SkBuff)")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("(  CInt -> SkBuff   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("(CInt -> SkBuff   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("   (CInt -> SkBuff   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("   (CInt -> SkBuff   )    ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("(CInt -> SkBuff   )    ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("  (  CInt -> SkBuff   )")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("  (  CInt -> SkBuff   )   ")) == true );
	assert(pfq_signature_equal(make_string_view("CInt->SkBuff"), make_string_view("(( CInt -> SkBuff   )   )")) == true );

	assert(pfq_signature_equal(make_string_view("Maybe CInt"), make_string_view("(Maybe  CInt   )   ")) == true );
	assert(pfq_signature_equal(make_string_view("Maybe CInt"), make_string_view("(MaybeCInt )   ")) == false );
	assert(pfq_signature_equal(make_string_view("Maybe CInt"), make_string_view("(Maybe -> CInt )   ")) == false );

	assert(pfq_signature_equal(make_string_view("CInt->String -> SkBuff -> Action SkBuff"), make_string_view("(( CInt -> (String) -> SkBuff -> (Action SkBuff)  )   )")) == true );

	assert(pfq_signature_equal(make_string_view("CInt->(Int -> String) -> SkBuff -> Action SkBuff"), make_string_view("(( CInt -> (Int-> String) -> (SkBuff -> (Action SkBuff))  )   )")) == true );

	printf("--- f15:\n");

	string_view_t b0 = pfq_signature_bind(f15, 0);
	string_view_t b1 = pfq_signature_bind(f15, 1);
	string_view_t b2 = pfq_signature_bind(f15, 2);
	string_view_t b3 = pfq_signature_bind(f15, 3);
	string_view_t b4 = pfq_signature_bind(f15, 4);
	string_view_t b5 = pfq_signature_bind(f15, 5);
	string_view_t b6 = pfq_signature_bind(f15, 42);

	printf("b0: "); string_view_put(b0);  putchar('\n');
	printf("b1: "); string_view_put(b1);  putchar('\n');
	printf("b2: "); string_view_put(b2);  putchar('\n');
	printf("b3: "); string_view_put(b3);  putchar('\n');
	printf("b4: "); string_view_put(b4);  putchar('\n');
	printf("b5: "); string_view_put(b5);  putchar('\n');
	printf("b6: "); string_view_put(b6);  putchar('\n');

	string_view_t a0 = pfq_signature_arg(f15, 0);
	string_view_t a1 = pfq_signature_arg(f15, 1);
	string_view_t a2 = pfq_signature_arg(f15, 2);
	string_view_t a3 = pfq_signature_arg(f15, 3);

	printf("arg0: "); string_view_put(a0); putchar('\n');
	printf("arg1: "); string_view_put(a1); putchar('\n');
	printf("arg2: "); string_view_put(a2); putchar('\n');
	printf("arg3: "); string_view_put(a3); putchar('\n');

	printf("---: f16\n");

	string_view_t x0 = pfq_signature_bind(f16, 0);
	string_view_t x1 = pfq_signature_bind(f16, 1);
	string_view_t x2 = pfq_signature_bind(f16, 2);
	string_view_t x3 = pfq_signature_bind(f16, 3);
	string_view_t x4 = pfq_signature_bind(f16, 4);

	string_view_t d0 = pfq_signature_arg(f16, 0);
	string_view_t d1 = pfq_signature_arg(f16, 1);
	string_view_t d2 = pfq_signature_arg(f16, 2);
	string_view_t d3 = pfq_signature_arg(f16, 3);

	printf("arg0: "); string_view_put(d0); putchar('\n');
	printf("arg1: "); string_view_put(d1); putchar('\n');
	printf("arg2: "); string_view_put(d2); putchar('\n');
	printf("arg3: "); string_view_put(d3); putchar('\n');

	printf("All test passed.\n");
	return 0;
}
