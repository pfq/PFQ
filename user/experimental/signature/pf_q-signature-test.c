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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "pf_q-signature.h"

int main()
{
	char buffer0[64];
	char buffer1[64];
	char buffer2[64];
	char buffer3[64];
	char buffer4[64];
	char buffer5[64];
	char buffer6[64];
	char buffer7[64];
	char buffer8[64];
	char buffer9[64];
	char buffer10[64];
	char buffer11[64];
	char buffer12[64];
	char buffer13[64];

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

	assert( pfq_signature_arity(f0) == -1 );
	assert( pfq_signature_arity(f1) == 0  );
	assert( pfq_signature_arity(f2) == -1 );
	assert( pfq_signature_arity(f3) == 1  );
	assert( pfq_signature_arity(f4) == 2  );
	assert( pfq_signature_arity(f5) == -1  );
	assert( pfq_signature_arity(f6) == 0  );
	assert( pfq_signature_arity(f7) == -1  );
	assert( pfq_signature_arity(f8) == 1  );
	assert( pfq_signature_arity(f9) == 2  );
	assert( pfq_signature_arity(f10) == 1  );
	assert( pfq_signature_arity(f11) == 2  );
	assert( pfq_signature_arity(f12) == 2  );
	assert( pfq_signature_arity(f13) == 3  );

	assert( pfq_signature_redundant_brackets(f0)  == 0 );
	assert( pfq_signature_redundant_brackets(f1)  == 0 );
	assert( pfq_signature_redundant_brackets(f2)  == 0 );
	assert( pfq_signature_redundant_brackets(f3)  == 0 );
	assert( pfq_signature_redundant_brackets(f4)  == 0 );
	assert( pfq_signature_redundant_brackets(f5)  == 1 );
	assert( pfq_signature_redundant_brackets(f6)  == 1 );
	assert( pfq_signature_redundant_brackets(f7)  == 1 );
	assert( pfq_signature_redundant_brackets(f8)  == 1 );
	assert( pfq_signature_redundant_brackets(f9)  == 1 );
	assert( pfq_signature_redundant_brackets(f10) == 0 );
	assert( pfq_signature_redundant_brackets(f11) == 1 );
	assert( pfq_signature_redundant_brackets(f12) == 2 );
	assert( pfq_signature_redundant_brackets(f13) == 1 );

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

	string_view_sprintf(buffer0,  c0);
	string_view_sprintf(buffer1,  c1);
	string_view_sprintf(buffer2,  c2);
	string_view_sprintf(buffer3,  c3);
	string_view_sprintf(buffer4,  c4);
	string_view_sprintf(buffer5,  c5);
	string_view_sprintf(buffer6,  c6);
	string_view_sprintf(buffer7,  c7);
	string_view_sprintf(buffer8,  c8);
	string_view_sprintf(buffer9,  c9);
	string_view_sprintf(buffer10, c10);
	string_view_sprintf(buffer11, c11);
	string_view_sprintf(buffer12, c12);
	string_view_sprintf(buffer13, c13);

	printf("simplified f0 '%s'\n",  buffer0);
	printf("simplified f1 '%s'\n",  buffer1);
	printf("simplified f2 '%s'\n",  buffer2);
	printf("simplified f3 '%s'\n",  buffer3);
	printf("simplified f4 '%s'\n",  buffer4);
	printf("simplified f5 '%s'\n",  buffer5);
	printf("simplified f6 '%s'\n",  buffer6);
	printf("simplified f7 '%s'\n",  buffer7);
	printf("simplified f8 '%s'\n",  buffer8);
	printf("simplified f9 '%s'\n",  buffer9);
	printf("simplified f10 '%s'\n", buffer10);
	printf("simplified f11 '%s'\n", buffer11);
	printf("simplified f12 '%s'\n", buffer12);
	printf("simplified f13 '%s'\n", buffer13);

	printf("---\n");

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


	string_view_t b0 = pfq_signature_bind(f12, 0);
	string_view_t b1 = pfq_signature_bind(f12, 1);
	string_view_t b2 = pfq_signature_bind(f12, 2);
	string_view_t b3 = pfq_signature_bind(f12, 3);
	string_view_t b4 = pfq_signature_bind(f12, 4);

	string_view_sprintf(buffer0,  b0);
	string_view_sprintf(buffer1,  b1);
	string_view_sprintf(buffer2,  b2);
	string_view_sprintf(buffer3,  b3);
	string_view_sprintf(buffer4,  b4);

	printf("bind 0 => '%s'\n",  buffer0);
	printf("bind 1 => '%s'\n",  buffer1);
	printf("bind 2 => '%s'\n",  buffer2);
	printf("bind 3 => '%s'\n",  buffer3);
	printf("bind 4 => '%s'\n",  buffer4);

	string_view_t a0 = pfq_signature_arg(f12, 0);
	string_view_t a1 = pfq_signature_arg(f12, 1);
	string_view_t a2 = pfq_signature_arg(f12, 2);
	string_view_t a3 = pfq_signature_arg(f12, 3);

	printf("---\n");

	string_view_sprintf(buffer0,  a0);
	string_view_sprintf(buffer1,  a1);
	string_view_sprintf(buffer2,  a2);
	string_view_sprintf(buffer3,  a3);

	printf("arg0 => '%s'\n",  buffer0);
	printf("arg1 => '%s'\n",  buffer1);
	printf("arg2 => '%s'\n",  buffer2);
	printf("arg3 => '%s'\n",  buffer3);

	printf("All test passed.\n");
	return 0;
}
