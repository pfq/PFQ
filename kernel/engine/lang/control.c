/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/lang/control.h>
#include <engine/lang/module.h>
#include <engine/lang/headers.h>


static ActionQbuff
inv(arguments_t args, struct qbuff * buff)
{
	function_t expr = GET_ARG(function_t, args);
	struct qbuff * nbuff = EVAL_FUNCTION(expr, buff).qbuff;

	if (!nbuff || is_drop(nbuff->monad->fanout))
		return Copy(nbuff);

	return Drop(nbuff);
}


static ActionQbuff
par(arguments_t args, struct qbuff * buff)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	fanout_t fout = buff->monad->fanout;
        ActionQbuff a;

	a = EVAL_FUNCTION(f0, buff);
	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	return EVAL_FUNCTION(f1, buff);
}


static ActionQbuff
par3(arguments_t args, struct qbuff * buff)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	fanout_t fout = buff->monad->fanout;
        ActionQbuff a;

	a = EVAL_FUNCTION(f0, buff);
	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	return EVAL_FUNCTION(f2, buff);
}


static ActionQbuff
par4(arguments_t args, struct qbuff * buff)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	fanout_t fout = buff->monad->fanout;
        ActionQbuff a;

	a = EVAL_FUNCTION(f0, buff);
	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	return EVAL_FUNCTION(f3, buff);
}


static ActionQbuff
par5(arguments_t args, struct qbuff * buff)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	fanout_t fout = buff->monad->fanout;
        ActionQbuff a;

	a = EVAL_FUNCTION(f0, buff);
	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	return EVAL_FUNCTION(f4, buff);
}

static ActionQbuff
par6(arguments_t args, struct qbuff * buff)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	fanout_t fout = buff->monad->fanout;
        ActionQbuff a;

	a = EVAL_FUNCTION(f0, buff);
	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	return EVAL_FUNCTION(f5, buff);
}


static ActionQbuff
par7(arguments_t args, struct qbuff * buff)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	function_t f6 = GET_ARG_6(function_t, args);
	fanout_t fout = buff->monad->fanout;
        ActionQbuff a;

	a = EVAL_FUNCTION(f0, buff);
	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f5, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	return EVAL_FUNCTION(f6, buff);
}


static ActionQbuff
par8(arguments_t args, struct qbuff * buff)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	function_t f6 = GET_ARG_6(function_t, args);
	function_t f7 = GET_ARG_7(function_t, args);
	fanout_t fout = buff->monad->fanout;
        ActionQbuff a;

	a = EVAL_FUNCTION(f0, buff);
	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f5, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	a = EVAL_FUNCTION(f6, buff);

	if (a.qbuff && !is_drop(a.qbuff->monad->fanout))
		return a;

	buff->monad->fanout = fout;
	return EVAL_FUNCTION(f7, buff);
}


struct pfq_lang_function_descr control_functions[] = {

        { "conditional", "(Qbuff -> Bool) -> (Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff ",  conditional  },
        { "when",        "(Qbuff -> Bool) -> (Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff",	when	},
        { "unless",      "(Qbuff -> Bool) -> (Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff",	unless	},

        { "shift",       "(Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff",  shift   },
        { "src",	 "(Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff",  src_ctx },
        { "dst",	 "(Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff",  dst_ctx },

        { "inv",	"(Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", inv },
        { "par",	"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", par },

	{ "par3",	"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", par3 },
	{ "par4",	"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", par4 },
	{ "par5",	"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", par5 },
	{ "par6",	"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", par6 },
	{ "par7",	"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", par7 },
	{ "par8",	"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> "
			"(Qbuff -> Action Qbuff) -> (Qbuff -> Action Qbuff) -> Qbuff -> Action Qbuff", par8 },
        { NULL }};


