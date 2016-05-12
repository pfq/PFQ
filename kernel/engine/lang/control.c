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

#include <engine/lang/control.h>
#include <engine/lang/module.h>
#include <engine/lang/headers.h>


static ActionSkBuff
inv(arguments_t args, SkBuff skb)
{
	function_t expr = GET_ARG(function_t, args);
	SkBuff nskb = EVAL_FUNCTION(expr, skb).skb;

	if (!nskb || is_drop(PFQ_CB(nskb)->monad->fanout))
		return Copy(nskb);

	return Drop(nskb);
}


static ActionSkBuff
par(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f1, skb);
}


static ActionSkBuff
par3(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f2, skb);
}


static ActionSkBuff
par4(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f3, skb);
}


static ActionSkBuff
par5(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f4, skb);
}

static ActionSkBuff
par6(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f5, skb);
}


static ActionSkBuff
par7(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	function_t f6 = GET_ARG_6(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f5, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f6, skb);
}


static ActionSkBuff
par8(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	function_t f6 = GET_ARG_6(function_t, args);
	function_t f7 = GET_ARG_7(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f5, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f6, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f7, skb);
}


struct pfq_lang_function_descr control_functions[] = {

        { "conditional", "(SkBuff -> Bool) -> (SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff ",  conditional  },
        { "when",        "(SkBuff -> Bool) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff",	when	},
        { "unless",      "(SkBuff -> Bool) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff",	unless	},

        { "shift",       "(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff",  shift   },
        { "src",	 "(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff",  src_ctx },
        { "dst",	 "(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff",  dst_ctx },

        { "inv",	"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", inv },
        { "par",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par },

	{ "par3",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par3 },
	{ "par4",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par4 },
	{ "par5",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par5 },
	{ "par6",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par6 },
	{ "par7",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par7 },
	{ "par8",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par8 },
        { NULL }};


