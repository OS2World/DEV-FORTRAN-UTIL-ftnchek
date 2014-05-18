/* exprtype.c -- propagates datatype thru expressions.


Copyright (c) 2001 by Robert K. Moniot.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Acknowledgement: the above permission notice is what is known
as the "MIT License."
*/

/* I. */

/* $Id: exprtype.c,v 1.13 2002/01/06 23:20:25 moniot Rel $

	Routines to propagate datatype through expressions.

	binexpr_type()		Yields result type of binary expression.
	unexpr_type()		Yields result type of unary expression.
	assignment_stmt_type()	Checks assignment statement type.
	func_ref_expr(id,args,result) Forms token for a function invocation.
	primary_id_expr()	Forms token for primary which is an identifier.
	stmt_fun_arg_cmp(t1,t2) Checks agreement between stmt func args.
    int	int_power(x,n)		Computes x**n for value propagation.
        init_typesizes(wdsize)	Sets standard type sizes
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#define EXPRTYPE
#include "symtab.h"
#include "symutils.h"
#include "tokdefs.h"



PROTO(PRIVATE char* sized_typename,( int type, long size ));
PROTO(PRIVATE void report_mismatch,( const Token *term1, const Token *op, const Token *term2 ));
PROTO(PRIVATE void report_type,( const Token *t ));
PROTO(PRIVATE int int_power,( int x, int n ));
PROTO(PRIVATE int eval_intrins,( IntrinsInfo *defn, Token *args ));


	/* shorthand for datatypes.  must match those in symtab.h */
	/* N.B. Also, the fact that type_DEFAULT=0 is assumed in size
	   propagation code. */
#define E 0	/*  Error for invalid type combos  */
#define I 1
#define R 2
#define D 3
#define C 4
#define Z 5
#define L 6
#define S 7
#define H 8
#define NumT (H+1)		/* number of types in tables below */

#define W 10		/*  Warning for nonstandard type combos: W>NumT */

			/* for  + - / * **	ANSI book pp. 6-5,6-6	*/
			    /* Mixed double+complex = double complex with
			       warning, double + double complex is OK */
PRIVATE unsigned char arith_expr_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  I,  R,  D,  C,  Z,  E,  E,  E },	/* I */
{ E,  R,  R,  D,  C,  Z,  E,  E,  E },	/* R */
{ E,  D,  D,  D,W+Z,  Z,  E,  E,  E },	/* D */
{ E,  C,  C,W+Z,  C,  Z,  E,  E,  E },	/* C */
{ E,  Z,  Z,  Z,  Z,  Z,  E,  E,  E },	/* Z */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* S */
{ E,  E,  E,  E,  E,  E,  E,  E,  E }	/* H */
};

			/* for  relops.  Corresponds to arith type table
			   except that nonstandard comparisons of like
			   types have warning, not error. */
PRIVATE unsigned char rel_expr_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  L,  L,  L,  L,  L,  E,  E,W+L },	/* I */
{ E,  L,  L,  L,  L,  L,  E,  E,  E },	/* R */
{ E,  L,  L,  L,W+L,  L,  E,  E,  E },	/* D */
{ E,  L,  L,W+L,  L,  L,  E,  E,  E },	/* C */
{ E,  L,  L,  L,  L,  L,  E,  E,  E },	/* Z */
{ E,  E,  E,  E,  E,  E,W+L,  E,W+L },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  L,  E },	/* S */
{ E,W+L,  E,  E,  E,  E,W+L,  E,W+L }	/* H */
};

			/* Result of assignment:  lvalue = expr.  Here rows
			   correspond to type of lvalue, columns to type
			   of expr */
PRIVATE unsigned char assignment_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  I,  I,  I,  I,  I,  E,  E,W+I },	/* I */
{ E,  R,  R,  R,  R,  R,  E,  E,W+R },	/* R */
{ E,  D,  D,  D,  D,  D,  E,  E,W+D },	/* D */
{ E,  C,  C,  C,  C,  C,  E,  E,W+C },	/* C */
{ E,  Z,  Z,  Z,  Z,  Z,  E,  E,W+Z },	/* Z */
{ E,  E,  E,  E,  E,  E,  L,  E,W+L },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  S,  E },	/* S */
{ E,  E,  E,  E,  E,  E,  E,  E,  E }	/* H not possible for lvalue */
};


#define INTRINS_ARGS (opclass == ',') /* Flag to modify behavior of binexpr_type */

	/* Routine used in printing diagnostics: returns string "type" for
	   unsized objects, "type*size" for explicitly sized things.  Due
	   to use of local static variable, cannot be invoked twice in the
	   same expression.  */
PRIVATE char*
#if HAVE_STDC
sized_typename(int type, long int size)
#else /* K&R style */
sized_typename(type,size)
  int type; long size;
#endif /* HAVE_STDC */
{
  static char strbuf[]="type*000000"; /* template */
  static char *char_unk="char*(?)";
  static char *char_adj="char*(*)";
  if(size == size_DEFAULT) {
    return type_name[type];	/* no explicit size */
  }
  else {
    if(type != S || size > 0) {
      (void)sprintf(strbuf,"%4s*%ld",	/* type*size */
	    type_name[type],
	    size%1000000);
    }
    else {			/* handle special character size codes */
      if(size == size_ADJUSTABLE)
	return char_adj;
      else /*size_UNKNOWN*/
	return char_unk;
    }
  }
  return strbuf;
}


void
init_typesizes(VOID)
		/* Only executes once.  Thus cannot change wordsize
		   after processing starts. */
{
  static int trapdoor=FALSE, ptr_trapdoor=FALSE;
  if(trapdoor) {
    if(given_wordsize != local_wordsize) {
      (void)fprintf(stderr,
	      "\nSorry-Cannot change wordsize after processing starts");
    }
    given_wordsize = local_wordsize;
  }
  else {
    trapdoor = TRUE;
    local_wordsize = given_wordsize;
    if(given_wordsize != 0) {
      if(given_wordsize != BpW) {
	type_size[I] = type_size[R] = type_size[L] = (BYTE)given_wordsize;
	type_size[D] = type_size[C] = (BYTE)(2*given_wordsize);
	type_size[Z] = (BYTE)(4*given_wordsize);
      }
    }
  }

				/* Cray pointer size is set separately */
  if(ptr_trapdoor) {
    if(given_ptrsize != local_ptrsize) {
      (void)fprintf(stderr,
	      "\nSorry-Cannot change pointer size after processing starts");
    }
    given_ptrsize = local_ptrsize;
  }
  else {
    ptr_trapdoor = TRUE;
    local_ptrsize = given_ptrsize;
  }
}


	/* this routine propagates type in binary expressions */

void
#if HAVE_STDC
binexpr_type(Token *term1, Token *op, Token *term2, Token *result)
#else /* K&R style */
binexpr_type(term1,op,term2,result)
	Token *term1, *op, *term2, *result;
#endif /* HAVE_STDC */
{
    int	opclass = op->tclass,
	type1 = datatype_of(term1->TOK_type),
	type2 = datatype_of(term2->TOK_type),
	result_type;
    long
	size1 = term1->size,
	size2 = term2->size,
        result_size;
    int I_logop_I=FALSE;		/* for f90_mixed_type warning */

    if( ! is_computational_type(type1) ) {
		syntax_error(term1->line_num,term1->col_num,
			"numeric or character quantity expected:");
		report_type(term1);
		result_type = E;
    }
    else if( ! is_computational_type(type2) ) {
		syntax_error(term2->line_num,term2->col_num,
			"numeric or character quantity expected:");
		report_type(term2);
		result_type = E;
    }
    else {
	switch(opclass) {
				/* arithmetic operators: use lookup table */
	    case '+':
	    case '-':
	    case '*':
	    case '/':
	    case tok_power:
		result_type = (unsigned)arith_expr_type[type1][type2];
		break;

				/* relational operators: use lookup table */
 	    case tok_relop:
		result_type = (unsigned)rel_expr_type[type1][type2];
		break;

				/*  logical operators: operands should be
				    logical, but allow integers with a
				    warning. */
	    case tok_AND:
	    case tok_OR:
	    case tok_EQV:
	    case tok_NEQV:
		if(type1 == L && type2 == L)
		    result_type = L;
		else if(type1 == I && type2 == I) {
		    result_type = W+I;
		    I_logop_I = TRUE;
		}
		else
		    result_type = E;
		break;

				/*  // operator: operands must be strings */
	    case tok_concat:
		if(type1 == S && type2 == S)
		    result_type = S;
		else
		    result_type = E;
		break;

			/* Intrinsic function argument list: no promotion
			   across type categories.  Accept matching type
			   categories: size match will be checked later. */
	    case ',':
		if( type_category[type1] != type_category[type2] )
		  result_type = E;
		else if(type1 == S)
		  result_type = S;
		else
		  result_type = (unsigned)arith_expr_type[type1][type2];
		break;

	    default:
		oops_message(OOPS_NONFATAL,
			     op->line_num,op->col_num,
			     "operator unknown: type not propagated");
		result_type = type1;
		break;
	}

	if( (type1 != E && type2 != E) ) {
	    if( result_type == E) {
		syntax_error(op->line_num,op->col_num,
			"type mismatch");
		if(INTRINS_ARGS) {
		  msg_tail("between intrinsic function arguments:");
		}
		else {
		  msg_tail("in expression:");
		}
		report_mismatch(term1,op,term2);
	    }
	    else if(result_type >= W) {	/* W result */
				/* F90 warning suppressed for numeric exprs */
	      if(f77_mixed_expr ||
		 (f90_mixed_expr && ((type1>=L || type2>=L) || I_logop_I)) ) {
		nonstandard(op->line_num,op->col_num,f90_mixed_expr,0);
		msg_tail(": incompatible type combination in expression:");
		report_mismatch(term1,op,term2);
	      }
	      result_type -= W;
	    }
				/* Obscure standard rule 6.2.2. We have to look
				   for IN_ASSIGN flag in any of 3 places, since
				   it gets prematurely turned off in
				   fun_or_substr_handle production if one of
				   operands is a substring expression.  */
	    else if( f77_mixed_expr &&
		     opclass == tok_concat &&
		     !is_true(IN_ASSIGN,
			 (term1->TOK_flags|op->TOK_flags|term2->TOK_flags)) &&
		 ((size1==size_ADJUSTABLE && !is_true(CONST_EXPR,term1->TOK_flags))
	       || (size2==size_ADJUSTABLE && !is_true(CONST_EXPR,term2->TOK_flags))) ) {
		nonstandard(op->line_num,op->col_num,0,0);
		msg_tail(": adjustable size cannot be concatenated here");
	    }
	}
    }

				/* Figure out the size of result */
    result_size = size_DEFAULT;
    if(result_type != E &&	/* Error type gets DEFAULT size */
       opclass != tok_relop) {	/* Result of compare gets DEFAULT size */

      if(opclass == tok_concat) {	/* string//string yields sum of lengths */
	if(size1 == size_UNKNOWN || size2 == size_UNKNOWN)
	  result_size = size_UNKNOWN;
	else
	  if(size1 == size_ADJUSTABLE || size2 == size_ADJUSTABLE)
	    result_size = size_ADJUSTABLE;
	  else {
	    result_size = size1 + size2;
	    if(port_long_string && result_size > 255)
	    nonportable(op->line_num,op->col_num,
			"character expression length exceeds 255");
	  }

      }
			/* DEFAULT op DEFAULT always yields DEFAULT. So need
			   to handle only explicitly sized expressions,
			   except intrinsic arglists, where no promotion
			   of plain real to dble or plain complex to dcpx,
			   and check for promotions of real types.
			 */
      else if(INTRINS_ARGS?
	      (type1 != type2 || 
	       (type1 == type2  && is_numeric_type(type1) &&
		(size1 != size_DEFAULT || size2 != size_DEFAULT))) :
	      ((size1 != size_DEFAULT || size2 != size_DEFAULT) ||
	        (trunc_promotion &&
		 is_float_type(type1) && is_float_type(type2))))
     {
				/* Local variables for convenience.
				   N.B. Use tc1/2,ls1/2 for tests,
				   t1/2,s1/2 for assigning result.
				 */
	int t1,t2;	/* sorted types: t1 <= t2. */
	long s1,s2;	/* sizes of t1 and t2. */
	int tc1,tc2;	/* type categories: D->R and Z->C */
	long ls1,ls2;	/* local sizes = declared size else type_size */
	int defsize1,defsize2; /* flags for default size */

				/* Sort so that t1 <= t2 */
	if(type1 <= type2) {
	  t1 = type1; s1 = size1;
	  t2 = type2; s2 = size2;
	}
	else {
	  t1 = type2; s1 = size2;
	  t2 = type1; s2 = size1;
	}
				/* Assign type categories and local sizes */
	tc1 = type_category[t1];
	tc2 = type_category[t2];

	defsize1 = (s1 == size_DEFAULT);
	defsize2 = (s2 == size_DEFAULT);
	ls1 = (defsize1? type_size[t1]: s1);
	ls2 = (defsize2? type_size[t2]: s2);

#ifdef DEBUG_EXPRTYPE
if(debug_latest)
  (void)fprintf(list_fd,"\nt1=%s s1=%d ls1=%d t2=%s s2=%d ls2=%d",
	  type_name[t1],s1,ls1, type_name[t2], s2, ls2);
#endif
	if(tc1 == tc2) {/* same type category */
				/* Intrins args: size promotion illegal */
	  if(INTRINS_ARGS && ls1 != ls2) {
	    syntax_error(op->line_num,op->col_num,
			 "precision mismatch in intrinsic argument list:");
	    report_mismatch(term1,op,term2);
	  }
				/* Give -port warning if e.g. plain I+I*2
				   (variables only) */
	  else if(port_mixed_size || local_wordsize==0) {
	    if(defsize1 != defsize2
	        && !is_true(CONST_EXPR,term1->TOK_flags)
	        && !is_true(CONST_EXPR,term2->TOK_flags))
	    {
	      nonportable(op->line_num,op->col_num,
			  INTRINS_ARGS?"intrinsic argument list":"expr");
	      msg_tail("mixes default and explicit");
	      msg_tail((is_numeric_type(t1)&&is_numeric_type(t2))?
			 "precision":"size");
	      msg_tail("operands:");
	      report_mismatch(term1,op,term2);
	    }
	  }

		/* If same type category, use the larger of the two sizes if
		   both declared.  If only one size declared, use the
		   larger of the declared size and the default size.
		   If result is equal in size to default, use size_DEFAULT.
		*/
	  if(ls1 > ls2) {
	    result_size = s1;
	  }
	  else if(ls2 > ls1) {
	    result_size = s2;
	  }
	  else /*ls1 == ls2*/{
	    if(!defsize1 && !defsize2)
	      result_size = s1;	/* otherwise DEFAULT */
	  }
	}/* end(tc1==tc2) */
	else /* tc1!=tc2 */ {
			/* Differing type categories: only two cases. */

				/* Case 1:  I + R|D|C|Z
				   Result: size of dominant type */
	  if(tc1 == I) {
	    result_size = s2;
	  }
				/* Case 2:  R|D + C|Z
				   Result: larger of C|Z and 2*size of R|D */
	  else {
	    if(ls2 >= 2*ls1)
	      result_size = s2;
	    else
	      result_size = 2*s1; /* 2*size_DEFAULT = 0 is still DEFAULT */
	  }
	}/* end tc1 != tc2 */
				/* change D or Z to default size or else
				   to explicitly sized R or C
				 */
	if(result_type == D || result_type == Z) {
	  if(result_size != size_DEFAULT
	     && result_size != type_size[result_type])
	       result_type = (result_type==D)?R:C;
	     else
	       result_size = size_DEFAULT;
	}

				/* Give -trunc warning if a real or
				   complex type is promoted to double. */
	if(trunc_promotion && !INTRINS_ARGS && is_float_type(t1) ) {
		  /* First clause checks R+R size agreement */
	  if( (type_category[result_type] == R && ls1 != ls2)
		     /* Second clause checks R+C and C+C */
	     || (type_category[result_type] == C &&
		 (type_category[t1] == R? ls2 != 2*ls1 : ls2 != ls1)) ){
	    warning(op->line_num,op->col_num,
		    "promotion may not give desired precision:");
	    report_mismatch(term1,op,term2);
	  }
	}

      }/*end if(non-DEFAULT sizes)*/

    }/*end if(result_size != E)*/

#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
(void)fprintf(list_fd,"\nsize of %s %c",sized_typename(type1,size1),
	ispunct(opclass)?opclass:'~');
(void)fprintf(list_fd," %s = ",sized_typename(type2,size2));
(void)fprintf(list_fd,"%s",sized_typename(result_type,result_size));
}
#endif

    result->TOK_type = type_byte(class_VAR, result_type);
    result->TOK_flags = 0;	/* clear all flags */
    result->size = result_size;


		/* Keep track of constant expressions */
    if( is_true(CONST_EXPR,term1->TOK_flags)
	 && is_true(CONST_EXPR,term2->TOK_flags)
         && !(opclass==tok_power && type2!=I) ) { /* exclude **REAL */
		make_true(CONST_EXPR,result->TOK_flags);
    }

		/* Parameter expressions are like constant exprs
		   except we bend the rules to allow intrinsic functions
		   and **REAL */
    if( is_true(PARAMETER_EXPR,term1->TOK_flags)
	 && is_true(PARAMETER_EXPR,term2->TOK_flags) ) {
		make_true(PARAMETER_EXPR,result->TOK_flags);
    }

			/* Keep track of dimension bound expressions */
    if( is_true(DIM_BOUND_EXPR,term1->TOK_flags)
	&& is_true(DIM_BOUND_EXPR,term2->TOK_flags) ) {
      make_true(DIM_BOUND_EXPR,result->TOK_flags);
    }


    if( is_true(EVALUATED_EXPR,term1->TOK_flags)
	 && is_true(EVALUATED_EXPR,term2->TOK_flags) ) {
		make_true(EVALUATED_EXPR,result->TOK_flags);
    }
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,"\nconst param eval: (%d %d %d) %s (%d %d %d) = (%d %d %d)",
is_true(CONST_EXPR,term1->TOK_flags),
is_true(PARAMETER_EXPR,term1->TOK_flags),
is_true(EVALUATED_EXPR,term1->TOK_flags),

operator->src_text,

is_true(CONST_EXPR,term2->TOK_flags),
is_true(PARAMETER_EXPR,term2->TOK_flags),
is_true(EVALUATED_EXPR,term2->TOK_flags),

is_true(CONST_EXPR,result->TOK_flags),
is_true(PARAMETER_EXPR,result->TOK_flags),
is_true(EVALUATED_EXPR,result->TOK_flags));
#endif

  if(! INTRINS_ARGS) {		/* Remaining steps only applicable to exprs */

		/* Remember if integer division was used */
    if(result_type == type_INTEGER &&
	   (opclass == '/' ||
	    (is_true(INT_QUOTIENT_EXPR,term1->TOK_flags) ||
	     is_true(INT_QUOTIENT_EXPR,term2->TOK_flags))) ) {
		make_true(INT_QUOTIENT_EXPR,result->TOK_flags);
    }
		/* Issue warning if integer expr involving division is
		   later converted to any real type, or if it is used
		   as an exponent. */
    if( is_true(INT_QUOTIENT_EXPR,term1->TOK_flags)
	|| is_true(INT_QUOTIENT_EXPR,term2->TOK_flags) ) {

	int r=result_type;
	if(r == type_LOGICAL)		/* relational tests are equivalent */
	    r = arith_expr_type[type1][type2];		/* to subtraction */

	if(opclass == tok_power && is_true(INT_QUOTIENT_EXPR,term2->TOK_flags) ) {
	  if(trunc_int_div_exponent) {
	    warning(op->line_num,op->col_num,
		    "integer quotient expr");
	    msg_expr_tree(term2);
	    msg_tail("used in exponent");
	  }
	  if( ! is_true(INT_QUOTIENT_EXPR,term1->TOK_flags) )
		make_false(INT_QUOTIENT_EXPR,result->TOK_flags);
	}
	else if( r == type_REAL || r == type_DP || r == type_COMPLEX) {
	  if(trunc_int_div_real) {
	    warning(op->line_num,op->col_num,
		    "integer quotient expr");
	    msg_expr_tree(is_true(INT_QUOTIENT_EXPR,term1->TOK_flags)?
				  term1:term2);
	    msg_tail(" converted to real");
	  }
	}
    }

			/* If either term is an identifier, set use flag */
    if(is_true(ID_EXPR,term1->TOK_flags))
	use_variable(term1);
    if(is_true(ID_EXPR,term2->TOK_flags))
	use_variable(term2);

		/* Propagate the value of integer constant expressions */
    if(is_true(EVALUATED_EXPR,result->TOK_flags)) {
	if(result_type == type_INTEGER) {	/* Only ints propagated */
	  int a = int_expr_value(term1),
	      b = int_expr_value(term2),
	      c;
	  switch(opclass) {
	    case '+': c = a+b; break;
	    case '-': c = a-b; break;
	    case '*': c = a*b; break;
	    case '/': if(b == 0) {
			syntax_error(term2->line_num,term2->col_num,
				"division by zero attempted");
			c = 0;
		      }
		      else {
			c = a/b;
		      }
		      break;
	    case tok_power: c = int_power(a,b); break;
	    case tok_AND: c = a&b; break;
	    case tok_OR: c = a|b; break;
	    case tok_EQV: c = ~(a^b); break;
	    case tok_NEQV: c = a^b; break;
	    default:
	      oops_message(OOPS_NONFATAL,
			   op->line_num,op->col_num,
			   "invalid int expr operator");
			c = 0; break;
	  }

	  make_true(EVALUATED_EXPR,result->TOK_flags);
	  result->value.integer = c;	/* Result goes into token value */

				/* Integer division (including i**neg)
				   that yields 0 is suspicious.  */
	  if(trunc_int_div_zero)
	    if(c==0 && (opclass=='/' || opclass==tok_power)) {
	      warning(op->line_num,op->col_num,
	    		"integer const expr yields result of 0");
	    }
	}
      }
				/* Also nonconstant**neg is 0 unless
				   nonconstant=1 */
      else if(trunc_int_neg_power)
	if(result_type == type_INTEGER && opclass == tok_power
	      && is_true(EVALUATED_EXPR,term2->TOK_flags)
	      && int_expr_value(term2) < 0) {
	  warning(op->line_num,op->col_num,
		  "integer to negative power usually yields 0");
	}
  }/* end if !INTRINS_ARGS */
}/*binexpr_type*/


	/* this routine propagates type in unary expressions */

void
#if HAVE_STDC
unexpr_type(Token *op, Token *term1, Token *result)
#else /* K&R style */
unexpr_type(op,term1,result)
	Token *term1, *op, *result;
#endif /* HAVE_STDC */
{
   int	opclass = op->tclass,
	type1 = datatype_of(term1->TOK_type),
	result_type;

    if( ! is_computational_type(type1) ) {
		syntax_error(term1->line_num,term1->col_num,
			"numeric quantity expected:");
		report_type(term1);
		result_type = E;
    }
    else {
	switch(opclass) {
			/* arith operators: use diagonal of lookup table */
	    case '+':
	    case '-':
		result_type = arith_expr_type[type1][type1];
		break;

				/*  NOT: operand should be
				    logical, but allow integers with a
				    warning. */
	    case tok_NOT:
		if(type1 == L)
		    result_type = L;
		else if(type1 == I)
		    result_type = W+I;
		else
		    result_type = E;
		break;

	    default:
		oops_message(OOPS_NONFATAL,
			     op->line_num,op->col_num,
			     "unary operator type not propagated");
		result_type = type1;
		break;
	}

	if( type1 != E ) {
	    if( result_type == E) {
		syntax_error(op->line_num,op->col_num,
			"expression incompatible with operator:");
		msg_tail(op->src_text);
		msg_tail("used with");
		report_type(term1);
	    }
	    else if(result_type >= W) {
	      if(f77_mixed_expr || f90_mixed_expr) {
		nonstandard(op->line_num,op->col_num,f90_mixed_expr,0);
		msg_tail(": incompatible type used with operator:");
		msg_tail(op->src_text);
		msg_tail("used with");
		report_type(term1);
	      }
	      result_type -= W;
	    }
	}
    }

    result->TOK_type = type_byte(class_VAR, result_type);
    result->TOK_flags = 0;	/* clear all flags */
    result->size = term1->size;	/* result is same size as operand */

		/* Keep track of constant expressions */
    copy_flag(CONST_EXPR,result->TOK_flags,term1->TOK_flags);
    copy_flag(PARAMETER_EXPR,result->TOK_flags,term1->TOK_flags);
    copy_flag(DIM_BOUND_EXPR,result->TOK_flags,term1->TOK_flags);

		/* Remember if integer division was used */
    if(result_type == type_INTEGER)
	    copy_flag(INT_QUOTIENT_EXPR,result->TOK_flags,term1->TOK_flags);

    if(is_true(ID_EXPR,term1->TOK_flags))
	use_variable(term1);

		/* Propagate the value of integer constant expressions */
    if(is_true(EVALUATED_EXPR,term1->TOK_flags)) {
	if(result_type == type_INTEGER) {	/* Only ints propagated */
	  int a = int_expr_value(term1),
	      c;
	  switch(opclass) {
	    case '+': c = a; break;
	    case '-': c = -a; break;
	    case tok_NOT: c = ~a; break;
	    default: oops_message(OOPS_NONFATAL,
			     op->line_num,op->col_num,
			     "invalid int expr operator");
			c = 0; break;
	  }
	  make_true(EVALUATED_EXPR,result->TOK_flags);
	  result->value.integer = c;	/* Result goes into token value */
	}
    }
}

	/* this routine checks type and size match in assignment statements
	   and in parameter assignments */

void
#if HAVE_STDC
assignment_stmt_type(Token *term1, Token *equals, Token *term2)
#else /* K&R style */
assignment_stmt_type(term1,equals,term2)
	Token *term1, *equals, *term2;
#endif /* HAVE_STDC */
{
    int type1 = datatype_of(term1->TOK_type),
	type2 = datatype_of(term2->TOK_type),
	result_type;

    if( ! is_computational_type(type1) ) {
		syntax_error(term1->line_num,term1->col_num,
			"numeric or character quantity expected:");
		report_type(term1);
		result_type = E;
    }
    else if( ! is_computational_type(type2) ) {
		syntax_error(term2->line_num,term2->col_num,
			"numeric or character quantity expected:");
		report_type(term2);
		result_type = E;
    }
    else {
	result_type = (unsigned)assignment_type[type1][type2];


	if( (type1 != E && type2 != E) ) {
	    if( result_type == E) {
		syntax_error(equals->line_num,equals->col_num,
			"type mismatch:");
		report_type(term2);
		msg_tail("assigned to");
		report_type(term1);
	    }
	    else {
	      if(result_type >= W) {		/* W result */
		if(f77_mixed_expr || f90_mixed_expr) {
		  nonstandard(equals->line_num,equals->col_num,f90_mixed_expr,0);
		  msg_tail(": incompatible type combination:");
		  report_type(term2);
		  msg_tail("assigned to");
		  report_type(term1);
		}
		result_type -= W;
	      }

			/* Watch for truncation to lower precision type */
	      if(trunc_precision ||
		 port_mixed_size || local_wordsize==0) {
		long size1 = term1->size;
		long size2 = term2->size;
		int type_trunc=FALSE, /* flags for kind of truncation */
		    size_trunc=FALSE,
		    mixed_size=FALSE,
		    promotion=FALSE,
		    trunc_warn,mixed_warn;

		if(size1 == size_DEFAULT && size2 == size_DEFAULT) {
		  type_trunc = ( is_numeric_type(type1) &&
				 is_numeric_type(type2) &&
				(type1 < type2 ||
					/* C = D truncates precision of D */
				(type1 == C && type2 == D)) );

				/* Watch for promotions also */
		  if(type_category[type2] == R) {
		    if(type_category[type1] == R) /* R|D = R|D */
		      promotion = (type1 > type2);
		    else if(type_category[type1] == C) /* C|Z = R|D */
		      promotion =
			((int)type_size[type1] > 2*(int)type_size[type2]);
		  }
		  else if(type_category[type2] == C) /* any = C|Z */
		    promotion = (type1 > type2);
		}
		else if(type1 == S) { /* character strings */
		  if(size1>0 && size2>0) /* ignore ADJUSTABLE and UNKNOWN */
		    size_trunc = size1 < size2;
		} else {
		  int tc1,tc2;/* type categories: D->R, Z->C, H->I */
		  int ls1,ls2;/* local sizes */

				/* Assign type categories and local sizes */
		  tc1 = type_category[type1];
		  tc2 = type_category[type2];
		  ls1 = size1; if(ls1 == size_DEFAULT)  ls1 = type_size[type1];
		  ls2 = size2; if(ls2 == size_DEFAULT)  ls2 = type_size[type2];

				/* type truncation: any numeric type category
				   to a lower category. */
		  type_trunc = ( /***is_numeric_type(type1) &&
				 is_numeric_type(type2) &&***/
				 tc1 < tc2 );

				/* size truncation: assigned to smaller
				   local size.  For C = R correct test is
				   Csize < 2*Rsize */
		  if(tc1 == C && tc2 == R) {
		    size_trunc = (ls1 < ls2*2);
		    promotion = (ls1 > ls2*2);
		  }
		  else {
				/* Suppress size truncation warning if rhs
				   is a literal constant that is sure to fit.
				   For logicals this is always the case; for
				   integers we use a suitable threshold.
				 */
		    if( (size_trunc = (ls1 < ls2)) &&
			is_true(LIT_CONST,term2->TOK_flags) ){
			switch(tc2) {
			  case L:
			      size_trunc = FALSE;
			      break;
			  case I:
			      if( term2->value.integer <= SMALL_INT_VALUE )
				  size_trunc = FALSE;
			      break;
			}
		    }
		    promotion = ((tc2 == R || tc2 == C) && (ls1 > ls2));
		  }
				/* mixed size: default size assigned to
				   declared size of like type category
				   or vice-versa. -port only, and superseded
				   by truncation warning if any. */
		  mixed_size = (tc1 == tc2) &&
			   (size1==size_DEFAULT ||
			   (size2==size_DEFAULT &&
			    !is_true(CONST_EXPR,term2->TOK_flags)));

		}

			/* Under -trunc, report type truncation or size
			   truncation.  Say "possibly" if -nowordsize.
			   Also report promotions under -trunc.
			   If no truncation warning given and under -port,
			   report mixed assignment */
#ifdef DEBUG_EXPRTYPE
#define TorF(x) ((x)?"":"no")
if(debug_latest) {
(void)fprintf(list_fd,"\nassign %s =",sized_typename(type1,size1));
(void)fprintf(list_fd," %s : ",sized_typename(type2,size2));
(void)fprintf(list_fd,"%s type %s size %s mixed",
	TorF(type_trunc),
	TorF(size_trunc),
	TorF(mixed_size));
}
#endif
		trunc_warn = (trunc_promotion && promotion) ||
			     (trunc_type_demotion && type_trunc) ||
			     (trunc_size_demotion && size_trunc);
		mixed_warn = ((port_mixed_size || local_wordsize==0) &&
				mixed_size);
		if( trunc_warn ) {
		  warning(equals->line_num,equals->col_num,"");
		  report_type(term2);
		  if(trunc_warn && !type_trunc && mixed_size
		       && local_wordsize == 0)
		    msg_tail("possibly");
		  if(promotion)
		    msg_tail("promoted to");
		  else
		    msg_tail("truncated to");
		  report_type(term1);
		  if(promotion)
		    msg_tail(": may not give desired precision");
		}
		else if(mixed_warn) {
		  nonportable(equals->line_num,equals->col_num,
		    "mixed default and explicit");
		  msg_tail((is_numeric_type(type1)&&is_numeric_type(type2))?
			 "precision":"size");
		  msg_tail("items:");
		  report_type(term2);
		  msg_tail("assigned to");
		  report_type(term1);
		}
	      }
	    }/*end else (result_type != E)*/
	}/*end if (type1,type2 != E)*/
    }/*end else (is_computational_type(type2))*/


		/* Issue warning if integer expr involving division is
		   later converted to any real type. */
    if(trunc_int_div_real)
      if( is_true(INT_QUOTIENT_EXPR,term2->TOK_flags) ) {

	int r=result_type;

	if( r == type_REAL || r == type_DP || r == type_COMPLEX) {
	    warning(equals->line_num,equals->col_num,
			"integer quotient expr");
	    msg_expr_tree(term2);
	    msg_tail(" converted to real");
	}
      }


    if(is_true(ID_EXPR,term2->TOK_flags))
	use_variable(term2);

    use_lvalue(term1);
}

void
check_initializer_type(Token *assignee_list, Token *equals, Token *expr_list)
{
    Token *t;
    if( expr_list->next_token == (Token*)NULL ) {
	t = expr_list;		/* simple token, not a list */
    }
    else {
				/* token lists are built in reverse, so
				   restore to order in source statement */
	t = expr_list->next_token = reverse_tokenlist(expr_list->next_token);
    }

				/* Go thru list, checking match.
				   At this time, assignee can only be a single
				   variable
				 */
    while( t!=NULL ) {
	assignment_stmt_type(assignee_list,t,t);
	t = t->next_token;
    }
}

	/* Make an expression-token for a function invocation */

void
#if HAVE_STDC
func_ref_expr(Token *id, Token *args, Token *result)
#else /* K&R style */
func_ref_expr(id,args,result)
	Token *id,*args,*result;
#endif /* HAVE_STDC */
{
	Lsymtab *symt;
	IntrinsInfo *defn;
	int rettype, retsize;

	symt = hashtab[id->value.integer].loc_symtab;

	if( symt->intrinsic ) {
	    defn = symt->info.intrins_info;
			/* Intrinsic functions: type stored in info field */
	    rettype = defn->result_type;
	    retsize = size_DEFAULT;
	    if( defn->intrins_flags & I_QUAD ) { /* Quad intrinsic */
				/* These are either R*16 or X*32 */
	      retsize = ((rettype==type_QUAD)? size_QUAD: size_CQUAD);
	    }
		/* Generic Intrinsic functions: use propagated arg type */
	    if(rettype == type_GENERIC) {
		if(args->next_token == NULL) {
		  rettype = type_UNDECL;
		  retsize = size_DEFAULT;
		}
		else {
#ifdef OLDSTUFF
		  rettype = args->next_token->TOK_type;
		  retsize = args->next_token->size;
#else
		  rettype = args->TOK_type;
		  retsize = args->size;
#endif
		}
			/* special case: REAL(integer|[d]real) ->  real */
		if((defn->intrins_flags&I_SP_R) &&
		   (rettype != type_COMPLEX) && (rettype != type_DCOMPLEX)) {
			rettype = type_REAL;
			retsize = size_DEFAULT;
		}

			/* special cases: */
			/*       ABS([d]complex) -> [d]real */
			/*      IMAG([d]complex) -> [d]real */
			/*      REAL([d]complex) -> [d]real */
		if(rettype == type_COMPLEX && (defn->intrins_flags&I_C_TO_R)) {
			rettype = type_REAL;
			retsize = retsize/2;
		}
		if(rettype == type_DCOMPLEX &&(defn->intrins_flags&I_C_TO_R)) {
			rettype = type_DP;
			retsize = size_DEFAULT;
		}
	      }
	      else {		/* non-generic */

				/* special case: CHAR(code): size=1 */
		if(defn->intrins_flags&I_CHAR) {
		  retsize = 1;
		}
	      }
	}
	else {			/* non-intrinsic */
	    rettype = get_type(symt);
	    retsize = get_size(symt,rettype);
	}
		/* referencing function makes it no longer a class_SUBPROGRAM
		   but an expression. */
#ifndef TOK_type
	result->tclass = id->tclass;
#endif
	result->tsubclass = 0;
	result->TOK_type = type_byte(class_VAR,rettype);
#ifndef TOK_flags
	result->TOK_flags = 0;	/* clear all flags */
#endif
	result->size = retsize;
	result->next_token = (Token *)NULL;

#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
(void)fprintf(list_fd,"\n%sFunction %s() = %s",
symt->intrinsic?"Intrinsic ":"",
symt->name,sized_typename(rettype,retsize));
}
#endif

		/* If intrinsic and all arguments are PARAMETER_EXPRs,
		   then result is one too. */
	if( symt->intrinsic ) {
				/* Evaluate intrinsic if result is
				   integer, the args are const (except for
				   LEN), and a handler is defined.
				 */
	    if(rettype == type_INTEGER &&
	           (defn->intrins_flags&I_EVALUATED) )
	    {
		     result->value.integer = eval_intrins(defn,args);
				/* Evaluation routines can affect the flags */
		     copy_flag(EVALUATED_EXPR,result->TOK_flags,args->TOK_flags);
	    }
	    copy_flag(PARAMETER_EXPR,result->TOK_flags,args->TOK_flags);
#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
(void)fprintf(list_fd,"\n%s(...) ",defn->name);
if(is_true(EVALUATED_EXPR,args->TOK_flags))
  (void)fprintf(list_fd,"=%d",result->value.integer);
else
  (void)fprintf(list_fd,"not evaluated");
(void)fprintf(list_fd,": const param eval=(%d %d %d)",
is_true(CONST_EXPR,result->TOK_flags),
is_true(PARAMETER_EXPR,result->TOK_flags),
is_true(EVALUATED_EXPR,result->TOK_flags));
}
#endif
	}
}/*func_ref_expr*/



		/* Make an expression-token for primary consisting of
		   a symbolic name */

void
#if HAVE_STDC
primary_id_expr(Token *id, Token *primary)
#else /* K&R style */
primary_id_expr(id,primary)
	Token *id,*primary;
#endif /* HAVE_STDC */
{
	Lsymtab *symt;
	int id_type;
	symt = hashtab[id->value.integer].loc_symtab;
	id_type=get_type(symt);
#ifndef TOK_type
	primary->tclass = id->tclass;
#endif
	primary->tsubclass = 0;
	primary->TOK_type = type_byte(storage_class_of(symt->type),id_type);
#ifndef TOK_flags
	primary->TOK_flags = 0;
#endif
	primary->size =get_size(symt,id_type);
	primary->left_token = (Token *) NULL;

	make_true(ID_EXPR,primary->TOK_flags);

	if( storage_class_of(symt->type) == class_VAR) {
		if(symt->parameter) {
		    make_true(CONST_EXPR,primary->TOK_flags);
		    make_true(PARAMETER_EXPR,primary->TOK_flags);
		    make_true(EVALUATED_EXPR,primary->TOK_flags);
		}
		else {
		    make_true(LVALUE_EXPR,primary->TOK_flags);
		}
		if(symt->active_do_var) {
		    make_true(DO_VARIABLE,primary->TOK_flags);
		}
		if(symt->array_var)
		    make_true(ARRAY_ID_EXPR,primary->TOK_flags);
		if(symt->set_flag || symt->common_var || symt->parameter
				  || symt->argument)
		    make_true(SET_FLAG,primary->TOK_flags);
		if(symt->assigned_flag)
		    make_true(ASSIGNED_FLAG,primary->TOK_flags);
		if(symt->used_before_set)
		    make_true(USED_BEFORE_SET,primary->TOK_flags);
	}
	else if(storage_class_of(symt->type) == class_STMT_FUNCTION) {
		make_true(STMT_FUNCTION_EXPR,primary->TOK_flags);
	}

#ifdef DEBUG_PARSER
if(debug_parser){
	(void)fprintf(list_fd,"\nprimary %s: TOK_type=0x%x TOK_flags=0x%x",
		symt->name,primary->TOK_type,primary->TOK_flags);
      }
#endif
}/*primary_id_expr*/

int
#if HAVE_STDC
intrins_arg_cmp(IntrinsInfo *defn, Token *t)
                       		/* Definition */
              			/* Argument */
#else /* K&R style */
intrins_arg_cmp(defn,t)
     IntrinsInfo *defn;		/* Definition */
     Token *t;			/* Argument */
#endif /* HAVE_STDC */
{
  int defn_types=defn->arg_type;
  int a_type = datatype_of(t->TOK_type);
  int type_OK;
				/* Check for argument type mismatch.
				 */
	    type_OK = ( (1<<a_type) & defn_types );
	    if(! type_OK) {
	      int ct;/* compatible type */
				/* Accept compatible types if
				   sizes agree, e.g. DSQRT(REAL*8).
				   The macros check the two cases and
				   set ct to the compatible type.
				 */
#define EXCEPTION1 (a_type==type_REAL && ((1<<(ct=type_DP))&defn_types))
#define EXCEPTION2 (a_type==type_COMPLEX&&((1<<(ct=type_DCOMPLEX))&defn_types))

	      if(!( (EXCEPTION1||EXCEPTION2) && t->size==type_size[ct] )){
		syntax_error(t->line_num,t->col_num,
			"illegal argument data type for intrinsic function");
		msg_tail(defn->name);
		msg_tail(":");
		report_type(t);
	      }
	      else {
		if(port_mixed_size || local_wordsize==0) {
		  nonportable(t->line_num,t->col_num,
	      "argument precision may not be correct for intrinsic function");
		  msg_tail(defn->name);
		  msg_tail(":");
		  report_type(t);
		}
		type_OK = TRUE; /* Acceptable after all */
	      }
	    }/* end if(! type_OK) */
			/* Quad intrinsics need a special check
			   to verify that real or cplx arg size is right.
			 */
	    else if(defn->intrins_flags & I_QARG) {
	      if(t->size != ((a_type==type_REAL)? size_QUAD: size_CQUAD)) {
		syntax_error(t->line_num,t->col_num,
			"illegal argument data type for intrinsic function");
		msg_tail(defn->name);
		msg_tail(":");
		report_type(t);
	      }
	    }

  return type_OK;
}/*intrins_arg_cmp*/


				/* Check agreement between statement function
				   dummy (t1) and actual (t2) args.  At this
				   time, checks only class, type and size,
				   not arrayness.  */
void
#if HAVE_STDC
stmt_fun_arg_cmp(const Lsymtab *symt, const Token *d_arg, const Token *a_arg)
#else /* K&R style */
stmt_fun_arg_cmp(symt,d_arg,a_arg)
     Lsymtab *symt;
     Token *d_arg,*a_arg;
#endif /* HAVE_STDC */
{
  int d_class = class_VAR,
      a_class = storage_class_of(a_arg->TOK_type),
      d_type = datatype_of(d_arg->TOK_type),
      a_type = datatype_of(a_arg->TOK_type),
      d_size = d_arg->size,
      a_size = a_arg->size,
      d_defsize = (d_size == size_DEFAULT),
      a_defsize = (a_size == size_DEFAULT);
  int d_cmptype= (d_type==type_HOLLERITH && a_type!=type_STRING)?
				a_type:type_category[d_type];
  int a_cmptype= (a_type==type_HOLLERITH && d_type!=type_STRING)?
				d_type:type_category[a_type];

  if(!(port_mixed_size || local_wordsize==0)) {
    if(d_defsize)
      d_size = type_size[d_type];
    if(a_defsize)
      a_size = type_size[a_type];
  }

  if(d_size < 0 || a_size < 0) { /* char size_ADJUSTABLE or UNKNOWN */
    d_size = a_size = size_DEFAULT;	/* suppress warnings on size */
    d_defsize = a_defsize = TRUE; /* these are not used at present */
  }

  if(d_class != a_class || d_cmptype != a_cmptype ||
     (d_type == type_STRING? d_size > a_size: d_size != a_size) ) {
		syntax_error(a_arg->line_num,a_arg->col_num,
		  "argument mismatch in stmt function");
		msg_tail(symt->name); /* Give the stmt func name */
		msg_tail(": dummy");
		report_type(d_arg); /* Dummy arg type */
		msg_tail("vs actual");
		report_type(a_arg);
  }
}/*stmt_fun_arg_cmp*/


				/* Routine to document the types of
				   two terms and their operator */
PRIVATE void
#if HAVE_STDC
report_mismatch(const Token *term1, const Token *op, const Token *term2)
#else /* K&R style */
report_mismatch(term1,op,term2)
     Token *term1,*op,*term2;
#endif /* HAVE_STDC */
{
  report_type(term1);
  msg_tail(op->src_text);
  report_type(term2);
}
				/* Routine to document the type
				   of a token, with its name if it
				   has one. */
PRIVATE void
#if HAVE_STDC
report_type(const Token *t)
#else /* K&R style */
report_type(t)
     Token *t;
#endif /* HAVE_STDC */
{
  msg_tail(sized_typename((int)datatype_of(t->TOK_type),t->size));
  if(is_true(ID_EXPR,t->TOK_flags))
    msg_tail(hashtab[t->value.integer].name);
  else if(is_true(LIT_CONST,t->TOK_flags)) {
    msg_tail("const");
    msg_expr_tree(t);
  }
  else {
    msg_tail("expr");
    msg_expr_tree(t);
  }
}


int
#if HAVE_STDC
substring_size(Token *id, Token *limits)
#else /* K&R style */
substring_size(id,limits)
     Token *id,*limits;
#endif /* HAVE_STDC */
{
	int id_type,id_len;
	int startindex,endindex,substr_len;
#ifdef DEBUG_EXPRTREES
	Lsymtab *symt = hashtab[id->value.integer].loc_symtab;
#endif
/***	id_type=get_type(symt); **/
	id_type = datatype_of(id->TOK_type);

	substr_len=size_UNKNOWN;

	if(id_type != type_STRING) {
	  syntax_error(id->line_num,id->col_num,
		       "string variable expected");
	}
	else {
	  id_len = id->size;
#ifdef DEBUG_EXPRTREES
	  if(debug_latest) {
	    fprintf(list_fd,"\nSubstring %s :: ",symt->name);
	    print_expr_list(limits);
	  }
#endif
		/* fortran.y stores (startindex:endindex) in
		   TOK_start, Tok_end */
	  startindex = limits->TOK_start;
	  endindex = limits->TOK_end;
	  if(startindex != size_UNKNOWN && endindex != size_UNKNOWN) {
		/* Check limits unless endindex=0 */
	    if( startindex > endindex && endindex > 0 ) {
	      syntax_error(limits->line_num,limits->col_num,
		      "invalid substring limits");
	    }
	    else {
	      if(endindex == 0)	/* 0 means it was (startindex: ) */
		endindex=id_len;
	      substr_len = endindex-startindex+1;
	      if(id_len > 0 && substr_len > id_len)
		syntax_error(limits->line_num,limits->col_num,
		      "substring size exceeds string size");
	    }
	  }
	}
	return substr_len;
}

	/* Integer power: uses recursion x**n = (x**(n/2))**2 */
PRIVATE int
#if HAVE_STDC
int_power(int x, int n)
#else /* K&R style */
int_power(x,n)
	int x,n;
#endif /* HAVE_STDC */
{
	int temp;
			/* Order of tests puts commonest cases first */
	if(n > 1) {
		temp = int_power(x,n>>1);
		temp *= temp;
		if(n&1) return temp*x;	/* Odd n */
		else	return temp;	/* Even n */
	}
	else if(n == 1) return x;
	else if(n < 0) return 1/int_power(x,-n);	/* Usually 0 */
	else return 1;
}

				/* Intrinsic function handlers */

PROTO(PRIVATE int ii_abs,( Token *args ));
PROTO(PRIVATE int ii_dim,( Token *args ));
PROTO(PRIVATE int ii_ichar,( Token *args ));
PROTO(PRIVATE int ii_index,( Token *args ));
PROTO(PRIVATE int ii_len,( Token *args ));
PROTO(PRIVATE int ii_max,( Token *args ));
PROTO(PRIVATE int ii_min,( Token *args ));
PROTO(PRIVATE int ii_mod,( Token *args ));
PROTO(PRIVATE int ii_sign,( Token *args ));


/* Array of pointers to functions for evaluating integer-valued intrinsic
   functions.  The order matches definitions of I_ABS thru I_INDEX in
   symtab.h */

PROTO(PRIVATE int (*ii_fun[]),( Token *args ))
={
  NULL,
  ii_abs,
  ii_sign,
  ii_dim,
  ii_mod,
  ii_max,
  ii_min,
  ii_ichar,
  ii_len,
  ii_index,
};

PRIVATE int
#if HAVE_STDC
eval_intrins(IntrinsInfo *defn, Token *args)
#else /* K&R style */
eval_intrins(defn,args)
     IntrinsInfo *defn;
     Token *args;
#endif /* HAVE_STDC */
{
    intrins_flags_t fun_num;
    fun_num = (defn->intrins_flags & I_EVALUATED);

				/* Args must be evaluated, except for LEN */
    if( (is_true(EVALUATED_EXPR,args->TOK_flags) || fun_num==I_LEN) &&
       fun_num > 0 && fun_num < (sizeof(ii_fun)/sizeof(ii_fun[0])) ) {
      return (*ii_fun[fun_num])(args);
    }
    else {
#ifdef DEBUG_EXPRTYPE
      if(debug_latest)
	(void)fprintf(list_fd,"\nIntrinsic %s not handled",defn->name);
      make_false(EVALUATED_EXPR,args->TOK_flags);
#endif
      return 0;
    }
}


PRIVATE int
#if HAVE_STDC
ii_abs(Token *args)
#else /* K&R style */
ii_abs(args)
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t;
  int val, result=0;
  t = args->next_token;
  if(t->TOK_type != type_INTEGER) {/* wrong arg type: message given elsewhere */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val = int_expr_value(t);
    result = (val >= 0? val: -val);
  }
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_sign(Token *args)			/* SIGN(value,sign) */
#else /* K&R style */
ii_sign(args)			/* SIGN(value,sign) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  int val1,val2, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->TOK_type != type_INTEGER
     || t2->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val1 = int_expr_value(t1);
    if(val1 < 0) val1 = -val1;
    val2 = int_expr_value(t2);
    result = (val2 >= 0? val1: -val1);
  }
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_dim(Token *args)			/* DIM(int,int) */
#else /* K&R style */
ii_dim(args)			/* DIM(int,int) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  int val, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->TOK_type != type_INTEGER
     || t2->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val = int_expr_value(t1)-int_expr_value(t2);
    result = (val >= 0? val: 0);
  }
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_mod(Token *args)			/* MOD(int,int) */
#else /* K&R style */
ii_mod(args)			/* MOD(int,int) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  int val1,val2,quotient, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->TOK_type != type_INTEGER
     || t2->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    val1 = int_expr_value(t1);
    val2 = int_expr_value(t2);
    if((val1 < 0) == (val2 < 0)) {
      quotient = val1/val2;	/* Both positive or both negative*/
    }
    else {
      quotient = -(-val1/val2);	/* Unlike signs */
    }
    result = val1 - quotient*val2;
  }
  return result;
}


PRIVATE int
#if HAVE_STDC
ii_max(Token *args)			/* MAX(int,int,...) */
#else /* K&R style */
ii_max(args)			/* MAX(int,int,...) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args;
  int val,result=0,n=0;
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,"\nEvaluating MAX(");
#endif
  while( (t=t->next_token) != NULL) {

      if(t->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
	make_false(EVALUATED_EXPR,args->TOK_flags);
	break;
      }
      else {
	val = int_expr_value(t);
	if(n++ == 0 || val > result)
	  result = val;
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,"%d ",val);
#endif
      }
  }
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
(void)fprintf(list_fd,") = %d",result);
#endif
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_min(Token *args)			/* MIN(int,int,...) */
#else /* K&R style */
ii_min(args)			/* MIN(int,int,...) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args;
  int val,result=0,n=0;
  while( (t=t->next_token) != NULL) {
      if(t->TOK_type != type_INTEGER) {/* wrong arg type: message given elswr */
	make_false(EVALUATED_EXPR,args->TOK_flags);
	break;
      }
      else {
	val = int_expr_value(t);
	if(n++ == 0 || val < result)
	  result = val;
      }
  }
  return result;
}

PRIVATE int
#if HAVE_STDC
ii_ichar(Token *args)		/* ICHAR(string) */
#else /* K&R style */
ii_ichar(args)		/* ICHAR(string) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args->next_token;

  if(t->TOK_type != type_STRING || !is_true(LIT_CONST,t->TOK_flags)) {
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    return t->value.string[0];	/* Processor collating sequence is used */
  }
  return 0;
}

PRIVATE int
#if HAVE_STDC
ii_len(Token *args)		/* LEN(string) */
#else /* K&R style */
ii_len(args)		/* LEN(string) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t=args->next_token;
  int val,result=0;

		/* Set the PARAMETER_EXPR flag since LEN of string does
		   not require contents to be known */
  if( t->TOK_type == type_STRING && (val = t->size) > 0 ) {
    make_true(PARAMETER_EXPR,args->TOK_flags);
    make_true(EVALUATED_EXPR,args->TOK_flags);
    result = val;
  }
  else {			/* nonstring or adjustable or unknown */
    make_false(PARAMETER_EXPR,args->TOK_flags);
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }

  return result;
}

PRIVATE int
#if HAVE_STDC
ii_index(Token *args)		/* INDEX(str1,str2) */
#else /* K&R style */
ii_index(args)		/* INDEX(str1,str2) */
     Token *args;
#endif /* HAVE_STDC */
{
  Token *t1,*t2;
  t1=args->next_token;
  t2=t1->next_token;

  if(t2 == NULL || t1->TOK_type != type_STRING
     || t2->TOK_type != type_STRING
     || !is_true(LIT_CONST,t1->TOK_flags) || !is_true(LIT_CONST,t2->TOK_flags)) {
    make_false(EVALUATED_EXPR,args->TOK_flags);
  }
  else {
    int i;
    char *s1=t1->value.string;
    char *s2=t2->value.string;
    int n1=strlen(s1), n2=strlen(s2);

    for(i=1; n1 > 0 && n1 >= n2; i++,s1++,n1--) {
      if(strncmp(s1,s2,n2) == 0)
	return i;
    }
  }
  return 0;
}




				/* Undefine special macros */
#undef E
#undef I
#undef R
#undef D
#undef C
#undef L
#undef S
#undef H
#undef W
