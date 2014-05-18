/* $Id: keywords.c,v 1.12 2003/02/14 20:42:10 moniot Exp $

	Routines to distinguish keywords from identifiers.


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


#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "ftnchek.h"
#include "symtab.h"
#include "tokdefs.h"
#include "forlex.h"

extern int in_attrbased_typedecl; /* shared with fortran.y */

PROTO( PRIVATE int is_keyword,( int i ));

/*

  Keyword definition table holds names of keywords and flags used
  to differentiate them from identifiers.

*/

#define IK 0x01	/* Allowed only in initial keyword of a statement (can be
		   preceded only by non-EK keywords) */
#define NP 0x02	/* Never followed by ( or =  */
#define MP 0x04	/* Must be followed by ( */
#define NI 0x08	/* Disallowed in logical IF */
#define EK 0x10	/* Cannot be followed by IK keyword: turns off initial_flag */
#define TY 0x20	/* Data type name */
#define NA 0x40	/* Never followed by alphabetic.  Put this onto any that
		   can stand alone or be followed by a second keyword, so
		   that the longer form will be taken if present. */
#define CN 0x80 /* Can be preceded by construct-name and colon.  (Only set
		   this flag if there is support in the parser.) */
#define MB 0x100/* Blanks mandatory between two words (in free form) */
#define TK 0x200/* Type name that can be followed by kind-spec or len-spec */ 

				/* Bisection search done at each
				   length step requires fixed-length
				   strings for keyword names.  Macro
				   below identifies which keyword is
				   the longest.
				 */
#define LONGEST_KEYWORD "DOUBLEPRECISION"

				/* Number of keywords in table */
#define NUM_KEYWORDS (sizeof(keywords)/sizeof(keywords[0]))

	/* Macro to determine whether a token class C is that of a data
	   type (for purposes of is_keyword) */
#ifndef OLDDEF
#define is_a_type_token(C) (((C)>=keytab_offset&&\
	        (unsigned)((C)-keytab_offset)<keytab_size)?\
		  (keywords[keytab_index[(C)-keytab_offset]].context&TY):FALSE)
#else
				/* This is a simpler defn that will work
				   for is_keyword's needs. */
#define is_a_type_token(C) ((C)>=tok_BYTE && ((C)<=tok_REAL))
#endif

		/* Keyword list must be maintained in alphabetical
		   order.  New keywords can be added so long as their
		   context info is specified.  No other source code
		   changes are necessary, but of course new keywords
		   won't be recognized by the parser till you add
		   productions to fortran.y.  Also, if IK flag is not
		   set, is_keyword will have to look at it specially.
		   Field split_pos is 0 if keyword cannot be split; otherwise
		   it is the length of the first moiety.  END BLOCK DATA
		   is the only 3-word keyword, and the implementation happens
		   to check its "END" based on ENDSUBROUTINE's split_pos so OK.
		 */
PRIVATE struct {
	char name[sizeof(LONGEST_KEYWORD)];
	short tclass,		/* token class */
	      context;		/* local-context flags */
	short split_pos;	/* where keyword may have space */
} keywords[]={
{"ACCEPT",	tok_ACCEPT,	IK | EK,			0},
{"ASSIGN",	tok_ASSIGN,	IK | NP | EK | NA,		0},
{"BACKSPACE",	tok_BACKSPACE,	IK | EK,			0},
{"BLOCKDATA",	tok_BLOCKDATA,	IK | EK | NP | NI,		5},
{"BYTE",	tok_BYTE,	IK | NI | EK | TY,		0},
{"CALL",	tok_CALL,	IK | NP | EK,			0},
{"CASE",	tok_CASE,	IK | MP | NI | EK | NA,		0},
{"CASEDEFAULT",	tok_CASEDEFAULT,IK | NP | NI | EK | MB,		4},
{"CHARACTER",	tok_CHARACTER,	IK | NI | EK | TY | TK,		0},
{"CLOSE",	tok_CLOSE,	IK | EK | MP | NA,		0},
{"COMMON",	tok_COMMON,	IK | NP | NI | EK,		0},
{"COMPLEX",	tok_COMPLEX,	IK | NI | EK | TY | TK,		0},
{"CONTINUE",	tok_CONTINUE,	IK | NP | EK | NA,		0},
{"CYCLE",	tok_CYCLE,	IK | NP | EK,			0},
{"DATA",	tok_DATA,	IK | NI | EK,			0},
{"DIMENSION",	tok_DIMENSION,	IK | NP | NI | EK,		0},
{"DO",		tok_DO,		IK | NP | NI | EK | CN,		0},
{"DOUBLECOMPLEX",tok_DOUBLECOMPLEX,	IK | NI | EK | TY,	6},
{"DOUBLEPRECISION",tok_DOUBLEPRECISION,	IK | NI | EK | TY,	6},
{"DOWHILE",	tok_DOWHILE,	IK | NI | EK | MB,		2},
{"ELSE",	tok_ELSE,	IK | NP | NI,			0},
#if 0	/* ELSEIF not lexed: lexes ELSE and IF separately */
{"ELSEIF",	tok_ELSEIF,	IK | NI | EK | MP | NA,		4},
#endif
{"END",		tok_END,	IK | NP | NI | NA,		0},
{"ENDBLOCKDATA",tok_ENDBLOCKDATA,IK | NP | NI | EK,		8},
{"ENDDO",	tok_ENDDO,	IK | NP | NI | EK,		3},
{"ENDFILE",	tok_ENDFILE,	IK | EK,			3},
{"ENDFUNCTION",	tok_ENDFUNCTION,IK | NP | NI | EK,		3},
{"ENDIF",	tok_ENDIF,	IK | NP | NI | EK,		3},
{"ENDPROGRAM",	tok_ENDPROGRAM,	IK | NP | NI | EK,		3},
{"ENDSELECT",	tok_ENDSELECT,	IK | NP | NI | EK,		3},
{"ENDSUBROUTINE",tok_ENDSUBROUTINE,	IK | NP | NI | EK,	3},
{"ENTRY",	tok_ENTRY,	IK | NP | NI | EK,		0},
{"EQUIVALENCE",	tok_EQUIVALENCE,IK | NI | EK | MP | NA,		0},
{"EXIT",	tok_EXIT,	IK | NP | EK,			0},
{"EXTERNAL",	tok_EXTERNAL,	IK | NP | NI | EK,		0},
{"FORMAT",	tok_FORMAT,	IK | NI | EK | MP | NA,		0},
{"FUNCTION",	tok_FUNCTION,	NP | NI | EK,			0},
{"GOTO",	tok_GOTO,	IK | EK,			2},
{"IF",		tok_IF,		IK | NI | EK | MP | NA | CN,	0},
{"IMPLICIT",	tok_IMPLICIT,	IK | NP | NI,			0},
{"INCLUDE",	tok_INCLUDE,	IK | NP | NI | EK | NA,		0},
{"INQUIRE",	tok_INQUIRE,	IK | EK | MP | NA,		0},
{"INTEGER",	tok_INTEGER,	IK | NI | EK | TY | TK,		0},
{"INTRINSIC",	tok_INTRINSIC,	IK | NP | NI | EK,		0},
{"LOGICAL",	tok_LOGICAL,	IK | NI | EK | TY | TK,		0},
{"NAMELIST",	tok_NAMELIST,	IK | NP | NI | EK,		0},
{"NONE",	tok_NONE,	IK | NI | EK | TY | NA,		0},
{"OPEN",	tok_OPEN,	IK | EK | MP | NA,		0},
{"PARAMETER",	tok_PARAMETER,	IK | NI | EK | MP | NA,		0},
{"PAUSE",	tok_PAUSE,	IK | NP | EK,			0},
#ifdef ALLOW_CRAY_POINTERS
{"POINTER",     tok_POINTER,    IK | MP | NI | EK | NA,		0},
#endif
{"PRINT",	tok_PRINT,	IK | EK,			0},
{"PROGRAM",	tok_PROGRAM,	IK | NP | NI | EK,		0},
{"READ",	tok_READ,	IK | EK,			0},
{"REAL",	tok_REAL,	IK | NI | EK | TY | TK,		0},
{"RETURN",	tok_RETURN,	IK | EK,			0},
{"REWIND",	tok_REWIND,	IK | EK,			0},
{"SAVE",	tok_SAVE,	IK | NP | NI | EK,		0},
{"SELECTCASE",	tok_SELECTCASE,	IK | MP | NI | EK | NA | CN | MB,6},
{"STOP",	tok_STOP,	IK | NP | EK,			0},
{"SUBROUTINE",	tok_SUBROUTINE,	IK | NP | NI | EK,		0},
{"THEN",	tok_THEN,	IK | NP | EK,			0},
{"TO",		tok_TO,		NI | EK,			0},
{"TYPE",	tok_TYPE,	IK | EK,			0},
{"WHILE",	tok_WHILE,	NI | EK | MP | NA,		0},
{"WRITE",	tok_WRITE,	IK | EK | MP | NA,		0},
};


		/* Lookup table to allow index in keywords table of
		   a given keyword to be found by its token number.
		   Initialized by init_keyhashtab. */
PRIVATE short
  keytab_offset,	/* lowest keyword token number */
  *keytab_index;	/* array of keyword indices  */
PRIVATE unsigned
  keytab_size;		/* Number of elements in keytab_index */


	/* get_identifier reads a string of characters satisfying
	   isidletter.  As they are read and as long as they are
	   alphabetic, it looks for a match to a keyword, and
	   whenever one is found, checks with is_keyword to see
	   if the context is right.  If so, it returns the keyword.
	   Otherwise it keeps going and eventually returns the id.
	 */
void
#if HAVE_STDC
get_identifier(Token *token)
#else /* K&R style */
get_identifier(token)
	Token *token;
#endif /* HAVE_STDC */
{
	int c,		/* Uppercase version of current letter */
	    preceding_c,/* Char preceding latest id */
	    has_embedded_space,	/* Spaces inside keyword or id */
	    split_pos,		/* Where space allowed */
	    kwd_has_embedded_space, /* Keyword does not follow freeform rules*/
	    kwd_not_separated,	/* keyword followed by alphanumeric w/o spc */
	    kwd_split_pos,	/* for MB keyword: actual location of blank */
	    kwd_not_split,	/* for checking MB keywords */
	    space_seen_lately,	/* Flag for catching embedded space */
	    lo,hi,	/* Indices in keyword table where match may be */
	    keywd_class;/* Class number returned by is_keyword */
	unsigned klen;	/* Length of id read so far (after keyword test) */
	int possible_keyword;

	token->tclass = tok_identifier;
	keywd_class = FALSE;

	klen = 0;
	lo = 0;
	hi = NUM_KEYWORDS-1;
	split_pos = 0;

	/* Define shorthand for the keyword letter under study */
#define KN(i) keywords[i].name
#define KL(i) keywords[i].name[klen]

	possible_keyword = TRUE;
	preceding_c = prev_char;
	has_embedded_space = kwd_has_embedded_space = kwd_not_split = FALSE;
	kwd_not_separated = FALSE;
	kwd_split_pos = 0;
	space_seen_lately = FALSE;

			/* set stmt class in case is_keyword not invoked */
	if(initial_flag && !in_attrbased_typedecl)
	  curr_stmt_class = tok_identifier;

			/* This loop gets  letter [letter|digit]* forms */
	while(isidletter(curr_char) || isadigit(curr_char)) {
	  c = makeupper(curr_char); /* Get the next char of id */
	  if(src_text_len < MAX_SRC_TEXT)
	    src_text_buf[src_text_len++] = (int)makeupper(curr_char);

	  if(space_seen_lately) {
	    has_embedded_space = TRUE;
				/* If space occurs, it is OK if located where
				   it is allowed in keyword pair.  If split
				   where it's OK, record for checking MB.
				 */
	    if( klen != split_pos )
		kwd_has_embedded_space = TRUE;
	    else
		kwd_split_pos = split_pos;
	  }
	  bi_advance();		/* Pull in the next character */

	  space_seen_lately = iswhitespace(prev_char);

				/* As long as it may yet be a keyword,
				   keep track of whether to invoke is_keyword.
				 */
	  if(possible_keyword) {

	    if(!isaletter(c)	/* If not alphabetic, cannot be keyword */
	       || klen >= sizeof(keywords[0].name)-1) /* or overlength */
	    {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
src_text_buf[src_text_len] = '\0';
(void)fprintf(list_fd,"\n%s not a keyword because",src_text_buf);
if(!isaletter(c))
  (void)fprintf(list_fd," non-letter at %c",c);
if(klen >= sizeof(keywords[0].name)-1)
  (void)fprintf(list_fd,"length %d >= max %d",klen,sizeof(keywords[0].name)-1);
}
#endif
	      possible_keyword = FALSE;
	    }
	    else {
	      int mid;
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
(void)fprintf(list_fd,"\nklen=%d c=%c",klen,c);
(void)fprintf(list_fd,"\nBisecting [lo,hi]=[%d,%d] \"%s\"..\"%s\"",
	   lo,hi,KN(lo),KN(hi));
}
#endif
				/* Bisect lo .. hi looking for match
				   on characters found so far. */
	      while(lo <= hi) {
		mid = (lo + hi)/2;
		if( KL(mid) < c ) {	/* No match in lower half */
		  lo = mid+1;
		}
		else if( KL(mid) > c ) {/* No match in upper half */
		  hi = mid-1;
		}
		else {		/* Match at midpoint: Bisect each
				   half to find the new subinterval. */
		  int midlo=mid, midhi=mid;
				/* Bisect lo .. mid */
		  while( lo < midlo-1 &&  KL(lo) != c) {
		    mid = (lo + midlo)/2;
		    if(  KL(mid) < c ) {
		      lo = mid+1;
		    }
		    else {	/* equal */
		      midlo = mid;
		    }
		  }
		  if( KL(lo) != c )
		    lo = midlo;
				/* Bisect mid .. hi */
		  while( midhi < hi-1 && KL(hi) != c ) {
		    mid = (midhi + hi)/2;
		    if( KL(mid) > c ) {
		      hi = mid-1;
		    }
		    else {	/* equal */
		      midhi = mid;
		    }
		  }
		  if( KL(hi) != c )
		    hi = midhi;

		  break;	/* After bisecting each half, we are done */
		}		/* end else KL(mid) == c */
	      }			/* end while(lo <= hi) */

	      klen++;		/* Now increment the length */

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
(void)fprintf(list_fd,"\nNew [lo,hi]=[%d,%d] \"%s\"..\"%s\"",
	   lo,hi,KN(lo),KN(hi));
}
#endif
			/* If range is null, a match has been ruled out. */
	      if(lo > hi) {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
src_text_buf[src_text_len] = '\0';
(void)fprintf(list_fd,"\nKeyword ruled out for %s at length %d since lo %d > hi %d",
	   src_text_buf,klen,lo,hi);
}
#endif
		possible_keyword = FALSE;
	      }
			/* If length of first keyword in range is equal
			   to the new length, then we have a match at
			   this point.  Check it out with is_keyword.
			 */
	      else if(KN(lo)[klen] == '\0') {
		if( (keywd_class = is_keyword(lo)) != FALSE) {
		  token->tclass = keywd_class;	/* It's a keyword */
		  token->value.string = NULL;
		  token->src_text = KN(lo);
				/* Keyword butting against following token
				   is warned about separately if token is an
				   identifier.  Here we catch cases like
				   BACKSPACE6 where next token is a number.
				 */
		  kwd_not_separated = (!space_seen_lately && isdigit(curr_char));
				/* If keyword is required to have a space
				   between parts, check if it does.
				 */
		  if( keywords[lo].context & MB )
		      kwd_not_split = (kwd_split_pos != keywords[lo].split_pos);
		  break;	/* Quit the input loop */
		}
		else if(lo == hi) {	/* Match is unique and ruled out */
		  possible_keyword = FALSE;
		}
	      }
	    }/* end else isaletter(c) */
				/* Longest match gives split pos if any.
				   This fails for DO UBLE...
				 */
	    split_pos = (hi >= 0)? keywords[hi].split_pos: 0;

	  }/* end if(possible_keyword) */
	}/* end while(isidletter || isadigit) */

	if(keywd_class == FALSE) {		/* it is an identifier */

				/* Identifier: find its hashtable entry or
				   create a new entry.	*/
		    int h;
		    Lsymtab *symt;
#ifdef ALLOW_TYPELESS_CONSTANTS
				/* Watch out for const like X'nnn' */
		    if(src_text_len == 1 && isaquote(curr_char)) {
				/* Read the string, append the trailing quote
				   then invoke routine to interpret it. */
		      get_string(token);
#ifndef LEX_RAWSTRINGS
		      if(src_text_len < MAX_SRC_TEXT)
			src_text_buf[src_text_len++] = quote_char;
#endif
		      get_binary_const(token,src_text_buf[0],space_seen_lately);
		      return;
		    }
#endif

		    if(src_text_len < MAX_SRC_TEXT)
		      src_text_buf[src_text_len] = '\0';
		    token->value.integer = h = hash_lookup(src_text_buf);
		    token->src_text = hashtab[h].name;
				/* If it is an array give it a special token
				   class, so that arrays can be distinguished
				   from functions in the grammar. */
		    if((symt=hashtab[h].loc_symtab) != NULL
		       && symt->array_var) {
		      token->tclass = tok_array_identifier;

	  }
	}
	else {			/* It is a keyword */
	    has_embedded_space = kwd_has_embedded_space;
	}

				/* Check identifiers for being juxtaposed
				   to keywords or having internal space.
				   Keywords are warned about if they are
				   split where they are not allowed to be.
				   Special case: ELSEIF is never a problem.
				 */

	if(( ((pretty_no_space || (free_form && f90_freeform_space))
		 && (isidletter(preceding_c) || isadigit(preceding_c) ||
		     kwd_not_separated)
	      && !(prev_token_class==tok_ELSE && keywd_class==tok_IF) )
	  || ((pretty_extra_space || (free_form && f90_freeform_space))
		   && has_embedded_space) ) ) {
	    if(token->tclass==tok_identifier || token->tclass==tok_array_identifier) {
	      space_violation(token->line_num,token->col_num,"identifier");
	      msg_tail(hashtab[token->value.integer].name);
	    }
	    else {
	      space_violation(token->line_num,token->col_num,"keyword");
	      msg_tail(keywords[keytab_index[keywd_class-keytab_offset]].name);
	    }
	  if(has_embedded_space)
	    msg_tail("has embedded space");
	  else
	    msg_tail("not clearly separated from context");
	}
	if( keywd_class != FALSE ) {
	  if( free_form && f90_freeform_space && kwd_not_split ) {
	    space_violation(token->line_num,token->col_num,"keyword");
	    msg_tail(keywords[keytab_index[keywd_class-keytab_offset]].name);
	    msg_tail("lacks required space between parts");
	  }
	}
#ifdef DEBUG_FORLEX
	if(debug_lexer){
	    switch((int)(token->tclass)) {
		case tok_identifier:
			(void)fprintf(list_fd,"\nIdentifier:\t\t%s",
				      token->src_text);
			break;
		case tok_array_identifier:
			(void)fprintf(list_fd,"\nArray_identifier:\t%s",
				      token->src_text);
			break;
		default:
			(void)fprintf(list_fd,"\nKeyword:\t\ttok_%s",
				      token->src_text);
			break;
	    }
	}
#endif
} /* get_identifier */

/*  iskeyword:

	Determines (to the best of its current ability) whether a
	given identifier is a keyword or not.  Hopefully now no
	keywords are reserved.  For keywords that can be split, like
	DO WHILE, the longest form of the keyword is gotten, i.e. this
	yields tok_DOWHILE not tok_DO then tok_WHILE.  There are some
	exceptions (NA flag not present) like IMPLICIT.

	Method uses context from start of statement up to and
	including the character following the putative keyword to
	eliminate as many cases as possible.  Any non-IK keywords
	(those that need not be in the initial series of keywords of
	statement) have special code to handle them.  The rest are
	handed off to looking_at_keywd which tries to see if it is an
	assignment statement.

	Note that some rules that could be used if F77 Standard were
	adhered to strictly are not used here.  The idea is to allow
	extensions, and leave catching syntax errors in the parser.
	For example, specification-statement keywords are not excluded
	after the first executable statement has been seen.  The status
	of a variable as declared array or character type is not consulted
	in ruling out an assignment statement if following parentheses
	are present.  Etc.  */


		/* Macro to test if all the specified bits are set */
#define MATCH(CONTEXT) ((keywords[i].context & (CONTEXT)) == (CONTEXT))


PRIVATE int
#if HAVE_STDC
is_keyword(int i)
           			/* Index in keywords table */
#else /* K&R style */
is_keyword(i)
     int i;			/* Index in keywords table */
#endif /* HAVE_STDC */
{
  int ans = FALSE;
  int putative_keyword_class;	/* Class of the supposed keyword */
  extern int 	construct_name_seen,	/* helper variables set by parser */
		stmt_sequence_no;	/* shared with fortran.y */

  while(iswhitespace(curr_char))	      /* Move to lookahead char */
    advance();

#ifdef DEBUG_IS_KEYWORD
  if(debug_lexer){
    (void)fprintf(list_fd,
		"\nkeyword %s: initialflag=%d implicitflag=%d ",
	    keywords[i].name,initial_flag,implicit_flag);
    (void)fprintf(list_fd,
		"context=%o, next char=%c %o",keywords[i].context,
						curr_char,curr_char);
  }
#endif

  putative_keyword_class = keywords[i].tclass;

  if( !initial_flag && MATCH(IK) ) {
			/* Dispose of keywords which can only occur in initial
			   part of statement, if found elsewhere. One exception
			   is something with a construct-name tacked on in
			   front of it, which is a cinch.
			*/
    if( (ans = (construct_name_seen && MATCH(CN))) ) {
      if( putative_keyword_class == tok_DO &&
	  looking_at_keywd(tok_WHILE) ) {
	WHILE_expected = TRUE;
      }
    }

  }

#if 0 /* This does not work: curr_stmt_class not cleared beforehand */
  else if(curr_stmt_class == tok_IF && MATCH(NI)) {
			/* Dispose of keywords which cannot occur in stmt
			   field of logical IF if that is where we are.
			 */
    ans = FALSE;
  }
#endif

  else if(MATCH(NA) && isalpha(curr_char)) {
			/* Dispose of keywords which cannot be followed
			   by alphabetic character if that is so.

			   Handle variant unparenthesized PARAMETER stmt.
			   Reject if it follows a stmt fun or executable stmt.
			 */
    if(putative_keyword_class != tok_PARAMETER
       || stmt_sequence_no > SEQ_STMT_FUN) {
      ans = FALSE;
    }
    else {		  /* non-paren form _should_ look like an assignment */
      ans = ! looking_at_keywd(putative_keyword_class);
    }
  }

  else if(putative_keyword_class == tok_TO) {/* A non-IK case */
				/* TO following GO is handled with GO.
				   Here identify it if it follows a label
				   in ASSIGN statement.
				 */
    ans = ( curr_stmt_class == tok_ASSIGN
	   && prev_token_class == tok_integer_const);

  }
  else if(putative_keyword_class == tok_FUNCTION /* A non-IK case */
    && (stmt_sequence_no != 0 /* not the first statement of module */

	|| !(initial_flag  /* if not initial can only be preceded by type */
	     || is_a_type_token(curr_stmt_class)) )) {
    ans = FALSE; /* otherwise it will be handled correctly by looking_at */
  }
  else if(putative_keyword_class == tok_WHILE) { /* A non-IK case */
	 /* Only occurs in DO label [,] WHILE and constructname: DO WHILE  */
    ans = WHILE_expected;
    WHILE_expected = FALSE;
  }
		/* Remaining cases are IK in initial part */

			/*   Eliminate those which can never be followed
			     by '(' or '=' if that is what we have.
			     Exception for DIMENSION in attr-based type
			     declaration, which must be followed by left paren.
			 */
  else if(MATCH(NP) &&
	  ((curr_char == '('
	    && !(in_attrbased_typedecl && putative_keyword_class == tok_DIMENSION))
	   || curr_char == '=') ) {
    ans = FALSE;
  }

			/* Likewise with those that must be followed by
			   '(' but aren't.
			   Exception for PARAMETER in attr-based type
			   declaration, which must be followed by comma or
			   double colon.
			*/
  else if(MATCH(MP) && curr_char != '(') {
    ans = (in_attrbased_typedecl && putative_keyword_class == tok_PARAMETER
	&& (curr_char == ',' || curr_char == ':'));
  }


				/* END DO: handle its DO here */
  else if( putative_keyword_class == tok_DO && curr_char == EOS ) {
	/* Also must have prev_token_class == tok_END, but
	   no need to check since end-of-statement suffices. */
    ans = TRUE;
  }


				/* Other type names always follow the word
				   IMPLICIT */
  else if( implicit_flag ) {
    ans =  MATCH(TY);
  }

  else {
		     /* Remaining cases are keywords that must be in
			initial position. If followed by '=' must be an
			identifier.  If followed by '(' then may be an array
			or character lvalue, so use looking_at to scan ahead
			to see if this is an assignment statement. */
      ans =  looking_at_keywd(putative_keyword_class);
  }


			/* Save initial token class for use by parser.
			   Either set it to keyword token or to id for
			   assignment stmt. */
  if(initial_flag && !in_attrbased_typedecl) {
    curr_stmt_class = (ans? keywords[i].tclass: tok_identifier);
  }


		/* Turn off the initial-keyword flag if this is a
		   keyword that cannot be followed by another keyword
		   or if it is not a keyword.
		*/
  if(ans) {
    if(keywords[i].context & EK) {
	/* In case lookahead to :: was not done or failed, a type keyword
	   followed immediately by comma must be attr-based form.
	   If TK type keyword is followed by parenthesis, it signals a
	   parentheized KIND or LEN spec, which needs a special left paren
	   to avoid shift/reduce conflict in the grammar.
	 */
      if( MATCH(TY) ) {
	if( curr_char == ',' ) {
	  in_attrbased_typedecl = TRUE;
	}
	else if( MATCH(TK) && curr_char == '(' ) {
	  if(! (implicit_flag && looking_at_implicit_list()) )
	    need_special_lparen = TRUE;
	}
      }
      if( !in_attrbased_typedecl )
	initial_flag = FALSE;
    }
    return keywords[i].tclass;
  }
  else {	/* If no more letters follow, then keyword here
		   is ruled out.  Turn off initial_flag. */
    if( ! isalpha(curr_char) )
      initial_flag = FALSE;

    return 0;	/* Not found in list */
  }
}/* End of is_keyword */


/*    init_keyhashtab:
*/
		/* Hashing is no longer used.  This guy now only
		   initializes the table of indices that allow
		   keywords to be looked up by their token class*/
void
init_keyhashtab(VOID)
{
  unsigned i;
  int k,kmin,kmax;
  kmin = kmax = keywords[0].tclass;	/* Find min and max token classes */
  for(i=1; i<NUM_KEYWORDS; i++) {
    k = keywords[i].tclass;
    if(k < kmin)  kmin = k;
    if(k > kmax)  kmax = k;
  }

  keytab_offset = kmin;	/* Index table from [kmin..kmax] -> [0..size-1] */
  keytab_size = (unsigned) (kmax-kmin+1);
  if( (keytab_index=(short *)calloc(keytab_size,sizeof(keytab_index[0])))
     == (short *)NULL) {
    oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
	   "cannot allocate space for keytab_index");
  }

				/* Now fill in the lookup table, indexed
				   by class - offset */
  for(i=0; i<NUM_KEYWORDS; i++) {
    k = keywords[i].tclass;
    keytab_index[k - keytab_offset] = i;
  }
}

/* keytok_name: given token class number, returns pointer to its name.
   Works only for keyword tokens.  This is inefficient, but it is only
   used in error messages.
 */

char *
keytok_name(int tclass)
{
    int i;
    for(i=0; i<NUM_KEYWORDS; i++) {
	if( keywords[i].tclass == tclass ) {
	    return keywords[i].name;
	}
    }
    return "noname";		/* not in the list */
}
