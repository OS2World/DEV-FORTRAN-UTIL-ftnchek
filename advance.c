/* $Id: advance.c,v 1.13 2003/03/21 22:34:11 moniot Exp $

	Low-level input routines for Fortran program checker.

	Shared functions defined:
		init_scan()	Initializes an input stream.
		finish_scan()	Finishes processing an input stream.
		advance()	Reads next char, removing comments and
				handling continuation lines.
			looking_at_x Handles lookahead up to end of line:
		looking_at_cplx() Identifies complex constant.
		looking_at_keywd() Identifies assgnmt stmts vs keywords.
		looking_at_relop() Distinguishes .EQ. from .Eexp .
		flush_line_out(n) Prints lines up to line n if not already
				printed, so error messages come out looking OK.
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
#define ADVANCE
#include "advance.h"

	/* Define tab stops: NXTTAB(col_num) is column of next tab stop.
	   This formula maps [1-8]->9, [9-16]->17, etc.
	 */
#define NXTTAB(COL) ( (((COL)-1)/8+1)*8+1 )

		/* Standard maximum line length depends on source form. */
PRIVATE COLNO_t std_max_stmt_col;

	/* Lookahead routines that scan the input
	   line for various things.  The is_whatever routines take a
	   string as argument and return TRUE if it satisfies the
	   criterion. The skip_whatever routines take an index and
	   string as argument and return the index of the next
	   nonspace character in the string after the expected thing,
	   which must be there in a syntactically correct program.
	   The given index points at the character after a known
	   lead-in (except for see_a_number, which can be given the
	   index of 1st char of number).  The see_whatever routines
	   are similar but return -1 if the expected thing is not
	   seen, which it need not be. */


PROTO(PRIVATE char * getstrn,( char s[], int n, FILE *fd ));

PROTO(PRIVATE int is_comment,( char s[] ));

PROTO(PRIVATE int is_fixed_continuation,( char s[], int *cont_index, COLNO_t *cont_col_num ));

PROTO(PRIVATE int is_overlength,( char *s, COLNO_t maxcol ));

PROTO(PRIVATE int line_is_blank,(char *s));

PROTO(PRIVATE int see_a_number,( int i, char s[], int can_be_holl ));

PROTO(PRIVATE int see_dowhile,( int indx, char ll[] ));

PROTO(PRIVATE int see_expression,( int indx, char ll[] ));

PROTO(PRIVATE int see_keyword,( int indx, char ll[], char *matchstr ));

PROTO(PRIVATE int skip_balanced_parens,( int indx, char ll[] ));

PROTO(PRIVATE int skip_idletters,( int indx, char ll[] ));

PROTO(PRIVATE int skip_quoted_string,( int indx, char ll[] ));

PROTO(PRIVATE int skip_hollerith,( int i, char s[] ));

#ifdef ALLOW_UNIX_CPP
PROTO(PRIVATE int take_cpp_line,( char *s ));
#endif





PRIVATE int line_is_overlength, prev_line_overlength;

				/* Variables to hold continuation-line state
				   for free source form.
				 */
PRIVATE int freeform_contin, freeform_char_contin, freeform_contin_start;

void
init_scan(VOID)			/* Starts reading a file */
{
	tab_filename = NULL;
	incdepth = 0;
	inctable_index = -1;
#ifdef ALLOW_UNIX_CPP
	cpp_inc_depth = 0;
	cpp_start_of_file = TRUE;
#endif
	top_file_line_num = 1;

			/* Choose source form based on filename extension,
			   unless overridden by -source=fixed or free.
			 */
	if(source_free_form) {
	    free_form = TRUE;
	}
	else if(source_fixed_form) {
	    free_form = FALSE;
	}
	else {
				/* For *.f90, use free form.  For all others,
				   use fixed form.
				 */
	    if( has_extension(top_filename,".f90") ) {
		free_form = TRUE;
	    }
	    else {
		free_form = FALSE;
	    }
	}

		/* Set max line length according to source form */
	if( free_form ) {
	    max_stmt_col = (COLNO_t)132;	/* not adjustable */
	    std_max_stmt_col = (COLNO_t)132;
	}
	else {
	    max_stmt_col = (COLNO_t)fixed_max_stmt_col; /* may be changed by -column */
	    std_max_stmt_col = (COLNO_t)72;
	}

	freeform_contin = freeform_char_contin = freeform_contin_start = FALSE;

	line = lineA;		/* Start out reading into buffer A */
	prev_line = lineB;

	init_stream();
}

void
init_stream(VOID)		/* Initializes a new input stream */
{
	curr_comment_line = FALSE;
	inside_string = FALSE;
	inside_hollerith = FALSE;
	line_is_printed = TRUE;
	prev_line_is_printed = TRUE;
	line_is_overlength = prev_line_overlength = FALSE;
	noncomment_line_count = 0;

	next_index = -1;	/* Startup as if just read a blank line */
	next_char = EOS;
	curr_char = EOS;
	next_col_num = 0;
	next_line_num = 0;
	prev_line_num = prev_stmt_line_num = 0;
	sticky_EOF = TRUE;
	contin_count = 0;

	line[0] = '\0';
	advance();		/* put 1st two chars in the pipeline */
	advance();
	advance();		/* gobble the artificial initial EOS */
}


void
finish_scan(VOID)
{
		/* clean up if no END statement at EOF */
	check_seq_header((Token *)NULL);
		/* print last line if not already done */
	if(do_list)
	    (void)flush_line_out(line_num);
}

	/* next_up(c) is true if next_char == c outside of character or
	   hollerith context.   Must look at curr_char to avoid
	   being fooled by '!' without messing up on 'xxx'! either.
	   Also don't be fooled by '''!''' which is the string '!'
	   Note that inside_string does not yet reflect curr_char.
	   Test is that inside_string is true but about to become false,
	   or false and not about to become true. Think about it. */

#define next_up(c) ( (next_char==(c)) &&\
	(inside_string? (curr_char == quote_char) : !isaquote(curr_char)) &&\
	(!inside_hollerith) )

	/* char_context() is true if next_char is inside character string. */
#define char_context() (inside_string?(curr_char!=quote_char):isaquote(curr_char))


		/* Routine to move forward one character in source */

void
advance(VOID)
{

    prev_char = curr_char;

    if(next_char == EOF) {	  /* Don't advance past EOF */
	if(curr_char == EOS || curr_char == EOF) {

			 /* Pause to allow parse actions at end of stmt
			    to have correct file context before popping
			    the include file.  Effect is to send an extra
			    EOS to parser at end of file. */
		  if(sticky_EOF) {
		    sticky_EOF = FALSE;
		    return;
		  }
				/* At EOF: close include file if any,
				   otherwise yield an EOF character. */
		  if( ! pop_include_file() ) {
		    curr_char = EOF;
		  }
		  return;
	}
	else {
		  curr_char = EOS;
		  return;
	}
    }

    if(curr_char == EOS)
	initial_flag = TRUE;

    curr_char = next_char;	  /* Step to next char of input */
    col_num = next_col_num;
    line_num = next_line_num;

				/* See if line is officially overlength */
    if( col_num > std_max_stmt_col && !iswhitespace(curr_char)) {
	line_is_overlength = TRUE;
    }

    if(next_char == '\t'){	   /* Handle tabs in input */
				/* N.B. dec-tabs orthogonal to free-form */
	if(source_dec_tab && next_col_num < 7) {
	    next_col_num = 7; /* initial DEC tab -> col 7  */
	}
	else
	{
	    next_col_num = NXTTAB(next_col_num);
	}

	if( ! (inside_string || inside_hollerith) )
	    if(tab_filename == NULL)
		tab_filename = current_filename;	/*  for portability warning */
    }
    else {
	next_col_num++;
    }

    next_char = line[++next_index];

				/* Handle semicolon statement separator */
    if(next_up(';')) {
	next_char = EOS;
	if(f77_semicolon) {
	    nonstandard(next_line_num,next_col_num,0,0);
	    msg_tail("semicolon statement separator");
	}
    }

			/* See if we have reached a continuation character */

    if( free_form && next_char == '&' ) {

		/* Inside char context & must be last nonblank of line */
	if( char_context() ) {
	  freeform_contin = freeform_char_contin =
	      line_is_blank(&line[next_index+1]);
	}
	else {
	  freeform_contin = TRUE;
	}
	freeform_contin_start = freeform_contin;
    }
			/* If end of line is reached, input a new line.
			 */
    while(next_col_num > (COLNO_t)max_stmt_col || next_char == '\0'
	|| next_up(INLINE_COMMENT_CHAR) || freeform_contin_start ){
	freeform_contin_start = FALSE; /* don't loop on '&' */
	do{
	    if(do_list) /* print prev line if not printed yet */
		(void)flush_line_out(prev_line_num);

				/* Warn if stmt field has been extended
				   and the extended part has been used. */
	    if(!prev_comment_line) {
		if( ((f77_overlength || f90_overlength)
		     && max_stmt_col>std_max_stmt_col)
		    && prev_line_overlength){
		    nonstandard(prev_line_num,(std_max_stmt_col+1),
				f90_overlength,0);
		    msg_tail(": significant characters past");
		    if( !free_form ) {
			msg_tail("72 columns");
			if(f90_overlength) { /* explain still wrong */
			    msg_tail("in fixed source form");
			}
		    }
		    else {
			msg_tail("132 columns in free source form");
		    }
		}
			/* Otherwise warn if any chars past 72 cols,
			   fixed format only */
		else if(pretty_overlength
			&& !free_form
			&& is_overlength(prev_line,MAXLINE)) {
		    ugly_code(prev_line_num,(COLNO_t)73,
			      "characters past 72 columns");
		}
	    }


	    if( f77_inline_comment) {
		if( !curr_comment_line && next_up(INLINE_COMMENT_CHAR)){
		    nonstandard(next_line_num,next_col_num,0,0);
		    msg_tail(": inline comment");
		}
	    }


			    /* Swap input buffers to get ready for new line.
			       But throw away comment lines if do_list is
			       false, so error messages will work right.
			     */
			if(do_list || ! curr_comment_line) {
			    char *temp=line;
			    line = prev_line;
			    prev_line=temp;
			    if(! curr_comment_line)
			      prev_stmt_line_num = line_num;
			    prev_line_num = next_line_num;
			    prev_line_is_printed = line_is_printed;
			    prev_line_overlength = line_is_overlength;
			    line_is_overlength = FALSE;
			}

			++next_line_num;
			line_is_printed = FALSE;
			if( getstrn(line,MAXLINE+1,input_fd) == NULL ) {
				next_char = EOF;
				line_is_printed = TRUE;
				return;
			}
#ifdef ALLOW_UNIX_CPP
			else
			  if(line[0] == '#')
			    cpp_handled = take_cpp_line(line);
#endif
			++tot_line_count; /* count lines processed */

			/*  Keep track of prior-comment-line situation */
			prev_comment_line = curr_comment_line;

	} while( (curr_comment_line = is_comment(line)) != FALSE);
	++tot_stmt_line_count;
	++noncomment_line_count;

			/* Handle fixed-form continuation lines */
	if( !free_form &&
	    is_fixed_continuation(line,&next_index,&next_col_num) ) {
				/* It is a continuation */
			next_char = EOL;

				/* Issue warnings if contin in funny places */
			if(noncomment_line_count == 1)
			    warning(next_line_num,(COLNO_t)6,
		    "Continuation mark found in first statement of file");
			if( pretty_contin && prev_comment_line )
			    ugly_code(next_line_num,(COLNO_t)6,
		    "Continuation follows comment or blank line");
			if(contin_count++ == 19)
			  if(f77_20_continue || f90_continue) {
			    nonstandard(next_line_num,(COLNO_t)6,0,0);
			    msg_tail(": > 19 continuation lines");
			    if(f90_continue)
				msg_tail("in fixed source form");
			  }
	}
			/* Resume free form continuation */
	else if( free_form && freeform_contin ) {
				/* Look for leading '&' continuation line.
				   It is mandatory for character context,
				   optional otherwise.
				 */
	    int i, col, c;
	    for(i=0,col=1; (c=line[i]) != '\0' && isspace(c); i++) {
		if(c == '\t')
			col = NXTTAB(col);
		else
			col++;
	    }
	    if( c == '&' ) {	/* Skip over everything up to the '&' */
		next_index = i+1;
		next_char = line[next_index];
		next_col_num = col+1;
	    }
	    else {
		if( freeform_char_contin ) {
		    syntax_error(next_line_num,(COLNO_t)col,
		 "Continuation in character context must resume with '&'");
		}
				/* If no '&' treat line break as whitespace */
		next_index = -1;
		next_char = EOL;
		next_col_num = 0;
	    }
	    if(contin_count++ == 39) {
		if(f90_continue) {
		    nonstandard(next_line_num,(COLNO_t)6,0,0);
		    msg_tail(": > 39 continuation lines");
		}
	    }
	    freeform_contin = freeform_char_contin = FALSE;
	}
	else {
				/* It is not a continuation */
		    next_index = -1;
		    next_char = EOS;
		    next_col_num = 0;

		    contin_count = 0;
	}
    }/*end while( end of line reached )*/

		/* Avoid letting a '0' in column 6 become a token in fixed form*/
    if(next_col_num == 6 && next_char == '0' && !free_form)
		next_char = ' ';


}/* end advance */




	/*  Function which returns 0 if line is not a comment, 1 if it is.
	 */

PRIVATE int
#if HAVE_STDC
is_comment(char *s)
#else /* K&R style */
is_comment(s)
	char s[];
#endif /* HAVE_STDC */
{
    int i,c= makeupper(s[0]);
    COLNO_t col;

    if( !free_form ) {
				/* Handle F77 standard comments here. */
	if( c == 'C' || c == '*' )
		return TRUE;

				/* Tolerate D comment lines.  There is
				   no provision for optionally
				   treating them as source code lines.
				 */
	if( c == 'D' ) {
		if(f77_d_comment || f90_d_comment) {
		  nonstandard(next_line_num,1,f90_d_comment,0);
		  msg_tail(": D in column 1 (treated as comment)");
		}
		return TRUE;
	}
    }
				/* Now see if line is blank or only contains
				   an inline comment.
				 */
    for(i=0,col=1; s[i] != '\0'; i++) {
		if( !isspace(s[i]))
		{
		/* Initial "!" starts a comment, except in col. 6 of
		   fixed form it must be taken as continuation mark */
			 if(s[i]==INLINE_COMMENT_CHAR &&
			    (free_form || col != 6) ) {
			     if(f77_inline_comment) {
				 nonstandard(next_line_num,col,0,0);
				 msg_tail(": inline comment");
			     }
			     return TRUE;
			  }
		/* Standard prohibits an & on a line with nothing but
		   blanks or commentary.  We allow it but flag it here.
		 */
			  else if( s[i] == '&' && free_form ) {
			    i++;
			    while( s[i] != '\0' && (isspace(s[i])) ) {
			      i++;
			    }
			    if( s[i] == '\0' || s[i] == INLINE_COMMENT_CHAR ) {
			      if(f90_continue) {
				nonstandard(next_line_num,col,0,0);
				msg_tail(": continuation mark alone on blank or comment line");
			      }
			      return TRUE; /* treat as comment line */
			    }
			    else
			      return FALSE;
			  }
			  else
			      return FALSE;
		}
		else {
			  if(s[i] == '\t') col = NXTTAB(col);
			  else		   col++;
		}
    } /* end for */

    return TRUE;		/* blank line */
}

		/* Function to see if line is blank (under freeform rules) */
PRIVATE int
line_is_blank(char *s)
{
    int i, c;
    for(i=0; (c=s[i]) != '\0' && isspace(c); i++)
	continue;
    return (c == '\0');
}


	/* Here we handle Unix preprocessor lines.  The only ones
	   processed now are those that set the line number and filename.
	     Form 1: # line 10 "filename"
	     Form 2: # 10 "filename"
	   We replace next_filename and next_line_num by the
	   given values.
	 */
#ifdef ALLOW_UNIX_CPP
PRIVATE int
#if HAVE_STDC
take_cpp_line(char *s)
#else /* K&R style */
take_cpp_line(s)
     char *s;
#endif /* HAVE_STDC */
{
  LINENO_t linenum;
  int nchars, handled;
  char *filename;

  handled=FALSE;

  do { ++s; } while( isspace(*s) );	/* Skip space after the '#' */

  if(strncmp(s,"line",4) == 0) {	/* Look for the keyword "line" */
    s += 4;			/* Skip the word "line" */
    while( isspace(*s) ) ++s;	/* Skip space after the word "line" */
  }

  if( isdigit(*s) ) {		/* See that we are now looking at a number */
    handled = TRUE;

			/* Get the line number */
    linenum=0;
    while( isdigit(*s) )
      linenum = linenum*10 + BCD(*s++);

			/* Now find the filename */

    filename = (char *)NULL;
    while( isspace(*s) ) ++s;	/* Skip space after the line number */

    if( *s == '"') {		/* Filename must be preceded by " */

      ++s;			/* Skip the " */

      nchars = 0;		/* Count chars in the filename */
      while( s[nchars] != '"' && s[nchars] != '\0')
	++nchars;

      if( s[nchars] == '"') {	/* Filename must be followed by " */

	s[nchars] = '\0';/* terminate it temporarily */

				/* See if this filename is on cpp_include_stack
				   at the previous level, and if so, re-use it.
				   If not, make new global space for it. */
	if( cpp_inc_depth > 0 &&
	    strcmp(s,cpp_include_stack[cpp_inc_depth-1].filename) == 0 ) {
	  filename = cpp_include_stack[cpp_inc_depth-1].filename;
	}
 	else if( strcmp(s,top_filename) == 0 ) {
 	  filename = top_filename;
	}
	else {
	  filename = new_global_string(s); /* put it in permanent space */
	}

	s[nchars] = '"'; /* restore line as it was */

      }
    }
  }/*end handling #line */

  if(handled) {
    next_top_file_line_num = next_line_num; /* save this in case it's needed */
    next_line_num = linenum-1;
    next_filename = filename;
  }
  else {
    next_filename = (char *)NULL;
  }

  return handled;		/* Return TRUE if it was a #line category */

}/*take_cpp_line*/
#endif

	/* Function which returns FALSE if line is a not continuation
	 *  line.  If line is a continuation, returns TRUE.  In either
	 *  case, sets cont_index to index in line of the continuation
	 *  mark and cont_col_num to corresponding column number.  If
	 *  source_dec_tab in effect, tab moves to column 7 and a nonzero
	 *  digit there implies continuation.  */
PRIVATE int
#if HAVE_STDC
is_fixed_continuation(char *s, int *cont_index, COLNO_t *cont_col_num)
#else /* K&R style */
is_fixed_continuation(s,cont_index,cont_col_num)
	char s[];
        int *cont_index;
	COLNO_t *cont_col_num;
#endif /* HAVE_STDC */
{
	COLNO_t col;
	int i,c;

				/* Handle DEC tabs: <tab><digit> is a
				   continuation card */
#ifdef DEC_TABS
	if( source_dec_tab && s[0] == '\t' ) {
	  if( isadigit((int)s[1]) && s[1] != '0' ) {
	    if(f77_dec_tabs || f90_dec_tabs) {
	      nonstandard(next_line_num,7,f90_dec_tabs,0);
	      msg_tail(": continuation mark not in column 6");
	    }
	    (*cont_index) = 1;
	    (*cont_col_num) = 7;
	    return TRUE;
	  }
	  else {		/* Tab then non-digit: regular stmt */
	    (*cont_index) = 0;
	    (*cont_col_num) = 7; /* (not used) */
	    return FALSE;
	  }
	}
#endif
				/* skip to col 6 */
	for(i=0,col=1; col < 6 && s[i] != '\0'; i++) {
		if(s[i] == '\t')
			col = NXTTAB(col);
		else
			col++;
	}
	c = s[i];

	if ( col == 6 && c != '\0' && !isspace(c) && c != '0'
#ifdef ALLOW_UNIX_CPP
				/* Veto if it is a preprocessor line */
	    && s[0] != '#'
#endif
	    ) {
	       (*cont_index) = i;
	       (*cont_col_num) = 6;
	       return TRUE;
	}
	else {
	       (*cont_index) = 0;
	       (*cont_col_num) = 0; /* (not used) */
	       return FALSE;
	}
}

int
#if HAVE_STDC
flush_line_out(LINENO_t n)	/* Prints lines up to line #n if not yet printed */
               		/* Returns TRUE if line was printed, else FALSE */
#else /* K&R style */
flush_line_out(n)	/* Prints lines up to line #n if not yet printed */
    LINENO_t n;		/* Returns TRUE if line was printed, else FALSE */
#endif /* HAVE_STDC */
{
			/* Print previous line only if do_list TRUE */
	if( !prev_line_is_printed
	 && ((n == prev_line_num) || (n > prev_line_num && do_list)) ) {
	   print_a_line(list_fd,prev_line,prev_line_num);
	   prev_line_is_printed = TRUE;
	}
	if(n >= next_line_num && !line_is_printed) {
	   print_a_line(list_fd,line,next_line_num);
	   line_is_printed = TRUE;
	}
    return ( do_list ||
	     (prev_line_is_printed && n == prev_line_num) ||
	     (line_is_printed && n == next_line_num) );
}


	/*  Function to read n-1 characters, or up to newline, whichever
	 *  comes first.  Differs from fgets in that the newline is replaced
	 *  by null, and characters up to newline (if any) past the n-1st
	 *  are read and thrown away.
	 *  Returns NULL when end-of-file or error is encountered.
	 */
PRIVATE char *
#if HAVE_STDC
getstrn(char *s, int n, FILE *fd)
#else /* K&R style */
getstrn(s,n,fd)
	char s[];
	int n;
	FILE *fd;
#endif /* HAVE_STDC */
{
	int i=0,c;
	static int prev_CR=FALSE;	/* records if <CR> ended prev line */

	for(;;) {
	     c=getc(fd);
			/* End of line is taken as <LF> or <CR>, for
			   compatibility with both Unix and Mac.  A
			   <CR> is followed by <LF> in MS-DOS. In that
			   case, skip over the <LF>.  We do not support
			   <LF><CR>.  Hope that's nobody's convention.
			*/
	     if( c == '\n' ) {	/* <LF> case */
		  if( prev_CR ) {
		       prev_CR = FALSE; /* ignore <LF> after <CR> */
		       continue;
		  }
		  else {
		       break;
		  }
	     }
	     else if( c == '\r' ) { /* <CR> case */
		  prev_CR = TRUE;
		  break;
	     }
	     prev_CR = FALSE;

		if(c == EOF)
			return NULL;

		if(i < n-1)
			s[i++] = c;
	}
	s[i] = '\0';
	return s;
}


	/* Functions which look ahead as far as end of line to see if input
	   cursor is sitting at start of a token of the given class.  Used
	   to resolve ambiguities that need more than one token of lookahead.
	   */

int
looking_at_cplx(VOID)
{
    int indx;

    if( next_char != EOS )	/* Looking at next line already */
    {
	indx = next_index;

	if( (indx = see_a_number(indx,line,FALSE)) < 0 )
	  return FALSE;
	while(line[indx] != '\0' && isspace(line[indx]))
	  indx++;

	if( line[indx] != ',' )
	  return FALSE;
	++indx;

	if( (indx = see_a_number(indx,line,FALSE)) < 0 )
	  return FALSE;
	while(line[indx] != '\0' && isspace(line[indx]))
	  indx++;

	if(line[indx] != ')')
	  return FALSE;
    }

    return TRUE;	/* passed all the tests */

}

int
#if HAVE_STDC
looking_at_keywd(int token_class)
	                	/* Keyword class to be checked out */
#else /* K&R style */
looking_at_keywd(token_class)
	int token_class;	/* Keyword class to be checked out */
#endif /* HAVE_STDC */
{
				/* Distinguishing identifier from keyword.
				   If not sure, assumes true.   Ambiguity
				   must be resolved in current line. */
    int indx;
    int c;

    if( next_char != EOS )	/* Looking at next line already */
    {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,"\nlooking_at: curr_char=%c then %c",
curr_char,line[next_index]);
#endif
				/* Skip over leading
				   stuff that could be rest of identifier */

	if(isidletter(curr_char) || isdigit(curr_char) ||
	   isspace(curr_char)){
	  indx = skip_idletters(next_index,line);
	  c = line[indx];	/* Store following character in c */
	  ++indx;   /* Leave index pointing at char after c */
	}
	else {
	  c = curr_char;	/* Otherwise next input char is c */
	  indx = next_index;
	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," c=%c then %c",c,line[indx]);
#endif

				/* An initial identifier followed by single
				   : has to be a construct name, followed
				   by :: has to be a keyword.
				 */
	if( c == ':' ) {
	  while( isspace( (c=line[indx]) ) )
	    ++indx;
	  return (c == ':');
	} 

	if(token_class == tok_DO) {
	  int opt_comma = FALSE;

		/* DO: we must by now have skipped over optional label
		  to optional comma or over optional label and
		  variable name to = sign.  Look for expression and comma.
		  DOWHILE will be found as single keyword, but we have
		  to spot DO label WHILE(expr) here.  DO of END DO
		  is not seen here. */

	  WHILE_expected = FALSE; /* most cases do not use it */

	  if(c == ',' && isdigit(curr_char)) {
				/* Skip optional comma after label.
				   First, back up and check that we saw
				   only digits so far. Do it here since
				   this is rare and not worth cluttering
				   the foregoing code. */
	    int i=next_index;
	    while(isdigit(line[i]) || isspace(line[i]))
	      ++i;
	    if(line[i] != ',')
	      return FALSE;
				/* Checks out OK: */
	    indx = skip_idletters(indx,line);	/* skip DO index or WHILE */
	    c = line[indx];
	    ++indx;
	    opt_comma = TRUE;
	  }

	  if(c == '=') {	/* Traditional DO form */
	    indx = see_expression(indx,line);
	    return (indx != -1 && line[indx] == ',') || opt_comma;
	  }
	  else {		/* Nonstandard variants */
	    if(c == '(') {
				/* DO label WHILE (expr): rescan from the
				   word DO to see if it fits. */
	      if( see_dowhile(next_index,line) != -1 )
		WHILE_expected = TRUE;
	      return WHILE_expected || opt_comma;
	    }
	    else
	      return opt_comma;	/* The comma is found only in DO forms */
	  }
	}/* end of tok_DO forms */

		/* Otherwise, look for an assignment statement.  If there
		   is no left paren, then must be an equals sign here
		   if it is an assignment statement. */
	if(c != '(') {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,"\n Conclude %s",
	(c != '=')? "keyword": "assignment stmt");
#endif
	      return (c != '=');
	}

	else {			/* sitting at parenthesis */

		/* Skip to end of balancing parenthesis. Then if = sign, it
		   must be an assignment statement.  If ( is found,
		   presumably it is an array substring assignment. So skip
		   once more to check for the = sign.) */


	indx = skip_balanced_parens(indx,line);

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," to %c",line[indx]);
#endif

	if(line[indx] == '(') {
	  ++indx;		/* Move past the paren */
	  indx = skip_balanced_parens(indx,line);

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," to %c",line[indx]);
#endif

	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd," conclude %s",line[indx]!= '='?"keyword":"variable");
#endif

	return (line[indx] != '=');
      }
    }
				/* End of line: must be a keyword */
    return TRUE;

}/*looking_at_keywd*/

		/* This guy is called when an integer is followed by '.'
		   in cases where a real number or expression is allowed.
		   When an integer is followed by .E, it can either be a real
		   like 1.E10, or a comparison like (1.EQ.I).  This requires
		   looking for the 'Q' after the 'E'.  The other cases,
		   like ... 1.AND. ... are resolved by looking at next_char
		   to see if it is the 'D' of a d.p. constant or not.
		  */
int
looking_at_relop(VOID)
{
    int indx;
    int c;


    if( next_char != EOS )	/* Looking at next line already */
    {

#if 0				/* With closeup() this is no longer valid */
    if( eol_is_space && line_num != next_line_num )
	return FALSE;	/* Looking at next line already */
#endif
	indx = next_index;/* Start at next_char */

	while( (c=line[indx]) != '\0' && isspace(c))
	  ++indx;

	if( !isaletter( c ) )	/* next char must be letter */
		return FALSE;
	c = makeupper(c);
	if( c == 'D' )	/* D.P. exponent */
	  return FALSE;	/* No dotted keywords start with D */
	if( c == 'Q' )	/* Q.P. exponent */
	  return FALSE;	/* No dotted keywords start with Q */

			/* If next char is any other letter but 'E', cannot be
			    exponent.  If it is 'E', must be EQ or EQV to
			    be a relop.  So look ahead for the 'Q'. */
	else if( c == 'E' ) {
	  do {
	    ++indx;
	  } while( (c=line[indx]) != '\0' && isspace(c));

	  c = makeupper(c);
	  return (c == 'Q');
	}
	else		/* Next char not D or E: must be a dotted keyword */
	  return TRUE;
    }
				/* If EOS, then it is stmt like x=1. */
    return FALSE;

}

		/* This routine is called for an IMPLICIT statement if the
		   type keyword is followed by a left parenthesis, and
		   returns TRUE if what follows is a letter list and not
		   an F90 length-selector or kind-selector.  If insufficient
		   lookahead to tell, it returns TRUE as more likely.
		*/
int
looking_at_implicit_list(VOID)
{
    int indx;
    int c;

    if( next_char != EOS )	/* Looking at next line already */
    {
      int letter_next_up;
      indx = next_index;/* Start at next_char */

		/* scan across a letter-list inside parentheses */
      letter_next_up = TRUE;
      while( (c=line[indx]) != '\0' && c != ')' ) {
	if(! isspace(c) ) {
	  if(letter_next_up) {
	    if(! isaletter(c) )
	      return FALSE;	/* can't be letter-list */
	    letter_next_up = FALSE;
	  }
	  else {
	    if( c == ',' )
	      return TRUE;	/* can't be length or kind selector */
	    if( c != '-' )
	      return FALSE;	/* can't be letter-list */
	    letter_next_up = TRUE;
	  }
	}
	++indx;
      }

      if( c == ')' )		/* skip past closing paren */
	++indx;

		/* Having scanned what might be a letter-list, it still could
		   be an expression e.g. M-N where M and N are parameters.
		   If we are looking at a len or kind spec, then next nonblank
		   must be the left paren of the letter list.
		 */
      while( (c=line[indx]) != '\0' && isspace(c) )
	++indx;
      if(c != '\0')
	return (c != '(');
    }
    return TRUE;	/* End of line: guess that it isn't len/kind spec */
}

	/* see_a_number returns -1 if there is no valid numeric constant
	   in string s starting at index i.  If valid number found, it
	   returns the index of the next character after the constant.
	   Leading whitespace in s is skipped.*/


#define SKIP_SPACE    while(s[i] != '\0' && isspace(s[i])) i++

PRIVATE int
#if HAVE_STDC
see_a_number(int i, char *s, int can_be_holl)
                   /* context indication */
#else /* K&R style */
see_a_number(i,s,can_be_holl)
   int i;
   char s[];
   int can_be_holl;/* context indication */
#endif /* HAVE_STDC */
{
   int digit_seen = FALSE;
   int isave;
   while(s[i] != '\0' && isspace(s[i]))
     i++;

			/* move past optional preceding sign */
   if(s[i] == '-' || s[i] == '+' ) {
     i++;
     SKIP_SPACE;
     can_be_holl = FALSE;
   }
   isave=i;

		/* move past ddd or ddd. or .ddd or ddd.ddd */
   if(isdigit(s[i]))
     digit_seen = TRUE;
   while(isdigit(s[i])) {
     i++;
     SKIP_SPACE;
   }
   if(s[i] == 'H' && can_be_holl) {
     return skip_hollerith(isave,s);
   }
   if(s[i] == '.') {
     i++;
     SKIP_SPACE;
     if(isdigit(s[i]))
       digit_seen = TRUE;
     while(isdigit(s[i])) {
       i++;
       SKIP_SPACE;
     }
   }

		/* no digits seen: bail out now */
   if(! digit_seen)
     return -1;

		/* look for exponential part.  The standard does not
		   allow D or Q, but we will, just in case. */
   if(makeupper(s[i]) == 'E' || makeupper(s[i]) == 'D' ||
      makeupper(s[i]) == 'Q') {
     i++;
     SKIP_SPACE;
     if(s[i] == '+' || s[i] == '-') {
       i++;
       SKIP_SPACE;
     }
     if(!isdigit(s[i]))
       return -1;
     while(isdigit(s[i]) || isspace(s[i]))
       i++;
   }

   return i;
}/*see_a_number*/

	/* see_dowhile returns TRUE only if the stuff following the initial
	   DO is a label and the word WHILE followed by a parenthesized expr.
	   If not resolved on current line, assumes TRUE (how many arrays
	   are named DO10WHILE?).  The "DO WHILE" form is not handled
	   here so that DOWHILE will be gotten as a single token later.
	 */
PRIVATE int
#if HAVE_STDC
see_dowhile(int indx, char *ll)
#else /* K&R style */
see_dowhile(indx,ll)
     int indx;
     char ll[];
#endif /* HAVE_STDC */
{
    int c;
				/* Skip over the label */
    while(isdigit(c=ll[indx]) || isspace(c) )
      ++indx;

    if(c == ',')		/* Skip optional comma */
      ++indx;

    indx = see_keyword(indx,ll,"WHILE");

    if( indx == -1 || ll[indx] != '(')  /* Look for the opening paren */
      return -1;

    ++indx;			/* skip the opening paren */
    indx = skip_balanced_parens(indx,ll);
				/* Only = sign can follow the parens if this
				  is not a do-while. */
    return (ll[indx] != '=')? indx: -1;
}/*see_dowhile*/

		/* Routine to look ahead for the double colon that signals
		   an F90 form of declaration.
		 */
int
see_double_colon(VOID)
{
    int indx;
    if( next_char != EOS )	/* Looking at next line already */
    {
	indx = next_index;
	while(line[indx] != '\0') {

				/* Skip past commas */
	  if(line[indx] == ',')
	    ++indx;
				/* Check for a double colon */
	  else if(line[indx] == ':') {
	    while(isspace(line[++indx]))
	      continue;
	    if(line[indx] == ':')
	      return TRUE;
	  }
				/* Other possible things look like exprs */
	  else {
	    int old_indx = indx;
	    indx = see_expression(indx,line);
	    if( old_indx == indx ) /* if no expr there, give up */
	      break;
	  }
	}
    }
    return FALSE;
}

	/* Crude routine to scan forward past arithmetic expressions.
	   Function invocations and array or character elements will
	   have their parentheses skipped by skip_balanced_parens;
	   outside parens a comma will cause a halt.  Returns the index
	   of the nonblank character following the expression, or
	   -1 if something non-kosher was found (e.g. a faulty number)
	   It can be confused by holleriths containing significant
	   characters, i.e. ( ) ' !  and occurring outside parentheses.
	 */
PRIVATE int
#if HAVE_STDC
see_expression(int indx, char *ll)
#else /* K&R style */
see_expression(indx,ll)
     int indx;
     char ll[];
#endif /* HAVE_STDC */
{
    int c;
    while(indx != -1 && (c=ll[indx]) != '=' && c != '\0') {
	if(isidletter(c))
	  indx = skip_idletters(indx,ll);
	else if(isdigit(c))
	  indx = see_a_number(indx,ll,TRUE);
	else if(isspace(c))
	  ++indx;
	else if(c == '(')
	  indx = skip_balanced_parens(indx+1,ll);
	else if(c == '+' || c == '-' || c == '/' || c == '*' || c == '.')
	  ++indx;
	else if(c == '\'' || c == '"')	/* embedded strings confuse things */
	  indx = skip_quoted_string(indx,ll);
	else break;
    }
    return indx;
}/*see_expression*/

	/* see_keyword returns -1 if the line (ignoring blanks and
	   uppercasing alphabetics) does not match the given string
	   matchstr.  If it does match, returns index of next nonspace
	   character. Note that index must be at start of keyword. */

PRIVATE int
#if HAVE_STDC
see_keyword(int indx, char *ll, char *matchstr)
#else /* K&R style */
see_keyword(indx,ll,matchstr)
     int indx;
     char ll[];
     char *matchstr;
#endif /* HAVE_STDC */
{
    int c;
    while(*matchstr != 0 && (c=ll[indx]) != '\0') {
      if(! isspace(c) ) {
	if(makeupper(c) != *matchstr++)
	  return -1;
      }
      ++indx;
    }
    if(*matchstr == '\0') {	/* Match found */
      while(isspace(ll[indx]))
	++indx;
      return indx;
    }
    else			/* No match */
      return -1;
}/*see_keyword*/

		/* skip_balanced_parens returns index of the nonspace character
		   following the closing ')' that balances the opening
		   '(' preceding ll[indx], or of final nul if the
		   parentheses are not balanced within the line.
		*/
PRIVATE int
#if HAVE_STDC
skip_balanced_parens(int indx, char *ll)
#else /* K&R style */
skip_balanced_parens(indx,ll)
     int indx;
     char ll[];
#endif /* HAVE_STDC */
{
  int depth=1;		/* nesting depth in parens */
  int prevchar = '+';	/* arbitrary punctuation */
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,"\nskipping ()...");
#endif

  while(ll[indx] != '\0' && depth > 0) {
#ifdef INLINE_COMMENT_CHAR
    if(ll[indx] == INLINE_COMMENT_CHAR) /* inline comment ends line */
      break;
#endif
    if(ll[indx] == '\'' || ll[indx] == '"') {	/* embedded strings confuse things */
      indx = skip_quoted_string(indx,ll);
      prevchar = 'X';	/* Arbitrary non punctuation */
    }
    else if(ispunct(prevchar) && isdigit(ll[indx])) {
      indx = skip_hollerith(indx,ll); /* Skip hollerith or number */
      prevchar = ll[indx];
    }
    else {
				/* Keep track of nesting */
      if     (ll[indx] == '(') ++depth;
      else if(ll[indx] == ')') --depth;

      if(! isspace(ll[indx]) )
	prevchar = ll[indx];

      ++indx;
    }
  }

				/* We are now past the closing paren */
  while(ll[indx] != '\0' && isspace(ll[indx]))
    indx++;		/* skip trailing space */

  return indx;
}/*skip_balanced_parens*/


		/* skip_idletters returns index of the nonspace character
		   following a string of idletters: alphabetic characters
		   or digits, or underscore or dollar if those options are
		   enabled.  It does not look out for hollerith constants.
		*/
PRIVATE int
#if HAVE_STDC
skip_idletters(int indx, char *ll)
#else /* K&R style */
skip_idletters(indx,ll)
     int indx;
     char ll[];
#endif /* HAVE_STDC */
{
	int c;
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
(void)fprintf(list_fd,": skipping letters...");
#endif
	while(c=ll[indx],
	      (isidletter(c) || isadigit(c) || isspace(c)))
	  ++indx;
	return indx;
}/*skip_idletters*/

		/* Returns index of nonspace character following
		   quote mark that closes string whose opening quote
		   mark is before index. */
PRIVATE int
#if HAVE_STDC
skip_quoted_string(int indx, char *ll)
#else /* K&R style */
skip_quoted_string(indx,ll)
     int indx;
     char ll[];
#endif /* HAVE_STDC */
{
  int c;
  int start_quote_char = ll[indx++];	/* get opening quote char: ' or " */
  while( (c=ll[indx]) != '\0') {
    if( source_unix_backslash && c == '\\' ) {	/* skip any escaped char */
      if(ll[++indx] == '\0')	/* (check just in case) */
	  break;
    }
    if(c == start_quote_char) {	/* Closing quote? */
      if(ll[++indx] != start_quote_char) /* Quoted quote? */
	break;
    }
    ++indx;
  }

				/* We are now past the closing quote mark */
  while(ll[indx] != '\0' && isspace(ll[indx]))
    indx++;		/* skip trailing space */

  return indx;
}/*skip_quoted_string*/


			/* Skips holleriths.  Note: treats tabs within
			   hollerith as single characters. */
PRIVATE int
#if HAVE_STDC
skip_hollerith(int i, char *s)
#else /* K&R style */
skip_hollerith(i,s)
   int i;
   char s[];
#endif /* HAVE_STDC */
{
  int len=0;
  while(isdigit(s[i])) {
    len = len*10 + BCD(s[i]);
    i++;
    SKIP_SPACE;
  }
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd,"\nskip_hollerith: %d then %c:",
len,s[i]);
#endif
  if(makeupper(s[i]) != 'H')
    return i;

  i++;				/* Skip the 'H' */

  while(s[i] != '\0' && len > 0){ /* Move forward len characters */

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd,"%c",s[i]);
#endif
    --len; i++;
  }
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  (void)fprintf(list_fd," to %c",s[i]);
#endif
  return i;
}/*skip_hollerith*/


	/* Checks line for having nonblanks past col 72.  Used only for
	   fixed source format. */
PRIVATE int
#if HAVE_STDC
is_overlength(char *s, COLNO_t maxcol)
	        		/* The line to check */
	           		/* Max columns to check to */
#else /* K&R style */
is_overlength(s,maxcol)	/* checks line for having nonblanks past col 72 */
	char *s;		/* The line to check */
	COLNO_t maxcol;		/* Max columns to check to */
#endif /* HAVE_STDC */
{
	int i=0;
	COLNO_t col=1;

#ifdef DEC_TABS	/* support for DEC tab formatting */
	if(source_dec_tab && s[i] == '\t') {
	  ++i;
	  if( isadigit((int)s[i]) )
	    col = 6; /* continuation column */
	  else
	    col = 7; /* start of statement */
	}
#endif

	for( ; col<=maxcol && s[i] != '\0'; i++) {
			/* Inline comments are allowed to run past 72
			   columns without complaint.  The following test
			   will be fooled by ! in quote or hollerith, but
			   it isn't worth the trouble to catch those.  */
#ifdef INLINE_COMMENT_CHAR
	    if(col != 6 && s[i] == INLINE_COMMENT_CHAR)
	      return FALSE;
#endif
	    if(col > 72 && !isspace(s[i]))
		return TRUE;

			/* Count columns taking tabs into consideration */
	    if(s[i] == '\t')
		col = NXTTAB(col);
	    else
		++col;
	}
	return FALSE;
}/*is_overlength*/

/* End of module Advance */
