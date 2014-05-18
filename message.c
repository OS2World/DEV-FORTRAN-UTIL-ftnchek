/* $Id: message.c,v 1.11 2001/10/07 23:00:52 moniot Rel $

	Error and warning routines
*/

/*


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

PROTO(PRIVATE void error_message,( const char *filename, LINENO_t lineno,
	COLNO_t colno, const char *s, const char *tag ));
PROTO(PRIVATE void lintstyle_error_message,( const char *filename, LINENO_t lineno,
	COLNO_t colno, const char *s, const char *tag ));
PROTO(PRIVATE void oldstyle_error_message,( const char *filename, LINENO_t lineno,
	COLNO_t colno, const char *s, const char *tag ));

void
#if HAVE_STDC
lex_error(const char *s)
#else /* K&R style */
lex_error(s)
	char *s;
#endif /* HAVE_STDC */
{
	syntax_error(line_num,col_num,s);
}


void
#if HAVE_STDC
yyerror(const char *s)
#else /* K&R style */
yyerror(s)
	char *s;
#endif /* HAVE_STDC */
{
    static int parserror_explanation_given = FALSE;
    syntax_error(line_num,col_num,s);

			/* Novices are often thrown by terminology, so
			   we give them a little help.  This message is
			   only given once per run.
			 */
    if( novice_help && ! parserror_explanation_given &&
	strncmp(s,"parse error",sizeof("parse error")-1) == 0 ) {
	error_message((char *)NULL, NO_LINE_NUM, NO_COL_NUM,
    "(A parse error means that I am not able to make sense of this statement,",
		      (char *)NULL );
	msg_tail("because your program has broken some rule of Fortran syntax.)");
	parserror_explanation_given = TRUE;
    }
}


void
#if HAVE_STDC
syntax_error(LINENO_t lineno, COLNO_t colno, const char *s)		/* Syntax error message */
#else /* K&R style */
syntax_error(lineno,colno,s)		/* Syntax error message */
	LINENO_t lineno;
	COLNO_t colno;
	char *s;
#endif /* HAVE_STDC */
{
	++error_count;
	error_message(current_filename,lineno,colno,s,"Error");
}

void
#if HAVE_STDC
warning(LINENO_t lineno, COLNO_t colno, const char *s)  /* Print warning message */
#else /* K&R style */
warning(lineno,colno,s)
	LINENO_t lineno;
	COLNO_t colno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;

	error_message(current_filename,lineno,colno,s,"Warning");
}

void
#if HAVE_STDC
ugly_code(LINENO_t lineno, COLNO_t colno, const char *s) /* -pretty message */
#else /* K&R style */
ugly_code(lineno,colno,s)
	LINENO_t lineno;
	COLNO_t colno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;

	error_message(current_filename,lineno,colno,s,"Possibly misleading appearance");
}

void
#if HAVE_STDC
nonstandard(LINENO_t lineno, COLNO_t colno, int f90, int f95)
#else /* K&R style */
nonstandard(lineno,colno, f90, f95)
     LINENO_t lineno;
     COLNO_t colno;
     int f90, f95;
#endif /* HAVE_STDC */
{
	++warning_count;
	if( f95 ) {
	  error_message(current_filename,lineno,colno,"Syntax deleted in Fortran 95","Warning");
	}
	else {
	  error_message(current_filename,lineno,colno,"Nonstandard syntax","Warning");
	  if( f90 )
	    msg_tail("(not adopted in Fortran 90)");
	}
}

void
#if HAVE_STDC
nonportable(LINENO_t lineno, COLNO_t colno, const char *s) /* Print warning about nonportable construction */
#else /* K&R style */
nonportable(lineno,colno,s) /* Print warning about nonportable construction */
	LINENO_t lineno;
	COLNO_t colno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;
	error_message(current_filename,lineno,colno,s,"Nonportable usage");
}

		/* Routine to print messages from local symtab checking
		   routines.  */

void
#if HAVE_STDC
local_message(const char *filename, LINENO_t lineno, const char *s, const char *tag)
#else /* K&R style */
local_message(filename, lineno, s, tag)
	char *filename;
	LINENO_t lineno;
	char *s;
	char *tag;
#endif /* HAVE_STDC */
{
	error_message(filename,lineno,GLOBAL_NO_COL_NUM,s,tag);
}

		/* Routine to print messages from global checking
		   routines.  Here the filename is given as an
		   argument since it varies with each call.  */

void
#if HAVE_STDC
global_warning(const char *filename, LINENO_t lineno, const char *s)
#else /* K&R style */
global_warning(filename,lineno,s)
	char *filename;
	LINENO_t lineno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;
	error_message(filename,lineno,GLOBAL_NO_COL_NUM,s,"Warning");
}

void
#if HAVE_STDC
global_message(const char *filename, LINENO_t lineno, const char *s)
#else /* K&R style */
global_message(filename,lineno,s)
	char *filename;
	LINENO_t lineno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;
	error_message(filename,lineno,GLOBAL_NO_COL_NUM,s,NULL);
}

/* error_message prints out error messages and warnings.  It
   now comes in two flavors.  If using lintstyle_error_message(),
   messages are produced in style like UNIX lint:

	"main.f", line nn, col nn: Error: your message here

   Otherwise messages by oldstyle_error_message in old ftnchek style:

	Error near line nn col nn file main.f: your message here

   At this time, oldstyle_error_message is used when -novice is
   in effect, lintstyle_error_message otherwise.
*/

PRIVATE int errmsg_col;
	/* Crude macro to give number of digits in line and column numbers.
	   Used by line wrap computation. */
#define NUM_DIGITS(n) ((n)<10?1:((n)<100?2:((n)<1000?3:(n)<10000?4:5)))

PRIVATE void
#if HAVE_STDC
error_message(const char *filename,LINENO_t lineno, COLNO_t colno, const char *s, const char *tag)
#else /* K&R style */
error_message(filename,lineno,colno,s,tag)
	char *filename;
	LINENO_t lineno;
	COLNO_t colno;
	char *s,*tag;
#endif /* HAVE_STDC */
{
  if(novice_help)
    oldstyle_error_message(filename,lineno,colno,s,tag);
  else
    lintstyle_error_message(filename,lineno,colno,s,tag);
}

PRIVATE void
#if HAVE_STDC
lintstyle_error_message(const char *filename,LINENO_t lineno, COLNO_t colno, const char *s, const char *tag)
#else /* K&R style */
lintstyle_error_message(filename,lineno,colno,s,tag)
	char *filename;
	LINENO_t lineno;
	COLNO_t colno;
	char *s,*tag;
#endif /* HAVE_STDC */
{
	COLNO_t icol;
	extern LINENO_t prev_stmt_line_num; /* shared with advance.c */

	errmsg_col=1;		/* Keep track of line length */

			/* Print the character ^ under the column number.
			   But if colno == 0, error occurred in prior line.
			   If colno is NO_COL_NUM, then print message
			   without any column number given.  N.B. colno ==
			   GLOBAL_COL_NUM means this is from local or global
			   message routines, don't flush line out.
			 */

	if(lineno != NO_LINE_NUM && colno != GLOBAL_NO_COL_NUM) {
	    if(colno == NO_COL_NUM) {
		    /* colno == NO_COL_NUM means don't give column number.*/
		(void)flush_line_out(lineno);/* print line if not printed yet */
	    }
	    else if(colno != 0) {
			/* print line if not printed yet */
		if( flush_line_out(lineno) ) {
				/* If it was printed, put ^ under the col */
		    (void)fprintf(list_fd,"\n%8s","");

		    for(icol=1; icol<colno; icol++)
			(void)fprintf(list_fd," ");
		    (void)fprintf(list_fd,"^");
		}
	    }
	    else {		/* colno == 0 */
			/* print line if not printed yet */
		(void)flush_line_out(prev_stmt_line_num);
	    }
	}

	if( filename != (char *)NULL ) {
	    (void)fprintf(list_fd,"\n\"%s\"",filename);
	    errmsg_col += 2+(int)strlen(filename);
	}

	if(lineno != NO_LINE_NUM) { /* nonlocal error-- don't flush */
	    if(colno == NO_COL_NUM) {
		(void)fprintf(list_fd,
		   ", near line %u",lineno);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	    else if(colno == GLOBAL_NO_COL_NUM) {
		(void)fprintf(list_fd,
		   ", line %u",lineno);
		errmsg_col += 7+NUM_DIGITS(lineno);
	    }
	    else if(colno != 0) {
		(void)fprintf(list_fd,
		   ", line %u col %u",lineno,colno);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	    else {		/* colno == 0 */
		(void)fprintf(list_fd,
		   ", near line %u",prev_stmt_line_num);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	}

	if( tag != (char *)NULL ) {
	    msg_tail(":");
	    msg_tail(tag); /* "Warning", "Error", etc. */
	}
	if( s != (char *)NULL ) {
	    msg_tail(":");
	    msg_tail(s); /* now append the message string */
	}
}

				/* Our own style messages */
PRIVATE void
#if HAVE_STDC
oldstyle_error_message(const char *filename,LINENO_t lineno, COLNO_t colno, const char *s, const char *tag)
#else /* K&R style */
oldstyle_error_message(filename,lineno,colno,s,tag)
	char *filename;
	LINENO_t lineno;
	COLNO_t colno;
	char *s,*tag;
#endif /* HAVE_STDC */
{
	COLNO_t icol;
	extern LINENO_t prev_stmt_line_num; /* shared with advance.c */

	errmsg_col=1;		/* Keep track of line length */

			/* Print the character ^ under the column number.
			   But if colno == 0, error occurred in prior line.
			   If colno is NO_COL_NUM, then print message
			   without any column number given.  If tag is NULL,
			   this is an error_report from global checks, so
			   no location information is printed.
			 */

	if( tag == (char *)NULL ) {
	    (void)fprintf(list_fd,"\n");
	}
	else {
	  if(lineno == NO_LINE_NUM) { /* nonlocal error-- don't flush */
	      (void)fprintf(list_fd,"\n%s",tag);
	      errmsg_col += strlen(tag);
	  }
	  else {
	    if(colno == NO_COL_NUM) {
		    /* colno == NO_COL_NUM means don't give column number.*/
		(void)flush_line_out(lineno);/* print line if not printed yet */

		(void)fprintf(list_fd,"\n%s near line %u",tag,lineno);
		errmsg_col += 11+NUM_DIGITS(lineno)+(unsigned)strlen(tag);
	    }
				/* global warnings don't have column numbers
				   but line number is exact.
				 */
	    else if(colno == GLOBAL_NO_COL_NUM) {
		(void)fprintf(list_fd,
			      "\n%s at line %u",tag,lineno);
		errmsg_col += 9+NUM_DIGITS(lineno)+(unsigned)strlen(tag);
	    }
	    else if(colno != 0) {
			/* print line if not printed yet */
		if( flush_line_out(lineno) ) {
				/* If it was printed, put ^ under the col */
		    (void)fprintf(list_fd,"\n%8s","");

		    for(icol=1; icol<colno; icol++)
			(void)fprintf(list_fd," ");
		    (void)fprintf(list_fd,"^");
		}
		(void)fprintf(list_fd,
			      "\n%s near line %u col %u",tag,lineno,colno);
		errmsg_col += 16+NUM_DIGITS(lineno)+NUM_DIGITS(colno)
			+(unsigned)strlen(tag);
	    }
	    else {		/* colno == 0 */
			/* print line if not printed yet */
		(void)flush_line_out(prev_stmt_line_num);
		(void)fprintf(list_fd,
			      "\n%s near line %u",tag,prev_stmt_line_num);
		errmsg_col += 11+NUM_DIGITS(lineno)+(unsigned)strlen(tag);
	    }
	  }


	  if((!full_output	/* Append file name if not listing */
	   || doing_wrapup	/* or if this is a global error message */
	   || (doing_end_proc	/* or a local message referring to inc file */
	       && filename != top_filename)
	   || incdepth > 0)	/* Append include-file name if we are in one */
	     && filename != (char *)NULL ) { /* skip if multi-file message */
	    if(lineno == NO_LINE_NUM) { /* if no line no, preposition needed */
		(void)fprintf(list_fd," in");
		errmsg_col += 3;
	    }
	    (void)fprintf(list_fd," file %s",filename);
	    errmsg_col += 6+(unsigned)strlen(filename);
	  }
	}/*end if(tag != NULL)*/

	if( s != (char *)NULL ) {
	    if( tag != (char *)NULL ) {
		msg_tail(":");
	    }
	    msg_tail(s); /* now append the message string */
	}
}

		/* msg_tail appends string s to current error message.
		   It prints one word at a time, starting a new line
		   when the message gets to be too long for one line.
		 */
void
#if HAVE_STDC
msg_tail(const char *s)
#else /* K&R style */
msg_tail(s)
    char *s;
#endif /* HAVE_STDC */
{
    if( s[0] != '\0' ) {

	int wordstart,wordend,leading_skip,wordchars;


		/* Insert blanks between items.  Exceptions:
		   colon, semicolon, closing paren
		   are used at start of some items and should
		   not be separated from preceding item.
		*/
	if( s[0] != ':' && s[0] != ')' && s[0] != ';' ) {
	    (void)fprintf(list_fd," ");
	    errmsg_col++;
	}

		/* Each iteration of loop prints leading space and the
		   nonspace characters of a word.  Loop invariant: wordstart
		   is index of leading space at start of word, wordend is
		   index of space char following word. */
	wordstart=0;
	while(s[wordstart] != '\0') {
	  leading_skip = TRUE;
	  for(wordend=wordstart; s[wordend] != '\0'; wordend++) {
	    if(leading_skip) {	/* If skipping leading space chars */
	      if(!isspace(s[wordend]))
		leading_skip = FALSE; /* go out of skip mode at nonspace */
	    }
	    else {		/* If scanning word chars */
	      if(isspace(s[wordend]))
		break;		/* quit loop when space char found */
	    }
	  }
	  wordchars = wordend-wordstart;
				/* If word doesn't fit, wrap to next line */
	  if( wrap_column > 0 && (errmsg_col += wordchars) > wrap_column) {
				/* At start of line, replace zero or more
				   blanks by one blank. */
	    (void)fprintf(list_fd,"\n ");
	    while(isspace(s[wordstart])) {
		++wordstart;
		--wordchars;
	    }
	    errmsg_col = wordchars+1;
	  }
				/* Print the word */
	  while(wordstart < wordend) {
	    (void)putc(s[wordstart++],list_fd);
	  }
	}
    }
}


void
#if HAVE_STDC
oops_message(int severity, LINENO_t lineno, COLNO_t colno, const char *s)
#else /* K&R style */
oops_message(severity,lineno,colno,s)
	int severity;
	LINENO_t lineno;
	COLNO_t colno;
	char *s;
#endif /* HAVE_STDC */
{
	(void)fflush(list_fd);
	(void)fprintf(stderr,"\nOops");
	if(lineno != NO_LINE_NUM) {
	  (void)fprintf(stderr," at line %u",lineno);
	  if(colno != NO_COL_NUM)
	    (void)fprintf(stderr," at col %u",colno);
	}
	(void)fprintf(stderr," in file %s",current_filename);
	(void)fprintf(stderr," -- %s",s);
	if(severity == OOPS_FATAL) {
	  (void)fprintf(stderr,"\nFtnchek aborted\n");
	  exit(1);
	}
}

void
#if HAVE_STDC
oops_tail(const char *s)
#else /* K&R style */
oops_tail(s)
	char *s;
#endif /* HAVE_STDC */
{
	(void)fprintf(stderr," %s",s);
}


		/* Routine to convert a long unsigned int to a string.
		   This uses a static array, so only one call can be
		   in action at a time.  Intended for use when msg_tail
		   must print an integer.
		*/
char *
#if HAVE_STDC
ulongtostr(unsigned long num)
#else /* K&R style */
ulongtostr(num)
    unsigned long num;
#endif /* HAVE_STDC */
{
    static char str[MAX_ULONGTOSTR];
    (void)sprintf(str,"%lu",num);
    return str;
}
