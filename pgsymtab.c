/* $Id: pgsymtab.c,v 1.39 2001/10/07 22:59:51 moniot Rel $

	 Warning message routines for printing of global symbol table info

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

/*

    Shared functions defined:
	argcmp_error_head	prints intro to argument mismatch
	comcmp_error_head	prints intro to common mismatch
	arg_error_report	Follow-on message about an argument mismatch
	sub_error_report	Error message line about one subprogram
				   invocation
	com_error_report	Error message line about one common block
				   declaration
	comvar_error_report	Error message line about one common var
				   mismatch
	sort_gsymbols		sort a list of Gsymtab ptrs alphabetically

*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ftnchek.h"
#define PGSYMTAB
#include "symtab.h"
#include "pgsymtab.h"


			/* Private functions defined: */
PROTO(PRIVATE void arg_error_locate,( ArgListHeader *alh ));
PROTO(PRIVATE int cmp_error_head,(const char *name, const char *tag, const char *filename,
				  LINENO_t lineno, const char *msg ));
PROTO(PRIVATE void com_error_locate,( ComListHeader *clh ));
PROTO(PRIVATE void error_report,( const char *module_name,
				  const char *filename, LINENO_t lineno,
				  const char *topfile, LINENO_t top_lineno,
	     int i, const char *item_tag, const char *item_name, const char *msg ));
PROTO(PRIVATE void module_locate, (const char *name));
PROTO(PRIVATE void novice_err_locate,( const char *filename, LINENO_t linenum ));
PROTO(PRIVATE void novice_inc_locate,( const char *filename, const char *topfile,
				       LINENO_t top_linenum ));
PROTO(PRIVATE void report_intro,( const char *filename, LINENO_t lineno,
				  const char *topfile, LINENO_t top_lineno ));

			/* Var used to control spaces betw message blocks */
PRIVATE int global_warning_count=0;


/****  Definitions of shared functions ****/



		/* Intro line of warning about subprogram argument mismatches.
		 */
int
#if HAVE_STDC
argcmp_error_head(const char *name, ArgListHeader *alh, const char *msg)
#else /* K&R style */
argcmp_error_head(name, alh, msg)
    char *name;
    ArgListHeader *alh;
    char *msg;
#endif /* HAVE_STDC */
{
    return cmp_error_head(name,"Subprogram",
			  alh->filename,
			  alh->line_num,
			  msg);
}

		/* Ditto for common block declaration mismatches.
		 */
int
#if HAVE_STDC
comcmp_error_head(const char *name, ComListHeader *clh, const char *msg)
#else /* K&R style */
comcmp_error_head(name, clh, msg)
    char *name;
    ComListHeader *clh;
    char *msg;
#endif /* HAVE_STDC */
{
    return cmp_error_head(name,"Common block",
			  clh->filename,
			  clh->line_num,
			  msg);
}

		/* Follow-on message about an argument mismatch */
void
#if HAVE_STDC
arg_error_report(ArgListHeader *alh, const char *argtype, int i, const char *msg)
#else /* K&R style */
arg_error_report(alh, argtype, i, msg)
    ArgListHeader *alh;
    char *argtype;
    int i;
    char *msg;
#endif /* HAVE_STDC */
{
    error_report(alh->module->name,
		 alh->filename,alh->line_num,alh->topfile,alh->top_line_num,
		 i,argtype,alh->arg_array[i].name,msg);
}

		/* Formats an error message line about one subprogram
		   invocation.
		*/
void
#if HAVE_STDC
sub_error_report(ArgListHeader *alh, const char *msg)
#else /* K&R style */
sub_error_report(alh, msg)
    ArgListHeader *alh;
    const char *msg;
#endif /* HAVE_STDC */
{
    report_intro(alh->filename,alh->line_num,alh->topfile,alh->top_line_num);
    msg_tail(msg);
    arg_error_locate(alh);
}



		/* Formats an error message line about one common block
		   declaration.
		*/
void
#if HAVE_STDC
com_error_report(ComListHeader *clh, const char *msg)
#else /* K&R style */
com_error_report(clh, msg)
    ComListHeader *clh;
    char *msg;
#endif /* HAVE_STDC */
{
    report_intro(clh->filename,clh->line_num,clh->topfile,clh->top_line_num);
    msg_tail(msg);
    com_error_locate(clh);
}

		/* Formats an error message line about one common var mismatch.
		*/
void
#if HAVE_STDC
comvar_error_report(ComListHeader *clh, int i, const char *msg)
#else /* K&R style */
comvar_error_report(clh, i, msg)
    ComListHeader *clh;
    int i;
    char *msg;
#endif /* HAVE_STDC */
{
    error_report(clh->module->name,
		 clh->filename,clh->line_num,clh->topfile,clh->top_line_num,
		 i,"Variable",clh->com_list_array[i].name,msg);
}


/**** Definitions of private functions ****/

PRIVATE int at_position_printed;


	/* Increment error count, and if it is 1, print header for arg or com
	   mismatch error messages.  If it is past limit, print "etc"
	   and return TRUE, otherwise return FALSE.
	   */
PRIVATE int
#if HAVE_STDC
cmp_error_head(const char *name, const char *tag,
	       const char *filename, LINENO_t lineno, const char *msg)
#else /* K&R style */
cmp_error_head(name, tag,
	       filename, lineno, msg)
    char *name;
    char *tag;
    char *filename;
    LINENO_t lineno;
    char *msg;
#endif /* HAVE_STDC */
{
		/* stop after limit: probably a cascade */
	if( CASCADE_LIMIT(cmp_error_count) ) {
	  (void)fprintf(list_fd,"\n etc...");
	  return TRUE;
	}

			/* (For expert mode, line number helps smart editors,
			   but in novice mode it looks silly to have a line
			   number for a mismatch involving two lines.)
			 */
	if(novice_help) {
	    filename = (char *)NULL;
	    lineno = NO_LINE_NUM;
	}

	if(cmp_error_count == 1) {
				/* If -noquiet mode, put a space between
				   successive warnings.
				 */
	    if( (global_warning_count != 0) && !quiet)
		(void)fprintf(list_fd,"\n");
	    global_warning_count++;
	    global_warning(filename,lineno,tag);
	    msg_tail(name);
	    msg_tail(msg);
	}
	else {			/* for "and at position n" on new line */
	    global_message(filename,lineno," and");
	}
	at_position_printed = FALSE;

	return FALSE;
}



PRIVATE void
#if HAVE_STDC
error_report(const char *module_name, const char *filename, LINENO_t lineno,
	     const char *topfile, LINENO_t top_lineno,
	     int i, const char *item_tag, const char *item_name, const char *msg)
#else /* K&R style */
error_report(module_name, filename, lineno,
	     topfile, top_lineno,
	     i, item_tag, item_name, msg)
    char *module_name;
    char *filename;
    LINENO_t lineno;
    char *topfile;
    LINENO_t top_lineno;
    int i;
    char *item_tag;
    char *item_name;
    char *msg;
#endif /* HAVE_STDC */
{
    if( ! at_position_printed ) {
	char posn[12+3*sizeof(int)+2];
	(void)sprintf(posn,"at position %d:",i+1);
	msg_tail(posn);
	at_position_printed = TRUE;
    }

    report_intro(filename,lineno,topfile,top_lineno);
    msg_tail(item_tag);

    msg_tail(item_name);
		/* Print module name, and for -novice mode, location info
		   that was suppressed before.
		 */
    module_locate(module_name);

    if( novice_help ) {
				/* Error location itself */
	novice_err_locate(filename,lineno);
				/* Location where included */
	novice_inc_locate(filename,topfile,top_lineno);
    }

    msg_tail(msg);

}


PRIVATE void
#if HAVE_STDC
report_intro(const char *filename, LINENO_t lineno,
	     const char *topfile, LINENO_t top_lineno)
#else /* K&R style */
report_intro(filename, lineno,
	     topfile, top_lineno)
    char *filename;
    LINENO_t lineno;
    char *topfile;
    LINENO_t top_lineno;
#endif /* HAVE_STDC */
{
			/* In expert mode, if error is in include file,
			   need to give info about it.
			 */
    if( ! novice_help && filename != topfile) {
	global_message(filename,lineno,"(location of error)");
	global_message(topfile,top_lineno,"(where included)  ");
    }
    else {  
	global_message(filename,lineno,"  ");
    }
}

	/* Gives module and in novice mode line, filename for error messages
	 */
PRIVATE void
#if HAVE_STDC
arg_error_locate(ArgListHeader *alh)
#else /* K&R style */
arg_error_locate(alh)
     ArgListHeader *alh;
#endif /* HAVE_STDC */
{
				/* Module (subprogram) containing the error.
				   This gets printed in both modes. */
    module_locate(alh->module->name);

    if( novice_help ) {
				/* Error location itself */
	novice_err_locate(alh->filename,alh->line_num);
				/* Location where included */
	novice_inc_locate(alh->filename,alh->topfile,alh->top_line_num);
    }
}

PRIVATE void
#if HAVE_STDC
com_error_locate(ComListHeader *clh)
#else /* K&R style */
com_error_locate(clh)
     ComListHeader *clh;
#endif /* HAVE_STDC */
{
				/* Module (subprogram) containing the error.
				   This gets printed in both modes. */
    module_locate(clh->module->name);

    if( novice_help ) {
				/* Error location itself */
	novice_err_locate(clh->filename,clh->line_num);
				/* Location where included */
	novice_inc_locate(clh->filename,clh->topfile,clh->top_line_num);
    }
}

PRIVATE void
#if HAVE_STDC
module_locate(const char *name)
#else /* K&R style */
module_locate(name)
    const char *name;
#endif /* HAVE_STDC */
{
    msg_tail("in module");
    msg_tail(name);
}

			/* Non-include part of location, novice mode */
PRIVATE void
#if HAVE_STDC
novice_err_locate(const char *filename, LINENO_t linenum)
#else /* K&R style */
novice_err_locate(filename,linenum)
     char *filename;
     LINENO_t linenum;
#endif /* HAVE_STDC */
{
    msg_tail("line");
    msg_tail(ulongtostr((unsigned long)linenum));
    msg_tail("file");
    msg_tail(filename);
}

			/* Include-file part of location, novice mode */
PRIVATE void
#if HAVE_STDC
novice_inc_locate(const char *filename, const char *topfile, LINENO_t top_linenum)
#else /* K&R style */
novice_inc_locate(filename,topfile,top_linenum)
    char *filename;
    char *topfile;
    LINENO_t top_linenum;
#endif /* HAVE_STDC */
{
    if(filename != topfile) { /* Track include filename */
	msg_tail("(included at line");
	msg_tail(ulongtostr((unsigned long)top_linenum));
	msg_tail("in");
	msg_tail(topfile);
	msg_tail(")");
    }
}


void
#if HAVE_STDC
sort_gsymbols (Gsymtab **glist, int n)   /* same as sort_lsymbols */
#else /* K&R style */
sort_gsymbols ( glist,n )
	Gsymtab *glist[];
	int n;
#endif /* HAVE_STDC */
{
	int i,j,swaps;

	for (i=0; i<n; i++ ){
	    swaps = 0;
	    for  (j=n-1; j>=i+1; j--){
		if ((strcmp (glist[j-1]->name, glist[j]->name)) >0) {
		    Gsymtab *temp = glist[j-1]; /* swap ptrs j and j-1 */
		    glist[j-1] = glist[j];
		    glist[j] = temp;
		    swaps++;
		}
	    }
	    if (swaps == 0) break;
	}


}
