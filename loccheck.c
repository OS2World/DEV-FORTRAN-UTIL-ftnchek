/* $Id: loccheck.c,v 1.17 2001/11/03 00:55:37 moniot Rel $

	Functions that do local checks on each subprogram.

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

		check_flags()     Outputs messages about used-before-set etc.
		check_mixed_common() checks common for nonportable mixed type
		find_sixclashes() Finds variables with the same first 6 chars.
		check_loose_ends()  Miscellaneous end-of-subprog checks

	Private functions defined:

		has_nonalnum()	  True if string has non-alphanumeric char

*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "plsymtab.h"
#include "loccheck.h"

				/* Declarations of local functions */

PROTO(PRIVATE int has_nonalnum,( char *s ));

			/* Find symbols with nonstd chars _ $  */
int
#if HAVE_STDC
find_nonalnum_names(Lsymtab **sym_list)
#else /* K&R style */
find_nonalnum_names(sym_list)
	Lsymtab *sym_list[];
#endif /* HAVE_STDC */
{
	int i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
			/* Find all names with nonstd chars, but
			   exclude internal names like %MAIN */
	       if(has_nonalnum(loc_symtab[i].name) &&
		  loc_symtab[i].name[0] != '%')	/* exception for internals */
		  sym_list[n++] = &loc_symtab[i];
	}
	return n;
}

	/* Search thru local symbol table for clashes where identifiers
	   are not unique in 1st six characters. Return value =
	   number of clashes found, with pointers to symbol table
	   entries of clashers in array list. */
int
#if HAVE_STDC
find_sixclashes(Lsymtab **list)
#else /* K&R style */
find_sixclashes(list)
	Lsymtab *list[];
#endif /* HAVE_STDC */
{
	int i,h, clashes=0;
	int stg_class;
	unsigned long hnum;

	for(i=0; i<loc_symtab_top; i++) {	/* Scan thru symbol table */
	    stg_class = storage_class_of(loc_symtab[i].type);
	    hnum = hash( loc_symtab[i].name );
				/* First look for a clash of any kind.
				   (N.B. this loop will never quit if hash
				   table is full, but let's not worry) */
	    while( (h=hnum % HASHSZ), hashtab[h].name != (char *)NULL) {
		/* Now see if the clashing name is used locally and still
		   clashes at 6 chars.  Treat common blocks separately. */

	     if((stg_class == class_COMMON_BLOCK &&
		  (
		   hashtab[h].com_loc_symtab != NULL
		   && strcmp( hashtab[h].name,loc_symtab[i].name) != 0
		   && strncmp(hashtab[h].name,loc_symtab[i].name,6) == 0
		  )
		)  ||
		 (stg_class != class_COMMON_BLOCK &&
		  (
		   hashtab[h].loc_symtab != NULL
		   && strcmp( hashtab[h].name,loc_symtab[i].name) != 0
		   && strncmp(hashtab[h].name,loc_symtab[i].name,6) == 0
		  )
		 )
	       ) {
				/* If so, then i'th symbol is a clash */

			list[clashes++] = &loc_symtab[i];
			break;
		}
		else {
		    hnum = rehash(hnum);
		}
	    }
	}
	return clashes;
}


void
#if HAVE_STDC
check_mixed_common(Lsymtab **sym_list, int n)
#else /* K&R style */
check_mixed_common(sym_list,n)
     Lsymtab *sym_list[];
     int n;
#endif /* HAVE_STDC */
{
    int i;
    for(i=0; i<n; i++) {
	ComListHeader *chead = sym_list[i]->info.comlist;
	ComListElement *clist;
	char *mod_name;
	int j,nvars;
	int has_char=FALSE,has_nonchar=FALSE;
	int prev_size = 0;
	  /* initialize to remove lint warning about use before definition */
	int this_size, this_type;

	if(chead == NULL)
	  continue;

	mod_name = chead->module->name;
	clist=chead->com_list_array;
	nvars = chead->numargs;

	for(j=0; j<nvars; j++) {

	   /* Check conformity to ANSI rule: no mixing char with other types */

	  if( (this_type=datatype_of(clist[j].type)) == type_STRING) {
	    has_char = TRUE;
	    this_size = 1;/* char type size is 1 for alignment purposes */
	  }
	  else { /* other types use declared sizes */
	    has_nonchar = TRUE;
	    if( (this_size=clist[j].size) == size_DEFAULT)
	      this_size = type_size[this_type];
	  }
	  if(has_char && has_nonchar) {
	    if(f77_mixed_common){
		local_warn_head(mod_name,
			       choose_filename(sym_list[i],file_declared),
			       sym_list[i]->line_declared,
			       (Lsymtab *)NULL, TRUE,
			       "Common block");
	      msg_tail(sym_list[i]->name);
	      msg_tail("has mixed character and non-character variables (nonstandard)");
	    }
	    break;
	  }

	/* Check that variables are in descending order of type size */

	 if(j > 0) {
	  if( this_size > prev_size ) {
	    if(port_common_alignment) {
	      local_warn_head(mod_name,
			     choose_filename(sym_list[i],file_declared),
			     sym_list[i]->line_declared,
			     (Lsymtab *)NULL, TRUE,
			     "Common block");
	      msg_tail(sym_list[i]->name);
	      msg_tail("has long data type following short data type (may not be portable)");
	    }
	    break;
	  }
	 }
	 prev_size = this_size;
	}
    }
}


void
#if HAVE_STDC
check_flags(Lsymtab **list, int n, unsigned int used,
	    unsigned int set, unsigned int ubs, const char *msg, const char *mod_name)
#else /* K&R style */
check_flags(list,n,used,set,ubs,msg,mod_name)
	Lsymtab *list[];
	int n;
	unsigned used,set,ubs;
	char *msg,*mod_name;
#endif /* HAVE_STDC */
{
	int matches=0,col=0,unused_args=0,i,len;
	unsigned pattern = flag_combo(used,set,ubs);

	for(i=0;i<n;i++) {
	    if( list[i]->common_var )	/* common vars are immune */
	       continue;
				/* for args, do only 'never used' and
				   then only if -usage=arg-unused given */
	    if( list[i]->argument &&
		(pattern != flag_combo(0,0,0) || ! usage_arg_unused ) )
		continue;

				/* skip 'never used' if non-arg and
				   -usage=var-unused not given */
	    if( !(list[i]->argument) &&
		pattern == flag_combo(0,0,0) && ! usage_var_unused )
		continue;

#ifdef ALLOW_INCLUDE
				/* Skip variables 'declared but not used'
				   and parameters 'set but never used'
				   if defined in include file. */

	    if( list[i]->defined_in_include &&
	       ( pattern == flag_combo(0,0,0)
	       || (list[i]->parameter && pattern == flag_combo(0,1,0)) ) )
		continue;
#endif
			/*  function return val: ignore 'set but never used' */
	    if( list[i]->entry_point && pattern == flag_combo(0,1,0) )
		continue;

	    if((unsigned)flag_combo(list[i]->used_flag,list[i]->set_flag,
	       list[i]->used_before_set) == pattern) {

				/* Brief report style gives module name
				   followed by simple list of offenders.
				 */
	       if( brief ) {
		 if(matches++ == 0) {
		     local_warn_head(mod_name,
				    top_filename,
				    NO_LINE_NUM,
				    (Lsymtab *)NULL, FALSE,
				    msg);
		     (void)fprintf(list_fd,"\n");
		 }
		 len = strlen(list[i]->name);
		 col += len = (len <= 10? 10: len) + 9;
		 if(col > 78) {
		   (void)fprintf(list_fd,"\n");
		   col = len;
		 }
		 (void)fprintf(list_fd,"%10s",list[i]->name);
				/* arg never used: tag with asterisk */
		 (void)fprintf(list_fd,"%-9s",
			 list[i]->argument? (++unused_args,"*") : "" );
		}/* brief */
				/* Verbose report style gives file name
				   and line number of each offender.
				 */
		else {
		    LINENO_t lineno;
		    int inc_index;
		    char *filename;
		    char *tag;
		    char detail[MAXIDSIZE+MAX_TAG_LEN+6]; /* see sprintf below */
		    if( ubs ) {
			choose_tag(TAG_USED,list[i],&tag,&lineno);
			inc_index = list[i]->file_used;
		    }
		    else if( set ) {
			choose_tag(TAG_SET,list[i],&tag,&lineno);
			inc_index = list[i]->file_set;
		    }
		    else {
			choose_tag(TAG_DEFN,list[i],&tag,&lineno);
			inc_index = list[i]->file_declared;
		    }

		    if(inc_index >= 0) {
			filename = incfile_list[inc_index].fname;
		    }
		    else {
			filename = top_filename;
		    }

		    if(matches++ == 0) {
			local_warn_head(mod_name,
				       filename,
				       lineno,
				       (Lsymtab *)NULL,
				       FALSE,
				       msg);
		    }
				/* Make detail e.g. "FOO used" */
		    sprintf(detail,"    %s %s",list[i]->name,tag);
		    local_detail(inc_index,lineno,(char *)NULL,detail);

				/* For used-before-set, say also not set
				   or say where set.
				 */
		    if( ubs ) {
			if( list[i]->set_flag ) {
			    choose_tag(TAG_SET,list[i],&tag,&lineno);
			    inc_index = list[i]->file_set;
			    sprintf(detail,"    %s %s",list[i]->name,tag);
			    local_detail(inc_index,lineno,(char *)NULL,detail);
			}
			else {
			    msg_tail("; never set");
			}
		    }
		    if(list[i]->argument) {
			++unused_args;
			msg_tail("(dummy argument)");
		    }
		}

		 matches++;
	    }
	}

	if(brief && unused_args > 0)
	    (void)fprintf(list_fd,"\n  * Dummy argument");

}

PRIVATE int
#if HAVE_STDC
has_nonalnum(char *s)	/* Returns TRUE if s contains a $ or _ character
			   and -f77 or -f90 is given. */
#else /* K&R style */
has_nonalnum(s)
   char *s;
#endif /* HAVE_STDC */
{
   while( *s != '\0' ) {
     if( !isalnum(*s) ) {
	 if( (*s) == '_' ) {
	     if(f77_underscores)
		 return TRUE;
	 }
	 else {			/* treat all non _ same as $ */
	     if(f77_dollarsigns||f90_dollarsigns)
		 return TRUE;
	 }
     }
     s++;
   }
   return FALSE;
}

void
#if HAVE_STDC
check_nonpure(Lsymtab* sym_list[], int n, char *mod_name)
#else /* K&R style */
check_nonpure(sym_list, n, mod_name)
    Lsymtab* sym_list[];
    int n;
    char *mod_name;
#endif
{
    int i,
	com_vars_modified=0,	/* count of common variables which are set */
	args_modified=0;	/* count of arguments which are set */
    for(i=0; i<n; i++) {
	if( (sym_list[i]->argument || sym_list[i]->common_var)
	    && sym_list[i]->set_flag) {
	    if( (sym_list[i]->argument && !CASCADE_LIMIT(args_modified))
		|| (sym_list[i]->common_var && !CASCADE_LIMIT(com_vars_modified)) )
	    {
		char *filename = choose_filename(sym_list[i],file_set);

		local_warn_head(mod_name,
			       filename,
			       sym_list[i]->line_set,
			       (Lsymtab *)NULL,
			       TRUE,
			       "Function");
		if(sym_list[i]->assigned_flag)
		    msg_tail("modifies");
		else
		    msg_tail("may modify");
		if(sym_list[i]->argument)
		    msg_tail("argument");
		else
		    msg_tail("common variable");
		msg_tail(sym_list[i]->name);
	    }
	    else {
		break;
	    }
	}
    }
			/* If quit early due to cascade limit, print "etc" */

    if(error_cascade_limit  > 0 &&
       (args_modified > error_cascade_limit
	|| com_vars_modified > error_cascade_limit)) {
	(void)fprintf(list_fd,"\netc...");
    }
}

		/* This routine catches syntax errors that have to
		   wait till END is seen.  At the moment, only looks if
		   CHARACTER*(*) declarations are put on the wrong thing.
		   Has to wait since can use it for ENTRY pt.
		   Also checks if things SAVED that shouldn't be.
		   Also fixes size_is_expression flags if IMPLICIT makes
		   the variable so.
		 */
void
#if HAVE_STDC
check_loose_ends(int curmodhash)
#else /* K&R style */
check_loose_ends(curmodhash)
     int curmodhash;    /* current_module_hash from fortran.y */
#endif /* HAVE_STDC */
{
  int i;
  for(i=0;i<loc_symtab_top;i++) {

				/* Catch illegal CHARACTER*(*) */
    if( datatype_of(loc_symtab[i].type) == type_STRING &&
	loc_symtab[i].size == size_ADJUSTABLE &&
       !(loc_symtab[i].argument ||
	   loc_symtab[i].parameter ||
	     loc_symtab[i].entry_point) ) {
	local_err_head(hashtab[curmodhash].name,
		     choose_filename(&loc_symtab[i],file_declared),
		     loc_symtab[i].line_declared,
		     &loc_symtab[i], TRUE,
		     loc_symtab[i].name);
	msg_tail("cannot be adjustable size");
    }

				/* Catch unSAVEable SAVE */
    if(loc_symtab[i].saved &&
        (loc_symtab[i].common_var ||
	 loc_symtab[i].argument ||
	 loc_symtab[i].external ||
	 loc_symtab[i].parameter ||
	 loc_symtab[i].entry_point) ) {
	local_err_head(hashtab[curmodhash].name,
		     choose_filename(&loc_symtab[i],file_declared),
		     loc_symtab[i].line_declared,
		     &loc_symtab[i], TRUE,
		     loc_symtab[i].name);
      msg_tail("cannot be declared in SAVE statement");
    }

			/* Common block misspelled in SAVE stmt will
			   show up as a SAVEd block with no elements */
    if(loc_symtab[i].saved &&
       datatype_of(loc_symtab[i].type) == type_COMMON_BLOCK &&
       loc_symtab[i].info.comlist == NULL) {
      if(misc_warn) {
	local_err_head(hashtab[curmodhash].name,
		     choose_filename(&loc_symtab[i],file_declared),
		     loc_symtab[i].line_declared,
		     &loc_symtab[i], TRUE,
		     loc_symtab[i].name);
	msg_tail("declared in SAVE statement but no such common block");
      }
    }

			/* If IMPLICIT CHARACTER*(expr) is used, then
			   need to fix flag to reflect it. */
    if(datatype_of(loc_symtab[i].type) == type_UNDECL &&
       get_size_text(&loc_symtab[i],type_UNDECL) != NULL) {
      loc_symtab[i].size_is_expression = TRUE;
    }
  }
}
