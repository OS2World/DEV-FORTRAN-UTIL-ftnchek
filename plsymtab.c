/* $Id: plsymtab.c,v 1.33 2003/03/26 01:16:28 moniot Exp $

		Routines associated with printing of local symbol table info

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

	local_err_head		Prints module name and file name (for errors)
	local_warn_head		Prints module name and file name (for warnings)
	debug_symtabs()		Prints debugging info about symbol tables
	choose_tag		Decides on tag & line no to use
	print_lsyms		Prints symbol lists
	print_lsyms_briefly	Brief symbol lists
	print_variables		Prints variable symbol table
	sort_lsymbols		Sorts a list of Lsymtab ptrs alphabetically
	sort_parameters		Sorts Lsymtab list by seq num instead of name

  Private functions defined:

	local_msg_head          Print error/warning head.
	print_lsyms_verbosely(sym_list,n,do_types) Verbose symbol lists.

*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ftnchek.h"
#define PLSYMTAB
#include "symtab.h"
#include "plsymtab.h"

				/* Declarations of local functions */
PROTO(PRIVATE void local_msg_head,( const char *problem, const char *mod_name,
				    const char *filename,
				    LINENO_t lineno, const Lsymtab *symt,
				    int force_lineno, const char *msg ));
PROTO(PRIVATE int print_lsyms_verbosely,( Lsymtab **sym_list,
					    int n, int do_types ));
PROTO(PRIVATE int print_var_type,( FILE *fd, const Lsymtab *symt ));

#ifdef DEBUG_SYMTABS
PROTO(PRIVATE void print_arg_array,( ArgListHeader *arglist ));
PROTO(PRIVATE void print_com_array,( ComListHeader *cmlist ));
PROTO(PRIVATE void print_tokenlist,( TokenListHeader *toklist ));
#endif


void
#if HAVE_STDC
sort_parameters(Lsymtab **sp, int n) /* sort a given list by sequence num instead of name */
#else /* K&R style */
sort_parameters(sp,n)
    Lsymtab **sp;
    int n;
#endif /* HAVE_STDC */
{
    int i,j,swaps;

    for (i = 0; i < n; i++)
    {
	swaps = 0;
	for (j = n-1; j >= i+1; j--)
	{
	    if ( sp[j-1]->info.param->seq_num > sp[j]->info.param->seq_num )
	    {
		Lsymtab *temp = sp[j-1]; /* swap ptrs j and j-1 */
		sp[j-1] = sp[j];
		sp[j] = temp;
		swaps ++;
	    }
	}
	if(swaps == 0)
	    break;
    }
}


void
#if HAVE_STDC
sort_lsymbols(Lsymtab **sp, int n)      /* bubble-sorts a given list */
#else /* K&R style */
sort_lsymbols(sp,n)
	Lsymtab **sp;
	int n;
#endif /* HAVE_STDC */
{
	int i,j,swaps;
	for(i=0;i<n;i++) {
	    swaps = 0;
	    for(j=n-1;j>=i+1;j--) {
		if((strcmp(sp[j-1]->name, sp[j]->name)) > 0) {
		   Lsymtab *temp = sp[j-1]; /* swap ptrs j and j-1 */
		   sp[j-1] = sp[j];
		   sp[j] = temp;
		   swaps ++;
		}
	    }
	    if(swaps == 0) break;
	}
}


void
#if HAVE_STDC
local_err_head(const char *mod_name, const char *filename, LINENO_t lineno, const Lsymtab *symt,
	       int force_lineno, const char *msg)
#else /* K&R style */
local_err_head(mod_name, filename, lineno, symt, force_lineno, msg)
     char *mod_name;	        /* name of module where warning reported */
     char *filename;            /* -1 if not an include file */
     LINENO_t lineno;
     Lsymtab *symt;		/* line number for expert-style warnings */
     int force_lineno;          /* print line number even if brief/novice */
     char *msg;                 /* error message */
#endif /* HAVE_STDC */
{
    local_msg_head("Error", mod_name, filename, 
		   lineno, symt, force_lineno, msg);

    ++error_count;
}


void
#if HAVE_STDC
local_warn_head(const char *mod_name, const char *filename, LINENO_t lineno, const Lsymtab *symt,
		int force_lineno, const char *msg)
#else /* K&R style */
local_warn_head(mod_name, filename, lineno, symt, force_lineno, msg)
     char *mod_name;		/* name of module where warning reported */
     char *filename;		/* file name within which problem occurred */
     LINENO_t lineno;		/* line number for expert-style warnings */
     Lsymtab *symt;		/* symbol table entry of the item */
     int force_lineno;          /* print line number even if brief/novice */
     char *msg;                 /* error message */
#endif /* HAVE_STDC */
{
    local_msg_head("Warning", mod_name, filename, lineno, symt,
		   force_lineno, msg);

    ++warning_count;
}

PRIVATE void
#if HAVE_STDC
local_msg_head(const char *problem, const char *mod_name, const char *filename, LINENO_t lineno, const Lsymtab *symt,
	       int force_lineno, const char *msg)
#else /* K&R style */
local_msg_head(problem, mod_name, filename, lineno, symt, force_lineno, msg)
     char *problem;             /* Error or Warning */
     char *mod_name;		/* name of module where warning reported */
     char *filename;		/* file name within which problem occurred */
     LINENO_t lineno;		/* line number for expert-style warnings */
     Lsymtab *symt;		/* symbol table entry of the item */
     int force_lineno;          /* print line number even if brief/novice */
     char *msg;                 /* error message */
#endif /* HAVE_STDC */
{
    char intro[MAXIDSIZE+19];	/* introduction to warning/error message */
    char *tag; /* placeholder in choose_tag call */

		/* Line number makes no sense in brief or novice modes,
		   except when no detail follows.  For that case, caller
		   sets force_lineno = 1.  In verbose expert mode, must
		   get the right line number to accompany message, matching
		   the line number that will appear with the first detail
		   item (pointed to symt).  Caller sets symt to NULL to cause
		   given lineno to be used.
		*/
    if( (brief || novice_help) && ! force_lineno) {
	lineno = NO_LINE_NUM;
    }
    else if( symt != (Lsymtab *)NULL ) {
	choose_tag(TAG_DEFN,symt,&tag,&lineno);
				/* Use include-file name if applicable */
	filename = choose_filename(symt,file_used);
    }

    if( ! quiet )		/* space between warning blocks */
	(void)fprintf(list_fd,"\n");
    (void)sprintf(intro,"%s in module %s",problem,mod_name);
    local_message(filename,lineno,msg,intro);

}



		/* Print list of symbols, either in brief many-per-line
		   style, or verbosely one-per-line.
		 */
int
#if HAVE_STDC
print_lsyms(Lsymtab **sym_list, int n, int do_types)
#else /* K&R style */
print_lsyms(sym_list,n,do_types)
     Lsymtab **sym_list;
     int n;
     int do_types;
#endif /* HAVE_STDC */
{
    if(brief)
	return print_lsyms_briefly(sym_list,n,do_types);
    else
	return print_lsyms_verbosely(sym_list,n,do_types);
}

     /* This routine prints symbol names in brief format.  If do_types is true
	also prints types, with * next to implicitly
	typed identifiers, and returns count thereof. */

int
#if HAVE_STDC
print_lsyms_briefly(Lsymtab **sym_list, int n, int do_types)
#else /* K&R style */
print_lsyms_briefly(sym_list,n,do_types)
     Lsymtab **sym_list;
     int n;
     int do_types;
#endif /* HAVE_STDC */
{
     int i,col=0,len,implicits=0;

     (void)fprintf(list_fd,"\n");

     for(i=0;i<n;i++) {
	  len = strlen(sym_list[i]->name);/* len=actual length of name */
				/* Revise len to max(10,len)+extra 9=width
				   of field to be printed.  Adjust column
				   count to see where this will take us. */
	  col += len = (len <= 10? 10: len) + 9;
				/* If this will run past 78 start a new line */
	  if(col > 78) {
	    (void)fprintf(list_fd,"\n");
	    col = len;
	  }
	  (void)fprintf(list_fd,"%10s",sym_list[i]->name);/* Print the name in 10 cols */

	  if( do_types ) {	/* Optionally print the datatype */
	    if(sym_list[i]->intrinsic)
	      (void)fprintf(list_fd,": intrns ");
	    else {
	      (void)fprintf(list_fd,":");
	      (void) print_var_type(list_fd,sym_list[i]);
	      if(datatype_of(sym_list[i]->type) == type_UNDECL) {
		implicits++; /* Flag and count undeclareds */
		(void)fprintf(list_fd,"*");
	      }
	      else if(sym_list[i]->size == size_DEFAULT)
		(void)fprintf(list_fd," ");
	      (void)fprintf(list_fd,"  ");
	    }
	  }
	  else			/* Otherwise just 9 blanks */
	    (void)fprintf(list_fd,"%9s","");
     }

     (void)fprintf(list_fd,"\n");

     return implicits;

}/*print_lsyms_briefly*/


     /* This routine prints symbol names in verbose format, one per line
	with line number where defined and a tag message ("declared" or
	"first occurrence"). If do_types is true, also prints types, with
	* next to implicitly typed identifiers, and returns count thereof. 
     */

PRIVATE int
#if HAVE_STDC
print_lsyms_verbosely(Lsymtab **sym_list, int n, int do_types)
#else /* K&R style */
print_lsyms_verbosely(sym_list,n,do_types)
     Lsymtab **sym_list;
     int n;
     int do_types;
#endif /* HAVE_STDC */
{
     int i,implicits=0;
     char msgbuf[6+MAX_TAG_LEN+MAXIDSIZE]; /* see sprintf below */
     for(i=0;i<n;i++) {
	 char *tag;
	 LINENO_t lineno;
	 short inc_index;
	 choose_tag(TAG_DEFN, sym_list[i], &tag, &lineno);

	 if( sprintf(msgbuf,"    %s %s", sym_list[i]->name, tag)
	     > (int)sizeof(msgbuf)-1 ) {
	     oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
			  "buffer too small in print_lsyms_verbosely for");
	     msg_tail(msgbuf);
	 }
	 inc_index = sym_list[i]->file_declared;
	 local_detail(inc_index,lineno,(char *)NULL,msgbuf);
	 if( do_types ) {	/* Optionally print the datatype */
	    if(sym_list[i]->intrinsic)
	      msg_tail("(intrinsic function)");
	    else {
	      int t,s;
	      t = get_type(sym_list[i]);
	      s = get_size(sym_list[i],t);
	      msg_tail("with type");
	      msg_tail(typespec(t,(s != size_DEFAULT),s,FALSE,0L));
	      if(datatype_of(sym_list[i]->type) == type_UNDECL) {
		implicits++; /* Flag and count undeclareds */
		msg_tail("(implicitly typed)");
	      }
	    }
	 }
     }

     return implicits;

}/*print_lsyms_verbosely*/

		/* This routine handles the messy business of tracing
		   a local warning detail back into an include file.  The
		   argument inc_index is the index into incfile_list
		   for the instance (declared, used, set) being reported.
		   If that is -1 then no include file is involved.  Otherwise
		   the table entry gives the include file name and the line
		   in top file where the file was included.
		*/
void local_detail(int inc_index, LINENO_t lineno,
		  const char *tag, const char *msg)
{
    char *fname;
    if( inc_index >= 0 ) {
	fname = incfile_list[inc_index].fname;
    }
    else {
	fname = top_filename;
    }
				/* Issue the main message here. */
    local_message(fname,lineno,tag,msg);

    if( inc_index >= 0 ) {
	local_message(top_filename,
		      incfile_list[inc_index].line,
		      tag,
		      novice_help?"     included":"    (where included)");
    }
}

	/* This routine prints the variables nicely, and returns
	    count of number implicitly defined.
	 */
int
#if HAVE_STDC
print_variables(Lsymtab **sym_list, int n)
#else /* K&R style */
print_variables(sym_list,n)
     Lsymtab **sym_list;
     int n;
#endif /* HAVE_STDC */
{
     int i,implicits=0,adjustables=0;

     (void)fprintf(list_fd,"\n ");

     for(i=0; i<4; i++) {
	  (void)fprintf(list_fd,"%5sName Type Dims","");
		      /* 12345678901234567890 template for above*/
     }
     for(i=0; i<n; i++) {

	  if(i % 4 == 0)
	     (void)fprintf(list_fd,"\n");
	  else
	     (void)fprintf(list_fd," ");

	  (void)fprintf(list_fd,"%10s",sym_list[i]->name);
	  adjustables += print_var_type(list_fd,sym_list[i]);

			/* Print a * next to implicitly declared variables */
	  if(datatype_of(sym_list[i]->type) == type_UNDECL ) {
	    implicits++;
	    (void)fprintf(list_fd,"*");
	  }
	  else if(sym_list[i]->size == size_DEFAULT)
	    (void)fprintf(list_fd," "); /* print blank if no size or * */


			/* print no. of dimensions next to var name */
	  if(sym_list[i]->array_var) {
		(void)fprintf(list_fd," %ld",
			       array_dims(sym_list[i]->info.array_dim));
	  }
	  else {
		(void)fprintf(list_fd,"%2s","");
	  }
    }

    if(adjustables > 0)
      (void)fprintf(list_fd,"\nchar+ indicates adjustable size");
    (void)fprintf(list_fd,"\n");

    return implicits;

}/*print_variables*/


PRIVATE int
#if HAVE_STDC
print_var_type(FILE *fd, const Lsymtab *symt)	/* Prints type name then size if explicit */
#else /* K&R style */
print_var_type(fd,symt)	/* Prints type name then size if explicit */
#endif /* HAVE_STDC */
			/* Returns 1 if adjustable size, else 0 */
#if HAVE_STDC
#else /* K&R style */
     FILE *fd;
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
  int adjustable=0;
  int t = get_type(symt);
  int s = get_size(symt,t);

	  (void)fprintf(fd," %4s",type_name[t]);

		/* Usually either size or * will be printed, and usually
		   size is 1 digit.  So mostly we print 1 column in
		   the next set of (void)fprintf's.  Output will be ragged
		   if size > 9 or implicit type has explicit size. */
	  if( s != size_DEFAULT ) {
	    if(t != type_STRING || s > 1)
	      (void)fprintf(fd,"%d",s);
	    else
	      if(s == size_ADJUSTABLE) {
		adjustable++;
		(void)fprintf(fd,"+");
	      }
	      else
		(void)fprintf(fd," ");
	  }
  return adjustable;
}



#ifdef DEBUG_SYMTABS
PRIVATE void
print_arg_array(arglist)	/* prints type and flag info for arguments */
	ArgListHeader *arglist;
{
	int i, count;
	ArgListElement *a;

	count = arglist->numargs;
	if(arglist->external_decl || arglist->actual_arg)
	  count = 0;
	a = arglist->arg_array;
	(void)fprintf(list_fd,
		"\n     Arg list in module %s file %s line %u",
		arglist->module->name,
		arglist->filename,
		arglist->line_num
	);
	if( arglist->topfile != arglist->filename )
	    (void)fprintf(list_fd,
		  " (topfile %s line %u)",
		  arglist->topfile,
		  arglist->top_line_num
	    );
	(void)fprintf(list_fd,": defn%d call%d ext%d arg%d",
		arglist->is_defn,
		arglist->is_call,
		arglist->external_decl,
		arglist->actual_arg);
	if(count == 0)
		(void)fprintf(list_fd,"\n\t(Empty list)");
	else {
	    for (i=0; i<count; i++) {
		(void)fprintf(list_fd,
			"\n\t%d %s %s %s",
			i+1,
			a[i].name,
			class_name[storage_class_of(a[i].type)],
			type_name[datatype_of(a[i].type)]
		);
		if(a[i].size != size_DEFAULT)
		    (void)fprintf(list_fd,
			    "*%ld",
			    a[i].size
		    );
		if(a[i].array_var)
		    (void)fprintf(list_fd,
			    ":%ldD(%ld)",
			    array_dims(a[i].info.array_dim),
			    array_size(a[i].info.array_dim)
		    );
		(void)fprintf(list_fd,
			" lval%d set%d asg%d ubs%d ary%d are%d ext%d do%d",
			a[i].is_lvalue,
			a[i].set_flag,
			a[i].assigned_flag,
			a[i].used_before_set,
			a[i].array_var,
			a[i].array_element,
			a[i].declared_external,
			a[i].active_do_var
		);
		if(a[i].common_block != NULL)
		    (void)fprintf(list_fd,
			    "\n\t  item %ld in block %s",
			    a[i].common_index,
			    a[i].common_block->name
		    );
	    }
	}
}/* print_arg_array */
#endif

#ifdef DEBUG_SYMTABS
	       /* prints type and dimen info for common vars */
PRIVATE void
print_com_array(cmlist)
	ComListHeader *cmlist;
{
	int i, count;
	ComListElement *c;

	count = cmlist->numargs;
	c = cmlist->com_list_array;
	(void)fprintf(list_fd,
		"\n     Com list in module %s file %s line %u",
		cmlist->module->name,
		cmlist->filename,
		cmlist->line_num
	);
	if( cmlist->topfile != cmlist->filename )
	    (void)fprintf(list_fd,
		  " (topfile %s line %u)",
		  cmlist->topfile,
		  cmlist->top_line_num
	    );
	(void)fprintf(list_fd,
		": anyuse%d anyset%d saved%d",
		cmlist->any_used,
		cmlist->any_set,
		cmlist->saved
	);
	if(count == 0)
		(void)fprintf(list_fd,"\n\t(Empty list)");
	else {
	    for (i=0; i<count; i++){
		(void)fprintf(list_fd,
			"\n\t%d %s %s",
			i+1,
			c[i].name,
			type_name[datatype_of(c[i].type)]
		);
		if(c[i].size != size_DEFAULT)
		    (void)fprintf(list_fd,
			    "*%ld",
			    c[i].size
		    );
		if(c[i].dimen_info != array_dim_info(0,1))
		    (void)fprintf(list_fd,
			    ":%ldD(%ld)",
			    array_dims(c[i].dimen_info),
			    array_size(c[i].dimen_info)
		    );
		(void)fprintf(list_fd,
			" use%d set%d ubs%d asg%d",
			c[i].used,
			c[i].set,
			c[i].used_before_set,
			c[i].assigned
		);
	    }
	}
}/* print_com_array */
#endif

#ifdef DEBUG_SYMTABS
PRIVATE void
print_tokenlist(toklist)	/* prints list of token names or types */
	TokenListHeader *toklist;
{
	int numargs=0;
	Token *t;
	if (toklist == NULL){
	    (void)fprintf(list_fd,"\n\t(No list)");
	}
	else {
	    t = toklist->tokenlist;
	    while(t != NULL){
		++numargs;
		(void)fprintf(list_fd,"\n\t%d ",numargs);
		(void)fprintf(list_fd," %s %s %s",
			t->src_text,
			class_name[storage_class_of(t->TOK_type)],
			type_name[datatype_of(t->TOK_type)]
		);
		t = t->next_token;
	    }
	    if(numargs == 0)
		    (void)fprintf(list_fd,"\n\t(Empty list)");
	}
}/* print_tokenlist */
#endif




void
debug_symtabs(VOID) 	/* Debugging output: hashtable and symbol tables */
{
#ifdef DEBUG_SYMTABS
			/* local symtab info printout is very incomplete  */
  if(debug_loc_symtab) {
    int i;
    (void)fprintf(list_fd,"\nLocal Symbol table:\n");
    for(i=0; i < loc_symtab_top; i++) {
	(void)fprintf(list_fd,
		"\n%4d %s type %s %s",
		i,
		loc_symtab[i].name,
		class_name[storage_class_of(loc_symtab[i].type)],
		type_name[datatype_of(loc_symtab[i].type)]
	);
	if( loc_symtab[i].size != size_DEFAULT )
	    (void)fprintf(list_fd,
		   "*%ld",
		   loc_symtab[i].size
	    );
	if(loc_symtab[i].common_block != NULL)
	    (void)fprintf(list_fd,
		    "\n\t  item %ld in block %s",
		    loc_symtab[i].common_index,
		    loc_symtab[i].common_block->name
	    );
	switch( storage_class_of(loc_symtab[i].type) ) {
	case class_SUBPROGRAM:
	case class_COMMON_BLOCK:
	    print_tokenlist(loc_symtab[i].info.toklist);
	    break;
	}
    }
    (void)fprintf(list_fd,"\n");
  }

    if(debug_hashtab) {
	int i;
	(void)fprintf(list_fd,"\n\nContents of hashtable\n");
	for(i=0; i<HASHSZ; i++) {
	    if(hashtab[i].name != NULL) {
	      (void)fprintf(list_fd,"\n%4d %s",i,hashtab[i].name);
	      if(hashtab[i].loc_symtab != NULL)
		(void)fprintf(list_fd," loc %d",hashtab[i].loc_symtab-loc_symtab);
	      if(hashtab[i].glob_symtab != NULL)
		(void)fprintf(list_fd,
			" glob %d",hashtab[i].glob_symtab-glob_symtab);
	      if(hashtab[i].com_loc_symtab != NULL)
		(void)fprintf(list_fd,
			" Cloc %d",hashtab[i].com_loc_symtab-loc_symtab);
	      if(hashtab[i].com_glob_symtab != NULL)
		(void)fprintf(list_fd,
			" Cglob %d",hashtab[i].com_glob_symtab-glob_symtab);
	    }
	}
    }

    if(debug_glob_symtab) {
	int i;
	(void)fprintf(list_fd,"\n\nContents of global symbol table");

	for(i=0; i<glob_symtab_top; i++) {
	    (void)fprintf(list_fd,
		"\n%4d %s type %s %s",
		i,
		glob_symtab[i].name,
		class_name[storage_class_of(glob_symtab[i].type)],
		type_name[datatype_of(glob_symtab[i].type)]
	     );
	    if( glob_symtab[i].size != size_DEFAULT )
		(void)fprintf(list_fd,
		      "*%ld",
		      glob_symtab[i].size
		);
	    (void)fprintf(list_fd,
 ": use%d set%d asg%d rec%d lib%d ent%d inf%d vis%d smw%d def%d incl%d ext%d ",
		glob_symtab[i].used_flag,
		glob_symtab[i].set_flag,
		glob_symtab[i].assigned_flag,
		glob_symtab[i].recursive,
		glob_symtab[i].library_module,
		glob_symtab[i].internal_entry,
		glob_symtab[i].invoked_as_func,
		glob_symtab[i].visited,
		glob_symtab[i].visited_somewhere,
		glob_symtab[i].defined,
		glob_symtab[i].defined_in_include,
		glob_symtab[i].declared_external
	    );
	    switch(storage_class_of(glob_symtab[i].type)){
		case class_COMMON_BLOCK:{
		    ComListHeader *clist;
		    clist=glob_symtab[i].info.comlist;
		    while(clist != NULL){
			print_com_array(clist);
			clist = clist->next;
		    }
		    break;
		}
		case class_SUBPROGRAM:{
		    ArgListHeader *alist;
		    alist=glob_symtab[i].info.arglist;
		    while(alist != NULL){
			print_arg_array(alist);
			alist = alist->next;
		    }
		    break;
		}
	    }
	}
    }
#endif
}/* debug_symtabs*/


			/* Figure out the appropriate message to use
			   based on what kind of item.  Special cases:
			   if symbol is an external, its line_declared
			   is not set, so we need to change to
			   line_used and say "referenced" instead of
			   "defined"; if current module is a function
			   it has class_VAR but should say "declared"
			   even if not typed.
			 */
void
#if HAVE_STDC
choose_tag(int tag_type, const Lsymtab *symt, char **tag, LINENO_t *lineno)
#else /* K&R style */
choose_tag(tag_type, symt, tag, lineno)
    int tag_type;		/* what kind of tag: defn, set, used */
    Lsymtab *symt;		/* the item for which tag is needed */
    char **tag;			/* output var = string, e.g. "defined" */
    LINENO_t *lineno;		/* output var = relevant line number */
#endif /* HAVE_STDC */
{
			/* Maintainer note: the tags defined below must
			   not exceed MAX_TAG_LEN defined in plsymtab.h. */
    switch(tag_type) {
      case TAG_DEFN:
	(*lineno) = symt->line_declared;
	switch( storage_class_of(symt->type) ) {
	case class_VAR:
	    if(datatype_of(symt->type) == type_UNDECL
	       && !(symt->argument) /* args are considered declared */
	       && symt != hashtab[current_module_hash].loc_symtab)
		(*tag) = "first occurrence";
	    else
		(*tag) = "declared";
	    break;
	case class_COMMON_BLOCK:
	    (*tag) = "declared";
	    break;
	default:		/* subprograms & stmt functions */
	    (*tag) = "defined";
	    if( (*lineno) == NO_LINE_NUM ) {
			/* External routines not explicitly declared
			   will have line_declared unset. (Stmt functions
			   never do.)  Use invocation instead.  */
		(*tag) = "referenced";
		(*lineno) = symt->line_used;
	    }
	    break;
	}
	break;

      case TAG_USED:
	(*lineno) = symt->line_used;
	(*tag) = "used";
	break;

      case TAG_SET:
	(*lineno) = symt->line_set;
	if(storage_class_of(symt->type) != class_VAR)
	    (*tag) = "defined";
	else {
	    if(symt->assigned_flag)
		(*tag) = "set";
	    else
		(*tag) = "may be set";
	}
	break;

      default:			/* for our forgetful authors, just in case */
	oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		     "choose_tag called with unimplemented tag type");
	break;
    }/* switch(tag_type) */

#ifdef DEVELOPMENT		/* bug catcher */
    if(strlen(*tag) > MAX_TAG_LEN) {
	(void)fprintf(stderr,"\n%s",*tag);
	oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		     "choose_tag yields tag longer than MAX_TAG_LEN");
    }
#endif
}
