/* $Id: makedcls.c,v 1.15 2003/03/17 23:15:49 moniot Exp $

   Routines  for declaration file output

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

/* Originally written by Nelson H.F. Beebe before source text was
   saved in the symbol table.  Rewritten by R. Moniot to make use
   of said text. */

		
/*
	Shared functions defined:
		make_declarations	produces the declarations file

*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "plsymtab.h"

extern int free_form;		/* for choosing 'C' or '!' as comment char */

		/* Declarations of local functions */

PROTO(PRIVATE char * base_filename,( char *curr_filename ));

PROTO(PRIVATE void append_char_to_fragment,( int c ));
PROTO(PRIVATE void append_string_to_fragment,( char *s ));
PROTO(PRIVATE void append_expr_text_to_fragment,( char *s ));

PROTO(PRIVATE void maybe_print_module_header,( void ));
PROTO(PRIVATE void new_fragment,( void ));
PROTO(PRIVATE void print_blanks,( int nblanks ));
PROTO(PRIVATE void print_common_decls,( Lsymtab *sym_entry ));
PROTO(PRIVATE void print_empty_comment_line,( void ));
PROTO(PRIVATE void print_equivalence_decls,( Lsymtab *sym_entry ));
PROTO(PRIVATE int count_undeclared_variables,( Lsymtab *sym_entry ));
PROTO(PRIVATE void print_list_decls,( Lsymtab *sym_list[], int n, char
			      *header, char *list_type_name ));
PROTO(PRIVATE int print_list_name,( char *list_type_name, char *name ));
PROTO(PRIVATE void print_declaration_class,( Lsymtab *sym_list[], int n,
				    const char *header ));
PROTO(PRIVATE void print_one_list_decls,( Lsymtab *sym_entry, char
				  *list_type_name, char **pheader, int
				  *pnd ));
PROTO(PRIVATE void print_parameter_statement,( Lsymtab *symt ));
PROTO(PRIVATE void print_selected_declarations,( Lsymtab *sym_list[], int n,
					 int the_type, const char
					 *the_type_name, const char * (*pheader) ));
PROTO(PRIVATE int print_typename,( int the_type, const char *the_type_name, int
			    the_size, Lsymtab *symt ));
PROTO(PRIVATE int make_sym_list,( Lsymtab *sym_list[], int (*selector)(Lsymtab
							      *sym_entry) ));
PROTO(PRIVATE int select_arguments,( Lsymtab *sym_entry ));
PROTO(PRIVATE void strip_blanks,(char *s));
#if 0 /* not currently used */
PROTO(PRIVATE int select_commons,( Lsymtab *sym_entry ));
#endif
PROTO(PRIVATE int select_externals_by_name,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_externals_by_type,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_intrinsics_by_name,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_intrinsics_by_type,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_locals,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_common_blocks,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_namelists,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_parameters,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_statement_functions,( Lsymtab *sym_entry ));
PROTO(PRIVATE int sf3_internal_name,( Lsymtab *sym_entry ));

PROTO(PRIVATE char * get_dimension_list,( Lsymtab *symt ));
PROTO(PRIVATE char * get_parameter_value,( Lsymtab *symt ));
PROTO(PRIVATE char * get_size_expression,( Lsymtab *symt ));


#if 0			/* This is how Beebe wrote it */
#define ACTUAL_SIZE(p)		(((p)->size == 0) ? \
				 std_size[the_type] : (p)->size)
#else
		/* This is what it has to be if IMPLICIT types supported */
#define ACTUAL_SIZE(p)		(get_size((p),sym_type))
#endif


#define DECLARE_ONLY_UNDECLARED() (dcl_only_undeclared)
#define DECLARE_COMPACT()	(dcl_compact)
#define NO_CONTINUATION_LINES() (!(dcl_use_continuations))
#define EXCL_SF3_DECLARATIONS()	(dcl_excl_sftran3_internal_vars)
#define ASTERISK_COMMENT_CHAR()	(dcl_asterisk_comment_character)
#define FREE_FORM()		(dcl_free_form||free_form)
#define KEYWORDS_LOWERCASE()	(dcl_keywords_lowercase)
#define LOWERCASE_COMMENT_CHARACTER() (dcl_lowercase_comment_character)
#define VARIABLES_AND_CONSTANTS_LOWERCASE() (dcl_vars_and_consts_lowercase)
#define ARRAY_VARS_DIMENSIONED() (!(dcl_no_array_dimensions))

#define COLUMN_WIDTH		13

#ifndef PFORT_FIRST_VARIABLE_COLUMN
#define PFORT_FIRST_VARIABLE_COLUMN 26   /* to match Extended PFORT Verifier */
#endif

PRIVATE int first_variable_column;

#define NEXT_COLUMN(column)	(first_variable_column + \
				(((column) - first_variable_column + \
				COLUMN_WIDTH - 1) / COLUMN_WIDTH)*COLUMN_WIDTH)

#define isaletter(C)    isalpha((int)(C))

	/* define isidletter to allow underscore and/or dollar sign  */
#define isidletter(C)    (isalpha((int)(C)) || (C) == '_' || (C) == '$')


#define makelower(C) (isupper((int)(C)) ? tolower((int)(C)) : (int)(C))
#define makeupper(C) (islower((int)(C)) ? toupper((int)(C)) : (int)(C))

PRIVATE char *begin_module;

#define MAX_STMT		(72 + 19*72 + 1) /* longest Fortran stmt */

PRIVATE char stmt_fragment[MAX_STMT];

PRIVATE char comment_char = 'C'; /* default value */

PRIVATE int std_size[] =	/* NB: depends on type_XXX order in symtab.h */
{
    0,					/* unknown */
    4,					/* INTEGER*4 */
    4,					/* REAL*4 */
    8,					/* DOUBLE PRECISION == REAL*8 */
    8,					/* COMPLEX*8 */
    16,					/* DOUBLE COMPLEX == COMPLEX*16 */
    4,					/* LOGICAL*4 */
    1					/* CHARACTER*1 == CHARACTER */
};

PRIVATE int
pos_fragment = 0;		/* cursor in stmt_fragment buffer */

PRIVATE int
dcl_indent;			/* amount to indent declarations */


PRIVATE char *
#if HAVE_STDC
base_filename(char *curr_filename)
#else /* K&R style */
base_filename(curr_filename)
     char *curr_filename;
#endif /* HAVE_STDC */

{
  char *path_end=(char *)NULL;

#ifdef UNIX
  path_end = strrchr(curr_filename,'/');
#endif

#ifdef VMS
  path_end = strrchr(curr_filename,']');
  if( path_end == (char *)NULL )
    path_end = strrchr(curr_filename,':'); /* for symbolic names */
#endif

#ifdef MSDOS			/* look for either \ or / at end. */
  path_end = strrchr(curr_filename,'\\');
  if( path_end == (char *)NULL )
    path_end = strrchr(curr_filename,'/');
#endif

  if( path_end == (char *)NULL )
    path_end = curr_filename;
  else
    ++path_end;

  return (path_end);
}


PRIVATE void
#if HAVE_STDC
append_char_to_fragment(int c)
#else /* K&R style */
append_char_to_fragment(c)
int c;
#endif /* HAVE_STDC */
{
    if (pos_fragment < (MAX_STMT - 1))
	stmt_fragment[pos_fragment++] = c;
}


PRIVATE void
#if HAVE_STDC
append_string_to_fragment(char *s)
#else /* K&R style */
append_string_to_fragment(s)
char *s;
#endif /* HAVE_STDC */
{
    while (*s)
	append_char_to_fragment(*s++);
}

			/* Appends source text of an expression, up- or
			   down-casing the letters according to pref. */
PRIVATE void
#if HAVE_STDC
append_expr_text_to_fragment(char *s)
#else /* K&R style */
append_expr_text_to_fragment(s)
  char *s;
#endif /* HAVE_STDC */
{
    int quote_char, inside_quote;
    inside_quote = FALSE;
    for (; *s; ++s) {
      if(! inside_quote) {
	if(*s == '\'' || *s == '"') { /* Start of a quote */
	  inside_quote = TRUE;
	  quote_char = *s;
	}
	append_char_to_fragment(VARIABLES_AND_CONSTANTS_LOWERCASE()
				? makelower(*s) : makeupper(*s));
      }
      else {			/* inside quote */
	if(*s == quote_char) { /* End of quote (quoted quote_char is handled
				  as if consecutive strings) */
	  inside_quote=FALSE;
	}
	append_char_to_fragment(*s);
      }
    }
}


PRIVATE char *
#if HAVE_STDC
get_dimension_list(Lsymtab *symt)
#else /* K&R style */
get_dimension_list(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    int n, dims;

		/* Get list of array dimensions from symbol table */

    new_fragment();

    append_char_to_fragment('(');

    dims = array_dims(symt->info.array_dim);
    for (n = 0; n < dims; ++n)
    {
	if (n > 0)
	    append_char_to_fragment(',');
	append_expr_text_to_fragment(symt->src.textvec[n]);
    }

    append_char_to_fragment(')');
    append_char_to_fragment('\0');

    return (&stmt_fragment[0]);
}




PRIVATE char *
#if HAVE_STDC
get_parameter_value(Lsymtab *symt)
#else /* K&R style */
get_parameter_value(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    /* Construct parameter list "(NAME = value)" */

    new_fragment();
    append_char_to_fragment('(');

    append_expr_text_to_fragment(symt->name);

    append_string_to_fragment(" = ");

    append_expr_text_to_fragment(symt->info.param->src_text);

    append_char_to_fragment(')');
    append_char_to_fragment('\0');
    return (&stmt_fragment[0]);
}



PRIVATE char *
#if HAVE_STDC
get_size_expression(Lsymtab *symt)
#else /* K&R style */
get_size_expression(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    /* Get a CHARACTER size expression from the symbol table */

    new_fragment();

    append_char_to_fragment('*');

    append_expr_text_to_fragment(get_size_text(symt,0));

    append_char_to_fragment('\0');

    return (&stmt_fragment[0]);
}

void
#if HAVE_STDC
make_declarations(Lsymtab **sym_list, char *mod_name)
#else /* K&R style */
make_declarations(sym_list,mod_name)
     Lsymtab *sym_list[];
     char *mod_name;
#endif /* HAVE_STDC */
{
    const char *header;
    char begin[72+1+72+1+2+1];
    char *base_curr_filename;	/* basename of current input file */
    int len_base_curr_filename;

    if ( ! ANY_DCL_DECLARATIONS() )
	return;

    base_curr_filename = base_filename(current_filename);
    len_base_curr_filename = strlen(base_curr_filename);

			/* Establish indentation and comment character
			   for free form or fixed form.
			 */
    dcl_indent = 6;
    first_variable_column = PFORT_FIRST_VARIABLE_COLUMN;
    if (FREE_FORM()) {
	dcl_indent = DCL_FREEFORM_INDENT;
	first_variable_column = PFORT_FIRST_VARIABLE_COLUMN-(6-dcl_indent);
	comment_char = '!';
    }
    else if (LOWERCASE_COMMENT_CHARACTER())
	comment_char = 'c';
    else if (ASTERISK_COMMENT_CHAR())
	comment_char = '*';
    else
	comment_char = 'C';

    /* In the event there are no declarations to be output, we want
       the declaration file to be empty, because that reduces the
       number of files that the user has to deal with.  In fact, if it
       IS empty, it will be deleted on close.  Instead of printing the
       module header comment here, we point a global pointer at it,
       and then in the print_xxx() functions, print the header before
       the first declaration that is output.

       We also need to take care not be overwrite the begin[] array,
       which could happen if the module name or file name are
       exceptionally long.  We therefore take at most 20 characters
       from the start of the module name, and at most 25 (so the
       total length of 72 is not surpassed) from the END of the base
       of the filename, discarding the directory path prefix. */


    (void)sprintf(begin,
		  "%c====>Begin Module %-20.20s   File %-25.25s\n%c---->Makedcls Options: %-48s\n%c\n",
		  comment_char,
		  mod_name,
		  (len_base_curr_filename > 25) ?
			(base_curr_filename + len_base_curr_filename - 25) :
			base_curr_filename,
                  comment_char,
		  EXCL_SF3_DECLARATIONS()?
		   (DECLARE_ONLY_UNDECLARED() ?
			"Undeclared variables except SFTRAN3 internals" :
			"All variables except SFTRAN3 internals") :
		   (DECLARE_ONLY_UNDECLARED() ?
			"Undeclared variables" :
			"All variables"),
		  comment_char);
    begin_module = &begin[0];

    print_selected_declarations(sym_list,
				make_sym_list(sym_list,
					      select_intrinsics_by_name),
				type_ERROR, "INTRINSIC",
				(header = "Intrinsic functions", &header));
    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_intrinsics_by_type),
			    "Built-in functions");

    print_selected_declarations(sym_list,
				make_sym_list(sym_list,
					      select_externals_by_name),
				type_ERROR, "EXTERNAL",
				(header = "External functions", &header));
    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_externals_by_type),
			    (char*)NULL);

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_statement_functions),
			    "Statement functions");

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_parameters),
			    "Parameter variables");

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_arguments),
			    "Argument variables");

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_locals),
			    "Local variables");

    print_list_decls(sym_list,
			    make_sym_list(sym_list,select_namelists),
			    "Namelists","NAMELIST");

				/* Common block declarations must be last,
				   for dcl2inc to work correctly.
				 */
    print_list_decls(sym_list,
			    make_sym_list(sym_list,select_common_blocks),
			    "Common blocks","COMMON");

    if (begin_module == (char*)NULL) /* then need a trailer comment */
	(void)fprintf(dcl_fd,
		      "%c====>End Module   %-20.20s   File %-25.25s\n",
		      comment_char,
		      mod_name,
		      (len_base_curr_filename > 25) ?
			    (base_curr_filename + len_base_curr_filename - 25) :
			    base_curr_filename);

}



PRIVATE void
maybe_print_module_header(VOID)
{
    if (begin_module != (char*)NULL)
    {		/* print module header comment only once */
	(void)fputs(begin_module, dcl_fd);
	begin_module = (char*)NULL;
    }
}



PRIVATE void
new_fragment(VOID)
{
    pos_fragment = 0;
}



PRIVATE void
#if HAVE_STDC
print_blanks(int nblanks)
#else /* K&R style */
print_blanks(nblanks)
int	nblanks;
#endif /* HAVE_STDC */
{
    for ( ; nblanks > 0; --nblanks)
	(void)putc(' ',dcl_fd);
}

				/* Routine to print namelist and
				   common declarations. */

PRIVATE void
#if HAVE_STDC
print_common_decls(Lsymtab *sym_entry)
                        	/* COMMON block symbol table entry */
#else /* K&R style */
print_common_decls(sym_entry)
     Lsymtab *sym_entry;	/* COMMON block symbol table entry */
#endif /* HAVE_STDC */
{
    int h;
    int n;
    Token *t;

#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    static Lsymtab **sym_list=(Lsymtab **)NULL;

    if(sym_list == (Lsymtab **)NULL) { /* Initialize if not done before */
      if( (sym_list=(Lsymtab **)calloc(LOCSYMTABSZ,sizeof(Lsymtab *)))
	 == (Lsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for local symbol list");
      }
    }
#else
    Lsymtab *sym_list[LOCSYMTABSZ]; /* temp. list of symtab entries to print */
#endif

    for (n = 0, t = sym_entry->src.toklist->tokenlist;
	 t != NULL;
	 t = t->next_token)
      {
	h = t->value.integer;
	sym_list[n++] = hashtab[h].loc_symtab;
      }

    if (n > 0)
    {
	sort_lsymbols(sym_list,n);
	print_declaration_class(sym_list, n, "Common variables");
    }
}


PRIVATE void
print_empty_comment_line(VOID)
{
    (void)putc(comment_char,dcl_fd);
    (void)putc('\n',dcl_fd);
}


PRIVATE void
#if HAVE_STDC
print_equivalence_decls(Lsymtab *sym_entry)
                        	/* COMMON block symbol table entry */
#else /* K&R style */
print_equivalence_decls(sym_entry)
     Lsymtab *sym_entry;	/* COMMON block symbol table entry */
#endif /* HAVE_STDC */
{
    int h;
    int n;
    Lsymtab *s;
    Token *t;

#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    static Lsymtab **sym_list=(Lsymtab **)NULL;

    if(sym_list == (Lsymtab **)NULL) { /* Initialize if not done before */
      if( (sym_list=(Lsymtab **)calloc(LOCSYMTABSZ,sizeof(Lsymtab *)))
	 == (Lsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for local symbol list");
      }
    }
#else
    Lsymtab *sym_list[LOCSYMTABSZ]; /* temp. list of symtab entries to print */
#endif

    for (n = 0, t = sym_entry->src.toklist->tokenlist;
	 t != NULL;
	 t = t->next_token)
    {
	h = t->value.integer;
	for (s = hashtab[h].loc_symtab, s = s->equiv_link;
	     (s != NULL) && (s != hashtab[h].loc_symtab);
	     s = s->equiv_link)
	    sym_list[n++] = s;
    }

    if (n > 0)
    {
	sort_lsymbols(sym_list,n);
	print_declaration_class(sym_list, n,
				"Equivalenced common variables");
    }
}


PRIVATE int
#if HAVE_STDC
count_undeclared_variables(Lsymtab *sym_entry)
#else /* K&R style */
count_undeclared_variables(sym_entry)
     Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    int count, h;
    Token *t;
    Lsymtab *symt;

    for (count = 0, t = sym_entry->src.toklist->tokenlist;
	 t != NULL;
	 t = t->next_token)
    {			/* Loop over members */
	h = t->value.integer;
	symt = hashtab[h].loc_symtab;
	if (datatype_of(symt->type) == type_UNDECL)
	    count++;
    }
    return (count);
}


PRIVATE void
#if HAVE_STDC
print_list_decls(Lsymtab **sym_list, int n, char *header, char *list_type_name)
#else /* K&R style */
print_list_decls(sym_list, n, header, list_type_name)
     Lsymtab *sym_list[];
     int n;
     char *header;
     char *list_type_name;
#endif /* HAVE_STDC */
{
    int i, nd;

    if (DECLARE_ONLY_UNDECLARED() &&
	(strcmp(list_type_name,"NAMELIST") == 0)) /* These lists are always declared */
      return;

    nd = 0;
    for (i=0; i<n; i++)
    {					/* Loop over COMMON or NAMELIST lists */
	if (sym_list[i]->src.toklist != NULL)
	{
	    if (strcmp(list_type_name,"COMMON") == 0)
	    {				/* then COMMON list */
		if (!DECLARE_ONLY_UNDECLARED() ||
		    (DECLARE_ONLY_UNDECLARED() &&
		     (count_undeclared_variables(sym_list[i]) > 0)))
		{
		    print_common_decls(sym_list[i]);
		    if (!DECLARE_ONLY_UNDECLARED())
		        print_one_list_decls(sym_list[i], list_type_name,
					     &header, &nd);
		    print_equivalence_decls(sym_list[i]);
		}
	    }
	    else			/* must be NAMELIST list */
	        print_one_list_decls(sym_list[i], list_type_name, &header, &nd);
	}
    }

    if ((nd > 0) && (strcmp(list_type_name,"COMMON") != 0))
	print_empty_comment_line();
}
				/* routine to print COMMON or NAMELIST
				   name between slashes. */
PRIVATE int
#if HAVE_STDC
print_list_name(char *list_type_name, char *name)
#else /* K&R style */
print_list_name(list_type_name,name)
  char *list_type_name;
  char *name;
#endif /* HAVE_STDC */
{
    int column, len;
    char *p;

    maybe_print_module_header();

				/* Compact mode:   COMMON /blknam/
				   Padded mode:    COMMON / blknam /
				 */
    print_blanks(dcl_indent);
    column = dcl_indent;

    for (p = list_type_name; *p; ++p, ++column)
	(void)putc(KEYWORDS_LOWERCASE() ? makelower(*p) : makeupper(*p),
		   dcl_fd);

    print_blanks(1);
    column++;

    (void)putc('/',dcl_fd);
    column++;

    if (!DECLARE_COMPACT())
      {
	print_blanks(1);
	column++;
      }
    len = 0;
    if (strcmp(name,blank_com_name) != 0) {
      for (p=name; *p; ++p, ++len)
	(void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
		   makelower(*p) : makeupper(*p),dcl_fd);
    }
    column += len;
    if (!DECLARE_COMPACT())
      {
	if (len <= 6)		/* Max standard length */
	  {
	    print_blanks(7-len); /* Print padding */
	    column += 7-len;
	  }
      }

    (void)putc('/',dcl_fd);
    column++;

    if (DECLARE_COMPACT())
    {
	print_blanks(1);
	column++;
    }
    else if (column < first_variable_column)
    {
	print_blanks(first_variable_column-column);
	column = first_variable_column;
    }
    else  if (column == first_variable_column)
    {
	print_blanks(1);
	column++;
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    else
    {
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    return column;
}


PRIVATE void
#if HAVE_STDC
print_declaration_class(Lsymtab **sym_list, int n, const char *header)
#else /* K&R style */
print_declaration_class(sym_list, n, header)
     Lsymtab *sym_list[];
     int n;
     char *header;
#endif /* HAVE_STDC */
{
    unsigned t;
    static int types_table[] =	/* table defining output declaration order */
    {			/* (alphabetical by type name) */
	type_STRING,
	type_COMPLEX,
	type_DCOMPLEX,
	type_DP,
	type_INTEGER,
	type_LOGICAL,
	type_REAL,
    };

    if (n > 0)
    {
	for (t = 0; t < sizeof(types_table)/sizeof(types_table[0]); ++t)
	    print_selected_declarations(sym_list, n, types_table[t],
					(char*)NULL, &header);
    }
}


PRIVATE void
#if HAVE_STDC
print_one_list_decls(Lsymtab *sym_entry, char *list_type_name, char **pheader, int *pnd)
#else /* K&R style */
print_one_list_decls(sym_entry, list_type_name, pheader, pnd)
     Lsymtab *sym_entry;
     char *list_type_name;
     char **pheader;
     int *pnd;
#endif /* HAVE_STDC */
{
    int column, need, next_column, nv;
    int ncontin;
    int h;
    Token *t;
    Lsymtab *symt;
    char *p;

    column = 0;
    ncontin = 0;		/* count of continuation lines */
    nv = 0;		/* count of variables in statement */
    for(t = sym_entry->src.toklist->tokenlist;
	t != NULL;
	t = t->next_token)
      {			/* Loop over members */
        h = t->value.integer;
        symt = hashtab[h].loc_symtab;
        if (column == 0)		/* at beginning of line, so */
          {			/* we need a type name */
            maybe_print_module_header();
            if ((*pheader != (char*)NULL) &&
                (strcmp(list_type_name,"COMMON") != 0))
              {				/* print header only once */
                (void)fprintf(dcl_fd,"%c     %s\n", comment_char,*pheader);
                print_empty_comment_line();
                *pheader = (char*)NULL; /* so we don't print it again */
              }
            column = print_list_name(list_type_name,sym_entry->name);
            nv = 0;		/* no variables yet in statement */
            ncontin = 0;
            ++(*pnd);			/* count declarations produced */
          }
        if (DECLARE_COMPACT())
          next_column = (nv==0?column:column + 2);
        else
          next_column = NEXT_COLUMN(nv==0?column:column + 2);
        need = (int)strlen(symt->name);
        if ((next_column + need) > 72)  /* then must start new line */
          {
            if (nv>0 && (strcmp(list_type_name,"COMMON") == 0) &&
                (NO_CONTINUATION_LINES() || ncontin == (FREE_FORM()?39:19)))
              {
		(void)putc('\n',dcl_fd);
                column = print_list_name(list_type_name,sym_entry->name);
                nv = 0;	/* no variables yet in statement */
                ncontin = 0;
              }
            else
              {
		if( FREE_FORM() ) { /* do a free-form continuation */
		  print_blanks(next_column-column);
		  (void)fputs("&\n",dcl_fd);
		  print_blanks(dcl_indent);
		  column = dcl_indent;
		}
		else {		    /* do a fixed-form continuation */
		  (void)putc('\n',dcl_fd);
		  print_blanks(5);
		  (void)putc('x',dcl_fd);
		  column = 6;
		}
		if (DECLARE_COMPACT())
		  next_column = (nv==0?column:column + 2);
		else
		  next_column = NEXT_COLUMN(nv==0?column:column + 2);
		++ncontin;
              }
          }
        if (nv > 0)		/* multiple variables */
          {
            (void)fputs(", ",dcl_fd);
            print_blanks(next_column - column - 2);
            column = next_column;
          }
        for (p = symt->name; *p; ++p)
          (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
                     makelower(*p) : makeupper(*p),dcl_fd);

        column += need;
        nv++;			/* count variables */
      }
    if ((nv > 0) && (strcmp(list_type_name,"COMMON") == 0))
      {
    	if (column > 0)
              (void)putc('\n',dcl_fd);
          print_empty_comment_line();
          column = 0;
      }
    if (column > 0)
	(void)putc('\n',dcl_fd);
}


PRIVATE void
#if HAVE_STDC
print_parameter_statement(Lsymtab *symt)
#else /* K&R style */
print_parameter_statement(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    int column;
    int need;
    int i;

    column = print_typename(type_ERROR,"PARAMETER",0,symt);
    need = strlen(get_parameter_value(symt));
    if ((column + need) > 72)	/* then too long to fit on current line */
    {
	if( FREE_FORM() ) {
	    (void)fputs(" &\n",dcl_fd);
	    print_blanks(dcl_indent);
	    column = dcl_indent;
	}
	else {
	    (void)fputs("\n     x",dcl_fd);
	    column = 6;
	}
	if ((column + need) > 72)
	{	/* long parameter setting requires line break */
	    for (i = 0; stmt_fragment[i]; ++i)
	    {
		if (column == 72)
		{
		    if( FREE_FORM() ) {
			(void)fputs("&\n",dcl_fd);
			print_blanks(dcl_indent);
			(void)putc('&',dcl_fd);
			column = dcl_indent+1;
		    }
		    else {
			(void)fputs("\n     x",dcl_fd);
			column = 6;
		    }
		}
		(void)putc((int)stmt_fragment[i],dcl_fd);
		column++;
	    }
	}
	else
	    (void)fputs(stmt_fragment,dcl_fd);
    }
    else			/* fits on current line */
	(void)fputs(stmt_fragment,dcl_fd);
    (void)putc('\n',dcl_fd);
}


PRIVATE void
#if HAVE_STDC
print_selected_declarations(Lsymtab **sym_list, int n, int the_type,
 const char *the_type_name,
 const char * (*pheader)) /* **pheader is const, *pheader is not */
#else /* K&R style */
print_selected_declarations(sym_list, n, the_type, the_type_name, pheader)
     Lsymtab *sym_list[];
     int n;
     int the_type;
     char *the_type_name;
     char **pheader;
#endif /* HAVE_STDC */
{
    int column, i, last_size, need, next_column, nt, nv, ncontin,
	raw_type, sym_type, sym_size;
    char *p;

    column = 0;
    last_size = 0;
    nt = 0;				/* count of type declaration stmts */
    nv = 0;				/* count of variables in statement */
    for (i = 0; i < n; ++i)
    {				/* loop over variables */
	raw_type = datatype_of(sym_list[i]->type);
	if (DECLARE_ONLY_UNDECLARED())
	{
	    if (raw_type != type_UNDECL)
		continue; /* want declarations only for undeclared vars */
	    if (sym_list[i]->external) /* and not for explicit EXTERNAL */
		continue;
	    if (sym_list[i]->intrinsic) /* and not for explicit INTRINSIC */
		continue;
	}
	sym_type = (raw_type == type_UNDECL) ?
	    get_type(sym_list[i]) : datatype_of(sym_list[i]->type);

	if ((the_type != type_ERROR) && (sym_type != the_type))
	    continue;

	sym_size = ACTUAL_SIZE(sym_list[i]);
	if ((nv > 0) && (sym_size != last_size))
	{	/* have new length modifier, so must start new declaration */
	    (void)putc('\n',dcl_fd);
	    nt++;		/* count type declaration statements */
	    column = 0;
	    ncontin = 0;
	    nv = 0;
	}
	if (column == 0)		/* at beginning of line, so */
	{				/* we need a type name */
	    maybe_print_module_header();
	    if (*pheader != (char*)NULL)
	    {				/* print header only once */
		(void)fprintf(dcl_fd,"%c     %s\n",comment_char,*pheader);
		print_empty_comment_line();
		*pheader = (char*)NULL;	/* so we don't print it again */
	    }
	    column = print_typename(the_type,the_type_name, sym_size,
				     sym_list[i]);
	    last_size = sym_size;
	    nv = 0;		/* no variables yet in statement */
	    ncontin = 0;
	}
	if (DECLARE_COMPACT())
	    next_column = (nv==0?column:column + 2);
	else
	    next_column = NEXT_COLUMN(nv==0?column:column + 2);
	need = (int)strlen(sym_list[i]->name);

	if (sym_list[i]->array_var     /* leave space for "(...)" */
	    && ARRAY_VARS_DIMENSIONED())
	    need += strlen(get_dimension_list(sym_list[i]));

	if ((next_column + need) > 72)  /* then must start new declaration */
	{
	    nt++;		/* count type declaration statements */
	    if (nv>0 && (NO_CONTINUATION_LINES() || ncontin == 19))
	      {
		(void)putc('\n',dcl_fd);
		column = print_typename(the_type,the_type_name, sym_size,
				     sym_list[i]);
		ncontin = 0;
		nv = 0;		/* no variables yet in statement */
	      }
	    else
	      {
		if( FREE_FORM() ) { /* do a free-form continuation */
		  print_blanks(next_column-column);
		  (void)fputs("&\n",dcl_fd);
		  print_blanks(dcl_indent);
		  column = dcl_indent;
		}
		else {		    /* do a fixed-form continuation */
		  (void)putc('\n',dcl_fd);
		  print_blanks(5);
		  (void)putc('x',dcl_fd);
		  column = 6;
		}
		if (DECLARE_COMPACT())
		  next_column = (nv==0?column:column + 2);
		else
		  next_column = NEXT_COLUMN(nv==0?column:column + 2);
		++ncontin;
	      }
	    last_size = sym_size;
	}
	if (nv > 0)		/* multiple variables */
	{
	    (void)fputs(", ",dcl_fd);
	    print_blanks(next_column - column - 2);
	    column = next_column;
	}
	for (p = sym_list[i]->name; *p; ++p)
	    (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
		       makelower(*p) : makeupper(*p),dcl_fd);
	if (sym_list[i]->array_var
	    && ARRAY_VARS_DIMENSIONED())
	    (void)fputs(stmt_fragment,dcl_fd);
	column += need;
	nv++;			/* count variables */
	if (sym_list[i]->parameter)
	{
	    (void)putc('\n',dcl_fd);
	    print_parameter_statement(sym_list[i]);
	    column = 0;
	    nt++;
	    nv = 0;
	}
    }
    if (column > 0)
    {
	(void)putc('\n',dcl_fd);
	nt++;			/* count type declaration statements */
    }
    if (nt > 0)
	print_empty_comment_line();
}


PRIVATE int
#if HAVE_STDC
print_typename(int the_type, const char *the_type_name, int the_size, Lsymtab *symt)
   	         		/* type_ERROR if typename non-NULL */
    	           		/* non-NULL overrides type_table[] use */
#else /* K&R style */
print_typename(the_type,the_type_name,the_size,symt)
int	the_type;		/* type_ERROR if the_type_name non-NULL */
char	*the_type_name;		/* non-NULL overrides type_table[] use */
int	the_size;
Lsymtab *symt;
#endif /* HAVE_STDC */
{				/* return value is last column printed */
    int column;
    char digits[sizeof("*18446744073709551616")]; /* big enough for 2^64 */
    const char *p;
    char *size_expression;

    maybe_print_module_header();
    print_blanks(dcl_indent);
    column = dcl_indent;

    for (p = (the_type_name == (char*)NULL) ? type_table[the_type] : the_type_name;
	 *p; ++p, ++column)
	(void)putc(KEYWORDS_LOWERCASE() ? makelower(*p) : makeupper(*p),
		   dcl_fd);
    if (symt != NULL) {
      if (((symt->size_is_adjustable && (the_type == type_STRING))) ||
	  (the_size == size_ADJUSTABLE)) /* happens only for CHARACTER*(*) */
	{
	    /* size_is_adjustable overrides the_size because def_parameter() */
	    /* in symtab.c replaced size_ADJUSTABLE with actual size. */
	    (void)fputs("*(*)",dcl_fd);
	    column += 4;
	}
      else if (symt->size_is_expression && (the_type == type_STRING))
	{
	    size_expression = get_size_expression(symt);
	    (void)fputs(size_expression,dcl_fd);
	    column += strlen(size_expression);
	}
      else if ((the_size > 0) &&
	       (the_type != type_ERROR) &&
	       (the_size != std_size[the_type]))
	{	/* supply length modifier for non-standard type sizes */
	    (void)sprintf(digits,"*%d",the_size);
	    (void)fputs(digits,dcl_fd);
	    column += strlen(digits);
	}
    }
    if (DECLARE_COMPACT())
    {
	print_blanks(1);
	column++;
    }
    else if (column < first_variable_column)
    {
	print_blanks(first_variable_column-column);
	column = first_variable_column;
    }
    else  if (column == first_variable_column)
    {
	print_blanks(1);
	column++;
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    else
    {
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    return (column);
}


PRIVATE int
#if HAVE_STDC
select_arguments(Lsymtab *sym_entry)
#else /* K&R style */
select_arguments(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a module argument) */
    if (sym_entry->declared_external ||
	sym_entry->invoked_as_func)
	return (0);
    else if (sym_entry->argument)
	return (1);
    else
	return (0);
}


#if 0				/* this function not currently used */
PRIVATE int
#if HAVE_STDC
select_commons(Lsymtab *sym_entry)
#else /* K&R style */
select_commons(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is in a COMMON block) */
    if (sym_entry->common_var)
	return (1);
    else
	return (0);
}
#endif /*0*/


PRIVATE int
#if HAVE_STDC
select_externals_by_name(Lsymtab *sym_entry)
#else /* K&R style */
select_externals_by_name(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is external and must appear in EXTERNAL declaration) */

    if (sym_entry->declared_intrinsic) /* must appear first, because symbols */
	return (0); /* can be both declared_intrinsic and declared_external*/
		    /* ??? is this a bug in ftnchek 2.7 ??? */
    else if (storage_class_of(sym_entry->type) == class_STMT_FUNCTION)
	return (0);
    else if (sym_entry->declared_external)
	return (1);
    else if (sym_entry->declared_intrinsic || sym_entry->intrinsic)
	return (0);
    else if (sym_entry->invoked_as_func)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_externals_by_type(Lsymtab *sym_entry)
#else /* K&R style */
select_externals_by_type(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is external and must appear in a type declaration) */
    if (storage_class_of(sym_entry->type) == class_STMT_FUNCTION)
	return (0);
    else if (sym_entry->declared_external)
	return (1);
    else if (sym_entry->declared_intrinsic)
	return (0);
    else if (sym_entry->intrinsic)
    {
	if (datatype_of(sym_entry->type) == type_UNDECL)
	{			/* user provided no type declaration */
	    if ((sym_entry->info.intrins_info)->result_type == type_GENERIC)
		return (0);	/* generics CANNOT have explicit type */
	    else
		return (1);	/* not generic, so has explicit type */
	}
	else			/* user supplied an explicit type */
	    return (1);
    }
    else if (sym_entry->invoked_as_func)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_intrinsics_by_name(Lsymtab *sym_entry)
#else /* K&R style */
select_intrinsics_by_name(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is intrinsic and must appear in INTRINSIC declaration) */
    if (sym_entry->declared_intrinsic)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_intrinsics_by_type(Lsymtab *sym_entry)
#else /* K&R style */
select_intrinsics_by_type(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is intrinsic and must appear in a type declaration) */
    if (sym_entry->intrinsic &&
	((sym_entry->info.intrins_info)->result_type == type_GENERIC))
	return (0);
    else
	return (select_intrinsics_by_name(sym_entry));
}


PRIVATE int
#if HAVE_STDC
select_locals(Lsymtab *sym_entry)
#else /* K&R style */
select_locals(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a local variable) */

    if (EXCL_SF3_DECLARATIONS() && sf3_internal_name(sym_entry))
	return (0);
    else if (sym_entry->argument ||
	sym_entry->common_var ||
	sym_entry->declared_external ||
	sym_entry->declared_intrinsic ||
	sym_entry->entry_point ||
	sym_entry->external ||
	sym_entry->intrinsic ||
	sym_entry->invoked_as_func ||
	sym_entry->parameter)
	return (0);
    else
	return (1);
}


PRIVATE int
#if HAVE_STDC
select_common_blocks(Lsymtab *sym_entry)
#else /* K&R style */
select_common_blocks(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a COMMON block name) */
    if (storage_class_of(sym_entry->type) == class_COMMON_BLOCK)
	return (1);
    else
	return (0);
}

PRIVATE int
#if HAVE_STDC
select_namelists(Lsymtab *sym_entry)
#else /* K&R style */
select_namelists(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a NAMELIST name) */
    if (storage_class_of(sym_entry->type) == class_NAMELIST)
	return (1);
    else
	return (0);
}

PRIVATE int
#if HAVE_STDC
select_parameters(Lsymtab *sym_entry)
#else /* K&R style */
select_parameters(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a PARAMETER name) */
    if (sym_entry->parameter)
	return (1);
    else
	return (0);
}



PRIVATE int
#if HAVE_STDC
select_statement_functions(Lsymtab *sym_entry)
#else /* K&R style */
select_statement_functions(sym_entry)
     Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    if (storage_class_of(sym_entry->type) == class_STMT_FUNCTION)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
sf3_internal_name(Lsymtab *sym_entry)
#else /* K&R style */
sf3_internal_name(sym_entry)
     Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{    /* Return (symbol is an SFTRAN3 internal name). */
    char *p = sym_entry->name;

    /* The SFTRAN3 preprocessor uses internal names of the form NPRddd,
       NXdddd, N2dddd, and N3dddd, where d is a decimal digit. */

    if ((p[0] != 'N') || (strlen(p) != 6))
	return (0);
    switch (p[1])
    {
    case 'P':
	if ((p[2] == 'R') && isdigit(p[3]) && isdigit(p[4]) && isdigit(p[5]))
	    return (1);
	else
	    return (0);

    case 'X':                   /* fall through */
    case '2':                   /* fall through */
    case '3':
	if (isdigit(p[2]) && isdigit(p[3]) && isdigit(p[4]) && isdigit(p[5]))
	    return (1);
	else
	    return (0);

    default:
	return (0);
    }
}

PRIVATE int
#if HAVE_STDC
make_sym_list(Lsymtab **sym_list, int (*selector) (Lsymtab *))
#else /* K&R style */
make_sym_list(sym_list,selector)
     Lsymtab *sym_list[];
     PROTO(int (*selector),( Lsymtab *sym_entry ));
#endif /* HAVE_STDC */
{
    int i;
    int n;

    for (i = 0, n = 0; i < loc_symtab_top; ++i)
    {
	if (selector(&loc_symtab[i]))
	    sym_list[n++] = &loc_symtab[i];
    }
    if (n > 0)
    {

	if (selector == select_parameters) {
			/* Free form is not blank-insensitive, so go
			   thru parameter declarations and remove any
			   blanks from within numbers.
			*/
	    if( FREE_FORM() ) {
		for(i=0; i < n; i++) {
		    if( is_numeric_type(get_type(sym_list[i])) ) {
			strip_blanks(sym_list[i]->info.param->src_text);
		    }
		}
	    }
	/* original PARAMETER statement order must be preserved so that
	   the expressions do not refer to as-yet-undefined parameter names */
	    sort_parameters(sym_list,n);
	}
	else
	    sort_lsymbols(sym_list,n);
    }
    return (n);
}

			/* Routine to remove whitespace from a string */
PRIVATE void
strip_blanks(char *s)
{
    char *t;
    for( t=s; *s != '\0'; s++ ) {
	if( !isspace(*s) )
	    *t++ = *s;
    }
    *t = '\0';
}

