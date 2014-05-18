/*      $Id: makehtml.c,v 1.13 2003/03/17 23:24:16 moniot Exp $

   Routines to create HTML documents from FORTRAN analysis and comments

*/

/*

Copyright (C) 2000 by Robert K. Moniot.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
ROBERT K. MONIOT OR FORDHAM UNIVERSITY BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of ftnchek shall not be used
in advertising or otherwise to promote the sale, use or other dealings in
this Software without prior written authorization from the author.

*/

/* Adapted from makedcls.c by Mark McVeigh Sept. 10, '01 */


/*
        Shared functions defined:
                make_html               produces the html document

*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "ftnchek.h"
#include "symtab.h"
#include "plsymtab.h"
#include "tokdefs.h"

extern int free_form;           /* for choosing 'C' or '!' as comment char */

                /* Declarations of local functions */

PROTO(PRIVATE char * base_filename,( char *curr_filename ));

PROTO(PRIVATE char * str_end_strip,( char *str ));
PROTO(PRIVATE void append_char_to_fragment,( int c ));
PROTO(PRIVATE void append_string_to_fragment,( char *s ));
PROTO(PRIVATE void append_expr_text_to_fragment,( char *s ));

PROTO(PRIVATE void new_fragment,( void ));
PROTO(PRIVATE void print_blanks,( int nblanks ));
PROTO(PRIVATE void print_common_decls,( Lsymtab *sym_entry, char **header ));
PROTO(PRIVATE void print_empty_comment_line,( void ));
PROTO(PRIVATE void print_equivalence_decls,( Lsymtab *sym_entry, char **header ));
PROTO(PRIVATE void print_list_decls,( Lsymtab *sym_list[], int n, char
                              **header, char *list_type_name ));
PROTO(PRIVATE int print_list_name,( char *list_type_name, char *name ));
PROTO(PRIVATE void print_declaration_class,( Lsymtab *sym_list[], int n, char
                                     *header ));
PROTO(PRIVATE void print_one_list_decls,( Lsymtab *sym_entry, char
                                  *list_type_name, char **pheader, int
                                  *pnd ));
PROTO(PRIVATE void print_selected_declarations,( Lsymtab *sym_list[], int n,
                                         int the_type, char
                                         *the_type_name, char **pheader ));
PROTO(PRIVATE int  print_selected_common_decls,( Lsymtab *sym_list[], int n,
                                         int the_type, char
                                         *the_type_name, char **pheader ));
PROTO(PRIVATE int  print_selected_equiv_decls,(
                                          Lsymtab *sym_list[],
                                          int n,
                                          char **pheader));
PROTO(PRIVATE int print_typename,( int the_type, char *the_type_name, int
                            the_size, Lsymtab *symt ));
PROTO(PRIVATE int make_sym_list,( Lsymtab *sym_list[], int (*selector)(Lsymtab
                                                              *sym_entry) ));
PROTO(PRIVATE int make_unsorted_sym_list,( Lsymtab *sym_list[],
                           int (*selector)(Lsymtab *sym_entry) ));
PROTO(PRIVATE int select_arguments,( Lsymtab *sym_entry ));
PROTO(PRIVATE void strip_blanks,(char *s));

PROTO(PRIVATE int select_externals_by_name,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_entry_points_by_name,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_intrinsics_by_name,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_locals,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_common_blocks,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_namelists,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_equivalences,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_parameters,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_statement_functions,( Lsymtab *sym_entry ));
PROTO(PRIVATE int sf3_internal_name,( Lsymtab *sym_entry ));

PROTO(PRIVATE char * get_dimension_list,( Lsymtab *symt ));
PROTO(PRIVATE char * get_parameter_value,( Lsymtab *symt ));
PROTO(PRIVATE char * get_size_expression,( Lsymtab *symt ));
PROTO(PRIVATE void htmlout_io_unit_usages,( void ));

#if 0                   /* This is how Beebe wrote it */
#define ACTUAL_SIZE(p)          (((p)->size == 0) ? \
                                 std_size[the_type] : (p)->size)
#else
                /* This is what it has to be if IMPLICIT types supported */
#define ACTUAL_SIZE(p)          (get_size((p),sym_type))
#endif

#define DECLARE_ONLY_UNDECLARED() (html_only_undeclared) /*UNUSED*/
#define DECLARE_COMPACT()       (html_compact)
#define NO_CONTINUATION_LINES() (!(html_use_continuations))
#define EXCL_SF3_DECLARATIONS() (html_excl_sftran3_internal_vars)
#define FREE_FORM()             (html_free_form||free_form)
#define KEYWORDS_LOWERCASE()    (html_keywords_lowercase)
#define VARIABLES_AND_CONSTANTS_LOWERCASE() (html_vars_and_consts_lowercase)
#define ARRAY_VARS_DIMENSIONED() (!(html_no_array_dimensions))

#define SRC_COMMENT_MAX_WIDTH   66       /* This way the text of the HTML will
                                         *  fit in comment lines in original
                                         *  FORTRAN source. */
#define COLUMN_WIDTH            12       /* Fits 4 cols nicely in 66 columns
                                         *  which works well for local var refs */
#define MY_COLUMN_WIDTH         16       /* Used to set the 2nd-Xth variable column
                                         *  in common and equivalence references */

#ifndef PFORT_FIRST_VARIABLE_COLUMN
#define PFORT_FIRST_VARIABLE_COLUMN 18   /* to match Extended PFORT Verifier */
#endif

PRIVATE int first_variable_column;

#define NEXT_COLUMN(column)     (first_variable_column + \
                                (((column) - first_variable_column + \
                                COLUMN_WIDTH - 1) / COLUMN_WIDTH)*COLUMN_WIDTH)

/* MY_NEXT_COLUMN is used to print common block and equivalence references */
#define MY_NEXT_COLUMN(column) (MY_COLUMN_WIDTH + \
                                (((column) - MY_COLUMN_WIDTH + MY_COLUMN_WIDTH-1) \
                                / MY_COLUMN_WIDTH)*MY_COLUMN_WIDTH)

#define isaletter(C)    isalpha((int)(C))

        /* define isidletter to allow underscore and/or dollar sign  */
#define isidletter(C)    (isalpha((int)(C)) || (C) == '_' || (C) == '$')


#define makelower(C) (isupper((int)(C)) ? tolower((int)(C)) : (int)(C))
#define makeupper(C) (islower((int)(C)) ? toupper((int)(C)) : (int)(C))

#define MAX_STMT                (72 + 19*72 + 1) /* longest Fortran stmt */

PRIVATE char stmt_fragment[MAX_STMT];

PRIVATE Lsymtab *current_module;

PRIVATE char comment_char = 'C'; /* default value */

PRIVATE int std_size[] =         /* NB: depends on type_XXX order in symtab.h */
{
    0,                                  /* unknown */
    4,                                  /* INTEGER*4 */
    4,                                  /* REAL*4 */
    8,                                  /* DOUBLE PRECISION == REAL*8 */
    8,                                  /* COMPLEX*8 */
    16,                                 /* DOUBLE COMPLEX == COMPLEX*16 */
    4,                                  /* LOGICAL*4 */
    1                                   /* CHARACTER*1 == CHARACTER */
};

PRIVATE int
pos_fragment = 0;                /* cursor in stmt_fragment buffer */

PRIVATE int
html_indent;                      /* amount to indent declarations */

static int types_table[] =       /* table defining output declaration order */
   {                             /* (alphabetical by type name) */
   type_STRING,
   type_COMPLEX,
   type_DCOMPLEX,
   type_DP,
   type_INTEGER,
   type_LOGICAL,
   type_REAL,
   type_GENERIC,
   type_SUBROUTINE
   };



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>

/* Comment out everything except stub of make_html if system doesn't have
   regex.h */
#if HAVE_REGEX_H

#include <regex.h>

/*=================================================================================
*
* grep
*
* Function to perform UNIX grep function equivalent.
*
* Returns 0 if not found, 1 if found
*
*================================================================================*/
PRIVATE int
#if HAVE_STDC
 grep(
   const char *options,                          /* "i"=ignore case,""=no ignore  */
   const char *search_str,                       /* grep like search string       */
   const char *source_str )                      /* Text to search                */
#else /* K&R style */
 grep( options, search_str, source_str )
   char *options;                               /* "i"=ignore case,""=no ignore  */
   char *search_str;                            /* grep like search string       */
   char *source_str;                            /* Text to search                */
#endif /* HAVE_STDC */
   {
   regex_t re;
   int iret = 0, search_options = REG_NOSUB | REG_EXTENDED;

   memset( &re, 0, sizeof(re) );

   if ( strchr( options, 'i' ) ) search_options |= REG_ICASE;

   /*--- Look through one line at a time for a match. Break out if found --------*/
   if ( regcomp( &re, search_str, search_options ) == 0 )
      {
      if ( regexec( &re, source_str, 0, NULL, 0 ) ^ REG_NOMATCH )
         iret = 1;
      regfree( &re );
      }
   return( iret );
   }

/*=================================================================================
*
* Massage text strings to  1) remove newline (UNIX and DOS), 2) Expand tabs,
* 3) Strip trailing spaces.
*
* Returns pointer to original string for convenience.
*
*================================================================================*/
PRIVATE char *
#if HAVE_STDC
 massage_str(
   char *str )
#else /* K&R style */
 massage_str( str )
   char *str;
#endif /* HAVE_STDC */
   {
   int len,n,j;
   int tab = 8;

   for ( n = 0; n < (len=strlen( str ) ); )
      {
      if ( len > 0 )
         {
         if ( str[n] == '\t' )
            {
            j = tab - ((n+1) % tab);
            memmove( &str[n+1+j], &str[n+1], len );
            memset( &str[n], ' ', j+1 );
            n += j;
            }
         /* Remove DOS and UNIX new lines */
         else if ( str[n] == '\n' || str[n] == '\r' )
            {
            str[n] = '\0';          /* No need to increment n it will be > len */
            }
         else
            {
            n++;
            }
         }
      }
   str_end_strip( str );                        /* Strip blanks from the end */
   return( str );
   }

/*=================================================================================
*
*  Convenience routine to strip leading blanks from a character array
*
*================================================================================*/
PRIVATE char *
#if HAVE_STDC
 str_begin_strip(
   char *str )
#else /* K&R style */
 str_begin_strip( str )
   char *str;
#endif /* HAVE_STDC */
   {
   int n;

   if ( str == NULL ) return( str );

   if ( (n = strspn( str, " " )) ) strcpy( str, &str[n] );
   return( str );
   }

/*=================================================================================
*
*  Convenience routine to strip trailing blanks from a character array
*
*================================================================================*/
PRIVATE char *
#if HAVE_STDC
 str_end_strip(
   char *str )
#else /* K&R style */
 str_end_strip( str )
   char *str;
#endif /* HAVE_STDC */
   {
   int len;

   if ( str == NULL ) return( str );

   len = strlen( str ) - 1;
   while ( len >= 0 && str[len] == ' ' ) { str[len] = '\0'; len--; }
   return( str );
   }

/*=================================================================================
*
*  Convenience routine to strip leading and trailing blanks from a character array
*
*================================================================================*/
PRIVATE char *
#if HAVE_STDC
 str_strip(
   char *str )
#else /* K&R style */
 str_strip( str )
   char *str;
#endif /* HAVE_STDC */
   {
   if ( str == NULL ) return( str );

   return( str_begin_strip( str_end_strip( str ) ) );
   }

/*=================================================================================
*
*  Re-read the FORTRAN source to get comments for HTML output.  Only consider
*  lines that start with a comment char and are IMMEDIATELY adjacent to a
*  subprogram declaration.
*
*================================================================================*/
PRIVATE void
#if HAVE_STDC
 get_comments(
   char *source_file,                           /* FORTRAN source file (full path) */
   char *routine_name )                         /* Name of routine in source file */
#else /* K&R style */
 get_comments( source_file, routine_name )
   char *source_file;                           /* FORTRAN source file (full path) */
   char *routine_name;                          /* Name of routine in source file */
#endif /* HAVE_STDC */
   {
   char  linein[1024];
   char *buffer = NULL;
   char *pbuf = NULL;
   char  buff_end[] = " \n";
   int   buffcount = 0;
   int   found = 0;
   int   row_hdr = 0;
   int   len;
   int   min_leader = 132;
   char  leader_srch[133];
   int   more_than_blank_lines = 0;
   int   free_form_contin=0;

   FILE *infile = fopen( source_file, "r" );

   char search_str[1024];

   if ( routine_name == NULL || source_file == NULL || infile == NULL ) return;

   str_strip( routine_name ); str_strip( source_file );

   /*
   *  Setup a grep extended regular expression to look for the declaration
   *  of the routine in the source file. Var routine_name is set to "BLOCKDATA"
   *  by make_html() if this is an unnamed block data function.
   */
   if ( strcmp( routine_name, "BLOCKDATA" ) == 0 )
      {
      if ( ! free_form )
         sprintf( search_str, "^[^*!c]      *block data$" );
      else
         sprintf( search_str, "^[^!]  *block data$" );
      }
   else
      {
      if ( ! free_form )
         {
         sprintf( search_str,
            "^[^*!c]     [^!]* *function  *%s[ (]*|"
            "^[^*!c]     [^!]* *function  *%s$|"
            "^[^*!c]      *program  *%s$|"
            "^[^*!c]      *subroutine  *%s[ (]*|"
            "^[^*!c]      *subroutine  *%s|"
            "^[^*!c]      *block data  *%s[ (]*|"
            "^[^*!c]      *block data  *%s$",
            routine_name, routine_name, routine_name, routine_name, routine_name,
            routine_name, routine_name );
         }
      else /* freeform may start in column 1, so we have to wing it a bit.
	      The regexprs will turn up "end function" et al but that can't
	      be avoided if they are to find "real function" too.  Only causes
	      trouble if body is totally empty. -RKM
	    */
         {
         sprintf( search_str,
            "^[^!]*function  *%s[ (]*|"
            "^[^!]*function  *%s$|"
            "^[^!]*program  *%s$|"
            "^[^!]*subroutine  *%s[ (]*|"
            "^[^!]*subroutine  *%s$|"
            "^[^!]*block data  *%s[ (]*|"
            "^[^!]*block data  *%s$",
            routine_name, routine_name, routine_name, routine_name, routine_name,
            routine_name, routine_name );
         }
      }

   /*------------------------------------------------------------------------------
   *
   *  What we are trying to do is collect all the comments immediately before
   *  and after the declaration of the routine of interest and save them to the
   *  already open HTML file.
   *
   *  1. Go through the file collecting comments in buffer until there is an
   *     executable statement that is NOT the declaration of the routine of
   *     interest.
   *
   *  2. If the routine declaration was found mixed in with the collection of
   *     comments then write out all the comments to the HTML file, clear the
   *     buffer and re-initialize counter variables and close the file.
   *
   *  3. If the routine declarations was NOT found mixed with the current
   *     collection of comments, clear the buffer, initialize counter variables
   *     and continue reading.
   *
   *-----------------------------------------------------------------------------*/

   while ( fgets( linein, sizeof( linein ), infile ) )
      {
      /*
      *  Ignore everything after column 72 for standard fixed format or
      *  none for free format. This can be overridden with -col=# option
      *  to ftnchek
      */
      if ( ! free_form )
         linein[fixed_max_stmt_col] = '\0';

      massage_str( linein );     /* expand tabs, strip the end of whitespace */

      /* Is this line the declaration of our routine? */
      if ( grep( "i", search_str, linein ) )
         {
         found = 1;
	 free_form_contin = (free_form && grep( "i", "^[^!]*&", linein ));
         }

      /*
      *  If the declaration was found there could be continuation lines.
      *  That's OK.
      */
      else if ( !free_form && found && grep( "i", "^     [^ ]", linein ) )
         {
         continue;
         }
      else if ( free_form && found && grep( "i", "^[^!]*&", linein ) )
         {
	      free_form_contin = 1;
         continue;
         }

      /* Now, is this a comment line? If so accumulate it in buffer */
      else if ( grep( "i", free_form?"^[!]":"^[*!c]", linein ) )
         {
         /*------------------------------------------------------------------------
         * Handle POWERPLEX FORTRAN comment standard. POWERPLEX is a copywrited and
         * trademarked name of Framatome-ANP, Inc.
         *
         *  "C* " Documentation card used when there is more than one    (discard)
         *        routine in the source file
         *  "CC"  Comments for the old documenter program                (discard)
         *  "CD"  General description                                    (keep)
         *  "CDA" Arguments of subroutine or function with description   (keep)
         *  "CDC" ???                                                    (discard)
         *  "CDE" Restatement of subr or func definition with args       (discard)
         *  "CDH" Header                                                 (discard)
         *  "CDP" Purpose                                                (keep)
         *  "CDZ" End of comments                                        (discard)
         *
         *------------------------------------------------------------------------*/
         /* Ignore POWERPLEX comments that aren't useful here. */
         if ( grep( "i", "^c\\* ", linein ) || grep( "i", "^cc", linein ) ||
              grep( "i", "^cd[cehz]", linein ) )
            continue;

         /*
         *  Nobody writes comments where the comment char is immediately followed
         *  by non-whitespace so we'll clear all columns starting with the comment
         *  char followed by non-whitespace.
         */
         for ( pbuf = linein; *pbuf != '\0' && *pbuf != ' '; ) *(pbuf++) = ' ';

         str_end_strip( linein ); /* Strip only the end so we don't lose format */
         len = strspn( linein, " " );           /* Look for the first non-blank */

         if ( len )                 /* Line not blank after clearing col 72+ */
            {
            more_than_blank_lines = 1;
            min_leader = min_leader < len ? min_leader : len;
            }
         else if ( buffcount == 0 ) /* empty comment & no non-blank comments yet */
            {
            continue;
            }
         buffcount += strlen( linein ) + sizeof( buff_end );
         pbuf = buffer;
         buffer = (char *)realloc( buffer, buffcount );
         if ( pbuf == NULL ) buffer[0] = '\0';
         (void)strcat( buffer, linein );
         (void)strcat( buffer, buff_end );
         }
      else if ( grep( "i", "^[ \t]*$", linein ) )
         {
         continue;         /* Discard blank lines from consideration */
         /*-----------------------------------------------------------
         *  Use the following code if blank line inclusion in the
         *  documentation is desired.
         *
         *  str_end_strip( linein );
         *  buffcount += strlen( linein ) + sizeof( buff_end );
         *  pbuf = buffer;
         *  buffer = realloc( buffer, buffcount );
         *  if ( pbuf == NULL ) buffer[0] = '\0';
         *  (void)strcat( buffer, linein );
         *  (void)strcat( buffer, buff_end );
         */
         }
				/* discard last free-form continuation line */
      else if ( found && free_form && free_form_contin )
         {
	 free_form_contin = 0;
	 continue;
	 }
      else     /* Go here if this is not a text line of interest */
         {
         /*
         *  This section dumps the comments to the open HTML file ONLY if the
         *  declaration of the routine was found within the comments.
         */
         if ( found )
            {
            /* Only print if some non-blank lines were accumulated in the buffer */
            if ( buffer && more_than_blank_lines )
               {
               if ( row_hdr == 0 )  /* row_hdr is zero if the table header hasn't */
                  {                 /* been written */
                  fprintf( html_fd, "<table><tr><td align=\"left\"><b>Description</b></td></tr>\n<tr><td><pre>" );
                  row_hdr = 1;
                  }
               /* Remove the last \n and any end of line space */
               pbuf = buffer + strlen( buffer ) - 1;
               while ( pbuf > buffer && ( *pbuf == ' ' || *pbuf == '\n' || *pbuf == '\r' ) )
                  *(pbuf--) = '\0';

               /*
               *  min_leader is the minimum number of spaces on the left of the
               *  lines of text in the comment buffer. Use this value to left
               *  shift all the lines but without losing formatting.
               */
               /* Handle the first line of buffer */
               sprintf( leader_srch, "%*s", min_leader, " " );
               if ( strstr( buffer, leader_srch ) == buffer )
                  memmove( buffer, &buffer[min_leader],
                     strlen( &buffer[min_leader] )+1 );

               /*
               *  Handle the rest of the buffer which will contain \n followed by
               *  min_leader spaces.
               */
               sprintf( leader_srch, "\n%*s", min_leader, " " );
               pbuf = buffer;
               while ( (pbuf = strstr( pbuf, leader_srch ) ) )
                  {
                  pbuf++;                       /* Step past \n */
                  memmove( pbuf, &pbuf[min_leader],
                     strlen( &pbuf[min_leader] ) + 1);
                  }

               /* All done write it out to the HTML file */
               fprintf( html_fd, "%s", buffer );
               }
            break;                              /* We're done so exit early */
            }
         /* Clear the buffer and reset important counters and vars */
         found = 0;
         buffcount = 0;
         more_than_blank_lines = 0;
         min_leader = 132;
         if ( buffer ) free( buffer );
         buffer = pbuf = NULL;
         }

      if ( feof( infile ) ) break;
      }

   /*
   *  Typically buffer will contain some text when the file read is finished.
   *  Never is there a group of comments at the end of a file that we are
   *  interested in, so just free buffer prior to return from this routine.
   */
   if ( buffer )
      {
      free( buffer );
      buffer = pbuf = NULL;
      }

   /* If there were ANY comments written to the HTML file close the HTML table */
   if ( row_hdr == 1 )
      fprintf( html_fd, "</pre></td></tr></table>\n" );

   fclose( infile ); infile = NULL;             /* Close the source file */
   }


/*=================================================================================
*
*  Convenience routine determine filename only from path/filename
*
*================================================================================*/
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

#ifdef MSDOS                    /* look for either \ or / at end. */
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
    int quote_char = ' ', inside_quote;
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
      else {                    /* inside quote */
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

#endif /* HAVE_REGEX_H */

/*=================================================================================
*
*  make_html - Generate HTML page(s) from ftnchek analysis and source embedded
*              comments.  Creates one HTML page from each source analyzed
*
*================================================================================*/
void
#if HAVE_STDC
make_html(
   Lsymtab  **sym_list,
   char     *mod_name,
   Lsymtab  *module )                  /* entry of current module in symtab */
#else /* K&R style */
make_html( sym_list, mod_name, module )
   Lsymtab  *sym_list[];
   char     *mod_name;
   Lsymtab  *module;                   /* entry of current module in symtab */

#endif /* HAVE_STDC */
   {
#if HAVE_REGEX_H
   char *header;
   char module_str[256];
   char modname[256];
   char *base_curr_filename;           /* basename of current input file */
   int mod_type;                       /* datatype of this module */
   int n, nargs;

   if ( ! ANY_HTML_DECLARATIONS() )   /* Just return if no work to be done */
       return;

   base_curr_filename = base_filename(current_filename);

                       /* Establish indentation and comment character
                          for free form or fixed form.
                        */
   html_indent = 0;
   first_variable_column = PFORT_FIRST_VARIABLE_COLUMN;

   comment_char = ' ';

   /* Set up name & type, and see what kind of module it is */

   module = hashtab[current_module_hash].loc_symtab;
   current_module = module;

   strcpy( modname, module->name );
   mod_type = get_type(module);

   /* Print name & type of the module */

   switch ( mod_type )
      {
      case type_PROGRAM:
         (void)sprintf( module_str, "PROGRAM %s", modname );
         break;

      case type_SUBROUTINE:
         (void)sprintf( module_str, "SUBROUTINE %s", modname );
         break;

      case type_BLOCK_DATA:
         n = sprintf( module_str, "BLOCK DATA FUNCTION" );
         /*
         *  ftnchek assigns "%DAT0n" to unnamed common. We don't want this
         *  appearing in the HTML so fix it.  Named block data is fine
         */
         if ( strncmp( modname, "%DAT", 4 ) == 0 )
            strcpy( modname, "BLOCKDATA" );
         else
            sprintf( &module_str[n], " %s", modname );

         break;

      default:
         (void)sprintf( module_str, "%s FUNCTION %s", type_table[mod_type],
                        modname );
         break;
      }

   (void)fprintf( html_fd, "<a name=\"%s\"><h3>%s", modname, module_str );
   nargs = module->info.arglist->numargs;

   if ( mod_type == type_BLOCK_DATA )     /* Don't end BLOCK DATA with parens */
      {
      (void)fprintf( html_fd, "</h3></a>\n<blockquote>\n" );
      }
   else     /* Not BLOCK DATA module type */
      {
      fprintf( html_fd, " ( " );
      for ( n = 0;n < nargs; n++ )
         {
         fprintf( html_fd,"%s%s", module->info.arglist->arg_array[n].name,
                        (n<nargs-1)?", ":" " );
         }

      (void)fprintf( html_fd, ")</h3></a>\n<blockquote>\n" );
      }

   print_declaration_class(sym_list,
                           make_unsorted_sym_list(sym_list,select_arguments),
                           "Argument Definitions</b> <small>(+ indicates "
                           "altered content)</small><b>");

   /* Get special "doc" comments from source file */
   get_comments( current_filename, modname );


   (void)fprintf( html_fd, "<table><tr><td><b>Source file:</b></td>"
                  "<td>%s</td></tr>\n"
                  "<tr><td></td></tr>\n"
                  "</table>\n",
                  base_curr_filename );

   /*
   *  Report I/O usage of this routine. ftnchek has already determined this
   *  information listed by line number of operation (see also -symtab).
   *  For the HTML output line number information is discarded in favor of
   *  a summary.
   */
   htmlout_io_unit_usages();

   print_declaration_class(sym_list,
            make_sym_list(sym_list, select_entry_points_by_name),
            "Alternate Entry Definitions" ),

   print_declaration_class(sym_list,
            make_sym_list(sym_list, select_intrinsics_by_name),
            "Intrinsic Functions Called" ),

   print_declaration_class(sym_list,
            make_sym_list(sym_list,select_statement_functions),
            "Statement functions Defined");

   print_declaration_class(sym_list, make_sym_list(sym_list,
            select_externals_by_name),
            "External Functions and Subroutines Called");

   print_declaration_class(sym_list,
            make_sym_list(sym_list,select_parameters),
            "Parameter Variables Used");

   print_declaration_class(sym_list,
            make_sym_list(sym_list,select_locals),
            "Local Variables</b> <small>(+ indicates altered content)</small><b>");

   print_selected_equiv_decls(sym_list,
            make_sym_list(sym_list,select_equivalences),
            (header = "Referenced Equivalenced Variables</b> <small>"
                      "(+ indicates altered content)</small><b>", &header));

   print_list_decls(sym_list,
            make_sym_list(sym_list,select_namelists),
            (header = "Namelists</b> <small>(+ indicates altered content)</small><b>", &header),
            "NAMELIST" );

   print_list_decls(sym_list,
            make_sym_list(sym_list,select_common_blocks),
            (header = "Referenced Common Block Variables</b> <small>"
                      "(+ indicates altered content)</small><b>", &header),
            "COMMON");

  (void)fprintf(html_fd, "</blockquote>\n<BR>\n" );
#else /* HAVE_REGEX_H */
  if ( ! ANY_HTML_DECLARATIONS() )   /* Just return if no work to be done */
       return;
  (void)fprintf(html_fd,
	"The html documentation is disabled due to lack of regex.h header file"
	" during compilation of ftnchek.<br>\n");
#endif /* HAVE_REGEX_H */
   } /* end of make_html() */

#if HAVE_REGEX_H
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
int     nblanks;
#endif /* HAVE_STDC */
{
    for ( ; nblanks > 0; --nblanks)
        (void)putc(' ',html_fd);
}

                                /* Routine to print namelist and
                                   common declarations. */

PRIVATE void
#if HAVE_STDC
print_common_decls(
   Lsymtab  *sym_entry,         /* COMMON block symbol table entry */
   char    **header)            /* Header to print IF there are variables printed*/

#else /* K&R style */
print_common_decls(sym_entry, header )
   Lsymtab  *sym_entry;        /* COMMON block symbol table entry */
   char    **header;           /* Header to print IF there are variables printed*/
#endif /* HAVE_STDC */
{
    int num_print = 0;
    int h;
    int n,typ;
    Token *t;

#ifdef DYNAMIC_TABLES           /* tables will be malloc'd at runtime */
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
      if ( hashtab[h].loc_symtab->used_flag || hashtab[h].loc_symtab->set_flag )
         sym_list[n++] = hashtab[h].loc_symtab;
      }

   if (n > 0)
      {
      sort_lsymbols(sym_list,n);
      for (typ = 0; typ < sizeof(types_table)/sizeof(types_table[0]); ++typ)
         {
         num_print += print_selected_common_decls(sym_list, n, types_table[typ],
                                        sym_entry->name, header);
         }
    }
}


PRIVATE void
print_empty_comment_line(VOID)
{
    (void)putc('\n',html_fd);
}


PRIVATE void
#if HAVE_STDC
print_equivalence_decls(
   Lsymtab *sym_entry,
   char     **header)
                                /* COMMON block symbol table entry */
#else /* K&R style */
print_equivalence_decls( sym_entry, header )
     Lsymtab *sym_entry;        /* COMMON block symbol table entry */
     char    **header;          /* Title text for report */
#endif /* HAVE_STDC */
{
    int h;
    int n;
    Lsymtab *s;
    Token *t;

#ifdef DYNAMIC_TABLES           /* tables will be malloc'd at runtime */
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

    for ( n = 0, t = sym_entry->src.toklist->tokenlist;
          t != NULL;
          t = t->next_token)
         {
         h = t->value.integer;
         for ( s = hashtab[h].loc_symtab, s = s->equiv_link;
               (s != NULL) && (s != hashtab[h].loc_symtab);
               s = s->equiv_link)
            sym_list[n++] = s;
         }

   if (n > 0)
      {
      sort_lsymbols(sym_list,n);
      print_selected_equiv_decls(sym_list, n, header );
      }
   }



PRIVATE void
#if HAVE_STDC
print_list_decls(Lsymtab **sym_list, int n, char **header, char *list_type_name)
#else /* K&R style */
print_list_decls(sym_list, n, header, list_type_name)
     Lsymtab *sym_list[];
     int n;
     char **header;
     char *list_type_name;
#endif /* HAVE_STDC */
{
    int i, nd;

    nd = 0;
   for (i=0; i<n; i++)
      {                                   /* Loop over COMMON or NAMELIST lists */
      if (sym_list[i]->src.toklist != NULL)
         {
         if ( ( strcmp(list_type_name,"COMMON") == 0) ||
                 ( strcmp(list_type_name,"NAMELIST") == 0 ) )
            {                           /* then COMMON list */
               print_common_decls(sym_list[i], header );

               if ( i == n-1 )
                  {
                  (void)fprintf( html_fd, "</pre></td></tr></table>\n" );
                  }
            }
         else if ( strcmp( list_type_name, "EQUIVALENCE" ) == 0 )
            {
               print_equivalence_decls(sym_list[i], header );
               if ( i == n-1 )
                  {
                  (void)fprintf( html_fd, "</pre></td></tr></table>\n" );
                  }
            }
         else                        /* must be NAMELIST list */
            {
            (void)fprintf( html_fd, "<table><tr><td><pre>" );
            print_one_list_decls(sym_list[i], list_type_name, header, &nd);
            (void)fprintf( html_fd, "</pre></td></tr></table>\n" );
            }
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

                                /* Compact mode:   COMMON /blknam/
                                   Padded mode:    COMMON / blknam /
                                 */
    print_blanks(html_indent);
    column = html_indent;

    for (p = list_type_name; *p; ++p, ++column)
        (void)putc(KEYWORDS_LOWERCASE() ? makelower(*p) : makeupper(*p),
                   html_fd);

    print_blanks(1);
    column++;

    (void)putc('/',html_fd);
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
                   makelower(*p) : makeupper(*p),html_fd);
    }
    column += len;
    if (!DECLARE_COMPACT())
      {
        if (len <= 6)           /* Max standard length */
          {
            print_blanks(7-len); /* Print padding */
            column += 7-len;
          }
      }

    (void)putc('/',html_fd);
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
print_declaration_class(Lsymtab **sym_list, int n, char *header)
#else /* K&R style */
print_declaration_class(sym_list, n, header)
     Lsymtab *sym_list[];
     int n;
     char *header;
#endif /* HAVE_STDC */
{
    unsigned t;

    if (n > 0)
      {
      for (t = 0; t < sizeof(types_table)/sizeof(types_table[0]); ++t)
         {
         print_selected_declarations(sym_list, n, types_table[t],
                                        (char*)NULL, &header);
         }
      fprintf( html_fd, "</pre></td></tr></table>\n" );
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
    ncontin = 0;                /* count of continuation lines */
    nv = 0;             /* count of variables in statement */
    for(t = sym_entry->src.toklist->tokenlist;
        t != NULL;
        t = t->next_token)
      {                 /* Loop over members */
      h = t->value.integer;
      symt = hashtab[h].loc_symtab;
      if ( hashtab[h].loc_symtab->used_flag || hashtab[h].loc_symtab->set_flag )
         {
         if (column == 0)                /* at beginning of line, so */
           {                     /* we need a type name */
             if ((*pheader != (char*)NULL) &&
                 (strcmp(list_type_name,"COMMON") != 0))
               {                         /* print header only once */
                 (void)fprintf(html_fd,"%c     %s\n", comment_char,*pheader);
                 *pheader = (char*)NULL; /* so we don't print it again */
               }
             column = print_list_name(list_type_name,sym_entry->name);
             nv = 0;             /* no variables yet in statement */
             ncontin = 0;
             ++(*pnd);                   /* count declarations produced */
           }

         next_column = NEXT_COLUMN(nv==0?column:column + 2);
         need = (int)strlen(symt->name);

         if ((next_column + need) > SRC_COMMENT_MAX_WIDTH) /* ? start new line */
           {
             if (nv>0 && (strcmp(list_type_name,"COMMON") == 0) &&
                 (NO_CONTINUATION_LINES() || ncontin == (FREE_FORM()?39:19)))
               {
                 (void)putc('\n',html_fd);
                 column = print_list_name(list_type_name,sym_entry->name);
                 nv = 0; /* no variables yet in statement */
                 ncontin = 0;
               }
             else
               {
                 if( FREE_FORM() ) { /* do a free-form continuation */
                   print_blanks(next_column-column);
                   (void)fputs("&\n",html_fd);
                   print_blanks(html_indent);
                   column = html_indent;
                 }
                 else {              /* do a fixed-form continuation */
                   (void)putc('\n',html_fd);
                   print_blanks(5);
                   (void)putc('x',html_fd);
                   column = 6;
                 }
                 if (DECLARE_COMPACT())
                   next_column = (nv==0?column:column + 2);
                 else
                   next_column = NEXT_COLUMN(nv==0?column:column + 2);
                 ++ncontin;
               }
           }
         if (nv > 0)             /* multiple variables */
           {
             (void)fputs(", ",html_fd);
             print_blanks(next_column - column - 2);
             column = next_column;
           }
         for (p = symt->name; *p; ++p)
           (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
                      makelower(*p) : makeupper(*p),html_fd);

         column += need;
         nv++;                   /* count variables */
         }
      }
    if ((nv > 0) && (strcmp(list_type_name,"COMMON") == 0))
      {
        if (column > 0)
              (void)putc('\n',html_fd);
          print_empty_comment_line();
          column = 0;
      }
    if (column > 0)
        (void)putc('\n',html_fd);
}


PRIVATE void
#if HAVE_STDC
print_selected_declarations(Lsymtab **sym_list, int n, int the_type, char *the_type_name, char **pheader)
#else /* K&R style */
print_selected_declarations(sym_list, n, the_type, the_type_name, pheader)
     Lsymtab *sym_list[];
     int n;
     int the_type;
     char *the_type_name;
     char **pheader;
#endif /* HAVE_STDC */
{
    int column, i, last_size, need, next_column, nv;
    int ncontin = 0,
        raw_type, sym_type, sym_size;
    char *p;

    column = 0;
    last_size = 0;
    nv = 0;                             /* count of variables in statement */
    for (i = 0; i < n; ++i)
    {                           /* loop over variables */
        raw_type = datatype_of(sym_list[i]->type);
        sym_type = (raw_type == type_UNDECL) ?
            get_type(sym_list[i]) : datatype_of(sym_list[i]->type);

        if ((the_type != type_ERROR) && (sym_type != the_type))
            continue;

        sym_size = ACTUAL_SIZE(sym_list[i]);
        if ((nv > 0) && (sym_size != last_size))
        {       /* have new length modifier, so must start new declaration */
            (void)putc('\n',html_fd);
            column = 0;
            ncontin = 0;
            nv = 0;
        }
        if (column == 0)                /* at beginning of line, so */
        {                               /* we need a type name */
            if (*pheader != (char*)NULL)
            {                           /* print header only once */
               if ( strlen(*pheader) )
                  (void)fprintf(html_fd,
                     "<table><tr><td align=\"left\"><b>%s</b><br></td></tr>\n"
                     "<tr><td><pre>",*pheader);
               else
                  (void)fprintf(html_fd,"<pre>" );

                *pheader = (char*)NULL; /* so we don't print it again */
            }
            column = print_typename(the_type,the_type_name, sym_size,
                                     sym_list[i]);
            last_size = sym_size;
            nv = 0;             /* no variables yet in statement */
            ncontin = 0;
        }
        if (DECLARE_COMPACT())
            next_column = (nv==0?column:column + 2);
        else
            next_column = NEXT_COLUMN(nv==0?column:column + 2);
        need = (int)strlen(sym_list[i]->name);

        /* Add space for '+' or ' ' identifier to indicate var modification      */
        need++;

        if (sym_list[i]->array_var     /* leave space for "(...)" */
            && ARRAY_VARS_DIMENSIONED())
            need += strlen(get_dimension_list(sym_list[i]));

        if ((next_column + need) > SRC_COMMENT_MAX_WIDTH)  /* ? start new declaration */
        {
            if (nv>0 && (NO_CONTINUATION_LINES() || ncontin == 19))
              {
                (void)putc('\n',html_fd);
                column = print_typename(the_type,the_type_name, sym_size,
                                     sym_list[i]);
                ncontin = 0;
                nv = 0;         /* no variables yet in statement */
              }
            else
              {
                if( FREE_FORM() ) { /* do a free-form continuation */
                  print_blanks(next_column-column);
                  (void)fputs("&<br>\n",html_fd);
                  print_blanks(html_indent);
                  column = html_indent;
                }
                else {              /* do a fixed-form continuation */
                  (void)putc('\n',html_fd);
                  print_blanks(5);
                  (void)putc('x',html_fd);
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
        if (nv > 0)             /* multiple variables */
        {
            (void)fputs(", ",html_fd);
            print_blanks(next_column - column - 2);
            column = next_column;
        }

        /* Identify vars that are modified in this routine */
        if ( sym_list[i]->assigned_flag && ! sym_list[i]->parameter )
            (void)putc('+',html_fd);
        else
            (void)putc(' ',html_fd);

        for (p = sym_list[i]->name; *p; ++p)
            (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
                       makelower(*p) : makeupper(*p),html_fd);
        if (sym_list[i]->array_var
            && ARRAY_VARS_DIMENSIONED())
            (void)fputs(stmt_fragment,html_fd);
        column += need;
        nv++;                   /* count variables */
        if (sym_list[i]->parameter)
        {
            fprintf( html_fd, "%*s", 38 - column, " " );
            fprintf( html_fd, "%s\n",get_parameter_value(sym_list[i]) );
            column = 0;
            nv = 0;
        }
    }

    if (column > 0)
        (void)putc('\n',html_fd);

}


PRIVATE int
#if HAVE_STDC
print_selected_equiv_decls(
   Lsymtab **sym_list,
   int n,
   char **pheader)
#else /* K&R style */
int print_selected_equiv_decls(sym_list, n, pheader)
     Lsymtab *sym_list[];
     int n;
     char **pheader;
#endif /* HAVE_STDC */
   {
   int column, i, next_column,
       sym_type=0, sym_size, block_name_col_width;
   char *p, scope_str[128];
   Lsymtab *cur_sym, *tmp_sym;

   column = 0;
   block_name_col_width = first_variable_column + 1;

   for (i = 0; i < n; ++i)
      {                           /* loop over variables */
      if ( sym_list[i]->equiv_link == NULL ) continue;

      if (column == 0)                /* at beginning of line, so */
         {                            /* we need a type name */
         if ( *pheader != (char*)NULL )
            {
            if ( strlen( *pheader ) )
               (void)fprintf(html_fd,
                  "<table>\n<tr><td align=\"left\"><b>%s</b></td></tr>\n<tr><td><pre>",
                  *pheader);
            *pheader = (char *)NULL;
            (void)fprintf(html_fd, "\n" );
            }

         column += fprintf( html_fd, "\n%-*.*s",
                     block_name_col_width, block_name_col_width, "EQUIV" );
         }

      /*
      *  Circulate within current equivalence list to print all members.
      *  After each member is printed set it's equiv_link to NULL so we don't
      *  report it again.
      *
      *  NOTE: This affects the sym_list passed to make_html()
      *  Place this so remaining processing does not need the equiv_link.
      */
      cur_sym = sym_list[i];

      while ( cur_sym->equiv_link != NULL )
         {
         if ( column == 0 )
            column += fprintf( html_fd, "\n%*.*s",
                        block_name_col_width, block_name_col_width, "" );

         /*--- Print var type ---*/
         sym_size = ACTUAL_SIZE( cur_sym );
         sym_type = (datatype_of(cur_sym->type) == type_UNDECL) ?
            get_type(cur_sym) : datatype_of(cur_sym->type);
         column += print_typename(sym_type, NULL, sym_size, cur_sym);

         /*--- Print var name, but first prefix "set" flag if required ---*/
         next_column = MY_NEXT_COLUMN( column ) + 2; /* +2 to make it match common refs */
         if ( cur_sym->assigned_flag && ! cur_sym->parameter )
            (void)putc('+',html_fd);
         else
            (void)putc(' ',html_fd);
         column ++;

         /* Print var name upper or lower case as desired */
         for (p = cur_sym->name; *p; ++p)
            (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
                       makelower(*p) : makeupper(*p),html_fd);
         column += strlen( cur_sym->name );

         /* Append a hint that this is an array. Use "()" for array, "  " if not */
         column += fprintf( html_fd, "%s", cur_sym->array_var ? "()" : "  " );

         /* Finish up the var name print by spacing over for next "scope" column */
         for ( ; column < next_column; column++ ) (void)putc( ' ', html_fd );

         /*--- Print var scope ---*/
         if ( cur_sym->common_block )
            sprintf( scope_str, "COMMON /%s/", cur_sym->common_block->name );
         else
            sprintf( scope_str, "Local Var" );

         column += fprintf( html_fd, "%-s", scope_str );

         column = 0;
         tmp_sym = cur_sym->equiv_link;
         cur_sym->equiv_link = NULL;
         cur_sym = tmp_sym;
         }

      }

   if ( n > 0 )
      fprintf( html_fd, "</pre></td></tr></table>\n" );

   return( n );
   }


PRIVATE int
#if HAVE_STDC
print_selected_common_decls(
   Lsymtab **sym_list,
   int n,
   int the_type,
   char *comname,
   char **pheader)
#else /* K&R style */
int print_selected_common_decls(sym_list, n, the_type, comname, pheader)
     Lsymtab *sym_list[];
     int n;
     int the_type;
     char *comname;
     char **pheader;
#endif /* HAVE_STDC */
   {
   int column, i, tv, last_size, need, next_column, nv,
       raw_type, sym_type, sym_size, block_name_col_width;
   char *p;

   column = 0;
   block_name_col_width = first_variable_column + 1;
   last_size = 0;
   nv = 0;                             /* count of variables in statement */
   tv = 0;                             /* total number of vars printed */

   for (i = 0; i < n; ++i)
      {                           /* loop over variables */
      raw_type = datatype_of(sym_list[i]->type);
      sym_type = (raw_type == type_UNDECL) ?
         get_type(sym_list[i]) : datatype_of(sym_list[i]->type);

      if ((the_type != type_ERROR) && (sym_type != the_type))
         continue;

      sym_size = ACTUAL_SIZE(sym_list[i]);
      if ((nv > 0) && (sym_size != last_size))
         {       /* have new length modifier, so must start new declaration */
         (void)putc('\n',html_fd);
         column = 0;
         nv = 0;
         }
      if (column == 0)                /* at beginning of line, so */
         {                            /* we need a type name */
         if ( *pheader != (char*)NULL )
            {
            if ( strlen( *pheader ) )
               (void)fprintf(html_fd,
                  "<table>\n<tr><td align=\"left\"><b>%s</b></td></tr>\n<tr><td><pre>",
                  *pheader);
            *pheader = (char *)NULL;
            }

         column += fprintf( html_fd, "%-*.*s",
                     block_name_col_width, block_name_col_width, comname );
         column += print_typename(the_type, NULL, sym_size, sym_list[i]);
         last_size = sym_size;
         nv = 0;             /* no variables yet in statement */
         }
      next_column = MY_NEXT_COLUMN( nv == 0 ? column : column + 2 );
      need = (int)strlen(sym_list[i]->name);

      /* Add space for '+' or ' ' identifier to indicate var modification      */
      need++;

      if (sym_list[i]->array_var     /* leave space for "(...)" */
           && ARRAY_VARS_DIMENSIONED())
           need += strlen(get_dimension_list(sym_list[i]));

      /*
      *  Start new declaration if we are out of space on the line and one or more
      *  vars have already been printed
      */
      if ( nv > 0 && (next_column + need) > SRC_COMMENT_MAX_WIDTH )
         {
         (void)putc('\n',html_fd);
         column = 0;
         column += fprintf( html_fd, "%-*.*s",
                     block_name_col_width, block_name_col_width, comname );
         column += print_typename(the_type,NULL, sym_size, sym_list[i]);
         nv = 0;              /* no variables yet in statement */
         last_size = sym_size;
         }
      if (nv > 0)             /* multiple variables */
         {
         (void)fputs(", ",html_fd);
         print_blanks(next_column - column - 2);
         column = next_column;
         }

      /* Identify vars that are modified in this routine */
      if ( sym_list[i]->assigned_flag && ! sym_list[i]->parameter )
         (void)putc('+',html_fd);
      else
         (void)putc(' ',html_fd);

      for (p = sym_list[i]->name; *p; ++p)
          (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
                      makelower(*p) : makeupper(*p),html_fd);
      tv ++;

      if (sym_list[i]->array_var
          && ARRAY_VARS_DIMENSIONED())
           (void)fputs(stmt_fragment,html_fd);
      column += need;
      nv++;                   /* count variables */
      if (sym_list[i]->parameter)
         {
         fprintf( html_fd, "%*s", 39 - column, " " );
         fprintf( html_fd, "%s\n",get_parameter_value(sym_list[i]) );
         column = 0;
         nv = 0;
         }
      }

   if (column > 0)
      (void)putc('\n',html_fd);

   return( tv );
   }

PRIVATE int
#if HAVE_STDC
print_typename(int the_type, char *the_type_name, int the_size, Lsymtab *symt)
                                /* type_ERROR if typename non-NULL */
                                /* non-NULL overrides type_table[] use */
#else /* K&R style */
print_typename(the_type,the_type_name,the_size,symt)
int     the_type;               /* type_ERROR if typename non-NULL */
char    *typename;              /* non-NULL overrides type_table[] use */
int     the_size;
Lsymtab *symt;
#endif /* HAVE_STDC */
{                               /* return value is last column printed */
    int column;
    char digits[sizeof("*18446744073709551616")]; /* big enough for 2^64 */
    char *p;
    char *size_expression;

    print_blanks(html_indent);
    column = html_indent;

    for (p = (the_type_name == (char*)NULL) ? type_table[the_type] : the_type_name;
         *p; ++p, ++column)
        (void)putc(KEYWORDS_LOWERCASE() ? makelower(*p) : makeupper(*p),
                   html_fd);
    if (symt != NULL) {
      if (((symt->size_is_adjustable && (the_type == type_STRING))) ||
          (the_size == size_ADJUSTABLE)) /* happens only for CHARACTER*(*) */
        {
            /* size_is_adjustable overrides the_size because def_parameter() */
            /* in symtab.c replaced size_ADJUSTABLE with actual size. */
            (void)fputs("*(*)",html_fd);
            column += 4;
        }
      else if (symt->size_is_expression && (the_type == type_STRING))
        {
            size_expression = get_size_expression(symt);
            (void)fputs(size_expression,html_fd);
            column += strlen(size_expression);
        }
      else if ((the_size > 0) &&
               (the_type != type_ERROR) &&
               (the_size != std_size[the_type]))
        {       /* supply length modifier for non-standard type sizes */
            (void)sprintf(digits,"*%d",the_size);
            (void)fputs(digits,html_fd);
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


PRIVATE int
#if HAVE_STDC
 select_externals_by_name(
   Lsymtab *sym_entry )
#else /* K&R style */
 select_externals_by_name( sym_entry )
   Lsymtab *sym_entry;
#endif /* HAVE_STDC */
   {
   /* Select subroutines and external function calls that are NOT the current
   *  Module.
   */
   if ( sym_entry == current_module )
      return( 0 );
   else if (sym_entry->declared_intrinsic) /* must appear first, because symbols */
      return (0); /* can be both declared_intrinsic and declared_external*/
                    /* ??? is this a bug in ftnchek 2.7 ??? */
   else if ( datatype_of(sym_entry->type) == type_SUBROUTINE )
      return (1);
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
select_entry_points_by_name(Lsymtab *sym_entry)
#else /* K&R style */
select_intrinsics_by_name(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is intrinsic and must appear in INTRINSIC declaration) */
    if (sym_entry->entry_point && ! sym_entry->is_current_module )
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
    if (sym_entry->declared_intrinsic || sym_entry->intrinsic )
        return (1);
    else
        return (0);
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
        ( storage_class_of(sym_entry->type) == class_COMMON_BLOCK ) ||
        sym_entry->declared_external ||
        sym_entry->declared_intrinsic ||
        sym_entry->entry_point ||
        sym_entry->external ||
        sym_entry->intrinsic ||
        sym_entry->invoked_as_func ||
        sym_entry->parameter)
        return (0);
    else if ( sym_entry->used_flag || sym_entry->assigned_flag )
        return (1);
    else
        return( 0 );
}

PRIVATE int
#if HAVE_STDC
select_equivalences(Lsymtab *sym_entry)
#else /* K&R style */
select_equivalences(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a local variable) */

    if ( ( sym_entry->used_flag || sym_entry->assigned_flag ) &&
         ( sym_entry->equiv_link != NULL && sym_entry->equiv_link != sym_entry ) )
        return (1);
    else
        return( 0 );
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
      {
      return (1);
      }
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
    if (sym_entry->parameter && sym_entry->used_flag )
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


PRIVATE int
#if HAVE_STDC
make_unsorted_sym_list(Lsymtab **sym_list, int (*selector) (Lsymtab *))
#else /* K&R style */
make_unsorted_sym_list(sym_list,selector)
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
#if HAVE_STDC
strip_blanks(
   char *s)
#else /* K&R style */
strip_blanks( s )
   char *s;
#endif
   {
   char *t;
   for( t=s; *s != '\0'; s++ )
      {
      if ( !isspace(*s) )
         *t++ = *s;
      }
   *t = '\0';
   }

/*=================================================================================
*
*  Compare two structures (IO_Unit_Info).  If they are the same return 1,
*  otherwise return 0;
*
*================================================================================*/
PRIVATE int
#if HAVE_STDC
compare_info(
   IO_Unit_Info *io1,
   IO_Unit_Info *io2 )
#else /* K&R style */
compare_info( io1, io2 )
   IO_Unit_Info *io1;
   IO_Unit_Info *io2;
#endif
   {
   int i = 0;
   if ( io1->io_access == io2->io_access && io1->io_form == io2->io_form )
      if ( io1->unit_id == io2->unit_id )
         if ( io1->unit_no == io2->unit_no )
            i = 1;

   return( i );
   }

/*=================================================================================
*
*  Use the ftnchek operation types to accumulate a character string abbreviation
*  of the operations performed.
*
*================================================================================*/
PRIVATE void
#if HAVE_STDC
choose_opcode(
   char *opcode,
   int   io_operation )
#else /* K&R style */
choose_opcode( opcode, io_operation )
   char *opcode;
   int   io_operation;
#endif
   {
   switch( io_operation )
      {
      case tok_REWIND   : opcode[0] = 'A'; break;
      case tok_BACKSPACE: opcode[1] = 'B'; break;
      case tok_ENDFILE  : opcode[2] = 'E'; break;
      case tok_CLOSE    : opcode[3] = 'C'; break;
      case tok_INQUIRE  : opcode[4] = 'I'; break;
      case tok_OPEN     : opcode[5] = 'O'; break;
      case tok_READ     : opcode[6] = 'R'; break;
      case tok_WRITE    :
      case tok_PRINT    : opcode[7] = 'W'; break;
      }
   }

/*=================================================================================
*
*  Determine and set the access and form type based on
*  operation.  We want to get rid of "default" for read,
*  write, print and open but mark accordingly others
*================================================================================*/
PRIVATE void
#if HAVE_STDC
set_access_and_format_code(
   IO_Unit_Info *save_info )
#else /* K&R style */
set_access_and_format_code( sav_info )
   IO_Unit_Info *save_info;
#endif /* HAVE_STDC */
   {
   switch ( save_info->io_operation )
      {
      case tok_INQUIRE:
      case tok_CLOSE:
         save_info->io_access = IO_ACCESS_DEFAULT;
         save_info->io_form = IO_FORM_DEFAULT;
         break;

      case tok_BACKSPACE:
      case tok_REWIND:
      case tok_ENDFILE:
         save_info->io_access = IO_ACCESS_SEQUENTIAL;
         save_info->io_form = IO_FORM_DEFAULT;
         break;

      default:
         if ( save_info->io_access == IO_ACCESS_DEFAULT )
            save_info->io_access = IO_ACCESS_SEQUENTIAL;

         if ( save_info->io_form == IO_FORM_DEFAULT )
            {
            if ( save_info->io_access == IO_ACCESS_SEQUENTIAL )
               save_info->io_form = IO_FORM_FORMATTED;
            else
               save_info->io_form = IO_FORM_UNFORMATTED;
            }
         break;

      }  /* End switch */
   }


/*=================================================================================
*
*  Routine to summarize I/O usage for the HTML output module.  ftnchek generates
*  internal lists of I/O operations by line number.  This is too verbose for the
*  HTML summary so this routine produces a report of all I/O operations by each
*  unique combination of unit name, unit number, access type and format type.
*
*  We don't want to modify ftnchek's internal lists because they might be used
*  by some routine down-stream so local copies are made and destroyed after use.
*
*================================================================================*/
PRIVATE void
#if HAVE_STDC
htmlout_io_unit_usages(
   void )
#else /* K&R style */
htmlout_io_unit_usages( )
#endif
   {
   static int i, j;
   static char* IO_access[]={"    ","DIR","SEQ"};
   static char* IO_form[] ={"    ","UNF","FMTD"};

   static char  opcode[] = "          ";
   static IO_Unit_Info **my_ioinfo, *ioinfo_ptr;
   static const char *unit_name;
   static int h;

   IO_Unit_Info save_info = { 0 };

   if ( num_io_unit_usages > 0 )
      {
      fprintf(html_fd,"\n<table><tr><td><b>I/O Operations:</b></td></tr>\n");
      fprintf(html_fd,"<tr><td><pre>" );
      fprintf(html_fd,"Unit ID  Unit No       Access  Form   Operation");

      /* Copy the io information structure to a new array so we can rework it */
      my_ioinfo = (IO_Unit_Info **) malloc( sizeof(IO_Unit_Info) * num_io_unit_usages );
      memcpy( my_ioinfo, io_unit_info,
         sizeof(IO_Unit_Info) * num_io_unit_usages );

      /*---------------------------------------------------------------------------
      *  I/O information is stored by line number.  We don't want line no. for the
      *  HTML summary so loop through the ftnchek list.  We have a current I/O
      *  item of interest.  It is compared with others in the list.  Each match of
      *  unit name, unit no., access type AND format type results in accumulation of
      *  operation code in var opcode and setting the line number to -1.
      *
      *  The next time through the loop ignores already counted operations by
      *  looking for a line number of -1.
      *--------------------------------------------------------------------------*/
      for ( i = 0; i < num_io_unit_usages; i++ )
         {
         /* A new io reference */
         ioinfo_ptr = (IO_Unit_Info *)((char *)my_ioinfo + i * sizeof(IO_Unit_Info));

         /* If line_num is -1 we already accounted for this reference so skip */
         if ( ioinfo_ptr->line_num != -1 )
            {
            /* compare the current I/O reference to determine "NEWNESS" */
            if ( compare_info( ioinfo_ptr, &save_info ) == 0 )
               {
               /* This is a new reference.  Copy to the holding area, save_info */
               memcpy( &save_info, ioinfo_ptr, sizeof( IO_Unit_Info ) );
               ioinfo_ptr->line_num = -1;

               /*
               *  Set access and format parameters for our summary report
               */
               set_access_and_format_code( &save_info );

               strcpy( opcode, "          " );        /* Initialize for this ref */
               choose_opcode( opcode, ioinfo_ptr->io_operation ); /* get abbrev */

               /*------------------------------------------------------------------
               *  Look down the list for other matching references accumulating
               *  opcode abbreviations and setting matching reference line_num
               *  to -1.
               *-----------------------------------------------------------------*/
               for ( j = i + 1; j < num_io_unit_usages; j++ )
                  {
                  ioinfo_ptr = (IO_Unit_Info *)((char *)my_ioinfo +
                               j * sizeof(IO_Unit_Info));
                  /*
                  *  Set access and format parameters for our summary report
                  */
                  set_access_and_format_code( ioinfo_ptr );

                  /* compare the current I/O reference to the saved reference */
                  if ( compare_info( ioinfo_ptr, &save_info ) == 1 )
                     {
                     /* References match so set line_num and get opcode */
                     ioinfo_ptr->line_num = -1;

                     choose_opcode( opcode, ioinfo_ptr->io_operation );
                     }
                  }

               /*------------------------------------------------------------------
               *  Now we should have all of one io reference with opcode
               *  summary.  Print out results
               *-----------------------------------------------------------------*/

               /* Print unit name and number or blank if unknown */

               if ( (h=save_info.unit_id) < 0)
                  {
                  /* handle cases of unknown and default */
                  unit_name = (h == IO_UNIT_DEFAULT)? "*": "";
                  }
               else
                  {
                  /* handle cases where it is an identifier, unit_id=hashnum */
                  unit_name = hashtab[h].name;
                  }

               /* Name portion */
               fprintf( html_fd, "\n%7s%8s", unit_name,
                  /* if id is a parameter, print name=value */
                  ( save_info.unit_id >= 0 &&
                    save_info.unit_no >= 0 ) ? "=" : " " );

               /* Unit number portion */
               if ( save_info.unit_no < 0 )
                  fprintf(html_fd,"%7s","");
               else
                  fprintf(html_fd,"%-7d",save_info.unit_no);

               /* Print out access type, format and operations summary */
               fprintf(html_fd,"   %4s  %4s    %-9s",
                      IO_access[save_info.io_access],
                      IO_form[save_info.io_form] ,
                      opcode );

               }
            }
         }

      /* All units have been written, finish up the html table */
      fprintf( html_fd, "\n"
         "\n<small>Operation codes A=rewind,B=backspace,C=close,E=endfile"
         "\n                I=inquire,O=open,R=read,W=write</small>" );
      fprintf( html_fd, "</pre></td></tr></table>\n" );
      free( my_ioinfo );
      }
}

#endif /* HAVE_REGEX_H */

