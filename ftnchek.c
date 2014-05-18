/* $Id: ftnchek.c,v 1.45 2003/03/20 22:14:02 moniot Exp $

	Main program for Fortran Syntax Checker.
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
	Top-level input/output is done here: opening and closing files,
	and printing error, warning, and informational messages.

	Shared functions defined:
		print_a_line()	Prints source code line.
		yyerror()	Error messages from yyparse and elsewhere.
		syntax_error()	Error messages with line and column num.
		warning()	Warning messages.
		nonportable()	Portability warnings.
		wrapup()	Look at cross references, etc.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef DEVELOPMENT             /* For maintaining the program */
#define DEBUG_SIZES
#endif
#define MAIN
#include "ftnchek.h"
#include "intrins.h"
#include "options.h"
#include "utils.h"	/* we use strncasecmp */

#ifdef VMS
#define unlink(s) remove(s)
#else
#if HAVE_UNISTD_H
#include <unistd.h>
#else
#ifndef __LCC__    /* The LCC compiler doesn't like the following defn. */
PROTO( int unlink,( const char *pathname ) );
#endif
#endif
#endif

#ifdef USE_SHELL_MUNG
PROTO( int shell_mung, (int *  argc_p, char ***  argv_p,
			int  parameter_number,  char *  option_string));
#endif

PROTO(PRIVATE char * append_extension,( char *s, const char *ext, int mode ));

PROTO(PRIVATE void error_summary,( char *fname ));

PROTO(int main,( int argc, char *argv[] ));

PROTO(PRIVATE char * new_ext,( char *s, const char *ext ));

PROTO(PRIVATE void open_outfile,( char *s ));

PROTO(PRIVATE void do_preps,( void ));

#ifdef DEBUG_SIZES
PROTO(extern void print_sizeofs,( void ));	/* in symtab.c */
#endif

PROTO(PRIVATE void print_version_number,( void ));

PROTO(PRIVATE void resource_summary,( void ));

PROTO(PRIVATE void src_file_in,( char *infile ));

PROTO(PRIVATE void wrapup,( void ));

PRIVATE int project_file_input;	/* true if input is from .prj file */

#ifdef DEBUG_SIZES
PRIVATE unsigned long intrins_clashes;
#endif
				/* count of intrinsic hashtable clashes */
#ifdef COUNT_REHASHES
extern unsigned long rehash_count; /* count of calls to rehash() */
#endif


PRIVATE int checks_on=TRUE; /* Keep track whether -nocheck was given */

PRIVATE char *dclfile;
int
#if HAVE_STDC
main(int argc, char **argv)
#else /* K&R style */
main(argc,argv)
	int argc;
	char *argv[];
#endif /* HAVE_STDC */
{
	int iarg;
	int filecount=0;
	char *infile,*srcfile,*projfile;

				/* The shell_mung routine from GNU can be
				   used to expand wildcards etc. for VMS.
				*/
#ifdef USE_SHELL_MUNG
	shell_mung(&argc,&argv,1,NULL);
#endif

#ifdef __EMX__          /* MSDOS and OS/2 emx version: expand wildcards */
                        /* by Michael.Taeschner@dlr.de */
        _wildcard(&argc,&argv);
#endif

	list_fd = stdout;
	project_fd = (FILE *) NULL;
	error_count = 0;
	warning_count = 0;
	include_path_list = (IncludePathNode*) NULL;
	doing_wrapup = doing_end_proc = FALSE;

	get_env_options();	/* Pick up options from environment */
	get_rc_options();	/* Pick up options from "rc" file */

	init_tables();		/* Initialize tables */
	init_keyhashtab();
#ifdef DEBUG_SIZES
	intrins_clashes =
#else
	(void)
#endif
	    init_intrins_hashtab();
	init_globals();
	init_symtab();

	for(iarg=1; iarg < argc; iarg++) {

	  int argchar=0;/* location of start of option */
			/* Note to maintainer: since the /option version
			   has a loop here instead of an if, do not
			   use continue but goto next_arg for skipping
			   to the next argument.  This is a mess, isn't it?
			 */
#ifdef OPTION_PREFIX_SLASH
	  do {			/* loop on flags within argv[iarg] */
#endif
	    if( argv[iarg][argchar] == '-'
#ifdef OPTION_PREFIX_SLASH
		 || argv[iarg][argchar] == '/'	/* Allow VMS /option form */
#endif
					 ) {
			/* Process flags here */

		set_option(&argv[iarg][argchar],"commandline");

			/* Handle -version, -help, or -f77=help */
		if(print_version) goto do_action;

		if(help_screen) goto do_action;

				/* Allow checking to be turned off */
		if( !do_check && checks_on ) {
		  turn_off_checks();
		  checks_on = FALSE;	/* remember it was done */
		}

	    }
	    else if(strcmp(&argv[iarg][argchar],"?") == 0) {
		    help_screen = TRUE;
		    goto do_action;
	    }/*end of processing options*/

	    else {	/* Process file arguments */
do_action:

		if( must_open_outfile )
		    open_outfile(out_fname);

		if(actioncount == 0) {
		  print_version_number();
		}
		++actioncount;	/* Cause exit w/o reading stdin below */

			/* Honor -version, -help and -f77=help options */
		if(print_version) {
		  print_version = FALSE;
		  goto next_arg;
		}

		if(help_screen) {
		  help_screen = FALSE;
		  list_options(list_fd);
		}
		else {	/* Process files here */

		    if(filecount == 0)
		      do_preps(); /* Any preparations needed before processing */

		    ++filecount;

		    srcfile = add_ext(&argv[iarg][argchar],DEF_SRC_EXTENSION);
		    projfile = new_ext(&argv[iarg][argchar],DEF_PROJ_EXTENSION);
		    dclfile =  new_ext(&argv[iarg][argchar],DEF_DCL_EXTENSION);
                    html_filename = new_ext(&argv[iarg][argchar],DEF_HTML_EXTENSION);
#ifdef VCG_SUPPORT
				/* Initialize main_filename to 1st file arg */
		    if(main_filename == (char *)NULL)
		      main_filename = argv[iarg];
#endif
				/* Project file mode: open source for reading
				   and .prj file for writing. */
		    if(make_project_file) {

		      infile = srcfile;

		      if( has_extension(infile,DEF_PROJ_EXTENSION) ) {
			(void)fprintf(stderr,
			 "Input from %s disallowed in project mode\n",infile);
			goto next_arg;
		      }

		      if( (input_fd = fopen(infile,"r")) == (FILE *)NULL ) {
			(void)fprintf(stderr,"Cannot open file %s\n",infile);
			goto next_arg;
		      }

		      project_fd = fopen(projfile,"w");
		      project_file_input = FALSE;
		    }
		    else {
			/* Non project file mode: if input file extension
			   given, use it.  Otherwise read project file
			   if it exists else read source file. */
		      if( &argv[iarg][argchar]==srcfile
		       || (input_fd = fopen(projfile,"r")) == (FILE *)NULL) {
			infile = srcfile;
			if( (input_fd = fopen(infile,"r")) == (FILE *)NULL ) {
			  (void)fflush(list_fd);
			  (void)fprintf(stderr,"Cannot open file %s\n",infile);
			  goto next_arg;
			}
			project_file_input =
			  has_extension(infile,DEF_PROJ_EXTENSION);
		      }
		      else {
			infile = projfile;
			project_file_input = TRUE;
		      }
		    }

		    /* now that we have a source file, try to open the 
		       declaration file */
		    dcl_fd = (ANY_DCL_DECLARATIONS() &&  ! project_file_input) ?
		      fopen(dclfile,"w") : (FILE*)NULL;

                      
                    /* Create html tree file if requested */
                    if ( ANY_HTML_DECLARATIONS() && htmlcalltree_filename == NULL &&
                         print_call_tree && ! project_file_input )
                        {
                        /*
                        *  for lack of a better place, create CallTree.html in the
                        *  local directory
                        */
                        htmlcalltree_filename = "CallTree.html";
                        htmlcalltree_fd = fopen( htmlcalltree_filename, "w" );
                        (void)fprintf( htmlcalltree_fd,
                           "<HTML>\n<HEAD><TITLE>Program Call Tree</TITLE></HEAD>\n<BODY>\n<pre>" );
                        }
                     
                    /* Create an html output file if requested */
                    html_fd = ( ANY_HTML_DECLARATIONS() && ! project_file_input ) ?
                       fopen( html_filename, "w" ) : (FILE *)NULL;
                    if ( html_fd )
                       fprintf( html_fd, 
                          "<HTML>\n<HEAD><TITLE>Source %s</TITLE></HEAD>\n<BODY>\n",
                          srcfile );                        

				/* Always print input .f file name.  If
				   verbose mode, print .prj file names too.
				 */
		    if(!quiet || !project_file_input)
		      (void)fprintf(list_fd,"\nFile %s:%s",
			      infile,
			      full_output?"\n":""
			      );

				/* In verbose mode, print .prj output
				   file name to stderr.  Always print
				   error message if couldn't open it. */
		    if( make_project_file ) {
		      if(project_fd != (FILE *)NULL) {
			if(!quiet) {
			  (void)fflush(list_fd);
			  (void)fprintf(stderr,
				  "\nProject file is %s\n",projfile);
			}
		      }
		      else {
			(void)fflush(list_fd);
			(void)fprintf(stderr,
				"\nCannot open %s for output\n",projfile);
		      }
		    }


		    if(project_file_input) {

		        current_filename = projfile;
			proj_file_in(input_fd);

		    }
		    else {

		      src_file_in(infile);

		    }

		    (void) fclose(input_fd);
		}/*end processing file args*/
	      }
next_arg:
#ifdef OPTION_PREFIX_SLASH
				/* Here we allow /opts to be stuck together */
	    while(argv[iarg][++argchar] != '\0'
		 && argv[iarg][argchar] != '/') /* look for next opt */
	      continue;

	  } while(argv[iarg][argchar] != '\0'); /*end do-while*/
#else
	  continue;
#endif
	}	/* end for-loop on argument list */


				/* No files given: read stdin */
	if(actioncount == 0) {

		print_version_number();

		if( must_open_outfile )
		    open_outfile(out_fname);

		do_preps();	/* Any preparations needed before processing */

		if(make_project_file) {
		      projfile = STDIN_PROJ_FILENAME;
		      if( (project_fd = fopen(projfile,"w")) == (FILE *)NULL) {
			(void)fflush(list_fd);
			(void)fprintf(stderr,
				"\nCannot open %s for output\n",projfile);
		      }
		      else {
			if(!quiet) {
			  (void)fflush(list_fd);
			  (void)fprintf(stderr,
				"\nProject file is %s\n",projfile);
			}
		      }
		}

		++filecount;
		input_fd = stdin;

		src_file_in("std_input");
	}
	if(filecount > 0) {
	  wrapup();
	  (void)fprintf(list_fd,"\n");
	}

	if(show_resources)
	    resource_summary();

	exit(0);
	return 0;/*NOTREACHED*/
}

				/* do_preps does anything necessary prior
				   to processing 1st file, such as setting
				   the intrinsic function options.  It is
				   only called once.
				*/
PRIVATE void
do_preps(VOID)
{

  init_typesizes();	/* Put -wordsize and -pointersize into effect */

#ifndef STANDARD_INTRINSICS
  set_intrinsic_options(); /* Make intrinsic table match -intrinsic setting */
#endif
}

PRIVATE void
#if HAVE_STDC
src_file_in(char *infile)
                  		/* input filename */
#else /* K&R style */
src_file_in(infile)
     char *infile;		/* input filename */
#endif /* HAVE_STDC */
{
	note_filename(infile);

	make_legal_char_list();
	init_scan();
	init_parser();

	(void) yyparse();

	finish_scan();

	if(make_project_file) {
		  proj_file_out(project_fd);
		  (void) fclose(project_fd);
	}

        if ( html_fd != (FILE *) NULL )
            {
            fputs( "</BODY>\n</HTML>\n", html_fd );
            (void) fclose( html_fd );
            html_fd = NULL;
            }

	if ((dcl_declarations) && (dcl_fd != stdout))
	{

	    if (ftell(dcl_fd) == 0L)	/* delete an empty .dcl file */
            {
              /* some systems like OS/2 lock open files and can't  */
              /* remove an open file unless closed. SAD-10/96      */
	        (void) fclose(dcl_fd);   /* close file */
		(void) unlink(dclfile);
            }
	    else {
	      (void) fclose(dcl_fd);
	    }
	}

	if(port_tabs && (tab_filename != (char *)NULL)) {
	  if(! quiet)
	    (void)fprintf(list_fd,"\n");
	  if(tab_filename != top_filename) {
	    nonportable(NO_LINE_NUM,NO_COL_NUM,
			"Included file");
	    msg_tail(tab_filename);
	  }
	  else {
	    nonportable(NO_LINE_NUM,NO_COL_NUM,
		      "File");
	  }
	  msg_tail("contains tabs");
	}

	error_summary(infile);
}

PRIVATE void
print_version_number(VOID)
{
  if((full_output || !quiet) && !print_version)
    (void)fprintf(list_fd,"\n");
  (void)fprintf(list_fd,"%s",VERSION_NUMBER);
  if(help_screen || print_version)
    (void)fprintf(list_fd," %s",PATCHLEVEL);
  if(full_output || !quiet || print_version)
    (void)fprintf(list_fd,"\n");
}

PRIVATE void
#if HAVE_STDC
error_summary(char *fname)		/* Print out count of errors in file */
#else /* K&R style */
error_summary(fname)		/* Print out count of errors in file */
	char *fname;
#endif /* HAVE_STDC */
{
	FILE *fd = list_fd;

	if(full_output ||
	   (!quiet && error_count+warning_count != 0))
	  (void)fprintf(fd,"\n");

	if(full_output || !quiet || error_count != 0)
	  (void)fprintf(fd,"\n %u syntax error%s detected in file %s",
			error_count, error_count==1? "":"s",
			fname);

	if(warning_count != 0)
		(void)fprintf(fd,"\n %u warning%s issued in file %s",
			warning_count, warning_count==1? "":"s",
			fname);

	if(full_output ||
	   (!quiet && error_count+warning_count != 0))
	  (void)fprintf(fd,"\n");

	error_count = 0;
	warning_count = 0;
}

void
#if HAVE_STDC
print_a_line(FILE *fd, const char *line, LINENO_t num)  /* Print source line with line number */
#else /* K&R style */
print_a_line(fd,line,num)  /* Print source line with line number */
	FILE *fd;
	char *line;
	LINENO_t num;
#endif /* HAVE_STDC */
{
	(void)fprintf(fd,"\n %6u ",num); /* Print line number */

				/* Tab-formatted source lines: tab in
				   col 1-6 moves to col 7. */
	if(source_dec_tab) {
	  int i,col;
	  for(i=0,col=1; col < 7 && line[i] != '\0'; i++) {
	    if(line[i] == '\t') {
	      do{
		(void)fprintf(fd," ");
	      } while(++col < 7);
	    }
	    else {
		(void)fprintf(fd,"%c",line[i]);
		++col;
	    }
	  }
	  (void)fprintf(fd,"%s",line+i);
	}
	else
	  (void)fprintf(fd,"%s",line);
}




PRIVATE void
#if HAVE_STDC
open_outfile(char *s)		/* open the output file for listing */
#else /* K&R style */
open_outfile(s)		/* open the output file for listing */
	char *s;
#endif /* HAVE_STDC */
{
	char *fullname;		/* given name plus extension */
	FILE *fd;

	must_open_outfile = FALSE;	/* Turn off the flag */

	if(s == (char *) NULL || *s == '\0') {
		return;		/* No filename: no action  */
	}

	fullname = add_ext(s,DEF_LIST_EXTENSION);
	(void)fflush(list_fd);
	if( (fd = fopen(fullname,"w")) == (FILE *)NULL) {
		(void)fprintf(stderr,"\nCannot open %s for output\n",fullname);
	}
	else {
		(void)fprintf(stderr,"\nOutput sent to file %s\n",fullname);
		list_fd = fd;
	}
}


PRIVATE void
wrapup(VOID)	/* look at cross references, etc. */
{

    doing_wrapup = TRUE; /* for correct behavior in oldstyle_error_message */

	if(debug_hashtab || debug_glob_symtab)
	  debug_symtabs();

				/* VCG output file uses stem of file
				   containing main prog or 1st file on
				   command line. If none, output is to stdout.
				 */
#ifdef VCG_SUPPORT
	if(print_vcg_list) {
	  vcg_fd = (input_fd == stdin || main_filename == (char *)NULL)?
	    stdout :
	    fopen(new_ext(main_filename,DEF_VCG_EXTENSION) ,"w");
	}
#endif

	visit_children();	/* Make call tree & check visited status */
	check_com_usage();	/* Look for unused common stuff */
	check_comlists();	/* Look for common block mismatches */
	check_arglists();	/* Look for subprog defn/call mismatches */

        if ( htmlcalltree_fd ) 
         { 
         (void)fprintf( htmlcalltree_fd, "</pre>\n</body>\n</html>\n" );
         fclose( htmlcalltree_fd ); 
         htmlcalltree_fd = NULL; 
         }
#ifdef DEBUG_GLOBAL_STRINGS
	if(debug_latest)
	  print_global_strings();
#endif
}


#define MODE_DEFAULT_EXT 1
#define MODE_REPLACE_EXT 2
PRIVATE char *
#if HAVE_STDC
append_extension(char *s, const char *ext, int mode)
#else /* K&R style */
append_extension(s,ext,mode)
     char *s,*ext;
     int mode;
#endif /* HAVE_STDC */
{
		/* MODE_DEFAULT_EXT: Adds extension to file name s if
		   none is present, and returns a pointer to the
		   new name.  If extension was added, space is allocated
		   for the new name.  If not, simply  returns pointer
		   to original name.  MODE_REPLACE_EXT: same, except given
		   extension replaces given one if any.
		*/
	int i,len;
	char *newname;
#ifdef OPTION_PREFIX_SLASH	/* set len=chars to NUL or start of /opt */
	for(len=0; s[len] != '\0' && s[len] != '/'; len++)
	  continue;
#else
	len=(unsigned)strlen(s);
#endif
		/* Search backwards till find the dot, but do not
		   search past directory delimiter
		*/
	for(i=len-1; i>0; i--) {
	    if(s[i] == '.'
#ifdef UNIX
	       || s[i] == '/'
#endif
#ifdef VMS
	       || s[i] == ']' || s[i] == ':'
#endif
#ifdef MSDOS
	       || s[i] == '\\' || s[i] == ':'
#endif
	       )
		break;
	}

	if(mode == MODE_REPLACE_EXT) {
	  if(s[i] == '.')	/* declare length = up to the dot */
	    len = i;
	  newname = (char *) malloc( (unsigned)(len+(unsigned)strlen(ext)+1) );
	  (void)strncpy(newname,s,len);
	  (void)strcpy(newname+len,ext);
	}
	else {			/* MODE_DEFAULT_EXT */
#ifdef OPTION_PREFIX_SLASH
		/* create new string if new ext or trailing /option */
	  if(s[i] != '.' || s[len] != '\0') {
	    if(s[i] != '.') {	/* no extension given */
	      newname = (char *) malloc( (unsigned)(len+
						    (unsigned)strlen(ext)+1) );
	      (void)strncpy(newname,s,len);
	      (void)strcpy(newname+len,ext);
	    }
	    else {		/* extension given but /option follows */
	      newname = (char *) malloc( (unsigned)(len+1) );
	      (void)strncpy(newname,s,len);
	    }
	  }
#else
	  if(s[i] != '.') {
	    newname = (char *) malloc( (unsigned)(len+
						  (unsigned)strlen(ext)+1) );
	    (void)strcpy(newname,s);
	    (void)strcat(newname,ext);
	  }
#endif
	  else {
	    newname = s;	/* use as is */
	  }
	}

	return newname;
}

		/* Adds default extension to source file name, replacing
		   any that is present, and returns a pointer to the
		   new name.  Space is allocated for the new name.
		*/
char *
#if HAVE_STDC
add_ext(char *s, const char *ext)			/* adds default filename extension to s */
#else /* K&R style */
add_ext(s,ext)			/* adds default filename extension to s */
	char *s,*ext;
#endif /* HAVE_STDC */
{
  return append_extension(s,ext,MODE_DEFAULT_EXT);
}

PRIVATE char *
#if HAVE_STDC
new_ext(char *s, const char *ext)
#else /* K&R style */
new_ext(s,ext)
	char *s,*ext;
#endif /* HAVE_STDC */
{
  return append_extension(s,ext,MODE_REPLACE_EXT);
}


int
#if HAVE_STDC
has_extension(const char *name, const char *ext)		/* true if name ends in ext */
#else /* K&R style */
has_extension(name,ext)		/* true if name ends in ext */
  char *name,*ext;
#endif /* HAVE_STDC */
{
  unsigned name_len, ext_len;
  int stem_len;
  ext_len = strlen(ext);

#ifdef VMS	/* shell_glob adds version number: filename.ext;1 */
  if(strrchr(name,';') != (char *)NULL) {
    name_len = strrchr(name,';') - name; /* distance to the semicolon */
  }
  else
#endif
    name_len=strlen(name);	/* distance to the null */

  stem_len = (unsigned)(name_len - ext_len); /* distance to the dot */

  if( stem_len >= 0 &&
     (name_len-stem_len) == ext_len &&
     strncasecmp(name+stem_len,ext,ext_len) == 0 )
    return TRUE;
  else
    return FALSE;
}


PRIVATE void
resource_summary(VOID)
{
#ifdef DEBUG_SIZES
  if(debug_latest)
    print_sizeofs();	/* give sizeof various things */
#endif

  (void)fprintf(list_fd,
   "\n     Here are the amounts of ftnchek's resources that were used:\n");

  (void)fprintf(list_fd,
   "\nSource lines processed = %lu statement + %lu comment = %lu total",
		tot_stmt_line_count,
		tot_line_count-tot_stmt_line_count, /*tot_comment_line_count*/
		tot_line_count);

  (void)fprintf(list_fd,
   "\nTotal executable statements = %lu, max in any module = %lu",
		tot_exec_stmt_count,
		max_exec_stmt_count);

  (void)fprintf(list_fd,
   "\nTotal number of modules in program = %lu",
		tot_module_count);

  (void)fprintf(list_fd,
   "\nTotal statement labels defined = %lu, max in any module = %lu",
		tot_label_count, max_labels);

  (void)fprintf(list_fd,
   "\nMax identifier name chars used = %lu local, %lu global, chunk size %lu",
			max_loc_strings,
			glob_strings_used,
			(unsigned long)STRSPACESZ);
  (void)fprintf(list_fd,
    "\nMax token text chars used = %lu, chunk size %lu ",
			max_srctextspace,
			(unsigned long)STRSPACESZ);
  (void)fprintf(list_fd,
    "\nMax local symbols used =  %lu out of %lu available",
			max_loc_symtab,
			(unsigned long)LOCSYMTABSZ);
  (void)fprintf(list_fd,
    "\nMax global symbols used = %lu out of %lu available",
			max_glob_symtab,
			(unsigned long)GLOBSYMTABSZ);
  (void)fprintf(list_fd,
    "\nMax number of parameter info fields used = %lu, chunk size = %lu",
			max_paraminfo,
			(unsigned long)PARAMINFOSPACESZ);
  (void)fprintf(list_fd,
    "\nMax number of tokenlists used = %lu, chunk size = %lu",
			max_tokenlists,
			(unsigned long)TOKHEADSPACESZ);
  (void)fprintf(list_fd,
    "\nMax token list/tree space used = %lu, chunk size = %lu",
			max_token_space,
			(unsigned long)TOKENSPACESZ);
  (void)fprintf(list_fd,
    "\nNumber of subprogram invocations = %lu totaling %lu args",
			arglist_head_used,
			arglist_element_used);
  (void)fprintf(list_fd,
    "\nArgument list header and element chunk sizes = %lu and %lu",
			(unsigned long)ARGLISTHEADSZ,
			(unsigned long)ARGLISTELTSZ);
  (void)fprintf(list_fd,
    "\nNumber of common block decls = %lu totaling %lu variables",
			comlist_head_used,
			comlist_element_used);
  (void)fprintf(list_fd,
    "\nCommon list header and element chunk sizes = %lu and %lu",
			(unsigned long)COMLISTHEADSZ,
			(unsigned long)COMLISTELTSZ);
  (void)fprintf(list_fd,
    "\nNumber of array dim ptrs used = %lu, chunk size = %lu",
			max_ptrspace,
			(unsigned long)PTRSPACESZ);

#ifdef DEBUG_SIZES
  (void)fprintf(list_fd,
    "\nIdentifier hashtable size = %6lu",
			(unsigned long)HASHSZ);
#ifdef KEY_HASH/* not used any more*/
  (void)fprintf(list_fd,
    "\nKeyword hashtable size = %6lu",
			(unsigned long)KEYHASHSZ);
#endif
#ifdef COUNT_REHASHES
  (void)fprintf(list_fd,
    "\nIdentifier rehash count = %6lu",
			rehash_count);
#endif
  (void)fprintf(list_fd,
    "\nIntrinsic function hashtable size=%6lu, clash count=%lu",
			(unsigned long)INTRINS_HASHSZ,
			intrins_clashes);
#endif /*DEBUG_SIZES*/

  (void)fprintf(list_fd,"\n\n");
}
