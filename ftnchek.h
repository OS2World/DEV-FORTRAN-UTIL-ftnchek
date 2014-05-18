/* $Id: ftnchek.h,v 1.86 2003/03/20 22:13:17 moniot Exp $

	Common definitions for Fortran Program Checker
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
Acknowledgement: the above permission notice was copied, with minor
changes, from the XFree86 Project.

*/

#define COPYRIGHT_DATE \
"Copyright (C) 1994, 1997, 2000 by Robert K. Moniot"
#define COPYRIGHT_NOTICE \
"This program is free software.  Permission is granted to\n\
modify it and/or redistribute it, retaining this notice.\n\
No guarantees accompany this software."
#define VERSION_NUMBER		"FTNCHEK Version 3.2 November 2002"
#define PATCHLEVEL		"Patch Level 2"
#define PROJECT_VERSION		"P3" /* Project file format version number */

#include "config.h"		/* Get system-specific information */

		/* Define macro PROTO for declaring function prototypes.  If
		   compiler doesn't accept prototypes, use -DNO_PROTOTYPES.
		   The args in macro must be enclosed in parentheses.
		   Define macro VOID for declaring functions without
		   args.  Also define HAVE_STDC for declaring functions
		   in ANSI form or Kernighan-Ritchie form.  Each function
		   defn is in both forms, with #ifdef HAVE_STDC to select
		   between them.
		 */

#ifdef NO_PROTOTYPES
#define PROTO(TYPE_AND_NAME,ARGS) TYPE_AND_NAME()
#define VOID
#define HAVE_STDC	0
#else
#define PROTO(TYPE_AND_NAME,ARGS) TYPE_AND_NAME ARGS
#define VOID		void
#define HAVE_STDC	1
#endif

	/* The following system defines should be defined with the -D
	   (for UNIX) or /DEFINE (for VMS) compiler options in the makefile,
	   not here.  They are shown here so you know what to define.
	   MSDOS should be defined for IBM PC version.  We set it here
	   automatically if a DOS/Win compiler is detected, otherwise when
	   compiling you should define it on compiler command line.
	   Only one of MSDOS, VMS, or UNIX should be defined.  They control
	   details of behavior such as filename extensions and use of slash
	   as an option prefix.
	*/

/*#define VMS*/		/* Set flag for VMS system-dependent defns. */
/*#define UNIX*/	/* Set flag for UNIX (any flavor) defns. */
	/* Many (all?) Windows compilers autodefine _WIN32. I am guessing
	   about _MSDOS.  TurboC (DOS and Win versions) sets __TURBOC__ */
#if defined(_WIN32) || defined(_MSDOS) || defined(__TURBOC__)
#define MSDOS		/* Set flag for MSDOS (including Windows) */
#endif

			/* For portability, declare standard routines here. */
#if HAVE_STDLIB_H
#include <stdlib.h>
#else
PROTO(char *getenv,(char *));
PROTO(char *malloc,(unsigned));
PROTO(char *calloc,(unsigned,unsigned));
PROTO(VOID free,(void *));
PROTO(VOID exit,(int));
#endif


/*************************************************************************
     Begin section of defines for system-dependent configuration.
**************************************************************************/


	/* set flag to allow options to start with '/' */
#ifndef OPTION_PREFIX_SLASH
#ifndef NO_OPTION_PREFIX_SLASH
#ifdef VMS
#define OPTION_PREFIX_SLASH
#endif
#ifdef MSDOS
#define OPTION_PREFIX_SLASH
#endif
#endif
#endif

			/* Define standard home directory for VMS and MSDOS.
			   This directory is one place ftnchek looks for the
			   rc or ini file. */
#ifdef VMS
#ifndef SPECIAL_HOMEDIR
#define SPECIAL_HOMEDIR "SYS$DISK:[]"
#endif
#endif

#ifdef MSDOS
#ifndef SPECIAL_HOMEDIR
#define SPECIAL_HOMEDIR "c:\\lib"
#endif
#endif

	/* Define the Unix-style startup file name and a non-Unix-style
	   alternate.  Both are looked for on all systems. */

#ifndef UNIX_RC_FILE
#define UNIX_RC_FILE	".ftnchekrc"
#endif
#ifndef NONUNIX_RC_FILE
#define NONUNIX_RC_FILE	"ftnchek.ini"
#endif

		/* Define default source and output file extensions.  These
		 * can be overridden by defines on compiler commandline.
		 */
#ifndef DEF_SRC_EXTENSION
#ifdef VMS
#define DEF_SRC_EXTENSION ".for"		/* VMS default extension */
#endif
#ifdef MSDOS
#define DEF_SRC_EXTENSION ".for"		/* IBM PC default extension */
#endif
#endif /* DEF_SRC_EXTENSION */

#ifndef DEF_SRC_EXTENSION
#define DEF_SRC_EXTENSION ".f"		/* Unix and all others */
#endif
		/* define default list-file extension */
#ifndef DEF_LIST_EXTENSION
#define DEF_LIST_EXTENSION ".lis"
#endif
		/* define default project-file extension */
#ifndef DEF_PROJ_EXTENSION
#define DEF_PROJ_EXTENSION ".prj"
#endif
		/* define default declaration-file extension */
#ifndef DEF_DCL_EXTENSION
#define DEF_DCL_EXTENSION ".dcl"
#endif
		/* define default html documentation-file extension */
#ifndef DEF_HTML_EXTENSION
#define DEF_HTML_EXTENSION ".html"
#endif
		/* define default include-file extension */
#ifndef DEF_INC_EXTENSION
#define DEF_INC_EXTENSION DEF_SRC_EXTENSION
#endif
		/* define default declaration-file extension */
#ifndef DEF_VCG_EXTENSION
#define DEF_VCG_EXTENSION ".vcg"
#endif
		/* define project-file name for case of input from stdin */
#ifndef STDIN_PROJ_FILENAME
#define STDIN_PROJ_FILENAME "ftnchek.prj"
#endif

#ifndef ENV_PREFIX		/* prefix for option environment variables */
#define ENV_PREFIX "FTNCHEK_"
#endif

#ifndef ENV_INCLUDE_VAR
#define ENV_INCLUDE_VAR "INCLUDE" /* name of env variable for include dir */
#endif

#ifndef DEFAULT_INCLUDE_DIR
#ifdef UNIX
#define DEFAULT_INCLUDE_DIR "/usr/include"
#endif
#ifdef VMS
#define DEFAULT_INCLUDE_DIR "SYS$LIBRARY:"
#endif
#ifdef MSDOS
#define DEFAULT_INCLUDE_DIR "\\include"
#endif
#endif

/*************************************************************************
     End of section of defines for system-dependent configuration.

	Begin section of defines to control ftnchek's behavior
	      (syntax accepted, options supported, etc.)
**************************************************************************/

	/* The following macro turns on useful extra information saying
	   "expecting whatever" in "parse error" messages from GNU
	   bison-generated parser.  This macro may not do anything if
	   parser is generated by another version of bison or yacc.
	*/
#ifndef NO_YYERROR_VERBOSE
#define YYERROR_VERBOSE
#endif


			/* This is the default limit on "cascades" of error
			   messages where the trailer "etc..." is given
			   instead of printing them all.  The limit can be
			   changed at run time by -error=num.
			 */
#ifndef DEF_ERROR_CASCADE_LIMIT
#define DEF_ERROR_CASCADE_LIMIT 3
#endif

	/* VCG support is now standard. If you don't want it, you can
	   eliminate it by defining NO_VCG_SUPPORT.  You can customize
	   the graph options by changing the macro VCG_GRAPH_OPTIONS.
	   Include \n between options and after the last option.
	   (These options affect the graph as a whole.)  */

#ifndef NO_VCG_SUPPORT
#define VCG_SUPPORT
#ifndef VCG_GRAPH_OPTIONS
#define VCG_GRAPH_OPTIONS "color: lightgray\n"
#endif
#endif

	/* Define the amount of indentation of declarations produced
	   under -makedcls in free-form mode.  Must be >= 0.
	 */
#ifndef DCL_FREEFORM_INDENT
#define DCL_FREEFORM_INDENT 2
#endif

	/* The following defines control the default status of
	   warnings controlled by -arg, -f77/90/95, -port, -pretty, and -trunc.
	 */


			/* -arguments options default to ARGCHECK_ALL.  Define
			   LAX_ARGCHECK to make -arguments=none the default.
			   Otherwise -arguments=all is default.
			*/
#ifdef LAX_ARGCHECK
#define ARGCHECK_ALL FALSE
#else
#define ARGCHECK_ALL TRUE
#endif


			/* -array options default to ARRAYCHECK_ALL.  Define
			   LAX_ARRAYCHECK to make -array=none the default.
			   Otherwise -array=all is default.
			*/
#ifdef LAX_ARRAYCHECK
#define ARRAYCHECK_ALL FALSE
#else
#define ARRAYCHECK_ALL TRUE
#endif

			/* -common options default to COMCHECK_ALL, except for
			   volatile, which defaults to FALSE.  Define
			   LAX_COMCHECK to make -common=none the default.
			   Otherwise -common=type,length,exact is default.
			*/
#ifdef LAX_COMCHECK
#define COMCHECK_ALL FALSE
#else
#define COMCHECK_ALL TRUE
#endif


			/* -f77 options default to F77_ALL.  Define
			   STRICT_SYNTAX to make -f77=all the default.
			   Otherwise -f77=none is default.
			*/
#ifdef STRICT_SYNTAX
#define F77_ALL 1
#else
#define F77_ALL 0
#endif

			/* -f90 options default to none unless
			   F90_SYNTAX is defined.
			*/
#ifdef F90_SYNTAX
#define F90_ALL 1
#else
#define F90_ALL 0
#endif

			/* -f95 options default to none unless
			   F95_SYNTAX is defined.
			*/
#ifdef F95_SYNTAX
#define F95_ALL 1
#else
#define F95_ALL 0
#endif

			/* -mkhtml requires regex support.  This macro
			   controls only whether the -mkhtml command-line
			   option is made available.  The relevant variables
			   and make_html function are still defined, but the
			   latter (if somehow invoked) just apologizes.
			 */
#if HAVE_REGEX_H
#define HTML_OPTION
#endif

			/* -portability options default to PORT_ALL.  Define
			   STRICT_PORTABILITY to make -port=all the default.
			   Otherwise -port=none is default.
			*/
#ifdef STRICT_PORTABILITY
#define PORT_ALL TRUE
#else
#define PORT_ALL FALSE
#endif

			/* -pretty options default to PRETTY_ALL.  Define
			   UGLY_IS_OK to make -pretty=none the default.
			   Otherwise -pretty=all is default.
			*/
#ifdef UGLY_IS_OK
#define PRETTY_ALL FALSE
#else
#define PRETTY_ALL TRUE
#endif

			/* -source options are defined here.  Since
			   -source=all is unlikely to be desired, each
			   option has its own macro for default value */

#ifndef DEC_TABS	/* DEC-style ugly tabbed source is off by default */
#define DEC_TABS FALSE
#endif
			/* VMS-style INCLUDE supports defaulting
			   extension, /NOLIST feature.  It is default
			   for VMS, not for other systems. */
#ifndef VMS_INCLUDE
#ifdef VMS
#define VMS_INCLUDE TRUE
#else
#define VMS_INCLUDE FALSE
#endif
#endif

#ifndef UNIX_BACKSLASH	/* UNIX backslash escape sequences in strings */
#define UNIX_BACKSLASH FALSE
#endif

		/* standard parameters take data type from value */
#ifndef PARAMETER_IMPLICIT_TYPE
#define PARAMETER_IMPLICIT_TYPE FALSE
#endif

		/* DEC-style parameters get data type in standard way */
#ifndef DEC_PARAMETER_STANDARD_TYPE
#define DEC_PARAMETER_STANDARD_TYPE FALSE
#endif


			/* -style options default to STYLECHECK_ALL, which
			   should be FALSE to prevent ftnchek from spewing
			   warnings about perfectly normal code.
			 */
#ifndef STYLECHECK_ALL
#define STYLECHECK_ALL FALSE
#endif


			/* -truncation options default to TRUNC_ALL.  Define
			   LAX_TRUNCATION to make -trunc=none the default.
			   Otherwise -trunc=all is default.
			*/
#ifdef LAX_TRUNCATION
#define TRUNC_ALL FALSE
#else
#define TRUNC_ALL TRUE
#endif

			/* -usage options default to USAGE_ALL.  Define
			   LAX_USAGE to make -usage=none the default.
			   Otherwise -usage=all is default.
			*/
#ifdef LAX_USAGE
#define USAGE_ALL FALSE
#else
#define USAGE_ALL TRUE
#endif

		/* The macros in the following section control whether
		   certain groups of nonstandard intrinsics are compiled
		   into ftnchek.  By turning off all these macros using the
		   STANDARD_INTRINSICS define or selectively by using the
		   NO_ prefix, some efficiency is gained but these intrinsics
		   will then never be recognized by ftnchek, regardless of
		   the -intrinsic setting.  Turn these off only if you are
		   a purist who will never use non-F77 intrinsics.
		 */

#ifndef STANDARD_INTRINSICS

#ifndef NO_EXTRA_INTRINSICS
#define EXTRA_INTRINSICS	/* Common nonstandard intrinsic functions */
#endif

#ifndef NO_UNIX_INTRINSICS
#define UNIX_INTRINSICS		/* UNIX intrinsic functions */
#endif

#ifndef NO_VMS_INTRINSICS
#define VMS_INTRINSICS		/* VMS intrinsic functions */
#endif




		/* The macros in the next section define the default
		   value of the -intrinsic settings governing which
		   intrinsic groups to recognize and how to treat RAND
		   and IARGC, which may take no argument or one dummy
		   argument on different platforms.
		 */

			/* Define which sets to recognize by default:
			     bit 0 = commonly available nonstandard intrinsics
			     bit 1 = some Unix intrinsics
			     bit 2 = some VMS intrinsics
			 */

#ifdef UNIX	/* Unix default: accept Common & Unix intrinsics */
#define DEF_INTRINSIC_SET 3
#endif

#ifdef VMS	/* VMS default: accept Common & VMS intrinsics */
#define DEF_INTRINSIC_SET 5
#endif

#ifndef DEF_INTRINSIC_SET	/* Otherwise default = only common set */
#define DEF_INTRINSIC_SET 1
#endif

				/* Define which RAND form to use.
				     bit 0 = allow no-arg form
				     bit 1 = allow one-arg form
				 */

#ifdef RAND_NO_ARG	/*RAND() form*/
#define DEF_INTRINSIC_RAND 1
#else
#ifdef RAND_ONE_ARG	/*RAND(ISEED) form*/
#define DEF_INTRINSIC_RAND 2
#else				/* Default: allow either form */
#define DEF_INTRINSIC_RAND 3
#endif
#endif

				/* Define which IARGC form to use */
#ifdef IARGC_NO_ARG
#define DEF_INTRINSIC_IARGC 1
#else
#ifdef IARGC_ONE_ARG
#define DEF_INTRINSIC_IARGC 2
#else				/* Default: allow either form */
#define DEF_INTRINSIC_IARGC 3
#endif
#endif

#endif /* not STANDARD_INTRINSICS */

/*  Default BpW = 4 bytes per word, which matches many machines.
    This macro serves as the default value for -wordsize.
    If the Fortran code does not declare explicit sizes of
    numeric variables (e.g. REAL*8), then the value of -wordsize will
    not matter, since the table conforms to the standard in that
    sizeof(INTEGER)=sizeof(REAL), and sizeof(DOUBLE)=sizeof(COMPLEX)
    =2*sizeof(REAL).  If the code does declare explicit sizes of
    numeric types, then the value of -wordsize will matter if explicit
    and default sizes are expected to match.  If you want to
    suppress warnings of this kind, you may use the -wordsize setting
    or, to make it the default, change BpW to match
    your hardware.  Under the -portability option, explicit and
    default sizes never match: e.g. passing REAL*8 where DOUBLE
    PRECISION expected.  None of this applies to CHARACTER data:
    the default size (1) is well-defined, and the standard does
    not specify the ratio of sizeof(CHARACTER) to sizeof(REAL).

    For the Cray pointer extension, we also define PTRSIZE as the size of
    a pointer.  The default is the same as the wordsize.  This can be
    changed at run time by -pointersize=nnn.
*/

#ifndef BpW
#define BpW 4	/* Bytes per Word: might want to use sizeof(float) instead */
#endif
#ifndef PTRSIZE
#define PTRSIZE BpW
#endif

		/* Define to tolerate embedded blanks in numeric consts unless
		   feature turned off by defining NO_BLANKS_IN_NUMBERS. */
#ifndef NO_BLANKS_IN_NUMBERS
#define BLANKS_IN_NUMBERS
#endif

				/* Default value of -wrap option */
#ifndef WRAP_COLUMN
#define WRAP_COLUMN 79		/* When to wrap error messages to next line */
#endif

#ifndef DEF_IDLETTER_LIST	/* default nonalnums allowed in identifiers */
#define DEF_IDLETTER_LIST "$_"
#endif

		/* The following defines used to be optional, now they
		   are standard. If these defines are removed or
		   undef'd, the corresponding syntax will not be
		   recognized (resulting in parse errors).  I don't
		   recommend undefining them.  They are all warned about
		   by -f77 */

#define ALLOW_CRAY_POINTERS 1
#define ALLOW_DO_ENDDO 1
#define ALLOW_INCLUDE 1
#define ALLOW_QUOTEMARKS 1
#define ALLOW_TYPELESS_CONSTANTS 1
#define ALLOW_UNIX_BACKSLASH 1
#define ALLOW_UNIX_CPP 1
#define ALLOW_VMS_IO 1
#define ALLOW_IBM_IO 1

#define INLINE_COMMENT_CHAR '!' /* Inline comments start with '!' */


#define KEEP_ARG_NAMES 1	/* option was formerly experimental */


		/* SMALL_INT_VALUE is an integer that is small enough that
		   we don't need to worry if it is assigned to a small-size
		   int, for purposes of catching truncation errors.  We
		   conservatively use maximum signed 8-bit byte value.
		 */
#ifndef SMALL_INT_VALUE
#define SMALL_INT_VALUE 127
#endif

#ifdef DEVELOPMENT		/* For maintaining the program */
#define DEBUG_FORLEX
#endif

/*************************************************************************
	 End section of defines to control ftnchek's behavior

	  Begin section to define limits, table sizes, etc.
**************************************************************************/




#ifndef MAXLINE
#define MAXLINE 132	/* Maximum input line length.  Ignores past this. */
#endif
#ifndef MAXIDSIZE
#define MAXIDSIZE 31	/* Longest identifier allowed */
#endif
#ifndef MAX_SRC_TEXT
#define MAX_SRC_TEXT (20*66) /* Longest text string of a token */
#endif
#ifndef MAX_CHAR_CODE
#define MAX_CHAR_CODE 255 /* Largest possible value of a char (8 bits here) */
#endif
#ifndef MAX_INCLUDE_DEPTH
#define MAX_INCLUDE_DEPTH 16	/* Max nesting depth of include files */
#endif
#ifndef MAXEXPRTEXT
#define MAXEXPRTEXT 15		/* length of expr text saved in arg lists */
#endif
#ifndef MAX_RC_LINE
#define MAX_RC_LINE 500		/* Max length of line in rc file */
#endif
#ifndef RC_COMMENT_CHAR
#define RC_COMMENT_CHAR '#'	/* Comments start with this or space  */
#endif
		/* Definitions of table sizes
		     Constraints:
			STRSPACESZ > 20*72 so max f77 statement will fit
			LOCSYMTABSZ+GLOBSYMTABSZ < HASHSZ so hashtab never full
		 */

#define KEYHASHSZ 195	/* Size of keyword hashtable -- do not change */
#define INTRINS_HASHSZ 433 /* Chosen to give few clashes -- change with care */

#ifdef SMALL_MACHINE		/* Use these for e.g. IBM PC */
#ifndef HASHSZ			/* Hint: pick one with no square factors */
#define HASHSZ 798     /* SMALL_MACHINE Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 4000 /* SMALL_MACHINE chunk size of string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 300 /* SMALL_MACHINE Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 400 /* SMALL_MACHINE Size of global symbol table */
#endif
#ifndef PARAMINFOSPACESZ
#define PARAMINFOSPACESZ 20 /* SMALL_MACHINE ParamInfo structs per chunk */
#endif
#ifndef TOKHEADSPACESZ
#define TOKHEADSPACESZ 50 /* SMALL_MACHINE TokenListHeaders per chunk */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 200 /* SMALL_MACHINE tokens per token space chunk */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ	300 /* SMALL_MACHINE argument headers per chunk */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ	1000 /* SMALL_MACHINE argument list elts per chunk */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ	200 /* SMALL_MACHINE common list headers per chunk */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ	1000 /* SMALL_MACHINE common elts per chunk */
#endif
#ifndef PTRSPACESZ
#define PTRSPACESZ	200  /* SMALL_MACHINE ptrs-to-arraydim per chunk */
#endif
#ifndef LABTABLE_SIZE
#define LABTABLE_SIZE 109      /* SMALL_MACHINE initial size of label table */
#endif
#ifndef REF_LISTS_SIZE 
#define REF_LISTS_SIZE 200   /* SMALL_MACHINE Lab_ref_lists per chunk*/
#endif
#ifndef IO_UNIT_INFO_SIZE
#define IO_UNIT_INFO_SIZE 50	/* SMALL_MACHINE initial size of I/O unit info table */
#endif
 
#else  /* end if SMALL_MACHINE */

#ifdef MEDIUM_MACHINE		/* use these if default is too big */

#ifndef HASHSZ
#define HASHSZ 2002	/* MEDIUM_MACHINE Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 10000 /* MEDIUM_MACHINE chunk size of string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 800 /* MEDIUM_MACHINE Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 1200 /* MEDIUM_MACHINE Size of global symbol table */
#endif
#ifndef PARAMINFOSPACESZ
#define PARAMINFOSPACESZ 50 /* MEDIUM_MACHINE ParamInfo structs per chunk */
#endif
#ifndef TOKHEADSPACESZ
#define TOKHEADSPACESZ 200 /* MEDIUM_MACHINE TokenListHeaders per chunk */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 1000 /* MEDIUM_MACHINE tokens per token space chunk */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ	1500 /* MEDIUM_MACHINE argument headers per chunk */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ	5000	/* MEDIUM_MACHINE argument list elts per chunk */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ	1000 /* MEDIUM_MACHINE common list headers per chunk */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ	4000 /* MEDIUM_MACHINE common elts per chunk */
#endif
#ifndef PTRSPACESZ
#define PTRSPACESZ	400 /* MEDIUM_MACHINE Max number of ptrs to arraydim */
#endif
#ifndef LABTABLE_SIZE
#define LABTABLE_SIZE 209      /* MEDIUM_MACHINE initial size of labtable */
#endif
#ifndef REF_LISTS_SIZE 
#define REF_LISTS_SIZE 400  /* MEDIUM_MACHINE Lab_ref_lists per chunk*/
#endif
#ifndef IO_UNIT_INFO_SIZE
#define IO_UNIT_INFO_SIZE 200	/* MEDIUM_MACHINE initial size of I/O unit info table */
#endif
#else		/* Default is now LARGE_MACHINE, OK for most modern machines */

#ifndef HASHSZ		/* must be <= max int  */
#define HASHSZ 20930	 /* LARGE_MACHINE Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 10000 /* LARGE_MACHINE chunk size of string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 8000 /* LARGE_MACHINE Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 12000 /* LARGE_MACHINE Size of global symbol table */
#endif
#ifndef PARAMINFOSPACESZ
#define PARAMINFOSPACESZ 200 /* LARGE_MACHINE ParamInfo structs per chunk */
#endif
#ifndef TOKHEADSPACESZ
#define TOKHEADSPACESZ 500 /* LARGE_MACHINE TokenListHeaders per chunk */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 10000 /* LARGE_MACHINE tokens per token space chunk */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ	15000 /* LARGE_MACHINE argument headers per chunk */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ	50000	/* LARGE_MACHINE argument list elts per chunk */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ	10000 /* LARGE_MACHINE common list headers per chunk */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ	50000 /* LARGE_MACHINE common elts per chunk */
#endif
#ifndef PTRSPACESZ
#define PTRSPACESZ	2000 /* LARGE_MACHINE Max number of ptrs to arraydim */
#endif
#ifndef LABTABLE_SIZE
#define LABTABLE_SIZE 299      /* LARGE_MACHINE initial size of labtable */
#endif
#ifndef REF_LISTS_SIZE 
#define REF_LISTS_SIZE 2000  /* LARGE_MACHINE Lab_ref_lists per chunk*/
#endif
#ifndef IO_UNIT_INFO_SIZE
#define IO_UNIT_INFO_SIZE 500	/* LARGE_MACHINE initial size of I/O unit info table */
#endif
#endif /* end if LARGE_MACHINE else */

#endif/*end if SMALL_MACHINE else*/

/*************************************************************************
	  End of section to define limits, table sizes, etc.

		From here down should not be altered.
**************************************************************************/



#define FALSE 0
#define TRUE 1

				/* Make sure line numbers use 32 bits */
#if SIZEOF_INT >= 4
typedef unsigned LINENO_t;
#else
typedef unsigned long LINENO_t;
#endif
typedef unsigned COLNO_t;


#define NO_COL_NUM ((COLNO_t)999)/* Impossible column number to suppress
				 * printing in error messages
				 */
#define GLOBAL_NO_COL_NUM ((LINENO_t)998)/* Ditto for global-level warnings */

#define NO_LINE_NUM ((LINENO_t)0)/* Ditto for line number to suppress flushing
				 * of line if error not in local context
				 */

#define OOPS_NONFATAL 0		/* Severity of "oops" messages */
#define OOPS_FATAL 1

/*************************************************************************
	    Shared variable and function defns start here
**************************************************************************/

#ifdef MAIN
#define SHARED		/* (nothing) */
#else
#define SHARED extern	/* Non-main routines declare shared vars extern */
#endif

#define PRIVATE static	/* For non-shared functions */

	/* Except when DEBUG_FORLEX is turned on, ftnchek has no need
	   of floating-point calculations, so to allow it to run on
	   machines without coprocessor, we neutralize the few
	   floating-point operations that there are.  */

#ifndef DEBUG_FORLEX
#ifndef NO_FLOATING_POINT
#define NO_FLOATING_POINT
#endif
#endif

#ifdef NO_FLOATING_POINT
typedef long DBLVAL;
#else
typedef double DBLVAL;
#endif

SHARED FILE
            *input_fd,  /* Input file */
            *list_fd,	/* Output file for listing */
	    *dcl_fd,	/* Output type declaration file */
            *html_fd,   /* Output file for HTML documentation */
            *htmlcalltree_fd, /* Output file for html call tree documentation */
#ifdef VCG_SUPPORT
	    *vcg_fd,	/* Output VCG graph description file */
#endif
	    *project_fd;/* Project file for symtab info summary */

SHARED char *html_filename;     /* name of the html documentation file */
SHARED char *htmlcalltree_filename;  /* name of the html call tree doc file */
SHARED char *current_filename,	/* name of current input file */
	    *top_filename;	/* name of toplevel parent input file */
SHARED int incdepth;

#ifdef VCG_SUPPORT
SHARED char *main_filename;	/* name of file containing main program */
#endif



	/*****************************************************************/
	/*         Declare variables for command line options            */
	/*****************************************************************/



#ifdef MAIN
#define OPT(Type,Name,Value) Type Name=Value
#else
#define OPT(Type,Name,Value) extern Type Name
#endif

		/* These options are controlled by -arguments */
OPT(int,argcheck_argnumber,ARGCHECK_ALL);/* Check for wrong no. of args */
OPT(int,argcheck_arrayness,ARGCHECK_ALL);/* Check arg arrayness mismatches */
OPT(int,argcheck_argtype,ARGCHECK_ALL);	 /* Check for arg type mismatches */
OPT(int,argcheck_functype,ARGCHECK_ALL); /* Check for func type mismatches */
		/* End of -arguments options */

		/* These options are controlled by -array */
OPT(int,arraycheck_dims,ARRAYCHECK_ALL);/* Check for mismatch no.of dims */
OPT(int,arraycheck_size,ARRAYCHECK_ALL);/* Check for mismatch no.of elements */
		/* End of -array options */

		/* These options are controlled by -calltree */
OPT(int,print_call_tree,FALSE);	/* Print the call tree */
OPT(int,print_ref_list,FALSE);	/* Print reference (who-calls-who) list */
#ifdef VCG_SUPPORT
OPT(int,print_vcg_list,FALSE);	/* Print call graph in vcg format */
#endif
OPT(int,call_tree_prune,TRUE); /* do not repeat duplicates, use "see above"  */
OPT(int,call_tree_sort,TRUE);  /* alphabetized, not program order */
		/* End of -calltree options */

                /* These options are controlled by -crossref */
OPT(int,print_xref_list,FALSE);  /* print subprogram cross-references */
OPT(int,print_com_xrefs,FALSE);  /* print common block cross-refs */
OPT(int,print_lab_refs,FALSE);   /* print label cross-references */
                /* End of -crossref options */

		/* These options are controlled by -common */
OPT(int,comcheck_type,COMCHECK_ALL);/* Check for type mismatch */
OPT(int,comcheck_length,COMCHECK_ALL);/* Check for length mismatch  */
OPT(int,comcheck_dims,COMCHECK_ALL);/* Check for array dim mismatch  */
OPT(int,comcheck_by_name,COMCHECK_ALL);/* Check name-by-name */
OPT(int,comcheck_volatile,FALSE); /* Assume blocks are "volatile" */
		/* End of -common options */

		/* These options are controlled by -f77 */
OPT(int,f77_20_continue,F77_ALL);
OPT(int,f77_accept_type,F77_ALL);
OPT(int,f77_assignment,F77_ALL);
OPT(int,f77_array_bounds,F77_ALL);
OPT(int,f77_attrbased_typedecl,F77_ALL);
OPT(int,f77_automatic_array,F77_ALL);
OPT(int,f77_byte,F77_ALL);
OPT(int,f77_case_construct,F77_ALL);
OPT(int,f77_cray_pointers,F77_ALL);
OPT(int,f77_common_subprog_name,F77_ALL);
OPT(int,f77_construct_name,F77_ALL);
OPT(int,f77_cycle_exit,F77_ALL);
OPT(int,f77_d_comment,F77_ALL);
OPT(int,f77_dec_tabs,F77_ALL);
OPT(int,f77_do_enddo,F77_ALL);
OPT(int,f77_dollarsigns,F77_ALL);
OPT(int,f77_double_complex,F77_ALL);
OPT(int,f77_format_dollarsigns,F77_ALL);
OPT(int,f77_format_extensions,F77_ALL);
OPT(int,f77_function_noparen,F77_ALL);
OPT(int,f77_implicit_none,F77_ALL);
OPT(int,f77_include,F77_ALL);
OPT(int,f77_initializers,F77_ALL);
OPT(int,f77_inline_comment,F77_ALL);
OPT(int,f77_internal_list_io,F77_ALL);
OPT(int,f77_intrinsics,F77_ALL);
OPT(int,f77_io_keywords,F77_ALL);
OPT(int,f77_long_names,F77_ALL);
OPT(int,f77_mixed_common,F77_ALL);
OPT(int,f77_mixed_expr,F77_ALL);
OPT(int,f77_namelist,F77_ALL);
OPT(int,f77_overlength,F77_ALL);
OPT(int,f77_param_implicit_type,F77_ALL);
OPT(int,f77_param_intrinsic,F77_ALL);
OPT(int,f77_param_noparen,F77_ALL);
OPT(int,f77_quad_constants,F77_ALL);
OPT(int,f77_quotemarks,F77_ALL);
OPT(int,f77_relops,F77_ALL);
OPT(int,f77_semicolon,F77_ALL);
OPT(int,f77_stmt_order,F77_ALL);
OPT(int,f77_typeless_constants,F77_ALL);
OPT(int,f77_typesize,F77_ALL);
OPT(int,f77_underscores,F77_ALL);
OPT(int,f77_unix_backslash,F77_ALL);
OPT(int,f77_unix_cpp,F77_ALL);
OPT(int,f77_variable_format,F77_ALL);
		/* End of -f77 options */

		/* These options are controlled by -f90 */
OPT(int,f90_accept_type,F90_ALL);
OPT(int,f90_byte,F90_ALL);
OPT(int,f90_continue,F90_ALL);
OPT(int,f90_cray_pointers,F90_ALL);
OPT(int,f90_d_comment,F90_ALL);
OPT(int,f90_dec_tabs,F90_ALL);
OPT(int,f90_dollarsigns,F90_ALL);
OPT(int,f90_double_complex,F90_ALL);
OPT(int,f90_format_dollarsigns,F90_ALL);
OPT(int,f90_format_extensions,F90_ALL);
OPT(int,f90_function_noparen,F90_ALL);
OPT(int,f90_initializers,F90_ALL);
OPT(int,f90_intrinsics,F90_ALL);
OPT(int,f90_io_keywords,F90_ALL);
OPT(int,f90_mixed_expr,F90_ALL);
OPT(int,f90_overlength,F90_ALL);
OPT(int,f90_param_implicit_type,F90_ALL);
OPT(int,f90_param_noparen,F90_ALL);
OPT(int,f90_quad_constants,F90_ALL);
OPT(int,f90_stmt_order,F90_ALL);
OPT(int,f90_typeless_constants,F90_ALL);
OPT(int,f90_typesize,F90_ALL);
OPT(int,f90_unix_backslash,F90_ALL);
OPT(int,f90_unix_cpp,F90_ALL);
OPT(int,f90_variable_format,F90_ALL);
	/* Abuse of blank space in free format is not treated as a supported
	   extension to F90 but as a syntax error. Thus it is on by default. */
#define f90_freeform_space misc_warn
		/* End of -f90 options */

		/* These options are controlled by -f95 */
OPT(int,f95_real_do,F95_ALL);	/* real or d.p. DO variable */
OPT(int,f95_pause,F95_ALL);	/* PAUSE stmt */
OPT(int,f95_assign,F95_ALL);	/* ASSIGN stmt & kin */
OPT(int,f95_Hedit,F95_ALL);	/* H edit descriptor */
		/* End of -f95 options */

		/* These options are controlled by -makedcls */
OPT(int,dcl_declarations,FALSE);
OPT(int,dcl_only_undeclared,FALSE);
OPT(int,dcl_compact,FALSE);
OPT(int,dcl_use_continuations,FALSE);
OPT(int,dcl_keywords_lowercase,FALSE);
OPT(int,dcl_vars_and_consts_lowercase,FALSE);
OPT(int,dcl_excl_sftran3_internal_vars,FALSE);
OPT(int,dcl_asterisk_comment_character,FALSE);
OPT(int,dcl_lowercase_comment_character,FALSE);
OPT(int,dcl_no_array_dimensions,FALSE);
OPT(int,dcl_free_form,FALSE);
		/* End of -makedcls options */

		/* These options are controlled by -mkhtml */
OPT(int,html_documents,FALSE);
OPT(int,html_only_undeclared,FALSE);
OPT(int,html_compact,FALSE);
OPT(int,html_use_continuations,FALSE);
OPT(int,html_keywords_lowercase,FALSE);
OPT(int,html_vars_and_consts_lowercase,FALSE);
OPT(int,html_excl_sftran3_internal_vars,FALSE);
OPT(int,html_no_array_dimensions,FALSE);
OPT(int,html_free_form,FALSE);
		/* End of -makedcls options */

		/* These options are controlled by -portability */
OPT(int,port_common_alignment,PORT_ALL);/* Common not in desc size order */
OPT(int,port_backslash,PORT_ALL); 	/* Backslash used in standard way */
OPT(int,port_real_do,PORT_ALL);		/* Non-integer DO loop bounds */
OPT(int,port_long_string,PORT_ALL);	/* Char string > 255 in length */
OPT(int,port_hollerith,PORT_ALL);	/* Hollerith (except in FORMAT) */
OPT(int,port_mixed_equiv,PORT_ALL);	/* Different types equivalenced */
OPT(int,port_mixed_size,PORT_ALL);	/* sized, nonsized types mixed */
OPT(int,port_param_implicit_type,PORT_ALL);/* Implicit param type != default */
OPT(int,port_tabs,PORT_ALL);		/* Tabs in source */
		/* End of -portability options */


		/* These options are controlled by -pretty */
OPT(int,pretty_alt_return,PRETTY_ALL);/* alternate return out of range */
OPT(int,pretty_multiple_common,PRETTY_ALL);/* COMMON decl in multiple stmts */
OPT(int,pretty_multiple_namelist,PRETTY_ALL);/* NAMELIST decl in multiple stmts */
OPT(int,pretty_parens,PRETTY_ALL);	/* Parentheses around a variable */
OPT(int,pretty_overlength,PRETTY_ALL);	/* Lines over 72 columns */
OPT(int,pretty_extra_space,PRETTY_ALL);	/* Spaces in identifiers */
OPT(int,pretty_no_space,PRETTY_ALL);    /* Space missing btw id and keyword */
OPT(int,pretty_contin,PRETTY_ALL);	/* Continuation mark after comment */
		/* End of -pretty options */

		/* These options are controlled by -project */
OPT(int,make_project_file,FALSE);/* Save symtab defns in .prj file */
OPT(int,proj_trim_calls,TRUE);  /* Keep min no of calls in project file */
OPT(int,proj_trim_common,TRUE); /* Keep min no of block decls in proj file */
		/* End of -project options */


		/* These options are controlled by -source */
OPT(int,source_dec_tab,DEC_TABS); /*DEC Fortran tab-format*/
OPT(int,source_vms_include,VMS_INCLUDE); /*VMS INCLUDE statement*/
OPT(int,source_unix_backslash,UNIX_BACKSLASH); /*UNIX backslash escape*/
OPT(int,source_dec_param_std_type,DEC_PARAMETER_STANDARD_TYPE);
OPT(int,source_param_implicit,PARAMETER_IMPLICIT_TYPE);/* std params */
OPT(int,source_fixed_form,FALSE); /* force fixed format (see Note) */
OPT(int,source_free_form,FALSE); /* force free format (see Note) */
	/* Note: actual flag variable used to specify source form is
	   free_form in forlex.h.  It is set by init_scan based on
	   filename extension (.f or .f90), overridden by
	   source_fixed_format or source_free_format if set.*/

		/* end of -source options */

		/* These options are controlled by -style */
		/* require subprog END stmts to specify subprog class */
OPT(int,style_req_structured_end,STYLECHECK_ALL);
		/* require names on structured END stmts */
OPT(int,style_req_end_name,STYLECHECK_ALL);
		/* require names on IF, DO, and SELECT CASE constructs */
OPT(int,style_req_construct_name,STYLECHECK_ALL);
		/* require main prog to start with PROGRAM stmt */
OPT(int,style_req_prog_stmt,STYLECHECK_ALL);
		/* require use of block IF or logical IF */
OPT(int,style_req_block_if,STYLECHECK_ALL);
		/* require use of ENDDO with DO */
OPT(int,style_req_enddo,STYLECHECK_ALL);
		/* require use of ENDDO or CONTINUE with DO */
OPT(int,style_req_do_construct,STYLECHECK_ALL);
		/* no shared DO loop terminators */
OPT(int,style_shared_do_terminator,STYLECHECK_ALL);
		/* GOTO in all its forms */
OPT(int,style_goto,STYLECHECK_ALL);
		/* Labeled stmt other than FORMAT */
OPT(int,style_labeled_exec,STYLECHECK_ALL);
		/* Labeled FORMAT stmt */
OPT(int,style_labeled_format,STYLECHECK_ALL);

		/* End of -style options */

		/* These options are controlled by -truncation */
OPT(int,trunc_int_div_real,TRUNC_ALL);	/* Int/int --> real */
OPT(int,trunc_int_div_exponent,TRUNC_ALL);/* Int/int as exponentl */
OPT(int,trunc_int_div_zero,TRUNC_ALL);	/* Int/int const = 0 */
OPT(int,trunc_int_neg_power,TRUNC_ALL);	/* Int**(-int) */
OPT(int,trunc_real_subscript,TRUNC_ALL);/* Real array subscript */
OPT(int,trunc_real_do_index,TRUNC_ALL);	/* Real DO index with int bounds */
OPT(int,trunc_type_demotion,TRUNC_ALL);	/* High --> low precision */
OPT(int,trunc_size_demotion,TRUNC_ALL);	/* High --> low precision */
OPT(int,trunc_promotion,TRUNC_ALL);	/* Low --> high precision */
OPT(int,trunc_sigfigs,TRUNC_ALL);	/* Sngl const overspecified */
				/* truncation warnings related to precision */
#define trunc_precision (trunc_promotion||trunc_type_demotion||trunc_size_demotion)
		/* End of -truncation options */

		/* These options are controlled by -usage */
OPT(int,usage_arg_modified,USAGE_ALL);		/* const or expr arg modified */
OPT(int,usage_arg_alias_modified,USAGE_ALL);	/* arg same-as other modified */
OPT(int,usage_array_alias_modified,USAGE_ALL);/* arg in same array as other modified */
OPT(int,usage_arg_common_modified,USAGE_ALL);	/* arg in common modified */
OPT(int,usage_array_common_modified,USAGE_ALL);	/* array arg in common modified */
OPT(int,usage_arg_unused,USAGE_ALL);		/* arg declared but not used */
OPT(int,usage_com_block_unused,USAGE_ALL);	/* whole block declared but not used */
OPT(int,usage_com_block_volatile,USAGE_ALL);	/* block may lose defn */
OPT(int,usage_com_var_set_unused,USAGE_ALL);	/* set but not used */
OPT(int,usage_com_var_uninitialized,USAGE_ALL);/* used but not set */
OPT(int,usage_com_var_unused,USAGE_ALL);	/* declared but not used */
OPT(int,usage_do_var_modified,USAGE_ALL);	/* DO index var modified */
OPT(int,usage_ext_declared_only,USAGE_ALL);	/* declared EXTERNAL but not defined or used */
OPT(int,usage_ext_multiply_defined,USAGE_ALL);/* multiple definitions */
OPT(int,usage_ext_undefined,USAGE_ALL);	/* used but not defined (= -external flag) */
OPT(int,usage_ext_unused,USAGE_ALL);	/* defined but not used (like -library flag) */
OPT(int,usage_label_undefined,USAGE_ALL);  /* label used but undefined */
OPT(int,usage_label_unused,USAGE_ALL);     /* label defined but unused */
OPT(int,usage_var_set_unused,USAGE_ALL);	/* set but not used */
OPT(int,usage_var_uninitialized,USAGE_ALL);	/* used before set */
OPT(int,usage_var_unused,USAGE_ALL);		/* declared but not used */
		/* End of -usage options */


	/* The following flag is for warnings not covered by other flags,
	   so that all warnings can be turned off by -nocheck. */
OPT(int,misc_warn,TRUE);

	/* The following variables are controlled by command line options */

OPT(int,do_check,TRUE);		/* For -nocheck option */
OPT(int,decls_required,FALSE);	/* List all undeclared identifiers */
OPT(int,div_check,FALSE);	/* Check for possible division by zero */
OPT(int,ext_def_check,TRUE);	/* Check defined status of externals*/
OPT(int,help_screen,FALSE);	/* Print out help screen */
OPT(int,library_mode,FALSE);	/* Set used-flag for all modules in file */
OPT(int,brief,FALSE);		/* Briefer form of local error messages */
OPT(int,do_list,FALSE);		/* Listing flag */
OPT(int,novice_help,TRUE);	/* Extra help for novices */
OPT(int,pure_functions,TRUE);	/* Assume functions are pure */
OPT(int,quiet,FALSE);		/* Less verbose output format */
OPT(int,sixclash,FALSE);	/* To check if names unique in 1st 6 chars */
OPT(int,print_topo_sort,FALSE);	/* Topological sort of modules */
OPT(int,do_symtab,FALSE);	/* For symbol table printout */
OPT(int,print_version,FALSE);	/* Print version number and quit */
		/* Debugging flags */
OPT(int,debug_latest,FALSE);	/* debug the latest addition */
OPT(int,debug_glob_symtab,FALSE);/* global symtab contents */
OPT(int,debug_parser,FALSE);	/* grammar debug via DBG statements */
OPT(int,debug_hashtab,FALSE);	/* hash table contents */
OPT(int,debug_loc_symtab,FALSE); /* local symtab contents */
OPT(int,show_resources,FALSE);	/* space avail and used */
#ifdef DEBUG_FORLEX
OPT(int,debug_lexer,FALSE);	/* list of tokens as scanned */
#endif

		/* Declare variables for commandline settings */
OPT(int,call_tree_options,0);	/* Sum of: 1 (print it), */
				/* 2 (who-calls-who form), 4 (don't prune) */
				/* 8 (program order), 16 (VCG format) */


#ifndef STANDARD_INTRINSICS
				/* -intrinsic setting defaults */
OPT(int,intrinsic_rand_no_argument,(DEF_INTRINSIC_RAND & 1)==1); /* rand form */
OPT(int,intrinsic_rand_one_argument,(DEF_INTRINSIC_RAND & 2)==2);
OPT(int,intrinsic_iargc_no_argument,(DEF_INTRINSIC_IARGC & 1)==1);/* iargc form */
OPT(int,intrinsic_iargc_one_argument,(DEF_INTRINSIC_IARGC & 2)==2);
OPT(int,intrinsic_set_extra,(DEF_INTRINSIC_SET & 1)==1);/* common intrinsics */
OPT(int,intrinsic_set_unix,(DEF_INTRINSIC_SET & 2)==2);/* some unix intrinsics */
OPT(int,intrinsic_set_vms,(DEF_INTRINSIC_SET & 4)==4);/* some vms intrinsics */
#endif /* not STANDARD_INTRINSICS */

OPT(int,fixed_max_stmt_col,72);	/* End of fixed-form statement field ( <= MAXLINE )*/
OPT(int,error_cascade_limit,DEF_ERROR_CASCADE_LIMIT);/* max errors in cascade */
OPT(int,local_ptrsize,PTRSIZE);	/* Size of Cray pointer variables */
OPT(int,given_ptrsize,PTRSIZE);	/* Size requested for Cray pointer variables */

OPT(int,local_wordsize,BpW);	/* Bytes per word to use for default sizes */
OPT(int,given_wordsize,BpW);	/* User's request as per -wordsize=n  */
OPT(int,wrap_column,WRAP_COLUMN);/* For wrapping error messages */


			/* Shorthands for checking control settings */

#define usage_com_any (usage_com_block_unused || usage_com_var_set_unused \
       || usage_com_var_uninitialized || usage_com_var_unused)

#define COMCHECK_OFF (!(comcheck_by_name||comcheck_type||comcheck_length))

#define check_com_tree (comcheck_volatile&&usage_com_block_volatile) /* Check undef errors */

#define full_output	(do_list || do_symtab)

#define ANY_DCL_DECLARATIONS() (dcl_declarations\
  ||dcl_only_undeclared\
  ||dcl_compact||dcl_use_continuations\
  ||dcl_keywords_lowercase\
  ||dcl_vars_and_consts_lowercase\
  ||dcl_excl_sftran3_internal_vars\
  ||dcl_asterisk_comment_character\
  ||dcl_lowercase_comment_character\
  ||dcl_no_array_dimensions\
  ||dcl_free_form\
)
#define ANY_HTML_DECLARATIONS() (html_documents\
  ||html_only_undeclared\
  ||html_compact||html_use_continuations\
  ||html_keywords_lowercase\
  ||html_vars_and_consts_lowercase\
  ||html_excl_sftran3_internal_vars\
  ||html_no_array_dimensions\
  ||html_free_form\
)

		/* Declare variables for commandline StrSettings */
OPT(char*,out_fname,(char *)NULL);	/* Output filename */
#ifdef ALLOW_INCLUDE
OPT(char*,include_path,(char *)NULL);	/* An include-file directory */
#endif
OPT(char*,idletter_list,DEF_IDLETTER_LIST);/* non-alpha chars allowed in identifiers */

SHARED LINENO_t
    line_num,		/* line num of current char */
    next_line_num;	/* line num of lookahead char */
SHARED COLNO_t
    col_num,		/* column num of current char */
    next_col_num;	/* column num of lookahead char */

SHARED unsigned
    error_count,	/* Count of syntax error messages per file */
    warning_count;	/* Count of warning messages per file */

SHARED char *
    tab_filename;	/* Filename where there are tabs for -port */

		/* Resource usage information: */
SHARED unsigned long
    tot_line_count,	/* total source lines (stmts + comments) */
    tot_stmt_line_count,/* total source stmt lines  */
    tot_exec_stmt_count,/* count of executable stmts in program */
    tot_module_count,	/* count of subprograms in program */
    tot_label_count,	/* total number of statement labels defined */
    exec_stmt_count,	/* count of executable stmts in module */
    max_exec_stmt_count,/* max number of executable stmts in any module */
    max_loc_strings,	/* chars of local stringspace used */
    max_srctextspace,	/* chars of source text space used */
    max_paraminfo,	/* number of param info structs used */
    max_tokenlists,	/* number of tokenlists constructed */
    max_token_space,	/* number of tokens used */
    max_ptrspace,	/* number of pointers used */
    max_labels,		/* maximum no. of labels in any subprog */

    glob_strings_used,		/* chars of global stringspace used */
    arglist_element_used,	/* arg array elements used */
    arglist_head_used,		/* arg heads used (1 per call) */
    comlist_element_used,	/* com array elements used */
    comlist_head_used;		/* com heads used (1 per defn) */
SHARED long
    max_loc_symtab,	/* number of local symtab entries used */
    max_glob_symtab;	/* number of global symtab entries used */

SHARED int
    equivalence_flag,   /* true while parsing EQUIVALENCE statement */
    initial_flag,	/* true while only label or initial keywords read */
    implicit_flag,	/* true while parsing IMPLICIT statement */
    implicit_letter_flag, /* true while getting letters in IMPLICIT list */
    implicit_type_given,/* true if IMPLICIT type statement found */
    implicit_none,	/* true if IMPLICIT NONE statement found */
    prev_token_class,	/* token class of last-returned token */
    curr_stmt_class;	/* Token class of current stmt's leading token */

	/* flag for error messages to behave differently in global cases */
SHARED int doing_wrapup;
	/* ditto for local cases */
SHARED int doing_end_proc;

		/* Define linked-list structure for include-path list */
#ifdef ALLOW_INCLUDE
typedef struct IPNode {
  struct IPNode *link;		/* next path on the list */
  char *include_path;		/* one path (full directory name) */
} IncludePathNode;

SHARED IncludePathNode *include_path_list; /* header to the list */
#endif

		/* Declare routines shared with main module ftnchek.c */


	/* in exprtype.c */
PROTO(void init_typesizes, ( void ));

	/* in ftnchek.c */
PROTO( char * add_ext,( char *s, const char *ext ));
PROTO( int has_extension,( const char *name, const char *ext ));

	/* in forlex.c */
PROTO(void make_legal_char_list, ( void ));
PROTO(void finish_scan, ( void ));
PROTO(int flush_line_out, ( LINENO_t n ));
PROTO(void init_keyhashtab, ( void ));
PROTO(void init_scan, ( void ));
PROTO(void init_stream,( void ));

	/* in include.c */
PROTO(void open_include_file, ( char *fname, LINENO_t include_line_num ));
PROTO(int pop_include_file,( void ));

	/* in fortran.y/fortran.c */
PROTO(void init_parser, ( void ));
PROTO(int yyparse,  ( void ));

	/* in message.c */
PROTO(void msg_tail, ( const char *s ));
PROTO(void nonportable, ( LINENO_t lineno, COLNO_t colno, const char *s ));
PROTO(void nonstandard, ( LINENO_t lineno, COLNO_t colno, int f90, int f95 ));
PROTO(void oops_message, ( int severity, LINENO_t lineno, COLNO_t colno, const char *s ));
PROTO(void oops_tail, ( const char *s ));
PROTO(void print_a_line, ( FILE *fd, const char *line, LINENO_t num ));
PROTO(void syntax_error, ( LINENO_t lineno, COLNO_t colno, const char *s ));
PROTO(void ugly_code, ( LINENO_t lineno, COLNO_t colno, const char *s ));
PROTO(void warning, ( LINENO_t lineno, COLNO_t colno, const char *s ));
PROTO(void local_message, ( const char *filename, LINENO_t lineno, const char *s, const char *tag ));
PROTO(void global_warning, ( const char *filename, LINENO_t lineno, const char *s ));
PROTO(void global_message, ( const char *filename, LINENO_t lineno, const char *s ));
PROTO(void lex_error, (const char *s ));
PROTO(void yyerror, ( const char *s ));
PROTO(char* ulongtostr, (unsigned long num));
		/* Maximum length of a longtostr() result */
#define MAX_ULONGTOSTR (3*sizeof(unsigned long))


	/* in pgsymtab.c */
PROTO(void check_arglists, ( void ));
PROTO(void check_comlists, ( void ));
PROTO(void check_com_usage, ( void ));
PROTO(void visit_children, ( void ));

	/* in plsymtab.c */
PROTO(void debug_symtabs, ( void ));

	/* in project.c */
PROTO(void proj_file_out, ( FILE *fd ));
PROTO(void proj_file_in, ( FILE *fd ));

	/* in symtab.c */
PROTO(void init_tables, ( void ));
PROTO(void init_globals, ( void ));
PROTO(void init_symtab, ( void ));
PROTO(void note_filename, ( char *s ));
#ifdef DEBUG_SIZES
PROTO(void print_sizeofs, ( void ));
#endif

/* Misc. useful defines */
#undef MAX
#define MAX(X,Y) ((X)>(Y)?(X):(Y))
