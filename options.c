/* $Id: options.c,v 1.36 2003/03/20 22:15:42 moniot Exp $

	Definitions of command-line options and routines to set them.

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
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#include "options.h"	/* Prototypes of external routines defined below */
#include "utils.h"	/* we use strncasecmp */


typedef enum {		/* for isacheck fields.  Used to suppress -nocheck */
 NOT_A_CHECK, IS_A_CHECK
} isacheck_t;

				/* Define WarnOptionList struct here */
typedef struct {
  char *name;			/* user knows the sub-setting by this name */
  int *flag;			/* ptr to the option variable itself */
  char *explanation;		/* for use by -warning=help */
} WarnOptionList;

				/* Define StrsettingList struct here */
typedef struct {
    char *name;			/* user knows the setting by this name */
    char **strvalue;		/* the string argument goes here */
    char *turnon, *turnoff;	/* e.g. "all", "none" */
    isacheck_t isacheck;	/* tells -nocheck to turn it off */
    WarnOptionList *option_list;/* this holds the set of options */
				/* For compatibility with -option=num form: */
    PROTO(void (*numeric_form_handler),(int num, char *setting_name));
    char *explanation;		/* for use by -help */
} StrsettingList;

extern int yydebug;		/* for grammar debugging via -yydebug */

			/* Prototypes of private routines defined */

PROTO(PRIVATE void append_include_path,( char *new_path ));

PROTO(PRIVATE FILE *find_rc,( void ));

PROTO(PRIVATE void list_warn_options,( StrsettingList *strsetting ));

PROTO(PRIVATE void make_env_name,( char *env_name, char *option_name ));

PROTO(PRIVATE void mutual_exclude, (  WarnOptionList wList[], const char *opt_name,
		      int *thisflag, int *otherflags[] ));

PROTO(PRIVATE void process_warn_string,
 ( char *warn_string, StrsettingList *strsetting ));

PROTO(PRIVATE int str_to_num,(char *s));

PROTO(PRIVATE int read_setting,( char *s, int *setvalue, char *name, int
			 minlimit, int maxlimit, int turnoff, int
			 turnon, int min_default_value, int
			 max_default_value ));

PROTO(PRIVATE void set_warn_option,
 ( char *s, WarnOptionList warn_option[] ));

PROTO(PRIVATE void set_warn_option_value, ( int *flag, int value));

PROTO(PRIVATE void update_str_options,( StrsettingList *strset ));

PROTO(PRIVATE int wildcard_match, (char *pat, char *str));


		/* The following routines handle compatibility with older
		   numeric form of settings that are now WarnOptionLists.
		 */
PROTO(PRIVATE void argcheck_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void arraycheck_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void calltree_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void comcheck_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void intrinsic_numeric_option,( int value, char *setting_name ));

PROTO(PRIVATE void makedcl_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void mkhtml_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void source_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void usage_numeric_option, ( int value, char *setting_name ));

PROTO(PRIVATE void numeric_option_error,( char *s, int minlimit, int maxlimit ));

	/* Here we define the commandline options.  Most options are boolean
	   switchopts, with "no" prefix to unset them.  Others (called
	   settings) are numeric quantities, defined using "=num".
	   A third category (strsettings) are string quantities, eg filenames.
	   The argument "?" will cause list of options to be printed out.
	   For VMS, options can be prefixed with either "-" or "/",
	   but messages will use the canonical form.  Since VMS allows
	   options to be smushed together, end-of-option is signalled by
	   either NUL or the / of next option.
	 */

#ifdef OPTION_PREFIX_SLASH
#define OPT_PREFIX '/'	/* Canonical VMS prefix for commandline options */
#define END_OF_OPT( C )  ((C) == '\0' || (C) == '/')
#else
#define OPT_PREFIX '-'	/* Canonical Unix prefix for commandline options */
#define END_OF_OPT( C )  ((C) == '\0')
#endif

#define OPT_MATCH_LEN 3	/* Options are matched only in 1st 3 chars */
#define NUM_SWITCHES (sizeof(switchopt)/sizeof(switchopt[0]))
#define NUM_SETTINGS (sizeof(setting)/sizeof(setting[0]))
#define NUM_STRSETTINGS (sizeof(strsetting)/sizeof(strsetting[0]))
#define MAX_OPT_LEN 32		/* Big enough to hold any warn option name */


/*	Adding new options:

	   New options with boolean (switchopt) or numeric (setting)
	   values can be added to the lists below by inserting a definition
	   using the same syntax as the others, and declaring the
	   controlled variable with a line in ftnchek.h of the form:
	   	OPT(type,name,default-value);
	   No other changes are needed.  (For boolean options, make
	   sure they precede "-debug" in order for them to appear in
	   the -help page.)

	   New options with string values (strsetting) are added
	   similarly, but they have option_list and numeric_form_handler
	   fields that must also be defined.  The strsettings come in
	   two flavors: those whose string value is used literally
	   (like -include) and those whose string value is a list of
	   sub-options (like -f77).  For the first type, just set the
	   option_list and numeric_form_handler fields to NULL.  For
	   the second type, create a new WarnOptionList following the
	   pattern of f77_warn_option.  This list must precede the
	   strsettings definition.  For each item in this list, a
	   corresponding char * variable (e.g. f77_warn_list) must
	   be declared as well.  Then insert the name of the list
	   into the option_list field, and the name of the companion
	   variable into the strvalue field, of the strsetting entry.  The
	   numeric_form_handler field is used for strsettings that
	   used to take a numeric value and have been converted to the
	   option-list form.  See usage_numeric_handler for an example
	   of how these work.  This field is NULL if there is no
	   handler.  If there is a handler, put its prototype with the
	   others above, add the code at a suitable point in this
	   file, and put its name in the numeric_form_handler field of
	   the WarnOptionList.

*/


/* Option definitions: */

		/* List of switches is defined first.  Each entry gives the
		   name and the corresponding flag variable to be set
		   or cleared.  See set_option() for processing of switches.

		   N.B. list_options() will suppress printing of any options
		   whose explanation starts with "debug" unless the -debug
		   switch was previously given.
		 */
PRIVATE struct {
    char *name;			/* User knows it by this name */
    int *switchflag;		/* Pointer to variable that controls it */
    char *explanation;		/* For use by -help */
    isacheck_t isacheck;	/* Tells -nocheck to turn it off */
} switchopt[]={
	{"brief",	&brief,
		 "briefer form of error messages",NOT_A_CHECK},
	{"check",	&do_check,
		 "perform checking",IS_A_CHECK},
	{"declare",	&decls_required,
		 "list undeclared variables",IS_A_CHECK},
	{"division",	&div_check,
		 "catch possible div by 0",IS_A_CHECK},
	{"extern",	&usage_ext_undefined,
		 "check if externals defined",IS_A_CHECK},
	{"help",	&help_screen,
		 "print help screen",NOT_A_CHECK},
	{"library",	&library_mode,
		 "treat next files as library",NOT_A_CHECK},
	{"list",	&do_list,
		 "print program listing",NOT_A_CHECK},
	{"novice",	&novice_help,
		 "extra help for novices",NOT_A_CHECK},
	{"pure",	&pure_functions,
		 "functions have no side effects",IS_A_CHECK},
	{"quiet",	&quiet,
		 "less verbose output",NOT_A_CHECK},
	{"reference",	&print_ref_list,
		 "print who-calls-who reference list",NOT_A_CHECK},
	{"resources",	&show_resources,
		 "show info on resource usage",NOT_A_CHECK},
	{"sixchar",	&sixclash,
		 "catch nonunique names",IS_A_CHECK},
	{"sort",	&print_topo_sort,
		 "prerequisite-order sort of modules",NOT_A_CHECK},
	{"symtab",	&do_symtab,
		 "print symbol table info",NOT_A_CHECK},
#ifdef VCG_SUPPORT
	{"vcg",		&print_vcg_list,
		 "print call graph in vcg format",NOT_A_CHECK},
#endif
	{"version",	&print_version,
		 "print version number",NOT_A_CHECK},
	{"volatile",	&comcheck_volatile,
		 "assume volatile common blocks",IS_A_CHECK},

	{"debug",	&debug_latest,
		 "debug latest code",IS_A_CHECK},
	{"global",	&debug_glob_symtab,
		 "debug global symtab info",IS_A_CHECK},
	{"grammar",	&debug_parser,
		 "debug printout in parser",IS_A_CHECK},
	{"hashtable",	&debug_hashtab,
		 "debug printout of hashtable",IS_A_CHECK},
	{"local",	&debug_loc_symtab,
		 "debug local symtab info",IS_A_CHECK},
#ifdef DEBUG_FORLEX
	{"tokens",	&debug_lexer,
		 "debug printout in lexer",IS_A_CHECK},
#endif
	{"yydebug",	&yydebug,
		 "debug via yydebug",IS_A_CHECK},
};


		/* List of settings is defined here. Each entry gives
		   the name, the corresponding variable, the range
		   of permitted values, the value for turning it off,
		   the values to assign if below or above the limits rsptly,
		   whether it is a check to be turned off by -nocheck,
		   followed by brief explanation.
		   See set_option() for processing. */
PRIVATE struct {
    char *name;
    int *setvalue;
    int minlimit,maxlimit,turnoff,turnon,min_default_value,max_default_value;
    isacheck_t isacheck;
    char *explanation;
} setting[]={
  {"columns",	&fixed_max_stmt_col,  72, MAXLINE, 72, MAXLINE, 72, MAXLINE, NOT_A_CHECK,
			"max fixed-form line length processed"},
  {"errors",&error_cascade_limit, 0, 999, 0, DEF_ERROR_CASCADE_LIMIT, 0, 999, NOT_A_CHECK,
			"max number of error messages per cascade"},
  {"pointersize",&given_ptrsize, 1, 16, PTRSIZE, PTRSIZE, 1, 16, NOT_A_CHECK,
			"standard pointer size in bytes"},
  {"wordsize",	&given_wordsize, 0, 16, 0, BpW, 0, 16, NOT_A_CHECK,
			"standard wordsize in bytes (0=no default)"},
  {"wrap",	&wrap_column, 0, 999, 0, WRAP_COLUMN, 0, 999, NOT_A_CHECK,
			"width of page to wrap error messages"},
};




		/* Now we define the various "warn list" options.  Each
		   has a char* pointer used to point to the options given
		   with the command-line argument.
		   Each entry in the WarnOptionList has the name of the
		   sub-option, the address of the flag variable it
		   controls, and an explanation used when printing the
		   help page for the option.

		   Each list must be alphabetized or at least options with
		   matching prefix strings must be adjacent.  When a
		   new option list is defined, it must also be entered
		   into strsetting array below.
		*/

PRIVATE char *argcheck_warn_list=(char *)NULL;/* Arg mismatches to warn about */

PRIVATE WarnOptionList
 argcheck_warn_option[]={
  {
#if ARGCHECK_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,"Function Argument Mismatch Warning"},/* Title for list */
  {"arrayness",		&argcheck_arrayness,
				"argument arrayness mismatch"},
  {"type",		&argcheck_argtype,
				"argument type mismatch"},
  {"function-type",	&argcheck_functype,
				"function type mismatch"},
  {"number",		&argcheck_argnumber,
				"wrong number of arguments"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *arraycheck_warn_list=(char *)NULL;/* Arg arrayness warnings */

PRIVATE WarnOptionList
 arraycheck_warn_option[]={
  {
#if ARRAYCHECK_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,"Argument Arrayness Mismatch Warning"},/* Title for list */
  {"dimensions",	&arraycheck_dims,
				"different number of dimensions"},
  {"size",		&arraycheck_size,
				"different number of elements"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *calltree_opt_list=(char *)NULL;/* Subprogram call graph options */

PRIVATE WarnOptionList
 calltree_option[]={	/* not really a warning */
  {
   "none"	 /* used by -help */
     , (int *)NULL,"Call-Tree Output"},/* Title for list */
  {"prune",		&call_tree_prune,
				"prune repeated subtrees"},
  {"reference",		&print_ref_list,
				"produce call tree in who-calls-who format"},
  {"sort",		&call_tree_sort,
				"sort call tree alphabetically"},
  {"tree",		&print_call_tree,
				"produce call tree in text format"},
#ifdef VCG_SUPPORT
  {"vcg",		&print_vcg_list,
				"produce call tree in vcg format"},
#endif
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *comcheck_warn_list=(char *)NULL;/* Common block mismatch warnings */

PRIVATE WarnOptionList
 comcheck_warn_option[]={
  {
#if COMCHECK_ALL
   "dimensions,exact,length,type"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,"Common Block Mismatch Warning"},/* Title for list */
  {"dimensions",	&comcheck_dims,
				"arrays differ in dimensions"},
  {"exact",		&comcheck_by_name,
				"require variable-by-variable correspondence"},
  {"length",		&comcheck_length,
				"blocKs differ in total length"},
  {"type",		&comcheck_type,
				"data type mismatch at corresponding locations"},
  {"volatile",		&comcheck_volatile,
				"assume blocks are volatile"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *crossref_opt_list=(char *)NULL; /* Cross-ref listing options */

PRIVATE WarnOptionList
 crossref_option[]={    /* not a warning */
  {
   "none"       /* used by -help */
   , (int *)NULL, "Cross-Ref Output"}, /* Title for list */
  {"calls",           &print_xref_list, "print call cross-reference list"},

  {"common",          &print_com_xrefs, 
                                "print common block cross-reference list"},

  {"labels",          &print_lab_refs, "print label cross-reference list"},

  {(char *)NULL, (int *)NULL, (char *)NULL},
};
		/* Here define list of -f77 warning options.  These are set
		   or cleared by -[no]f77=list option.  Note that the variables
		   are FALSE if feature is ALLOWED, and TRUE if feature is
		   to be WARNED about.
		 */

PRIVATE char *f77_warn_list=(char *)NULL; /* Non F77 extensions to warn about */

PRIVATE WarnOptionList
 f77_warn_option[]={
  {
#if F77_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Fortran 77 Warning"},	/* Title for list */
  {"accept-type",	&f77_accept_type,
				"ACCEPT and TYPE I/O statements"},
  {"array-bounds",	&f77_array_bounds,
				"array bounds expressions"},
  {"assignment-stmt",	&f77_assignment,
				"assignment involving array"},
  {"attribute-based-decl",&f77_attrbased_typedecl,
			   "attribute-based (:: -style) variable declaration"},
  {"automatic-array",	&f77_automatic_array,
				"local array of variable size"},
  {"backslash",		&f77_unix_backslash,
				"Unix backslash escape in strings"},
  {"byte",		&f77_byte,
				"BYTE data type"},
  {"case-construct",	&f77_case_construct,
				"CASE construct"},
  {"common-subprog-name",&f77_common_subprog_name,
				"Common block & subprog with same name"},
  {"construct-name",	&f77_construct_name,
				"Construct names on DO statements"},
  {"continuation",	&f77_20_continue,
				"More than 19 continuation lines"},
  {"cpp",		&f77_unix_cpp,
				"Unix C preprocessor directives"},
  {"cycle-exit",	&f77_cycle_exit,
				"CYCLE or EXIT statement"},
  {"d-comment",		&f77_d_comment,
				"Debug comments starting with D"},
  {"dec-tab"	,	&f77_dec_tabs,
				"DEC Fortran tab-formatted source"},
  {"do-enddo",		&f77_do_enddo,
				"DO loop extensions"},
  {"double-complex",	&f77_double_complex,
				"Double complex datatype"},
  {"format-dollarsign",	&f77_format_dollarsigns,
				"$ control code in FORMAT"},
  {"format-edit-descr",	&f77_format_extensions,
				"Nonstandard edit descriptors"},
  {"function-noparen",	&f77_function_noparen,
				"FUNCTION defined without parens"},
  {"implicit-none",	&f77_implicit_none,
				"IMPLICIT NONE statement"},
  {"include",		&f77_include,
				"INCLUDE statement"},
  {"initializer",	&f77_initializers,
				"Variable initializer in declaration"},
  {"inline-comment",	&f77_inline_comment,
				"Inline comments starting with !"},
  {"internal-list-io",	&f77_internal_list_io,
				"List-directed I/O to internal file"},
  {"intrinsic",		&f77_intrinsics,
				"Nonstandard intrinsic functions"},
  {"io-keywords",	&f77_io_keywords,
				"Nonstandard I/O keywords"},
  {"long-line",		&f77_overlength,
				"Statements with code past 72 columns"},
  {"long-name",		&f77_long_names,
				"Identifiers over 6 chars"},
  {"mixed-common",	&f77_mixed_common,
				"Mixed char and nonchar data in common"},
  {"mixed-expr",	&f77_mixed_expr,
				"Incompatible type combinations in exprs"},
  {"name-dollarsign",	&f77_dollarsigns,
				"$ or other nonalnum (except _) in identifiers"},
  {"name-underscore",	&f77_underscores,
				"Underscores in identifiers"},
  {"namelist",		&f77_namelist,
				"NAMELIST statement"},
  {"param-implicit-type",&f77_param_implicit_type,
				"implicit typing of PARAMETERs"},
  {"param-intrinsic",	&f77_param_intrinsic,
				"Intrinsics and **real in PARAMETER defns"},
  {"param-noparen",	&f77_param_noparen,
				"PARAMETER statement without parens"},
  {"pointer",		&f77_cray_pointers,
				"Cray pointer syntax"},
  {"quad-constant",	&f77_quad_constants,
				"Quad precision constants like 1.23Q4"},
  {"quotemark",		&f77_quotemarks,
				"Strings delimited by \"quote marks\""},
  {"relops",		&f77_relops,
				"Relational operators < <= == /= > >="},
  {"semicolon",		&f77_semicolon,
				"Semicolon as statement separator"},
  {"statement-order",	&f77_stmt_order,
				"Statement out of order"},
  {"typeless-constant",	&f77_typeless_constants,
				"Typeless constants like Z'19AF'"},
  {"type-size",		&f77_typesize,
				"Sized type declarations like REAL*8"},
  {"variable-format",	&f77_variable_format,
				"Variable format repeat spec or field size"},
  {"vms-io",		&f77_io_keywords, /* same as "io-keywords" */
				"Nonstandard I/O keywords"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *f90_warn_list=(char *)NULL; /* Non F90 extensions to warn about */

PRIVATE WarnOptionList
 f90_warn_option[]={
  {
#if F90_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,	"Fortran 90 Violation Warning"},/* Title for list */
  {"accept-type",	&f90_accept_type,
				"ACCEPT and TYPE I/O statements"},
  {"backslash",		&f90_unix_backslash,
				"Unix backslash escape in strings"},
  {"byte",		&f90_byte,
				"BYTE data type"},
  {"continuation",	&f90_continue,
				"Too many or empty continuation lines"},
  {"cpp",		&f90_unix_cpp,
				"Unix C preprocessor directives"},
  {"d-comment",		&f90_d_comment,
				"Debug comments starting with D"},
  {"dec-tab"	,	&f90_dec_tabs,
				"DEC Fortran tab-formatted source"},
  {"double-complex",	&f90_double_complex,
				"Double complex datatype"},
  {"format-dollarsign",	&f90_format_dollarsigns,
				"$ control code in FORMAT"},
  {"format-edit-descr",	&f90_format_extensions,
				"Nonstandard edit descriptors"},
/* Note: f90_freeform_space == misc_warn, not controlled here */
  {"function-noparen",	&f90_function_noparen,
				"FUNCTION defined without parens"},
  {"initializer",	&f90_initializers,
				"Variable initializer using / / in declaration"},
  {"intrinsic",		&f90_intrinsics,
				"Nonstandard intrinsic functions"},
  {"io-keywords",	&f90_io_keywords,
				"Nonstandard I/O keywords"},
  {"long-line",		&f90_overlength,
				"Statements with code past max columns"},
  {"mixed-expr",	&f90_mixed_expr,
				"Incompatible type combinations in exprs"},
  {"name-dollarsign",	&f90_dollarsigns,
				"$ in identifiers"},
  {"param-implicit-type",&f90_param_implicit_type,
				"implicit typing of PARAMETERs"},
  {"param-noparen",	&f90_param_noparen,
				"PARAMETER statement without parens"},
  {"pointer",		&f90_cray_pointers,
				"Cray pointer syntax"},
  {"quad-constant",	&f90_quad_constants,
				"Quad precision constants like 1.23Q4"},
  {"statement-order",	&f90_stmt_order,
				"Statement out of order"},
  {"type-size",		&f90_typesize,
				"Sized type declarations like REAL*8"},
  {"typeless-constant",	&f90_typeless_constants,
				"Nonstandard constants like X'19AF'"},
  {"variable-format",	&f90_variable_format,
				"Variable format repeat spec or field size"},
  {"vms-io",		&f90_io_keywords, /* same as "io-keywords" */
				"Nonstandard I/O keywords"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *f95_warn_list=(char *)NULL; /* Non F95 old syntax to warn about */

PRIVATE WarnOptionList
 f95_warn_option[]={
  {
#if F95_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,	"Fortran 95 Violation Warning"},/* Title for list */
  {"real-do",	&f95_real_do,
				"real DO variable"},
  {"pause",	&f95_pause,
				"PAUSE stmt"},
  {"assign",	&f95_assign,
				"ASSIGN stmt, assigned GOTO, assigned format"},
  {"h-edit",	&f95_Hedit,
				"H edit descriptor"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};

#ifndef STANDARD_INTRINSICS
PRIVATE char *intrinsic_option_list=(char *)NULL; /* intrinsic fcn options */

PRIVATE WarnOptionList
 intrinsic_option[]={
  {
 	 /* Define -help message. This is not done right... */
#if (DEF_INTRINSIC_SET & 2)
  "unix"
#else
#if (DEF_INTRINSIC_SET & 4)
   "vms"
#else
#if (DEF_INTRINSIC_SET & 1)
   "common"
#else
   "none"
#endif
#endif
#endif

     , (int *)NULL,		"Intrinsic Function"},	/* Title for list */
  {"extra",		&intrinsic_set_extra,
			"recognize commonly supported nonstandard intrinsics"},
  {"iargc-no-argument",	&intrinsic_iargc_no_argument,
				"iargc takes no arguments"},
  {"iargc-one-argument",&intrinsic_iargc_one_argument,
				"iargc takes one argument"},
  {"rand-no-argument",	&intrinsic_rand_no_argument,
				"rand takes no arguments"},
  {"rand-one-argument",	&intrinsic_rand_one_argument,
				"rand takes one argument"},
  {"unix",		&intrinsic_set_unix,
				"recognize some unix intrinsics"},
  {"vms",		&intrinsic_set_vms,
				"recognize some vms intrinsics"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};
#endif /* not STANDARD_INTRINSICS */


               
			/* makedcls is not really a warning list,
			   but it uses the same style of control. */

PRIVATE char *makedcl_warn_list=(char *)NULL; /* Make-declarations options */

PRIVATE WarnOptionList
 makedcl_warn_option[]={
  {"none"			/* used by -help */
     , (int *)NULL,		"Make Type-Declarations"}, /* Title for list */

  {"asterisk-comment",	&dcl_asterisk_comment_character,
				"use asterisk as comment character"},
  {"comment-char-lowercase",&dcl_lowercase_comment_character,
				"use lowercase c as comment character"},
  {"compact",		&dcl_compact,
				"compact output format"},
  {"declarations",	&dcl_declarations,
				"produce file of declarations"},
  {"exclude-sftran3",	&dcl_excl_sftran3_internal_vars,
				"omit SFTRAN3 internal variables"},
  {"free-form",		&dcl_free_form,
				"produce declarations in free form"},
  {"keywords-lowercase",&dcl_keywords_lowercase,
				"output keywords in lowercase"},
  {"suppress-array-dimensions",&dcl_no_array_dimensions,
				"do not declare array dimensions"},
  {"undeclared-only",	&dcl_only_undeclared,
				"declare only undeclared things"},
  {"use-continuation-lines",&dcl_use_continuations,
				" use continuation lines"},
  {"vars-and-consts-lowercase",&dcl_vars_and_consts_lowercase,
				"output variables and constants in lowercase"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


               
			/* makehtmls is not really a warning list,
			   but it uses the same style of control. */

PRIVATE char *mkhtml_warn_list=(char *)NULL; /* Make-declarations options */

PRIVATE WarnOptionList
 mkhtml_warn_option[]={
  {"none"			/* used by -help */
     , (int *)NULL,		"Make HTML Documents"}, /* Title for list */

  {"compact",		&html_compact,
				"compact output format"},
  {"documents",		&html_documents,
				"produce html documents"},
  {"exclude-sftran3",	&html_excl_sftran3_internal_vars,
				"omit SFTRAN3 internal variables"},
  {"free-form",		&html_free_form,
				"produce declarations in free form"},
  {"keywords-lowercase",&html_keywords_lowercase,
				"output keywords in lowercase"},
  {"suppress-array-dimensions",&html_no_array_dimensions,
				"do not declare array dimensions"},
#if 0
  {"undeclared-only",	&html_only_undeclared,
				"declare only undeclared things"},
#endif
  {"use-continuation-lines",&html_use_continuations,
				" use continuation lines"},
  {"vars-and-consts-lowercase",&html_vars_and_consts_lowercase,
				"output variables and constants in lowercase"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *port_warn_list=(char *)NULL; /* Nonportable things to warn about */

PRIVATE WarnOptionList
 port_warn_option[]={
  {
#if PORT_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Portability Warning"},	/* Title for list */
  {"backslash",		&port_backslash,
				"Backslash in standard-conforming strings"},
  {"common-alignment",	&port_common_alignment,
				"COMMON not in descending size order"},
  {"hollerith",		&port_hollerith,
				"Hollerith constants (except in FORMAT)"},
  {"long-string",	&port_long_string,
				"Strings over 255 chars long"},
  {"mixed-equivalence",	&port_mixed_equiv,
				"Different data types equivalenced"},
  {"mixed-size",	&port_mixed_size,
				"Default and explicit size types mixed"},
  {"real-do",		&port_real_do,
				"Non-integer DO loops"},
  {"param-implicit-type",&port_param_implicit_type,
			"Implicit type of PARAMETER differs from default type"},
  {"tab",		&port_tabs,
				"Tabs in source code"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *pretty_warn_list=(char *)NULL; /* Misleading things to warn about */

PRIVATE WarnOptionList
 pretty_warn_option[]={
  {
#if PRETTY_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Appearance Warning"},	/* Title for list */
  {"alternate-return",	&pretty_alt_return,
				"Alternate return out of range"},
  {"embedded-space",	&pretty_extra_space,
				"Space in variable names or operators"},
  {"continuation",	&pretty_contin,
				"Continuation mark following comment line"},
  {"long-line",		&pretty_overlength,
				"Lines over 72 columns"},
  {"missing-space",	&pretty_no_space,
				"Missing space between variable & keyword"},
  {"multiple-common",	&pretty_multiple_common,
				"COMMON declared in multiple stmts"},
  {"multiple-namelist",	&pretty_multiple_namelist,
				"NAMELIST declared in multiple stmts"},
  {"parentheses",	&pretty_parens,
				"Parentheses around a variable"},
  {(char *)NULL, (int *)NULL, (char *)NULL},

};

				/* Project file is not really a warning list,
				   but it uses the same style of control. */

PRIVATE char *project_warn_list=(char *)NULL; /* Project file options */

PRIVATE WarnOptionList
 project_warn_option[]={
  {"none" , (int *)NULL,	"Project File"},	/* Title for list */

  {"create",	&make_project_file,
			   "Create project file"},
  {"trim-calls", &proj_trim_calls,
			   "Keep minimum information about subprogram calls"},
  {"trim-common", &proj_trim_common,
			   "Keep minimum information about common blocks"},
  {(char *)NULL, (int *)NULL, (char *)NULL},

};

			/* Source format is not really a warning list,
			   but it uses the same style of control. */

PRIVATE char *source_form_list=(char *)NULL; /* Source format options */

PRIVATE WarnOptionList
 source_form_option[]={
  {
#if VMS_INCLUDE
   "vms-include"       /* For -help.  This ignores the unlikely possibility
			  that other options may also be turned on by default. */
#else
   "none"
#endif
     , (int *)NULL,		"Source Format"}, /* Title for list */
  {"dec-param-standard-type",&source_dec_param_std_type,
				"DEC Fortran PARAMETERs typed as if standard"},
  {"dec-tab",   	&source_dec_tab,
				"DEC Fortran tab-format"},
  {"fixed",		&source_fixed_form,
				"force fixed source form"},
  {"free",		&source_free_form,
				"force free source form"},
  {"param-implicit-type",&source_param_implicit,
				"implicit typing of PARAMETERs by value"},
  {"unix-backslash",	&source_unix_backslash,
				"UNIX-style backslash escape char"},
  {"vms-include",	&source_vms_include,
				"VMS-style INCLUDE statement"},
  {(char *)NULL, (int *)NULL, (char *)NULL},

 };


PRIVATE char *stylecheck_warn_list=(char *)NULL; /* block structure style warnings */

PRIVATE WarnOptionList
 stylecheck_warn_option[]={
  {
#if STYLECHECK_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,"Picky Warnings About Block Structures"},/* Title for list */
  {"block-if",		&style_req_block_if,
   "require block IF or logical IF"},
  {"construct-name",	&style_req_construct_name,
   "require named block constructs"},
  {"distinct-do",	&style_shared_do_terminator,
   "DO loops not to share terminator"},
  {"do-construct",	&style_req_do_construct,
   "require ENDDO or CONTINUE as terminator of DO"},
  {"do-enddo",		&style_req_enddo,
   "require ENDDO as terminator of DO"},
  {"end-name",		&style_req_end_name,
   "require subprogram name on structured END statements"},
  {"format-stmt",	&style_labeled_format,
   "object to FORMAT statements"},
  {"goto",		&style_goto,
   "object to GOTO statements"},
  {"labeled-stmt",	&style_labeled_exec,
   "object to labeled statements except FORMAT"},
  {"program-stmt",	&style_req_prog_stmt,
   "require PROGRAM statement at head of program"},
  {"structured-end",	&style_req_structured_end,
   "require END PROGRAM et al, not plain END"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE char *trunc_warn_list=(char *)NULL; /* Truncation pitfalls to warn about */

PRIVATE WarnOptionList
 trunc_warn_option[]={
  {
#if TRUNC_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Truncation Warning"},	/* Title for list */
  {"int-div-exponent",	&trunc_int_div_exponent,
				"int/int used as exponent"},
  {"int-div-real",	&trunc_int_div_real,
				"int/int converted to real"},
  {"int-div-zero",	&trunc_int_div_zero,
				"int/int = constant 0 "},
  {"int-neg-power",	&trunc_int_neg_power,
				"int**(-int), usually equals 0"},
  {"promotion",		&trunc_promotion,
				"lower precision promoted to higher"},
  {"real-do-index",	&trunc_real_do_index,
				"real DO index with int bounds"},
  {"real-subscript",	&trunc_real_subscript,
				"real array subscript"},
  {"significant-figures",&trunc_sigfigs,
				"single precision const overspecified"},
  {"size-demotion",		&trunc_size_demotion,
			"higher precision truncated to lower, same type"},
  {"type-demotion",		&trunc_type_demotion,
			"higher precision truncated to lower, different type"},
  {(char *)NULL, (int *)NULL, (char *)NULL},

};


PRIVATE char *usage_warn_list=(char *)NULL; /* Variable usages to warn about */

PRIVATE WarnOptionList
 usage_warn_option[]={
  {
#if USAGE_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Usage Warning"},	/* Title for list */
  {"arg-alias",		&usage_arg_alias_modified,
		"scalar argument same as another is modified"},
  {"arg-array-alias",	&usage_array_alias_modified,
		"argument in same array as another is modified"},
  {"arg-common-alias",		&usage_arg_common_modified,
		"scalar argument same as common variable, either is modified"},
  {"arg-common-array-alias",	&usage_array_common_modified,
		"array argument same as common variable, either is modified"},
  {"arg-const-modified",	&usage_arg_modified,
		"constant or expression argument is modified"},
  {"arg-unused",	&usage_arg_unused,
		"dummy argument declared but not used"},
  {"com-block-unused",	&usage_com_block_unused,
		"whole common block declared but not used"},
  {"com-block-volatile", &usage_com_block_volatile,
		"common block may lose definition if volatile"},
  {"com-var-set-unused",	&usage_com_var_set_unused,
		"common variable set but not used"},
  {"com-var-uninitialized",	&usage_com_var_uninitialized,
		"common variable used but not set"},
  {"com-var-unused",	&usage_com_var_unused,
		"common variable declared but not used"},
  {"do-index-modified",	&usage_do_var_modified,
		"active DO index variable modified"},
  {"ext-multiply-defined",	&usage_ext_multiply_defined,
		"external multiply defined"},
  {"ext-declared-only",	&usage_ext_declared_only,
		"name declared EXTERNAL but not defined or used"},
  {"ext-undefined",	&usage_ext_undefined,	/* Also touched by -extern */
		"external declared or used but not defined (= -external)"},
  {"ext-unused",	&usage_ext_unused,
		"external defined but not used"},
  {"label-undefined", &usage_label_undefined,
		"label used but undefined"},
  {"label-unused", &usage_label_unused,
		"label defined but unused"},     
  {"var-set-unused",	&usage_var_set_unused,
		"local variable set but not used"},
  {"var-uninitialized",	&usage_var_uninitialized,
		"local variable used before set"},
  {"var-unused",	&usage_var_unused,
		"local variable declared but not used"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};

		/* List of strsettings is defined here. Each entry
		   gives the name of the corresponding string
		   variable, value to set if "=str" omitted, and brief
		   explanation.  See set_option() for processing. */

/*** (struct was declared above: repeated in comment here for reference)
StrsettingList {
    char *name;
    char **strvalue;
    char *turnon, *turnoff;
    isacheck_t isacheck;
    WarnOptionList *option_list;
    PROTO(void (*numeric_form_handler),(int num, char *setting_name));
    char *explanation;
};***/

PRIVATE StrsettingList strsetting[]={
  {"arguments",	&argcheck_warn_list, "all", "none", IS_A_CHECK,
     argcheck_warn_option, argcheck_numeric_option,
     "check subprogram argument agreement"},
  {"array",	&arraycheck_warn_list, "all", "none", IS_A_CHECK,
     arraycheck_warn_option, arraycheck_numeric_option,
     "check subprogram argument arrayness agreement"},
  {"calltree",	&calltree_opt_list, "tree", "none", NOT_A_CHECK,
     calltree_option, calltree_numeric_option,
     "subprogram call graph options"},
  {"common",	&comcheck_warn_list, "all", "none", IS_A_CHECK,
     comcheck_warn_option, comcheck_numeric_option,
     "check for common block mismatches"},
  {"crossref",  &crossref_opt_list, "all", "none", NOT_A_CHECK,
     crossref_option, NULL, 
     "cross-ref printing options"},
  {"f77",	&f77_warn_list,	"all", "none", IS_A_CHECK,
     f77_warn_option, NULL,
     "warn about non-F77 extensions"},
  {"f90",	&f90_warn_list,	"all", "none", IS_A_CHECK,
     f90_warn_option, NULL,
     "warn about non-F90 syntax"},
  {"f95",	&f95_warn_list,	"all", "none", IS_A_CHECK,
     f95_warn_option, NULL,
     "warn about non-F95 syntax"},
  {"identifier-chars", &idletter_list, DEF_IDLETTER_LIST, "", NOT_A_CHECK,
     (WarnOptionList *)NULL, NULL,
     "non-alphabetic chars allowed in identifiers"},
#ifdef ALLOW_INCLUDE
  {"include",	&include_path,  (char *)NULL, (char *)NULL, NOT_A_CHECK,
     (WarnOptionList *)NULL, NULL,
     "include-file directory"},
#endif
#ifndef STANDARD_INTRINSICS
  {"intrinsic", &intrinsic_option_list, "all", "none", NOT_A_CHECK,
     intrinsic_option, intrinsic_numeric_option,
     "specify intrinsic function options"},
#endif
			/* makedcls: turnon="declarations" instead of "all" */
  {"makedcls",  &makedcl_warn_list, "declarations", "none", NOT_A_CHECK,
     makedcl_warn_option, makedcl_numeric_option,
    "make type declaration statements"},
#ifdef HTML_OPTION
				/* mkhtml similar to makedcls */
  {"mkhtml",	&mkhtml_warn_list, "documents","none",NOT_A_CHECK,
     mkhtml_warn_option, mkhtml_numeric_option,
    "create html documents"},
#endif
  {"output",	&out_fname,	(char *)NULL, (char *)NULL, NOT_A_CHECK,
     (WarnOptionList *)NULL, NULL,
     "output file name"},
  {"portability",&port_warn_list,"all", "none", IS_A_CHECK,
     port_warn_option, NULL,
     "warn about portability problems"},
  {"pretty",	&pretty_warn_list,"all", "none", IS_A_CHECK,
     pretty_warn_option, NULL,
     "warn about deceiving appearances"},
  {"project",	&project_warn_list,"all", "none", NOT_A_CHECK,
     project_warn_option, NULL,
     "create project file"},
  {"source",	&source_form_list,"all", "none", NOT_A_CHECK,
     source_form_option, source_numeric_option,
     "select source format options"},
  {"style",	&stylecheck_warn_list, "all", "none", IS_A_CHECK,
     stylecheck_warn_option, NULL,
     "catch violations of structured style"},
  {"truncation",&trunc_warn_list,"all", "none", IS_A_CHECK,
     trunc_warn_option, NULL,
     "check for truncation pitfalls"},
  {"usage",	&usage_warn_list,"all", "none", IS_A_CHECK,
     usage_warn_option, usage_numeric_option,
     "warn about variable and common block usage problems"},
};

/*	get_env_options picks up any options defined in the
	environment.  A switch or setting is defined according to
	the value of an environment variable whose name is the switch
	or setting name (uppercased), prefixed by the string
	ENV_PREFIX (e.g.  FTNCHEK_).  For settings and strsettings,
	the value of the environment variable gives the value to be
	used.  For switches, the environment variable is set to "0" or
	"NO" to turn the switch off, or to any other value (including
	null) to turn it on.
*/

void
get_env_options(VOID)
{
		/* Size of env_option_name must be at least 1 +
                   strlen(ENV_PREFIX) + max over i of strlen of
                   switchopt[i].name, setting[i].name,
                   strsetting[i].name.
		*/
#define ENV_OPTION_NAME_LEN 32
	char env_option_name[ENV_OPTION_NAME_LEN];
	char *value;
	unsigned i, checklen;
	
			/* The following code checks size of
                           ENV_OPTION_NAME_LEN, which may become too small
                           as option names are added. This could be
                           commented out in released code, but it's a
                           minor overhead for insurance.
			*/
	checklen = 0;
	for(i=0; i<NUM_SWITCHES; i++) {
	  checklen = MAX(checklen,strlen(switchopt[i].name));
	}
	for(i=0; i<NUM_SETTINGS; i++) {
	  checklen = MAX(checklen,strlen(setting[i].name));
	}
	for(i=0; i<NUM_STRSETTINGS; i++) {
	  checklen = MAX(checklen,strlen(strsetting[i].name));
	}
	checklen += sizeof(ENV_PREFIX)+1;
	if(ENV_OPTION_NAME_LEN < checklen) {
	  fprintf(stderr,"\nOops -- ENV_OPTION_NAME_LEN=%d too small: make it %d\n",
		  ENV_OPTION_NAME_LEN, checklen);
	  exit(1);
	}


				/* OK, now we get down to it. */

	for(i=0; i<NUM_SWITCHES; i++) {
			/* Construct the env variable name for switch i */
	    make_env_name( env_option_name, switchopt[i].name);

			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {
		*(switchopt[i].switchflag) =
			!(strcmp(value,"0")==0 || strcmp(value,"NO")==0 );
	    }

	}

	for(i=0; i<NUM_SETTINGS; i++) {
			/* Construct the env variable name for setting i */
	    make_env_name( env_option_name, setting[i].name);
			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {
		if(read_setting(value, setting[i].setvalue, setting[i].name,
				setting[i].minlimit, setting[i].maxlimit,
				setting[i].turnon,
				setting[i].turnoff,
				setting[i].min_default_value,
				setting[i].max_default_value) != 0) {
		  (void)fflush(list_fd);
		  (void)fprintf(stderr,"Env setting garbled: %s=%s: ignored\n",
				env_option_name,value);
		}
	    }
	}


	for(i=0; i<NUM_STRSETTINGS; i++) {
			/* Construct the env variable name for setting i */
	    make_env_name( env_option_name, strsetting[i].name);
			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {

				/* setenv nothing or "1" or "YES" --> turnon*/
	      if(value[0] == '\0'
		 || strncasecmp(value,"1",strlen(value)) == 0
		 || strncasecmp(value,"yes",strlen(value)) == 0
		 ) {
		*(strsetting[i].strvalue) = strsetting[i].turnon;
	      }
	      else if(strncasecmp(value,"no",strlen(value)) == 0) {
		*(strsetting[i].strvalue) = strsetting[i].turnoff;
	      }
	      else {		/* Otherwise use the given value */
	        *(strsetting[i].strvalue) = value;
	      }

	      if( *(strsetting[i].strvalue) == (char *)NULL ) {
		(void)fflush(list_fd);
		(void)fprintf(stderr,
			 "Environment variable %s needs string value: ignored\n",
			 env_option_name);
	      }
	      else {
		update_str_options(&strsetting[i]);
	      }
	    }
	}
}

		/* Routine to concatenate ENV_PREFIX onto option name
		   and uppercase the result.
		*/
PRIVATE void
#if HAVE_STDC
make_env_name(char *env_name, char *option_name)
#else /* K&R style */
make_env_name( env_name, option_name)
	char *env_name, *option_name;
#endif /* HAVE_STDC */
{
    int i,c;

    (void)strcat(strcpy(env_name,ENV_PREFIX),option_name);
    for(i=sizeof(ENV_PREFIX)-1; (c=env_name[i]) != '\0'; i++) {
	if( islower(c) )
	    env_name[i] = toupper(c);
    }
}

		/* get_rc_options picks up options from an "rc" file.
		 */
void
get_rc_options(VOID)
{
  FILE *rc_fp;
  char rc_option_string[MAX_RC_LINE];
  int i;

  rc_option_string[0] = '-';

  if( (rc_fp = find_rc()) != (FILE *)NULL ) {
    for(;;) {
      if( fgets(rc_option_string+1,sizeof(rc_option_string)-1,rc_fp)
	 == (char *)NULL)
	break;
				/* Terminate line at start of comment.
				   This also changes final \n to \0. */
      for(i=1; rc_option_string[i] != '\0'; i++) {
	if(rc_option_string[i] == RC_COMMENT_CHAR ||
	   isspace(rc_option_string[i])) {
	  rc_option_string[i] = '\0';
	  break;
	}
      }
      if(i==1)			/* Skip blank line */
	continue;

      set_option(rc_option_string,"startup file");
    }
  }
}

		/* find_rc locates the "rc" file. */
PRIVATE FILE *
find_rc(VOID)
{
  FILE *fp;
  char *fname;
  char *homedir=getenv("HOME");

			/* Allocate enough space to hold rc file name.
			   Now you see why so many apps have buffer-overrun
			   bugs. */
  if( (fname = (char *)malloc(MAX(sizeof(UNIX_RC_FILE),sizeof(NONUNIX_RC_FILE)) +
		      (homedir!=NULL?strlen(homedir):
#ifdef SPECIAL_HOMEDIR
			strlen(SPECIAL_HOMEDIR)
#else
			0
#endif
		      )
#ifdef UNIX
		        +1	/* for the "/" */
#endif
		      )) == (char *)NULL ) {
    (void)fflush(list_fd);
    (void)fprintf(stderr,"\nCannot allocate memory for init file path");
    return (FILE *)NULL;
  }

			/* Look first for file in local directory */
  (void)strcpy(fname,UNIX_RC_FILE);
  if( (fp=fopen(fname,"r")) == (FILE *)NULL) {

			/* Look for alternate name in local directory */
    (void)strcpy(fname,NONUNIX_RC_FILE);
    if( (fp=fopen(fname,"r")) == (FILE *)NULL) {


			/* Allow local option of special home directory
			   for non-unix (usually VMS) systems. */
#ifdef SPECIAL_HOMEDIR
      if(homedir == (char *)NULL) {
	homedir = SPECIAL_HOMEDIR;
      }
#endif
			/* If not found, look in home directory */
      if(homedir != (char *)NULL) {
	(void)strcpy(fname,homedir);
#ifdef UNIX
	(void)strcat(fname,"/");
#endif
	(void)strcat(fname,UNIX_RC_FILE);
	
	if( (fp=fopen(fname,"r")) == (FILE *)NULL) {


			/* If look for alternate name in home directory */
	  (void)strcpy(fname,homedir);
#ifdef UNIX
	  (void)strcat(fname,"/");
#endif
	  (void)strcat(fname,NONUNIX_RC_FILE);
	  if( (fp=fopen(fname,"r")) == (FILE *)NULL) {
				/* no more alternatives */
	  }
	}
      }/* end if homedir != NULL */
    }
  }

  free(fname);
  return fp;
}


	/* set_option processes an option from command line.  Argument
	   s is the option string. First look if s starts with "no" or
	   "no-", and if so, check if the rest matches a boolean switch name
	   from list in switchopt[].  If it matches, corresponding
	   flag is set to FALSE.  If no match, then s is compared to
	   the same switch names without the "no", and if match is
	   found, corresponding flag is set to TRUE.  Finally, special
	   flags are handled.  If still no match, an error message is
	   generated.  */

void
#if HAVE_STDC
set_option(char *s, const char *where)
	        		/* Option to interpret, including initial - */
	            		/* String to identify cmd line vs rc file */
#else /* K&R style */
set_option(s,where)
	char *s,		/* Option to interpret, including initial - */
	     *where;		/* String to identify cmd line vs rc file */
#endif /* HAVE_STDC */
{
	unsigned i;
	int offset, orig_offset;
	int prefix_no=FALSE;

		/* look for noswitch flags first since otherwise
		   an option starting with no might take precedence.
		 */
	offset=1;	/* offset is no. of chars from s[0] to switch name */

				/* Allow either "-" or "--" prefix */
	if(
#ifdef OPTION_PREFIX_SLASH
	    s[0] == '-' &&	/* if / allowed, make sure this is -- not /- */
#endif
	    s[1] == '-' ) {
	    ++offset;
	}
	orig_offset = offset;
	if( strncmp(s+offset,"no",2) == 0 ) {
	  prefix_no = TRUE;
	  offset += 2;
	  if( s[offset] == '-' )	/* Allow "no" or "no-" */
	    offset += 1;
	}

	if( prefix_no ) {	/* "no" found */
	    for(i=0; i<NUM_SWITCHES; i++) {
		if( strncmp(s+offset,switchopt[i].name,OPT_MATCH_LEN) == 0) {
		    *(switchopt[i].switchflag) = FALSE;
		    return;
		}
	    }

		/* -noswitch not found: look for -nosetting flag */
	    for(i=0; i<NUM_SETTINGS; i++) {
		if( strncmp(s+offset,setting[i].name,OPT_MATCH_LEN) == 0) {
		    *(setting[i].setvalue) = setting[i].turnoff;
		    return;
		}
	    }
	}

				/* Next look for switches without "no" */
	for(i=0; i<NUM_SWITCHES; i++) {
	    if( strncmp(s+orig_offset,switchopt[i].name,OPT_MATCH_LEN) == 0) {
		*(switchopt[i].switchflag) = TRUE;
		return;
	    }
	}

		/* Handle settings of form "-opt=number" */
	for(i=0; i<NUM_SETTINGS; i++) {
	    if( strncmp(s+orig_offset,setting[i].name,OPT_MATCH_LEN) == 0) {
		char *numstr;

		numstr = s + offset + OPT_MATCH_LEN;
		while(++numstr, ! END_OF_OPT(*numstr) )
		{
		    if((*numstr == '=') || (*numstr == ':'))
		    {			/* Find the assignment operator */
			numstr++;
			break;
		    }
		}
		if(read_setting(numstr, setting[i].setvalue, setting[i].name,
				setting[i].minlimit, setting[i].maxlimit,
				setting[i].turnoff,
				setting[i].turnon,
				setting[i].min_default_value,
				setting[i].max_default_value) != 0) {
		  (void)fflush(list_fd);
		  (void)fprintf(stderr,"Setting garbled: %s: ignored\n",s);
		}
		return;
	    }
	}


		/* Handle settings of form "-opt=string" */
	for(i=0; i<NUM_STRSETTINGS; i++) {
	    int is_a_turnoff;
	    char *strstart;

				/* First look for setting prefixed by "no"
				   if it allows turnon/turnoff. */
	    if( strsetting[i].turnoff != (char *)NULL &&
	       prefix_no &&
	       strncmp(s+offset,strsetting[i].name,OPT_MATCH_LEN) == 0) {
	      is_a_turnoff = TRUE;
	      strstart = s + offset + OPT_MATCH_LEN;
	    }
	    else if( strncmp(s+orig_offset,strsetting[i].name,OPT_MATCH_LEN) == 0) {
	      is_a_turnoff = FALSE;
	      strstart = s + orig_offset + OPT_MATCH_LEN;
	    }
	    else {
		continue;	/* Doesn't match -nooption or -option: skip */
	    }
	    {
		int numchars;

		while( *strstart != '=' && *strstart != ':'
		      && ! END_OF_OPT(*strstart) )
			strstart++;	/* Find the = sign */
		if( END_OF_OPT(*strstart) ) {
				/* no = sign: use turnon/turnoff */
		  if(is_a_turnoff)
		    *(strsetting[i].strvalue) = strsetting[i].turnoff;
		  else
		    *(strsetting[i].strvalue) = strsetting[i].turnon;
		}
		else {		/* = sign found: use it but forbid -no form */
		    if(is_a_turnoff) {
		      (void)fflush(list_fd);
		      (void)fprintf(stderr,
			      "No string setting allowed for %s: ignored\n",s);
		      return;
		    }
		    ++strstart;	/* skip past the "=" */
				/* In VMS,MSDOS worlds, user might not leave
				   blank space between options.  If string
				   is followed by '/', must make a properly
				   terminated copy.  In any case, make a
				   copy in case this option comes from
				   the rc file. */
		    for(numchars=0;!END_OF_OPT(strstart[numchars]);numchars++)
		      continue;

		    *(strsetting[i].strvalue) = (char *)malloc(numchars+1);
		    (void)strncpy( *(strsetting[i].strvalue),
			       strstart,numchars);
		    (*(strsetting[i].strvalue))[numchars] = '\0';
		}

			/* Handle actions needed after new strsetting
			   is read. If it was a turn-on where turnon is
			   NULL, give a warning. */
		if( *(strsetting[i].strvalue) == (char *)NULL ) {
		  (void)fflush(list_fd);
		  (void)fprintf(stderr,
				"String setting missing: %s: ignored\n",s);
		}
		else {
		  update_str_options(&strsetting[i]);
		}

		return;
	    }

	}
		/* No match found: issue error message */

	(void)fflush(list_fd);
	(void)fprintf(stderr,"\nUnknown %s switch: %s\n",where,s);
}


	/* Routine to read integer setting from string s and check if valid */

PRIVATE int
#if HAVE_STDC
read_setting(char *s, int *setvalue, char *name, int minlimit, int maxlimit, int turnoff, int turnon, int min_default_value, int max_default_value)
#else /* K&R style */
read_setting(s, setvalue, name, minlimit, maxlimit, turnoff, turnon,
	     min_default_value,
	     max_default_value)
	char *s;
	int *setvalue;
	char *name;
	int minlimit, maxlimit,
	     turnon, turnoff,
	     min_default_value, max_default_value;
#endif /* HAVE_STDC */
{
	int given_val;

	if(strcmp(s,"NO")==0) {	/* -setting=no */
	  *(setvalue) = turnoff;
	}
	else if(END_OF_OPT(*s)) { /* -setting */
	  *(setvalue) = turnon;
	}
	else if(sscanf(s,"%d", &given_val) == 0) {
	    return -1;	/* error return: garbled setting */
	}
	else {		/* If outside limits, set to default */
	    int Ok=TRUE;
	    if(given_val < minlimit) {
		given_val = min_default_value;
		Ok = FALSE;
	    }
	    else if(given_val > maxlimit) {
		given_val = max_default_value;
		Ok = FALSE;
	    }

	    if(! Ok ) {
	        (void)fflush(list_fd);
		(void)fprintf(stderr,"\nSetting: %s",name);
		(void)fprintf(stderr," outside limits %d to %d",
				minlimit,maxlimit);
		(void)fprintf(stderr,": set to default %d\n",given_val);
	    }

	    *(setvalue) = given_val;
	}
	return 0;
}

			/* Handle actions needed to update things after
			   getting a non-null strsetting option.
			 */
PRIVATE void
#if HAVE_STDC
update_str_options(StrsettingList *strset)
#else /* K&R style */
update_str_options(strset)
  StrsettingList *strset;
#endif /* HAVE_STDC */
{

			/* Handle necessary action for  -out=listfile */
  if(strset->strvalue == &out_fname)
    must_open_outfile = TRUE;

				/* Update include path */
#ifdef ALLOW_INCLUDE
  if(strset->strvalue == &include_path) {
    append_include_path(include_path);
  }
#endif

				/* Handle warnings like -f77=list */
  if(strset->option_list != (WarnOptionList *)NULL) {
    char *s = *(strset->strvalue);
    int numvalue;
				/* Allow old-fashioned -flag=num for some */
    if( strset->numeric_form_handler != NULL &&
	(numvalue = str_to_num(s)) >= 0 ) {
      (*(strset->numeric_form_handler))(numvalue,strset->name);
    }
    else {
      process_warn_string(s, strset);
    }
  }
}

			/* Routine to return -1 if string is not all
                           digits and not null; otherwise returns
                           integer value of string. */
PRIVATE int
#if HAVE_STDC
str_to_num(char *s)
#else
str_to_num(s)
     char *s;
#endif
{
  int value=0;

  if( s == NULL || *s == '\0' )
    return -1;

  while( *s != '\0' ) {
    if(! isdigit(*s) )
      return -1;
    else
      value = value*10 + ((*s)-'0');
    s++;
  }
  return value;
}

				/* Process list of warn options. */
PRIVATE void
#if HAVE_STDC
process_warn_string(char *warn_string, StrsettingList *s)
#else /* K&R style */
process_warn_string( warn_string, s )
     char *warn_string;		/* Names of options to set */
     StrsettingList *s;    /* Warning-list option */
#endif /* HAVE_STDC */
{
  int i,c;
  char opt_buf[MAX_OPT_LEN+1];

  WarnOptionList *warn_option = s->option_list;

  if(strcmp(warn_string,"help") == 0) { /* Print warning help screen */
    list_warn_options(s);
    return;
  }
  else {
				/* Loop on warn options in string */
    while(!END_OF_OPT(*warn_string)) {
				/* Copy next warn option into buffer */
      for(i=0; !END_OF_OPT(*warn_string); ) {
	c = *warn_string++;
	if(c == ',' || c == ':') /* quit when reach next warn option */
	  break;
	if(i<MAX_OPT_LEN)
	  opt_buf[i++] = c;
      }
      opt_buf[i] = '\0';

      set_warn_option(opt_buf, warn_option );
    }
  }
  return;
}

			/* Routine to print list of warning options */
PRIVATE void
#if HAVE_STDC
list_warn_options(StrsettingList *s)
#else /* K&R style */
list_warn_options(s)
     StrsettingList *s; /* warning list item */
#endif /* HAVE_STDC */
{
  int i;
  WarnOptionList *warn_option = s->option_list;

  ++actioncount;	/* Treat as an action so if no files, quit */

  (void)fprintf(list_fd,"\n%s Options:",warn_option[0].explanation);
  for(i=1; warn_option[i].name != (char *)NULL; i++) {
    (void)fprintf(list_fd,"\n  %s [%s]: %s",
	    warn_option[i].name,
	    *(warn_option[i].flag)? "yes" : "no",
	    warn_option[i].explanation);
  }
  (void)fprintf(list_fd,"\nPrefix option name with no- to turn off option");
  if(s->turnon != (char *)NULL) {
    (void)fprintf(list_fd,"\nIf no options given, equivalent to %c%s=%s",
	    OPT_PREFIX, s->name, s->turnon);
  }
  (void)fprintf(list_fd,"\nSpecial keywords:");
  (void)fprintf(list_fd,"\n  %s: %s","help","Print this list");
  (void)fprintf(list_fd,"\n  %s: %s","all","Set all options");
  (void)fprintf(list_fd,"\n  %s: %s","none","Clear all options");
  (void)fprintf(list_fd,"\n");
}

			/* Routine to set warning options to given values */
PRIVATE void
#if HAVE_STDC
set_warn_option(char *s, WarnOptionList *warn_option)
#else /* K&R style */
set_warn_option(s, warn_option )
     char *s;
     WarnOptionList *warn_option;
#endif /* HAVE_STDC */
{
  int i, matchlen, offset;
  int value;

  if(s == NULL)		/* This happens when -nocheck handles -intrinsic */
    return;

			/* Special keyword "all": set all options on */
  if(strcmp(s,"all") == 0) {
	for(i=1; warn_option[i].name != (char *)NULL; i++)
	  set_warn_option_value(warn_option[i].flag,TRUE);
	return;
  }
			/* Special keyword "none": set all options off */
  else if(strcmp(s,"none") == 0 ) {
	for(i=1; warn_option[i].name != (char *)NULL; i++)
	  set_warn_option_value(warn_option[i].flag,FALSE);
	return;
  }
  else {
				/* Look for "no-" prefix on option name */
    if(strncmp(s,"no-",strlen("no-")) == 0) {
      offset = strlen("no-");
      value = FALSE;
    }
    else {
      offset = 0;
      value = TRUE;
    }
				/* See if the given option has a wildcard */
    if( strchr(s,'*') == NULL ) {

				/* No wildcard: go thru list to find a
				   match at minimum nonambiguous length.
				*/
     for(i=1,matchlen=1; warn_option[i].name != (char *)NULL; i++) {
			/* Look for a match at current matchlen, then 
			  if found see if unique.  List must have names
			  with matching prefixes adjacent. */
      while(strncmp(s+offset,warn_option[i].name,matchlen) == 0) {
	if(warn_option[i+1].name == (char *)NULL ||
	   strncmp(s+offset,warn_option[i+1].name,matchlen) != 0) {
	  set_warn_option_value(warn_option[i].flag,value);
	  return;
	}
	else {
	  if(   s[offset+matchlen] == '\0'
	     || warn_option[i].name[matchlen] == '\0') {
	    (void)fflush(list_fd);
	    (void)fprintf(stderr,
		   "\nAmbiguous %s Option: %s: ignored\n",
			  warn_option[0].explanation,s);
	    return;
	  }
	  ++matchlen;
	}
      }
     }
    }
    else {
				/* Wildcard in pattern: find all matches. */
     int matches=0;
     for(i=1; warn_option[i].name != (char *)NULL; i++) {
       if( wildcard_match(s+offset,warn_option[i].name) == 0 ) {
	 ++matches;
	 set_warn_option_value(warn_option[i].flag,value);
       }
     }
				/* If nothing matched, drop out for warning */
     if(matches > 0 ) {
       return;
     }
    }
  }
  (void)fflush(list_fd);
  (void)fprintf(stderr,"\nNo Such %s Option: %s: ignored\n",
			  warn_option[0].explanation,s);
  return;
}


		/* set_warn_option_value sets values of warnlist-style flags,
		   and also handles special cases of mutually exclusive
		   flags and suchlike.
		 */
PRIVATE void 
#if HAVE_STDC
set_warn_option_value(int *flag, int value)
#else /* K&R style */
set_warn_option_value(flag, value )
     int *flag;
     int value;
#endif /* HAVE_STDC */
{
  /* handle mutual exclusions here */

  if( value ) {
      if ( flag == &print_call_tree
	|| flag == &print_ref_list
#ifdef VCG_SUPPORT
	|| flag == &print_vcg_list
#endif
	  ) {

			/* Can select only one of -call=tree,ref,vcg */
	  static int *calltree_mutual_exc_flags[]={
	      &print_call_tree,
	      &print_ref_list,
#ifdef VCG_SUPPORT
	      &print_vcg_list,
#endif
	      (int *)NULL
	  };

	  mutual_exclude(calltree_option,"calltree",
		   flag, calltree_mutual_exc_flags);
      }
      else {
	  if( flag == &source_fixed_form
	   || flag == &source_free_form ) {

			/* Cannot set -source=fixed,free */
	      static int *source_form_mutual_exc_flags[]={
		  &source_fixed_form,
		  &source_free_form,
		  (int *)NULL
	      };
	      mutual_exclude(source_form_option,"source",
			     flag,source_form_mutual_exc_flags);
	  }

	  if( flag == &source_free_form
	   || flag == &source_dec_tab ) {

			/* Cannot have -source=dec-tabs with -source=free */
	      static int *dec_tab_mutual_exc_flags[]={
		  &source_dec_tab,
		  &source_free_form,
		  (int *)NULL
	      };

	      mutual_exclude(source_form_option,"source",
			     flag,dec_tab_mutual_exc_flags);
	  }
      }
  }
				/* Here we actually set the value. */
  *flag = value;
}

PRIVATE void
#if HAVE_STDC
mutual_exclude(  WarnOptionList wList[], const char *opt_name,
		      int *thisflag, int *otherflags[] )
#else
mutual_exclude( wList, opt_name,
		      thisflag, otherflags )
     WarnOptionList wList[];
     char *opt_name;
     int *thisflag;
     int *otherflags[];
#endif
{
  int i,j, thisflag_index= -1;
				/* Find thisflag in the list */
  for(i=0; wList[i].name != NULL; i++) {
    if(wList[i].flag == thisflag) {
      thisflag_index = i;
      break;
    }
  }
  if( thisflag_index < 0 ) {
    oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,"mutual_exclude routine");
  }
  else {
    for(j=0; otherflags[j] != NULL; j++) {

      if( otherflags[j] == thisflag ) /* thisflag cannot conflict with self */
	continue;

      if( *(otherflags[j]) ) {	/* exclusion conflict found: trace it */
	for(i=0; wList[i].name != NULL; i++) {
	  if(wList[i].flag == otherflags[j]) {
	    (void)fprintf(stderr,
	       "\nWarning: %c%s option %s overrides previous option %s\n",
#ifdef OPTION_PREFIX_SLASH
		    '/',
#else
		    '-',
#endif
		    opt_name,
		    wList[thisflag_index].name,wList[i].name);
	    break;
	  }
	}
	*(otherflags[j]) = FALSE; /* turn off the conflicting option */
      }
    }
  }
}


			/* The next few routines implement the
                          "grandfathering" of those settings that
                          were changed from numeric to warning-option
                          string form, so the numeric form will still
			  be acceptable.
			*/

PRIVATE void
#if HAVE_STDC
argcheck_numeric_option( int value, char *setting_name )
#else
argcheck_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{
  if( value < 0 || value > 3) {
    numeric_option_error(setting_name,0,3);
    return;
  }
  argcheck_argnumber = ((value & 01) != 0);
  argcheck_arrayness =  argcheck_argtype = argcheck_functype = ((value & 02) != 0);
}

PRIVATE void
#if HAVE_STDC
arraycheck_numeric_option( int value, char *setting_name )
#else
arraycheck_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{
  if( value < 0 || value > 3) {
    numeric_option_error(setting_name,0,3);
    return;
  }
  arraycheck_dims = ((value & 01) != 0);
  arraycheck_size = ((value & 02) != 0);
}

PRIVATE void
#if HAVE_STDC
calltree_numeric_option( int value, char *setting_name )
#else
calltree_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{
  int format;
  if( value < 0 || value > 15) {
    numeric_option_error(setting_name,0,15);
    return;
  }

  format = (value & 0x3); /* Low-order two bits => output format */
			/* if no format specified, tree is default
			   provided number is nonzero. */
  print_call_tree = (format == 1) || (format == 0 && value != 0);
  print_ref_list  = (format == 2);
#ifdef VCG_SUPPORT
  print_vcg_list  = (format == 3);
#endif

  call_tree_prune = ((value & 0x4) == 0); /* Include 4 for no-prune */
  call_tree_sort  = ((value & 0x8) == 0); /* Include 8 for no-sort */
}

PRIVATE void
#if HAVE_STDC
comcheck_numeric_option( int value, char *setting_name )
#else
comcheck_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{
  if( value < 0 || value > 3) {
    numeric_option_error(setting_name,0,3);
    return;
  }
  comcheck_type     = (value >= 1);
  comcheck_length   = (value >= 2);
  comcheck_dims = comcheck_by_name  = (value == 3);
/*comcheck_volatile was controlled by -volatile flag, not here. */
}


PRIVATE void
#if HAVE_STDC
intrinsic_numeric_option( int value, char *setting_name )
#else
intrinsic_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{

  int intrins_set = value % 10;
  int rand_form = (value/10) % 10;
  int iargc_form = (value/100) % 10;

  if( value < 0 || intrins_set > 3 || rand_form > 2 || iargc_form > 2) {
    numeric_option_error(setting_name,0,223);
    return;
  }

  intrinsic_set_extra = (intrins_set != 0);

  intrinsic_set_unix = (intrins_set == 2);

  intrinsic_set_vms = (intrins_set == 3);

  intrinsic_rand_no_argument = (rand_form == 0 || rand_form == 2);

  intrinsic_rand_one_argument = (rand_form == 1 || rand_form == 2);

  intrinsic_iargc_no_argument = (iargc_form == 0 || iargc_form == 2);

  intrinsic_iargc_one_argument = (iargc_form == 1 || iargc_form == 2);

}

PRIVATE void
#if HAVE_STDC
makedcl_numeric_option( int value, char *setting_name )
#else
makedcl_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{
  /* makedcls options, old style = sum of numbers as spelled out below */
  if( value < 0 || value > 0x7ff ) {
    numeric_option_error(setting_name,0,0x7ff);
    return;
  }

 dcl_declarations			= (value != 0);
 dcl_only_undeclared			= ((value & 0x0002) != 0);
 dcl_compact				= ((value & 0x0004) != 0);
 dcl_use_continuations			= ((value & 0x0008) != 0);
 dcl_keywords_lowercase			= ((value & 0x0010) != 0);
 dcl_vars_and_consts_lowercase		= ((value & 0x0020) != 0);
 dcl_excl_sftran3_internal_vars		= ((value & 0x0040) != 0);
 dcl_asterisk_comment_character		= ((value & 0x0080) != 0);
 dcl_lowercase_comment_character	= ((value & 0x0100) != 0);
 dcl_no_array_dimensions		= ((value & 0x0200) != 0);
 dcl_free_form				= ((value & 0x0400) != 0);
}

PRIVATE void
#if HAVE_STDC
mkhtml_numeric_option( int value, char *setting_name )
#else
mkhtml_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{
  /* mkhtml options, old style = sum of numbers as spelled out below */
  if( value < 0 || value > 0x7ff ) {
    numeric_option_error(setting_name,0,0x7ff);
    return;
  }

 html_documents				= (value != 0);
 html_only_undeclared			= ((value & 0x0002) != 0);
 html_compact				= ((value & 0x0004) != 0);
 html_use_continuations			= ((value & 0x0008) != 0);
 html_keywords_lowercase		= ((value & 0x0010) != 0);
 html_vars_and_consts_lowercase		= ((value & 0x0020) != 0);
 html_excl_sftran3_internal_vars	= ((value & 0x0040) != 0);
 html_no_array_dimensions		= ((value & 0x0200) != 0);
 html_free_form				= ((value & 0x0400) != 0);
}

PRIVATE void
#if HAVE_STDC
source_numeric_option( int value, char *setting_name )
#else
source_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{
  /* source format options, old style = sum of:
     1=DEC Fortran tab-format
     2=VMS-style INCLUDE statement
     4=UNIX-style backslash escape char
     8=implicit typing of standard-form PARAMETERs
    16=standard typing of DEC-Fortran-form PARAMETERs
  */

  if( value < 0 || value > 15 ) {
    numeric_option_error(setting_name,0,15);
    return;
  }
  source_dec_tab = ((value & 1) != 0);
  source_vms_include = ((value & 2) != 0);
  source_unix_backslash = ((value & 4) != 0);
  source_param_implicit = ((value & 8) != 0);
  source_dec_param_std_type = ((value & 0x10) != 0);
}


PRIVATE void
#if HAVE_STDC
usage_numeric_option( int value, char *setting_name )
#else
usage_numeric_option( value, setting_name )
     int value;
     char *setting_name;
#endif
{

  int var_usage = value % 10;
  int com_usage = (value/10) % 10;
  int ext_usage = (value/100) % 10;

  if( value < 0 || var_usage > 3 || com_usage > 3 || ext_usage > 3 ) {
    numeric_option_error(setting_name,0,333);
    return;
  }
			/* Set flag variables according to the old rules:
			   ones digit = vars, tens = com, hundreds = ext
			   1 = used-not-defined, 2 = unused, 3 = all

			   Note: the variable com-block-volatile is not
			   touched here.
			*/

  usage_var_uninitialized = usage_arg_modified =
    usage_arg_alias_modified = usage_array_alias_modified =
    usage_arg_common_modified = usage_array_common_modified = ((var_usage & 0x1)!=0);

  usage_var_set_unused = usage_var_unused = usage_arg_unused = ((var_usage & 0x2)!=0);

  usage_com_var_uninitialized = ((com_usage & 0x1)!=0);

  usage_com_var_set_unused = usage_com_block_unused =
    usage_com_var_unused = ((com_usage & 0x2)!=0);

  usage_ext_multiply_defined = usage_ext_declared_only =
    usage_ext_undefined = ((ext_usage & 0x1)!=0);

  usage_ext_unused = ((ext_usage & 0x2)!=0);

}

PRIVATE void
#if HAVE_STDC
numeric_option_error( char *setting_name, int minlimit, int maxlimit )
#else
numeric_option_error( setting_name, minlimit, maxlimit )
     char *setting_name;
     int minlimit;
     int maxlimit;
#endif
{
    (void)fflush(list_fd);
    (void)fprintf(stderr,"\nSetting: %s outside limits %d to %d",
		  setting_name,minlimit,maxlimit);
    (void)fprintf(stderr,": setting ignored\n");
}


	/* Routine to turn off all switches and numeric settings except
	   -word and -wrap.  The effect is as if -no had been given
	   for each switch and setting.  Useful when other features
	   like calltree are being used and checking is not needed.
	*/
void turn_off_checks(VOID)
{
	unsigned i;

				/* Put all switches to FALSE */
	for(i=0; i<NUM_SWITCHES; i++) {
	  if(switchopt[i].isacheck == IS_A_CHECK)
	    *(switchopt[i].switchflag) = FALSE;
	}

				/* Put all settings to turnoff value */
	for(i=0; i<NUM_SETTINGS; i++) {
	  if(setting[i].isacheck == IS_A_CHECK)
	    *(setting[i].setvalue) = setting[i].turnoff;
	}

				/* Turn off warn lists */
	for(i=0; i<NUM_STRSETTINGS; i++) {
	  if( strsetting[i].isacheck == IS_A_CHECK
	    && strsetting[i].option_list != (WarnOptionList *)NULL ) {
	    set_warn_option( strsetting[i].turnoff,
			      strsetting[i].option_list);
				/* Set strvalue so -help reports correctly */
	    *(strsetting[i].strvalue) = strsetting[i].turnoff;
	  }
	}
				/* Turn off checks without own options */
	misc_warn = FALSE;

}

				/* Routine to compare a string str against
				   a pattern pat, which can contain '*' to
				   match any character string.  Returns 0
				   (like strcmp) if match, 1 if not.
				*/
PRIVATE int
#if HAVE_STDC
wildcard_match(char *pat, char *str)
#else /* K&R style */
wildcard_match(pat, str)
  char *pat;
  char *str;
#endif /* HAVE_STDC */
{
  register char *s, *p;			/* pointers that run thru each */
  register int sc, pc;			/* current str char and pat char */
  s = str;
  p = pat;
  for( pc = *p++, sc = *s++; pc != '\0'; pc = *p++, sc = *s++ ) {
    if( pc != '*' ) {
      if(sc != pc) {
	return 1;		/* mismatch found */
      }
    }
    else {			/* wildcard found */
      do {
	pc = *p++;
      } while( pc == '*' );	/* skip past the wildcard */

      if(pc == '\0') {
	return 0;		/* pattern ends with '*' => match */
      }
      else {
				/* Try to match rest of patt with str starting
				   at some point from here to end. We do a
				   small optimization to avoid the recursive
				   call in many cases. */
	while(sc != '\0') { 
	  if( sc == pc && wildcard_match(p,s) == 0 )
	    return 0;
	  sc = *s++;
	}
	return 1;		/* No match found */
      }
    }
  }
  return (sc != '\0');		/* End of pattern: OK if end of string */
}


void
#if HAVE_STDC
list_options(FILE *fd)/* List all commandline options, strsettings, and settings */
#else /* K&R style */
list_options(fd)/* List all commandline options, strsettings, and settings */
     FILE *fd;
#endif /* HAVE_STDC */
{
	unsigned i;

			/* Print the copyright notice */
	(void)fprintf(fd,"\n%s",COPYRIGHT_DATE);
	(void)fprintf(fd,"\n%s\n",COPYRIGHT_NOTICE);

		/* Note: Headings say "default" but to be accurate they
		   should say "current value".  This would be confusing. */
	(void)fprintf(fd,"\nCommandline options [default]:");
	for(i=0; i<NUM_SWITCHES; i++) {

	  if( !debug_latest &&
	     strncmp(switchopt[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug switches unless debug mode */

	  (void)fprintf(fd,"\n    %c[no]%s",OPT_PREFIX,switchopt[i].name);
	  (void)fprintf(fd," [%s]",*(switchopt[i].switchflag)? "yes": "no");
	  (void)fprintf(fd,": %s",switchopt[i].explanation);
	}
		/* String settings follow switches w/o their own heading */
	for(i=0; i<NUM_STRSETTINGS; i++) {
	  if( !debug_latest &&
	     strncmp(strsetting[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug settings unless debug mode */

	  (void)fprintf(fd,"\n    %c%s=str ",OPT_PREFIX,strsetting[i].name);
			/* If strvalue has been given, list it.  Otherwise,
			   if this has an optionlist, the default value is
			   given as 'name' of option 0, which is the title
			   entry of the list.
			*/
	  (void)fprintf(fd,"[%s]",
		*(strsetting[i].strvalue)?
			*(strsetting[i].strvalue):
			strsetting[i].option_list != (WarnOptionList *)NULL?
			   strsetting[i].option_list[0].name:
			   "NONE");
	  (void)fprintf(fd,": %s",strsetting[i].explanation);
	  if( strsetting[i].option_list != (WarnOptionList *)NULL )
	    (void)fprintf(fd,"\n        Use %c%s=help for list of options",
#ifdef OPTION_PREFIX_SLASH
			  '/',
#else
			  '-',
#endif
			  strsetting[i].name);
	}

	(void)fprintf(fd,"\nSettings (legal range) [default]:");
	for(i=0; i<NUM_SETTINGS; i++) {

	  if( !debug_latest &&
	     strncmp(setting[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug settings unless debug mode */

	  (void)fprintf(fd,"\n    %c%s=dd ",OPT_PREFIX,setting[i].name);
	  (void)fprintf(fd,"(%d to %d) ",setting[i].minlimit,
		  setting[i].maxlimit);
	  (void)fprintf(fd,"[%d]",*(setting[i].setvalue));
	  (void)fprintf(fd,": %s",setting[i].explanation);
	}

    (void)fprintf(fd,
	"\n(First %d chars of option name significant)\n",OPT_MATCH_LEN);
}

		/* Add an include directory path to list of paths */
#ifdef ALLOW_INCLUDE
PRIVATE void
#if HAVE_STDC
append_include_path(char *new_path)
#else /* K&R style */
append_include_path(new_path)
     char *new_path;
#endif /* HAVE_STDC */
{
  IncludePathNode *new_path_node, *p;
  if((new_path_node=(IncludePathNode *)malloc(sizeof(IncludePathNode)))
     ==(IncludePathNode *)NULL) {
    (void)fflush(list_fd);
    (void)fprintf(stderr,"\nmalloc error getting path list");
  }
  else {
    new_path_node->link = (IncludePathNode *)NULL;
    new_path_node->include_path = new_path;
				/* Append the new node at end of list */
    if((p=include_path_list) == (IncludePathNode *)NULL)
      include_path_list = new_path_node;
    else {
      while(p->link != (IncludePathNode *)NULL)
	p = p->link;
      p->link = new_path_node;
    }
  }
#ifdef DEBUG_INCLUDE_PATH	/* Print path as it grows */
  if(getenv("DEBUG")) {
    (void)fprintf(list_fd,"\nINCLUDE path=");
    for(p=include_path_list; p != (IncludePathNode *)NULL; p=p->link) {
      (void)fprintf(list_fd,"%s ",p->include_path);
    }
    (void)fprintf(list_fd,"\n");
  }
#endif
}
#endif/*ALLOW_INCLUDE*/
