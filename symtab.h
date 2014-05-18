/* $Id: symtab.h,v 1.40 2003/03/26 01:12:04 moniot Exp $

	Shared declarations for symbol-table routines.  Note: uses
	declarations in ftnchek.h.

Copyright (c) 1999 by Robert K. Moniot.

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

#define SYMTAB_H		/* for intrins.h */

#ifdef SYMTAB			/* "home" for variables is symtab.c */
#define SYM_SHARED
#else
#define SYM_SHARED extern
#endif

#ifdef DYNAMIC_TABLES
#ifdef __TURBOC__	/* Turbo C has only one free() */
#define cfree free
#endif
#endif

				/* Statement sequence info for fortran.y
				   and to give hints to forlex.c */
#define SEQ_HEADER   1
#define SEQ_IMPLICIT 2
#define SEQ_SPECIF   3
#define SEQ_STMT_FUN 4
#define SEQ_EXEC     5
#define SEQ_END      6

				/* Separate sequence defs to allow checking
				   f90 sequence requirements. */
#define F90_SEQ_HEADER		1
#define F90_SEQ_USE		2
#define F90_SEQ_IMPLICIT_NONE	3
#define F90_SEQ_IMPLICIT	4
#define F90_SEQ_SPECIF		5
#define F90_SEQ_EXEC		6
#define F90_SEQ_CONTAINS	7
#define F90_SEQ_INTERNAL	8
#define F90_SEQ_END		9

		/* Definitions of symbol table information */

/*	Token subclasses (classes are in tokdefs.h)
 */

#define relop_EQ	0
#define relop_NE	1
#define relop_LE	2
#define relop_LT	3
#define relop_GE	4
#define relop_GT	5



	/* Storage Class types for variables, consts, and externals */
#define class_VAR 0
#define class_SUBPROGRAM 1
#define class_COMMON_BLOCK 2
#define class_STMT_FUNCTION 3
#define class_LABEL 4
#define class_NAMELIST 5


	/* Data types for variables, consts, and externals */
	/* N.B. 0 thru 7 are wired into lookup tables in exprtype.c */
#define type_UNDECL 0
#define type_ERROR 0		/* for result of erroneous operation */
#define type_INTEGER 1
#define type_REAL 2
#define type_DP 3
#define type_COMPLEX 4
#define type_DCOMPLEX 5
#define type_LOGICAL 6
#define type_STRING 7
#define type_HOLLERITH 8
#define type_GENERIC 9
#define type_SUBROUTINE 10
#define type_COMMON_BLOCK 11
#define type_PROGRAM 12
#define type_BLOCK_DATA 13
#define type_LABEL 14
#define type_NAMELIST 15

#define size_DEFAULT	(0L)	/* code for standard numeric sizes */
#define size_ADJUSTABLE	(-1L)	/* codes for special char string lengths */
#define size_UNKNOWN	(-2L)

			/* Defns for support of quad precision.  It is
			   implemented as REAL*4n, not a genuine type of
			   its own, since no QUAD type declaration exists.
			   Here n = BpW or whatever wordsize is set at runtime.
			 */
#define type_QUAD type_REAL
#define size_QUAD (4*type_size[type_REAL])
#define type_CQUAD type_COMPLEX
#define size_CQUAD (8*type_size[type_REAL])

				/* test for types usable in exprs */
#define is_computational_type(t) ((unsigned)(t) <= (unsigned)type_HOLLERITH)
				/* test for numeric types */
#define is_numeric_type(t) ((unsigned)(t) <= (unsigned)type_DCOMPLEX)
				/* test for arith, char, or logical type */
#define is_const_type(t) (((unsigned)(t)>(unsigned)0) && ((unsigned)(t)<=(unsigned)type_STRING))
				/* test for numeric or logical type */
#define is_num_log_type(t) ((unsigned)(t) <= type_LOGICAL)
				/* test for real/d.p./complex/d.complx type */
#define is_float_type(t) ((unsigned)(t)>=type_REAL && (unsigned)(t)<=type_DCOMPLEX)

	/* Type categories equate DoubleP to Real, Double Complex
	   to Complex, and Hollerith to Int to simplify expression
	   type propagation and argument checking.  Computational
	   types only, except that since subroutine can be passed
	   as an argument, table goes up that high.  */
SYM_SHARED
unsigned char type_category[]
#ifdef SYMTAB
={	type_UNDECL,
	type_INTEGER,
	type_REAL,
	type_REAL,
	type_COMPLEX,
	type_COMPLEX,
	type_LOGICAL,
	type_STRING,
	type_INTEGER,
	type_GENERIC,
	type_SUBROUTINE,
}
#endif
;
	/* Equivalence types equate Real, DoubleP, Complex and Double
	   Complex, for use in checking mixed equivalence and mixed
	   common, since it is standard and portable to interpret complex
	   as a pair of real values: real part and imag part */
SYM_SHARED
unsigned char equiv_type[]
#ifdef SYMTAB
={	type_UNDECL,
	type_INTEGER,
	type_REAL,
	type_REAL,
	type_REAL,
	type_REAL,
	type_LOGICAL,
	type_STRING,
	type_INTEGER}
#endif
;

/*
 * statement types referencing labels
 */

#define LAB_NO_TYPE 	   0               /* label never defined */           
#define LAB_SPECIFICATION  1
#define LAB_FORMAT	   2
#define LAB_EXECUTABLE	   3
#define LAB_GOTO	   4
#define LAB_ASSIGN	   5
#define LAB_DO		   6
#define LAB_IO		   7
#define LAB_CALL           8
SYM_SHARED
char *lab_type_name[]
#ifdef SYMTAB
 = {
    "undef",
    "specif",
    "format",
    "exec",
    "goto",
    "assign",
    "do",
    "I/O",
    "arg",
}
#endif
;

typedef unsigned char BYTE;

		/* Array of class and type name translations */
SYM_SHARED
char *class_name[]
#ifdef SYMTAB
 = {
	"",
	"subprog",
	"common",
	"stmt fun",
	"label",
	"namelist",
}
#endif
;
SYM_SHARED
char *type_name[]		/* Type names as used in warnings etc.  */
#ifdef SYMTAB
 = {
	"undf",
	"intg",
	"real",
	"dble",
	"cplx",
	"dcpx",
	"logl",
	"char",
	"holl",
	"genr",
	"subr",
	"comm",
	"prog",
	"data",
	"labl",
	"naml",
}
#endif
;

SYM_SHARED
char *type_table[]		/* Names as used in FORTRAN statements */
#ifdef SYMTAB
 =  {
	"??ERROR??",
	"INTEGER",
	"REAL",
	"DOUBLE PRECISION",
	"COMPLEX",
	"DOUBLE COMPLEX",		/* could be "COMPLEX*16" too */
	"LOGICAL",
	"CHARACTER",
/* The rest do not appear in actual use, here for completeness only */
	"HOLLERITH",
	"GENERIC",
	"SUBROUTINE",
	"COMMON",
	"PROGRAM",
	"BLOCK DATA",
	"LABEL",
	"NAMELIST",
    }
#endif
;


/* Here declare typical sizes of objects of each data type, for use in
checking argument and common block size matchups.  BpW (bytes per word)
is defined in ftnchek.h */


SYM_SHARED
BYTE type_size[]
#ifdef SYMTAB
={
	0, /*undf*/
      BpW, /*intg*/
      BpW, /*real*/
    2*BpW, /*dble*/
    2*BpW, /*cplx*/
    4*BpW, /*dcpx*/
      BpW, /*logl*/
	1, /*char*/
      BpW, /*holl*/
	0, /*genr*/
	0, /*subr*/
	0, /*comm*/
	0, /*prog*/
	0, /*data*/
	0, /*labl*/
	0, /*naml*/
}
#endif
;



		/* implicit and default typing lookup table.  Two extra spots
		   provided to accommodate '$' and '_' too.  The size defns
		   should accommodate EBCDIC as well as ASCII. */
SYM_SHARED
int implicit_type[('Z'-'A'+1)+2],	/* indexed by [char - 'A'] */
    implicit_size[('Z'-'A'+1)+2];
SYM_SHARED
char *implicit_len_text[('Z'-'A'+1)+2];


	/* Declaration of Token data structure.  N.B. do not change without
	   consulting preamble of fortran.y for uses with nonterminals.
	 */

			/* temporary equivs for future separate fields */
				/* these are for array-bounds tokens */
#define TOK_dims tclass
#define TOK_elts tsubclass

#define TOK_start tclass
#define TOK_end  tsubclass


struct tokstruct {
	union {
		long integer;
		DBLVAL dbl;
		char *string;
	} value;		/* Value of constant */
	struct tokstruct
	  *left_token,		/* Left child in expr tree */
	  *next_token;		/* Right child or next in linked list */
	char *src_text;		/* Original text string of token */
	long tclass,tsubclass;	/* Token category and subcategory */
	long size;		/* sizeof(datatype) */
	long TOK_type;		/* Storage class & data type of identifier */
	unsigned TOK_flags:32;	/* Exprtype flags (see defns below) */
	LINENO_t line_num;	/* Line where token occurred */
	COLNO_t col_num;	/* Column where token occurred */
	unsigned size_is_adjustable : 1;
	unsigned size_is_expression : 1;
};

typedef struct tokstruct Token;

#ifdef  YYSTYPE
#undef  YYSTYPE
#endif
#define YYSTYPE Token	/* Type defn for yylval and Yacc stack */



SYM_SHARED
long loc_symtab_top,	/* Next avail spot in local symbol table */
   glob_symtab_top;		/* Ditto global */

SYM_SHARED
unsigned long loc_str_top;	/* Top of local stringspace */

SYM_SHARED
   unsigned long srctextspace_top; /* Top of token src text space */

SYM_SHARED
   unsigned long ptrspace_top;	/* Top of pointer space */

SYM_SHARED
   unsigned long param_info_space_top;	/* Top of parameter info space */

SYM_SHARED
   unsigned long token_space_top,	/* Top of token space */
		 token_head_space_top;	/* Top of TL_head space */

	/* Counts of extra items dynamically allocated, for -resource */
SYM_SHARED
  int extra_locstrspace,
      extra_paraminfospace,
      extra_srctextspace,
      extra_tokheadspace,
      extra_tokspace,
      extra_ptrspace;

SYM_SHARED
  LINENO_t top_file_line_num;

SYM_SHARED
  int global_save;	/* module contains SAVE with no list */

		/* Define names for anonymous things */
#ifdef SYMTAB
char blank_com_name[] = "%BLANK",  /* id for blank common entry in symtab */
     unnamed_prog[]="%MAIN",	  /* id for unnamed program module */
     unnamed_block_data[]="%DAT00";  /* id for unnamed block data module */
int  block_data_number=0;       /* count of multiple anonymous block data */
#else
extern char blank_com_name[],
	    unnamed_prog[],
	    unnamed_block_data[];
extern int block_data_number;
#endif

typedef int LABEL_t;              /* a label (0-99999) */         

#define NO_LABEL ((LABEL_t)-1)    /* label never used/defined */

                /* Symbol table argument list declarations */

typedef union {		/* InfoUnion: misc info about symtab entry */
	     unsigned long array_dim;	/* array size and no. of dims */
	     struct ALHead *arglist;	/* ptr to func/subr argument list */
	     struct CMHead *comlist;    /* ptr to common block list */
	     struct TLHead *toklist;  /* ptr to token list */
	     struct IInfo *intrins_info;/* ptr to intrinsic func info */
	     struct PInfo *param;	/* parameter information field */
} InfoUnion;

typedef struct {	/* ArgListElement: holds subprog argument data */
	char *name;		/* name of dummy arg or text of actual arg */
	InfoUnion info;
	struct gSymtEntry *common_block; /* block it belongs to if any */
	long common_index;	/* index in block */
	long size;
	BYTE type;
	short same_as;	/* index if two actual arguments the same */
	unsigned is_lvalue: 1,
		 set_flag: 1,
		 assigned_flag: 1,
		 used_before_set: 1,
		 array_var: 1,
		 array_element: 1,
		 declared_external: 1,
		 active_do_var: 1;
} ArgListElement;


typedef struct ALHead {	    /* ArgListHeader: head node of argument list */
	long size;
	BYTE type;
	short numargs;
	ArgListElement *arg_array;
	struct gSymtEntry *module;
	char *filename,*topfile;
	LINENO_t line_num,top_line_num;
	unsigned
	     is_defn: 1,
	     is_call: 1,
	     external_decl: 1,	/* EXTERNAL decl, not arg list */
             actual_arg: 1;	/* subprog passed as arg */
	struct ALHead *next;
} ArgListHeader;

		/* Symbol table common block list declarations */

typedef struct {	/* ComListElement: holds common var data */
	char *name;		/* name of common variable */
	unsigned long dimen_info;
	long size;
	BYTE type;
	unsigned		/* copies of flags from symtab */
	  used:1,
	  set:1,
	  used_before_set:1,
	  assigned:1,
	  marked:1;		/* for listing of offenders */
} ComListElement;

typedef struct CMHead {	/* ComListHeader: head node of common var list */
	short numargs;
	LINENO_t line_num,top_line_num;
	ComListElement *com_list_array;
	struct gSymtEntry *module;
	char *filename,*topfile;
	struct CMHead *next;
	unsigned
	  any_used:1,		/* any of its variables accessed */
	  any_set:1,		/* any of its variables set */
	  saved:1;		/* declared in SAVE statement */
} ComListHeader;


typedef struct TLHead {	/* TokenListHeader: head node of token list */
	Token *tokenlist;
	struct TLHead *next;
	char *filename;
	LINENO_t line_num, top_line_num;
	unsigned
	  external_decl:1,
	  actual_arg:1;
} TokenListHeader;


			/* Structure for intrinsic-function info */

	/* Define special num_args values for intrinsics that have
	   variable numbers of arguments. */
#define I_1or2	(-1)		/* 1 or 2 arguments */
#define I_2up	(-2)		/* 2 or more arguments */
#define I_0or1	(-3)		/* 0 or 1 argument */

			/* for intrins_flags field */

	/* Integer-valued intrinsics that are evaluated if args const */
#define I_ABS		0x1
#define I_SIGN		0x2
#define I_DIM		0x3
#define I_MOD		0x4
#define I_MAX		0x5
#define I_MIN		0x6
#define I_ICHAR		0x7
#define I_LEN		0x8
#define I_INDEX		0x9
#define I_EVALUATED	0xf	/* any bit of digit set */

		/* Various properties of intrinsics*/
#define I_F77 		0x00	/* Standard intrinsic (no flag: placeholder) */
#define I_NONF77	0x10	/* Nonstandard */
#define I_MIXED_ARGS	0x20	/* Has mixed arg types */
#define I_NONPURE	0x40	/* Arg need not be set when called */
#define I_C_TO_R	0x80	/* Complex -> real in generic form */
#define I_NOTARG	0x100	/* Not allowed as actual argument */
#define I_SP_R		0x200	/* special for REAL function */
#define I_CHAR		0x400	/* special handling for CHAR function */
#define I_QARG		0x800	/* Arg type is R*16 or X*32 */
#define I_QUAD		0x1000	/* Result type is R*16 or X*32 */
#define I_EXTRA		0x2000	/* commonly found extra intrinsics */
#define I_VMS		0x4000	/* VMS systems only */
#define I_UNIX		0x8000	/* Unix systems only */
#define I_NONF90	0x10000	/* Not in F90 standard */

				/* Define flag type big enough for 17 bits */
#if (SIZEOF_SHORT > 2)
typedef unsigned short intrins_flags_t;
#else
#if (SIZEOF_INT > 2)
typedef unsigned int intrins_flags_t;
#else
#if (SIZEOF_LONG > 2)
typedef unsigned long intrins_flags_t;
#endif
#endif
#endif
typedef struct IInfo{
	char *name;
	short num_args,
	      arg_type,
	      result_type;
	intrins_flags_t
	      intrins_flags;	/* nonstandard,  mixed arg types */
} IntrinsInfo;

			/* Structure for parameter info */
typedef struct PInfo{
	char *src_text;		/* source text of parameter value */
	union {
	  long integer;		/* integer value */
	  DBLVAL dbl;		/* float value */
	  char *string;		/* character string value */
	} value;
	int seq_num;		/* position in parameter definitions */
} ParamInfo;
	

			/* Structure for call-tree child list */
typedef struct childlist {
  struct gSymtEntry *child;	/* Pointer to child's symtab entry */
  struct childlist *next;/* Pointer to next child on list */
} ChildList;

		/*  Identifier symbol table declaration */


typedef struct lSymtEntry{
	char *name;             /* Identifier name in stringspace */
	InfoUnion info;
	union{
	  char *text;		/* Source text string */
	  char **textvec;	/* List of source text strings */
	  TokenListHeader *toklist; /* for namelist & common block makedecls */
	} src;
	struct lSymtEntry *equiv_link;	/* Link for equivalence lists */
		/* common_block is a ptr to block if this is a common
		   variable, and common_index is its position (starting
		   from 1).  For block, common_index is the count of
		   variables in it. */
	struct gSymtEntry *common_block;
	long common_index;
	long size;		/* Size of object in bytes */
		/* Object can be referenced in an include file. Next fields
		   are line numbers within file where object is referred
		   to, then come indexes into include-file list.  */
	LINENO_t line_declared, line_set, line_used;
	short file_declared, file_set, file_used;
	BYTE  type;		/* Type & storage class: see macros below */
			/* Flags */
	unsigned
	     used_flag: 1,	/* value is accessed (read from variable) */
	     set_flag: 1,	/* variable is set or passed as subr arg */
	     assigned_flag: 1,	/* value is really set (by assignment stmt) */
	     used_before_set: 1,/* set_flag is not set when used_flag is set */
	     is_current_module: 1, /* this symtab entry is the main module */
	     library_module: 1,	/* module was processed in -library mode */
	     active_do_var: 1,	/* variable is an active DO index */
	     array_var: 1,	/* variable is dimensioned */
	     common_var: 1,	/* variable is in common */
	     entry_point: 1,	/* name of an entry point */
	     parameter: 1,	/* name of a parameter */
	     argument: 1,	/* dummy argument */
	     external: 1,	/* function or subr called by this routine */
	     intrinsic: 1,	/* intrinsic function */
	     saved: 1,		/* named in SAVE statement */
	     invoked_as_func: 1, /* usage as f(x) was seen */
	     defined_in_include: 1, /* to suppress some warnings if unused */
	     declared_external: 1, /* explicitly declared external */
	     declared_intrinsic: 1; /* explicitly declared intrinsic */
	     unsigned size_is_adjustable : 1; /* CHARACTER*(*) declaration */
	     unsigned size_is_expression : 1; /* CHARACTER*(expr) declaration */
} Lsymtab;

typedef struct gSymtEntry{	/* Global symbol table element */
	char *name;             /* Identifier name in stringspace */
	InfoUnion info;
	union {
	  struct childlist *child_list; /* List of callees (for module) */
	  struct gSymtEntry *module; /* Module (for interior entry) */
	} link;
	long size;
	BYTE  type;		/* Type & storage class: see macros below */
			/* Flags.  See remarks above */
	unsigned
	     used_flag: 1,
	     set_flag: 1,
	     assigned_flag: 1,
	     recursive: 1,
	     library_module: 1,
	     internal_entry: 1,	/* entry point other than at the top */
	     invoked_as_func: 1,
	     visited: 1,	   /* this entry point is in call tree */
	     visited_somewhere: 1, /* some entry point of module is in call tree */
	     defined: 1,	/* is defined somewhere */
	     defined_in_include: 1,
	     declared_external: 1,
			/* The following flags are for project-file use.
			   They get reset when a file is opened and accumulate
			   values as file is read.  */
	     used_this_file: 1,
	     set_this_file: 1,
	     invoked_as_func_this_file: 1,
	     declared_external_this_file: 1;
} Gsymtab;


		/*  Identifier hashtable declaration  */

typedef struct hashEntry {
	char	*name;		/* Identifier name in stringspace */
	Lsymtab	*loc_symtab,	/* Local symtab entry for vars etc. */
		*com_loc_symtab;/* Local symtab entry for common blocks */
	Gsymtab	*glob_symtab,	/* Global symtab entry for vars etc. */
		*com_glob_symtab;/* Global symtab entry for common blocks */
} HashTable;

SYM_SHARED
    int current_module_hash	/* hashtable index of current module name */
#ifdef SYMTAB
 = -1
#endif
;

				/* Symbolic names for I/O access modes */
typedef enum {
     IO_ACCESS_DEFAULT, IO_ACCESS_DIRECT, IO_ACCESS_SEQUENTIAL
} IO_ACCESS_TYPE;

				/* Symbolic names for I/O forms */
typedef enum {
     IO_FORM_DEFAULT, IO_FORM_UNFORMATTED, IO_FORM_FORMATTED
} IO_FORM_TYPE;

#define IO_UNIT_UNKNOWN -1
#define IO_UNIT_DEFAULT -2	/* For unit=* */
				/* Struct for I/O unit usage */
typedef struct {
     int line_num;		/* location of I/O usage */
     int unit_no;		/* unit number if known, else UNKNOWN */
     int unit_id;		/* hash num of unit if variable, else UNKNOWN or DEFAULT */
     IO_ACCESS_TYPE io_access;	/* access mode of file */
     IO_FORM_TYPE io_form;	/* form specified for file */
     int io_operation;		/* input, output, open, close, etc. */
} IO_Unit_Info;

SYM_SHARED
IO_Unit_Info* io_unit_info	/* Array of I/O usage instances */
#ifdef SYMTAB
 = (IO_Unit_Info*)NULL
#endif
;

SYM_SHARED
int max_io_unit_usages		/* current size of I/O usage array */
#ifdef SYMTAB
 = 0
#endif
;

SYM_SHARED
int num_io_unit_usages		/* number of I/O usage instances in list */
#ifdef SYMTAB
 = 0
#endif
;
				/* Struct for include-file list */
typedef struct {
    char *fname;		/* name of include file */
    LINENO_t line;		/* line of topfile where included */
    short footnote;             /* footnote number--for printing
				   labels */
} IncFile;

SYM_SHARED
IncFile* incfile_list
#ifdef SYMTAB
 = (IncFile*)NULL
#endif
;

SYM_SHARED
int num_incfiles		/* number of include-files in list */
#ifdef SYMTAB
 = 0
#endif
;

SYM_SHARED
short inctable_index;		/* index of current include-file in list */

				/* Struct for chunks of string space */
typedef struct STSpace {
  struct STSpace *next;
  char strspace[STRSPACESZ];
} StrSpace;

				/* Struct for providing chunks of space
				   for parameter info. */
typedef struct PISpace {
  struct PISpace *next;
  ParamInfo paraminfospace[PARAMINFOSPACESZ];
} ParamInfoSpace;

				/* Struct for providing chunks of space
				   for token list headers for arg lists etc. */
typedef struct THSpace {
  struct THSpace *next;
  TokenListHeader tokheadspace[TOKHEADSPACESZ];
} TokHeadSpace;


				/* Struct for providing chunks of space
				   for tokens for arg lists etc.  */
typedef struct TSpace {
  struct TSpace *next;
  Token tokenspace[TOKENSPACESZ];
} TokenSpace;

				/* Struct for providing chunks of space
				   for pointers to array & param text */
typedef struct PSpace {
  struct PSpace *next;
  char * ptrspace[PTRSPACESZ];
} PtrSpace;


		/* Macro to zero out symbol table entry */

#define clear_symtab_entry(S) {register unsigned i;\
				 for(i=0;i<sizeof(*S);i++)((char*)S)[i]=0;}


	/* These macros pack and unpack datatype and storage class in type
	   field of symbol table entry. Datatype is least 4 bits. */

#define datatype_of(TYPE) ((unsigned)((TYPE) & 0xF))
#define storage_class_of(TYPE) ((unsigned)((TYPE) >> 4))
#define type_byte(SCLASS,DTYPE) ((unsigned)(((SCLASS)<<4) + (DTYPE)))


	/* This macro is for pattern matching in flag checking */

#define flag_combo(A,B,C) (((A)<<2) | ((B)<<1) | (C))


	/* These macros are for dimensions & sizes of arrays */

#define array_dims(dim_info) ((dim_info)&0xF)
#define array_size(dim_info) ((dim_info)>>4)
#define array_dim_info(dim,size) (((long)(size)<<4)+(dim))



		/* Defns used by expression type propagation mechanisms
		   in fortran.y and exprtype.c  The flags go in token.TOK_flags
		   Make sure size of TOK_flags declared above suffices for
		   largest item in the list below.
		 */

#define make_true(flag,x) ((x) |= ((unsigned)flag))	/* x.flag <-- true   */
#define make_false(flag,x) ((x) &= ~((unsigned)flag))	/* x.flag <-- false  */
#define is_true(flag,x) ((x) & (flag))			/* x.flag == true?   */
#define copy_flag(flag,x,y)  ((x) |= ((y)&((unsigned)flag))) /* x.flag <-- y.flag */

#define ID_EXPR			0x1	/* a variable */
#define LVALUE_EXPR		0x2	/* assignable */
#define CONST_EXPR		0x4	/* compile-time constant per std 6.7*/
#define LIT_CONST		0x8	/* a number or string literal */
#define ARRAY_ID_EXPR		0x10	/* an array or array element */
#define ARRAY_ELEMENT_EXPR	0x20 	/* an array element */
#define INT_QUOTIENT_EXPR	0x40 	/* contains INT/INT */
#define STMT_FUNCTION_EXPR	0x80
#define PARAMETER_EXPR		0x100/* == CONST_EXPR || intrinsic || **real */
#define EVALUATED_EXPR		0x200  /* token.value has value of expr */
#define SET_FLAG		0x400  /* id may be set */
#define ASSIGNED_FLAG		0x800  /* id is set in assignment stmt */
#define USED_BEFORE_SET		0x1000  /* id used beforre set */
#define COMPLEX_FLAG		0x2000	/* remembers complex_const_allowed */
#define CHAR_ID_EXPR		0x4000	/* char var or array elt not substr */
#define DIM_BOUND_EXPR		0x8000	/* no array or func ref (5.1.1.1) */
#define IN_ASSIGN 		0x10000	/* for tracking assgn stmt lhs */
#define COMMA_FLAG		0x20000/* keeps track of extra or missing
				   	  commas in exprlists (obsolete) */
#define NONSTD_USAGE_FLAG	0x40000	/* concentrator for -f77 warnings */
#define NOT_DO_TERMINAL_STMT    0x80000 /* stmt illegal as end of DO loop */
#define DO_VARIABLE		0x100000 /* id is active DO index variable */
#define SYNTAX_ERROR_FLAG	0x200000/* concentrator for syntax errors */

#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
SYM_SHARED
Lsymtab	*loc_symtab
#ifdef SYMTAB
  =(Lsymtab *)NULL
#endif
;
SYM_SHARED
Gsymtab *glob_symtab
#ifdef SYMTAB
  =(Gsymtab *)NULL
#endif
;
SYM_SHARED
HashTable *hashtab
#ifdef SYMTAB
  =(HashTable *)NULL
#endif
;

#else				/* static tables declared at compile time */
		/* Each major table is housed in a separate file so that
		   on IBM PC architecture with huge memory model
		   each will be in its own 64K segment not all in one. */
#ifndef PLSYMTAB
extern
#endif
Lsymtab	loc_symtab[LOCSYMTABSZ]; /* Local identifiers */
#ifndef PGSYMTAB
extern
#endif
Gsymtab glob_symtab[GLOBSYMTABSZ]; /* Global identifiers: subrs and com blks */
#ifndef EXPRTYPE
extern
#endif
HashTable hashtab[HASHSZ];	/* Hash table for identifier lookup */

#endif/* end static tables */

	/* The following tables start life as statically declared
	   tables, but when add'l space is needed, new structs of same
	   kind will be allocated and linked via next field of struct.
	   Because they are dynamically extended, they are not in
	   the DYNAMIC_TABLES section or its complement above.  Note
	   that as global variables they start off at 0, so next field
	   of each is implicitly initialized to NULL.  */

#ifndef FORLEX
extern
#endif
TokenSpace tokspace;	/* Tokens for arg lists etc */

#ifndef PROJECT
extern
#endif
TokHeadSpace tokheadspace;/* Tokenlist headers */

#ifndef PROJECT
extern
#endif
ParamInfoSpace paraminfospace;/* Parameter info structs */

#ifndef PROJECT
extern
#endif
PtrSpace ptrspace;	/* Space for storing arrays of pointers */

#ifndef SYMTAB
extern
#endif
StrSpace lstrspace;	/* String space for local identifiers */

#ifndef SYMTAB
extern
#endif
StrSpace srctextspace;/* String space for token source text */


		/* Shared routines */


			/* in exprtype.c */
PROTO(void assignment_stmt_type,( Token *term1, Token *equals, Token *term2 ));
PROTO(void binexpr_type,( Token *term1, Token *op, Token *term2, Token
		  *result ));
PROTO(void check_initializer_type, ( Token *assignee_list, Token *equals, Token *expr_list));
PROTO(void func_ref_expr,( Token *id, Token *args, Token *result ));
PROTO(void primary_id_expr,( Token *id, Token *primary ));
PROTO(void stmt_fun_arg_cmp,( const Lsymtab *symt, const Token *d_arg, const Token *a_arg ));
PROTO(int substring_size,( Token *id, Token *limits ));
PROTO(void unexpr_type,( Token *term1, Token *op, Token *result ));
PROTO(int intrins_arg_cmp,( IntrinsInfo *defn, Token *t));

			/* in advance.c */
PROTO(int see_double_colon,( void ));

			/* in forlex.c */
PROTO(void implied_id_token,( Token *t, char *s ));
PROTO(int yylex,( void ));

			/* in keywords.c */
PROTO(char *keytok_name,(int tclass));

			/* in fortran.y/fortran.c */
PROTO(void check_seq_header,( Token *t ));

			/* in prlocsym.c */
PROTO(void print_loc_symbols,( void ));

			/* in makehtml.c */
PROTO(void make_html,(Lsymtab **sym_list, char *mod_name, Lsymtab *module ));

			/* in makedcls.c */
PROTO(void make_declarations,( Lsymtab *sym_list[], char *mod_name ));

			/* in symtab.c */
PROTO(void call_func,( Token *id, Token *arg ));
PROTO(void call_subr,( Token *id, Token *arg ));
PROTO(char * char_expr_value,( Token *t ));
PROTO(void check_loose_ends,( int curmodhash ));
PROTO(void declare_type,( Token *id, int datatype, long size, char *size_text ));
PROTO(void def_arg_name,( Token *id ));
PROTO(void def_array_dim,( Token *id, Token *arg ));
PROTO(void def_com_block,( Token *id, Token *comlist ));
PROTO(void def_com_variable,( Token *id ));
PROTO(int def_curr_module,( Token *id ));
PROTO(void def_do_variable,( Token *id ));
PROTO(void def_equiv_name,( Token *id ));
PROTO(void def_ext_name,( Token *id ));
PROTO(void def_function,( int datatype, long size, char *size_text, Token
		  *id, Token *args ));
PROTO(void def_intrins_name,( Token *id ));
PROTO(void def_namelist,( Token *id, Token *list ));
PROTO(void def_namelist_item,( Token *id ));
PROTO(void def_parameter,( Token *id, Token *val, int noparen ));
PROTO(void def_stmt_function,( Token *id, Token *args ));
PROTO(void do_ASSIGN,( Token *id ));
PROTO(void do_assigned_GOTO,( Token *id ));
PROTO(void do_ENTRY,( Token *id, Token *args, int hashno ));
PROTO(int do_RETURN,( int hashno, Token *keyword ));
PROTO(void equivalence,( Token *id1, Token *id2 ));
PROTO(DBLVAL float_expr_value,( Token *t ));
PROTO(int get_size,( const Lsymtab *symt, int type ));
PROTO(char * get_size_text,( const Lsymtab *symt, int type ));
PROTO(int get_type,( const Lsymtab *symt ));
PROTO(unsigned hash_lookup,( char *s ));
PROTO(Gsymtab* install_global,( int h, int datatype, int storage_class ));
PROTO(int int_expr_value,( Token *t ));
PROTO(char * new_global_string,( char *s ));
PROTO(void free_textvec,( char **p ));
PROTO(char * new_src_text,( const char *s, int len ));
PROTO(char * new_src_text_alloc,( int size ));
PROTO(char * new_tree_text,( Token *t ));
PROTO(char ** new_textvec,( int n ));
PROTO(Token * new_token,( void ));
PROTO(void msg_expr_tree, (const Token *t));
#ifdef DEBUG_EXPRTREES
PROTO(void print_src_text,( Token *t ));
PROTO(void print_expr_tree,( Token *t ));
PROTO(void print_expr_list,( Token *t ));
#endif
PROTO(void process_lists,( int curmodhash ));
PROTO(void record_io_unit_id, (Token *id));
PROTO(void record_io_usage, (Token *stmt));
PROTO(void ref_array,( Token *id, Token *subscrs ));
PROTO(void ref_namelist,( Token *id, int stmt_class ));
PROTO(void ref_identifier,( Token *id ));
PROTO(void ref_variable,( Token *id ));
PROTO(void save_com_block,( Token *id ));
PROTO(void save_variable,( Token *id ));
PROTO(void set_implicit_type,( int type, long size, char *len_text, int c1, int c2 ));
PROTO(void stmt_function_stmt,( Token *id ));
PROTO(char * token_name,( Token *t ));
PROTO(void undef_do_variable,( int h ));
PROTO(void use_actual_arg,( Token *id ));
PROTO(void use_implied_do_index,( Token *id ));
PROTO(void use_io_keyword,( Token *keyword, Token *value, int stmt_class ));
PROTO(void use_special_open_keywd,( Token *id ));
PROTO(void use_lvalue,( Token *id ));
PROTO(void use_parameter,( Token *id ));
PROTO(void use_variable,( Token *id ));

PROTO(char* typespec, ( int t, int has_size, long size,
			int has_len, long len));
				/* The following size is conservative,
				   to make sure no buffer overruns occur.
				 */
		/* Maximum length of a typespec() result.  */
#define MAX_TYPESPEC (4+4+6*sizeof(long))

				/* in symtab.c (formerly hash.c) */
PROTO(unsigned long hash,( const char *s ));
PROTO(unsigned long rehash,( unsigned long hnum ));


			/* To stop printing errors after limit is reached,
			   unless limit is 0.  Increment error count
			   even if don't print.
			 */
#define CASCADE_LIMIT(ERROR_COUNT) (++(ERROR_COUNT) > error_cascade_limit \
			&& error_cascade_limit > 0)


		/* prototypes of label routines */
void init_labtable(void);
void print_labels(void);
void print_label_refs(void); 
void check_labels(char *mod_name);
void sort_labtable(void);
int def_label(Token *t, int type);
void def_do_label(Token *t);
void ref_label(Token *t, int type);
void update_label_resources(void);
