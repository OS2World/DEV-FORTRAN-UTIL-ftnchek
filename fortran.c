/* A Bison parser, made from fortran.y
   by GNU bison 1.35.  */

#define YYBISON 1  /* Identify Bison output.  */

# define	tok_identifier	257
# define	tok_array_identifier	258
# define	tok_label	259
# define	tok_integer_const	260
# define	tok_real_const	261
# define	tok_dp_const	262
# define	tok_quad_const	263
# define	tok_complex_const	264
# define	tok_dcomplex_const	265
# define	tok_logical_const	266
# define	tok_string	267
# define	tok_hollerith	268
# define	tok_edit_descriptor	269
# define	tok_letter	270
# define	tok_relop	271
# define	tok_AND	272
# define	tok_OR	273
# define	tok_EQV	274
# define	tok_NEQV	275
# define	tok_NOT	276
# define	tok_power	277
# define	tok_concat	278
# define	tok_lparen	279
# define	tok_ACCEPT	280
# define	tok_ASSIGN	281
# define	tok_BACKSPACE	282
# define	tok_BLOCKDATA	283
# define	tok_BYTE	284
# define	tok_CALL	285
# define	tok_CASE	286
# define	tok_CASEDEFAULT	287
# define	tok_CHARACTER	288
# define	tok_CLOSE	289
# define	tok_COMMON	290
# define	tok_COMPLEX	291
# define	tok_CONTINUE	292
# define	tok_CYCLE	293
# define	tok_DATA	294
# define	tok_DIMENSION	295
# define	tok_DO	296
# define	tok_DOUBLECOMPLEX	297
# define	tok_DOUBLEPRECISION	298
# define	tok_DOWHILE	299
# define	tok_ELSE	300
# define	tok_END	301
# define	tok_ENDBLOCKDATA	302
# define	tok_ENDDO	303
# define	tok_ENDFILE	304
# define	tok_ENDFUNCTION	305
# define	tok_ENDIF	306
# define	tok_ENDPROGRAM	307
# define	tok_ENDSELECT	308
# define	tok_ENDSUBROUTINE	309
# define	tok_ENTRY	310
# define	tok_EQUIVALENCE	311
# define	tok_EXTERNAL	312
# define	tok_EXIT	313
# define	tok_FORMAT	314
# define	tok_FUNCTION	315
# define	tok_GOTO	316
# define	tok_IF	317
# define	tok_IMPLICIT	318
# define	tok_INCLUDE	319
# define	tok_INQUIRE	320
# define	tok_INTEGER	321
# define	tok_INTRINSIC	322
# define	tok_LOGICAL	323
# define	tok_NAMELIST	324
# define	tok_NONE	325
# define	tok_OPEN	326
# define	tok_PARAMETER	327
# define	tok_PAUSE	328
# define	tok_POINTER	329
# define	tok_PRINT	330
# define	tok_PROGRAM	331
# define	tok_READ	332
# define	tok_REAL	333
# define	tok_RETURN	334
# define	tok_REWIND	335
# define	tok_SAVE	336
# define	tok_SELECTCASE	337
# define	tok_STOP	338
# define	tok_SUBROUTINE	339
# define	tok_THEN	340
# define	tok_TO	341
# define	tok_TYPE	342
# define	tok_WHILE	343
# define	tok_WRITE	344
# define	tok_illegal	345
# define	tok_empty	346
# define	EOS	127
# define	REDUCE	347

#line 10 "fortran.y"


/*
  fortran.c:


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

	    This grammar is ANSI standard-conforming, except for:
		-- complex constant and a few other ambiguities needing
		   significant lookahead cannot be split across lines.

	    Extensions supported:
	        -- Case insensitive.
	 	-- Hollerith constants.
		-- Variable names may be longer than 6 characters.  Also
		   allows underscores and dollar signs in names.
		-- DO ... ENDDO and DO WHILE loop forms allowed.
		-- NAMELIST supported.
		-- TYPE and ACCEPT I/O statements allowed.
		-- Tabs are permitted in input, and (except in character data)
		   expand into blanks up to the next column equal to 1 mod 8.
		-- Type declarations INTEGER*2, REAL*8, etc. are allowed.
		-- IMPLICIT NONE allowed.
                -- CASE construct supported
*/


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#include "symtab.h"

	/* The following section is for use with bison-derived
	   parser.  Define alloca to be malloc for those cases
	   not covered by the cases covered there.  The ifdefs
	   are those in the skeleton parser with includes removed */
#ifdef AIXC	/* IBM RS/6000 xlc compiler does it this way */
#pragma alloca
#endif
#ifndef alloca
#ifdef __GNUC__
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#else /* Not sparc */
#ifdef MSDOS
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#define alloca malloc
#endif /* alloca now defined.  */

#ifndef YYDEBUG	/* If not declared otherwise... */
int yydebug;	/* declare yydebug to satisfy extern in ftnchek.c */
#ifdef DEVELOPMENT
#define YYDEBUG 1		/* For development it is handy */
#else
#define YYDEBUG 0
#endif
#endif

#ifdef DEVELOPMENT
#define DEBUG_PARSER
#endif

PRIVATE int current_datatype,	/* set when parse type_name or type_stmt */
    current_size_is_adjustable, /* set in CHARACTER declarations */
    current_size_is_expression, /* set in CHARACTER declarations */
    current_save_attr,	/* set if SAVE attribute given in type decl */
    current_external_attr,	/* set if EXTERNAL attr given */
    current_intrinsic_attr,	/* set if INTRINSIC attr given */
    current_parameter_attr,	/* set if PARAMETER attr given */
    label_dummy_arg_count,	/* number of labels in dummy argument list */
    len_selector_given, /* flag for use in processing CHARACTER decls */
    len_spec_item_count,/* count of items in CHARACTER len-selector list */
    control_item_count;	/* count of items in control_info_list */

PRIVATE Token *current_dim_bound_list;	/* attr-based dim bound tokenlist */

	/* Information about the current I/O statement */
int current_io_unit_id;		/* hashnum of unit id of current I/O operation */
int  current_io_unit_no;	/* unit number of current I/O operation */
IO_ACCESS_TYPE current_io_access;/* access mode (direct/sequential) */
IO_FORM_TYPE current_io_form;	/* form (formatted/unformatted) */

int io_internal_file,	/* Flag for catching misuse of internal files */
    io_list_directed,	/* Flag for use in processing io control lists */
    io_warning_given;		/* to prevent multiple warnings */
			/* Flag shared with forlex for lexing hints */
int stmt_sequence_no,   /* set when parsing, reset to 0 at end_stmt */
    f90_stmt_sequence_no;
PRIVATE long current_typesize;	/* for type*len declarations: value of len */
PRIVATE char *current_len_text;	/* for type*len declarations: text of len */

PRIVATE int kind_warning_given=FALSE; /* to say "not interpreted" only once */

PRIVATE Token save_token,	/* Holds token shared by productions */
    len_spec_token,		/* Holds character length spec temporarily */
    dim_bound_token;		/* Holds attr-based dim-bound list header */

extern LINENO_t prev_stmt_line_num; /* shared with advance */

LINENO_t true_prev_stmt_line_num;	/* shared with symtab.c */

PRIVATE int
    current_module_type,
    executable_stmt=FALSE,
    prev_stmt_class=0,		 /* flags for lexer */
    labeled_stmt_type,		 /* for label handling */
    if_line_num, if_col_num,	/* for picky construct-usage warnings */
    prev_goto=FALSE,
    goto_flag=FALSE;	/* if unconditional GOTO was encountered */

int 
    complex_const_allowed=FALSE, /* for help in lookahead for these */
    construct_name_seen=FALSE,	/* for help recognizing DO */
    param_noparen=FALSE,	/* for different PARAMETER stmt semantics */
    in_assignment_stmt=FALSE,
    in_attrbased_typedecl=FALSE,/* help is_keyword lex type, attr :: list */
    inside_format=FALSE,	/* when inside parens of FORMAT  */
    integer_context=FALSE;	/* says integers-only are to follow */


		/* Macro for initializing attributes of type decl. */
#define reset_type_attrs() (\
    current_save_attr = FALSE, \
    current_external_attr = FALSE, \
    current_intrinsic_attr = FALSE, \
    current_parameter_attr = FALSE, \
    current_dim_bound_list = NULL   )

			/* Define stuff for checking block nesting */

#define MAX_BLOCK_DEPTH 100	/* maximum depth of nesting blocks */

	/* USE_YYTNAME allows messages to incorporate token names from
	   the yytname array.  This option should be enabled if bison
	   is used to generate parser, not otherwise.  Enabling it
	   also assumes fortran.c has been edited to remove "tok_"
	   from token names (see Makefile).
	*/
#ifndef NO_YYTNAME
#if defined( YYDEBUG ) && defined( YYBISON )
#define USE_YYTNAME
#endif
#endif

				/* Convenience typedef for category of block */
typedef enum {subprog, construct} BLOCK_TYPE;

typedef struct {
    int sclass;			/* stmt_class of block opener */
    char *name;			/* name of block or subprogram */
    LABEL_t label;		/* label of closing statement for DO */
    LINENO_t first_line;	/* line number of block opener */
    BLOCK_TYPE blocktype;	/* category for wording of warnings */
    int do_var_hash;		/* hash index for index variable of DO block */
} BlockStack;

PRIVATE BlockStack block_stack[MAX_BLOCK_DEPTH];

PRIVATE char *
    curr_stmt_name;	/* subprog or END-subprog name; DO, IF construct name*/

PRIVATE int
    block_depth=0;		/* depth of nesting of current block */

				/* Defns of private functions */

PROTO(PRIVATE void push_block,(Token *t, int stmt_class, BLOCK_TYPE blocktype,
			       char *name, LABEL_t label));
PROTO(PRIVATE void pop_block,(Token *t, int stmt_class,
			      char *name, LABEL_t label));
PROTO(PRIVATE void check_construct_name_match,(Token *stmt, char *name));

PROTO(PRIVATE Token * add_tree_node,( Token *node, Token *left, Token *right ));
PROTO(PRIVATE Token * append_token,( Token *tlist, Token *t ));
PROTO(PRIVATE void check_stmt_sequence,( Token *t, int seq_num ));
PROTO(PRIVATE void check_f90_stmt_sequence,( Token *t, int f90_seq_num ));
PROTO(PRIVATE void do_binexpr,( Token *l_expr, Token *op, Token *r_expr,
			Token *result ));
PROTO(PRIVATE int do_bounds_type,( Token *t1, Token *t2, Token *t3 ));
PROTO(PRIVATE void do_unexpr,( Token *op, Token *expr, Token *result ));
PROTO(PRIVATE Token * empty_token,( Token *t ));
PROTO(PRIVATE void END_processing,( Token *t ));
PROTO(PRIVATE void init_io_ctrl_list,( void ));
PROTO(PRIVATE void record_default_io,( void ));
PROTO(PRIVATE void process_attrs,(Token *t,Token *dim_bounds));
PROTO(PRIVATE void give_kind_warning,(Token *t));
#ifdef DEBUG_PARSER
PROTO(PRIVATE void print_exprlist,( char *s, Token *t ));
PROTO(PRIVATE void print_comlist,( char *s, Token *t ));
#endif

		/* Uses of Token fields for nonterminals: */
/* NOTE: As of Aug 1994 these are undergoing revision to separate the
         use of class, subclass fields */
/*
  1. dim_bound_lists: dimensioning info for arrays:
       token.class = no. of dimensions,  --> TOK_dims
       token.subclass = no. of elements  --> TOK_elts
  2. expressions
       token.value.integer = hash index (of identifier)
       token.TOK_type = type_byte = storage_class << 4 + datatype
       token.TOK_flags: CONST_EXPR, LVALUE_EXPR, etc.
       token.TOK_flags: COMMA_FLAG used to handle extra/missing commas
  4. substring_interval
       token.class = start index  --> TOK_start
       token.subclass = end index --> TOK_end
*/


#ifndef YYSTYPE
# define YYSTYPE int
# define YYSTYPE_IS_TRIVIAL 1
#endif
#ifndef YYDEBUG
# define YYDEBUG 0
#endif



#define	YYFINAL		970
#define	YYFLAG		-32768
#define	YYNTBASE	108

/* YYTRANSLATE(YYLEX) -- Bison token number corresponding to YYLEX. */
#define YYTRANSLATE(x) ((unsigned)(x) <= 347 ? yytranslate[x] : 400)

/* YYTRANSLATE[YYLEX] -- Bison token number corresponding to YYLEX. */
static const char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,   104,     2,     2,     2,
      96,    95,    98,   105,    97,   102,   103,   100,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    99,     2,
     106,   101,   107,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    93,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    94
};

#if YYDEBUG
static const short yyprhs[] =
{
       0,     0,     2,     3,     5,     8,    10,    12,    14,    16,
      18,    21,    23,    25,    27,    29,    31,    34,    36,    38,
      40,    42,    44,    47,    49,    51,    54,    57,    61,    63,
      65,    67,    69,    73,    75,    77,    79,    81,    83,    85,
      87,    89,    91,    93,    95,    97,    99,   101,   103,   105,
     107,   109,   111,   113,   115,   117,   119,   121,   123,   125,
     127,   129,   131,   133,   135,   137,   139,   141,   143,   145,
     147,   149,   151,   153,   155,   157,   159,   161,   163,   165,
     167,   169,   171,   173,   175,   177,   179,   181,   183,   185,
     187,   189,   190,   195,   199,   206,   208,   212,   219,   223,
     230,   233,   235,   237,   239,   241,   243,   245,   249,   256,
     258,   259,   261,   263,   267,   269,   271,   274,   278,   280,
     284,   286,   290,   295,   297,   301,   303,   307,   309,   313,
     314,   319,   323,   329,   333,   337,   339,   341,   343,   348,
     351,   354,   358,   362,   367,   369,   372,   375,   379,   382,
     384,   386,   389,   391,   394,   396,   398,   402,   404,   407,
     410,   414,   416,   419,   421,   424,   428,   432,   436,   441,
     447,   453,   455,   459,   461,   465,   467,   471,   473,   477,
     482,   484,   486,   488,   490,   492,   496,   501,   503,   505,
     507,   509,   511,   513,   515,   517,   519,   523,   525,   528,
     531,   532,   537,   539,   541,   545,   547,   548,   549,   556,
     557,   562,   564,   565,   566,   573,   575,   577,   581,   583,
     584,   585,   592,   596,   598,   602,   603,   604,   611,   612,
     613,   622,   624,   628,   630,   634,   638,   640,   641,   646,
     647,   653,   655,   659,   661,   665,   667,   671,   673,   677,
     679,   683,   685,   687,   689,   691,   697,   698,   699,   705,
     707,   711,   712,   717,   721,   723,   727,   731,   733,   737,
     741,   743,   747,   753,   755,   757,   759,   762,   766,   768,
     772,   774,   778,   782,   784,   787,   791,   792,   793,   800,
     802,   806,   808,   810,   812,   816,   818,   822,   824,   826,
     828,   830,   832,   836,   838,   840,   848,   852,   858,   859,
     860,   867,   869,   871,   873,   875,   882,   887,   894,   902,
     906,   913,   921,   923,   926,   931,   942,   945,   949,   951,
     954,   955,   961,   962,   963,   972,   975,   979,   982,   986,
     989,   993,   994,  1001,  1003,  1006,  1009,  1013,  1018,  1020,
    1024,  1026,  1030,  1033,  1036,  1038,  1041,  1045,  1048,  1052,
    1058,  1059,  1066,  1067,  1075,  1078,  1080,  1083,  1087,  1092,
    1095,  1099,  1105,  1108,  1112,  1115,  1117,  1119,  1122,  1126,
    1129,  1133,  1137,  1141,  1142,  1144,  1146,  1148,  1149,  1153,
    1154,  1159,  1160,  1166,  1172,  1179,  1187,  1191,  1197,  1199,
    1203,  1209,  1213,  1214,  1215,  1223,  1227,  1228,  1229,  1237,
    1239,  1243,  1247,  1249,  1251,  1255,  1259,  1263,  1265,  1267,
    1271,  1273,  1275,  1283,  1284,  1291,  1292,  1299,  1300,  1307,
    1311,  1317,  1319,  1323,  1329,  1331,  1335,  1341,  1343,  1345,
    1347,  1349,  1351,  1352,  1359,  1360,  1362,  1364,  1367,  1369,
    1371,  1373,  1377,  1379,  1381,  1383,  1385,  1387,  1389,  1391,
    1393,  1395,  1397,  1399,  1401,  1403,  1406,  1409,  1410,  1411,
    1417,  1422,  1423,  1425,  1427,  1431,  1433,  1434,  1438,  1439,
    1445,  1446,  1453,  1456,  1458,  1462,  1464,  1468,  1471,  1475,
    1480,  1482,  1483,  1485,  1487,  1491,  1493,  1495,  1497,  1501,
    1505,  1507,  1511,  1513,  1517,  1519,  1522,  1524,  1528,  1530,
    1533,  1536,  1540,  1544,  1546,  1550,  1554,  1556,  1560,  1562,
    1566,  1568,  1570,  1572,  1574,  1576,  1580,  1582,  1584,  1586,
    1588,  1590,  1592,  1594,  1596,  1598,  1600,  1602,  1604,  1606,
    1608,  1613,  1618,  1620,  1624,  1626,  1629,  1632,  1635,  1638,
    1641,  1645,  1650,  1655,  1661,  1663,  1665,  1667,  1669,  1671,
    1673,  1675,  1678,  1680,  1682,  1684,  1687,  1690,  1692,  1694,
    1696,  1698,  1699
};
static const short yyrhs[] =
{
     109,     0,     0,   110,     0,   109,   110,     0,   111,     0,
     120,     0,    93,     0,   112,     0,   115,     0,     5,   113,
       0,   113,     0,   114,     0,   121,     0,   124,     0,   129,
       0,     1,    93,     0,   133,     0,   136,     0,   142,     0,
     148,     0,   116,     0,     5,   116,     0,   117,     0,   118,
       0,    47,    93,     0,   119,    93,     0,   119,   393,    93,
       0,    48,     0,    51,     0,    53,     0,    55,     0,    65,
      13,    93,     0,   122,     0,   219,     0,   208,     0,   239,
       0,   123,     0,   135,     0,   335,     0,   150,     0,   155,
       0,   162,     0,   169,     0,   175,     0,   176,     0,   227,
       0,   229,     0,   236,     0,   231,     0,   125,     0,   126,
       0,   259,     0,   261,     0,   264,     0,   294,     0,   297,
       0,   359,     0,   254,     0,   258,     0,   352,     0,   260,
       0,   293,     0,   298,     0,   127,     0,   305,     0,   307,
       0,   300,     0,   308,     0,   311,     0,   321,     0,   323,
       0,   325,     0,   128,     0,   331,     0,   327,     0,   329,
       0,   130,     0,   131,     0,   276,     0,   132,     0,   265,
       0,   266,     0,   286,     0,   292,     0,   270,     0,   274,
       0,   275,     0,   279,     0,   284,     0,   285,     0,     0,
      77,   134,   393,    93,     0,    56,   393,    93,     0,    56,
     393,    96,   145,    95,    93,     0,   137,     0,   138,   393,
      93,     0,   138,   393,    96,   145,    95,    93,     0,   139,
     393,    93,     0,   139,   393,    96,   145,    95,    93,     0,
     141,   140,     0,   140,     0,    61,     0,   181,     0,   185,
       0,   186,     0,   143,     0,   144,   393,    93,     0,   144,
     393,    96,   145,    95,    93,     0,    85,     0,     0,   146,
       0,   147,     0,   146,    97,   147,     0,   393,     0,    98,
       0,   149,    93,     0,   149,   393,    93,     0,    29,     0,
      41,   151,    93,     0,   152,     0,   151,    97,   152,     0,
     393,    96,   153,    95,     0,   154,     0,   153,    97,   154,
       0,   381,     0,   381,    99,   381,     0,    98,     0,   381,
      99,    98,     0,     0,    57,   156,   157,    93,     0,    96,
     158,    95,     0,   157,    97,    96,   158,    95,     0,   159,
      97,   159,     0,   158,    97,   159,     0,   393,     0,   160,
       0,   161,     0,   393,    96,   384,    95,     0,   393,   388,
       0,   160,   388,     0,    36,   166,    93,     0,    36,   163,
      93,     0,    36,   166,   163,    93,     0,   164,     0,   163,
     164,     0,   165,   166,     0,   100,   393,   100,     0,   100,
     100,     0,    24,     0,   167,     0,   166,   167,     0,   168,
       0,   168,    97,     0,   393,     0,   152,     0,    70,   170,
      93,     0,   171,     0,   170,   171,     0,   172,   173,     0,
     100,   393,   100,     0,   174,     0,   173,   174,     0,   393,
       0,   393,    97,     0,   181,   190,    93,     0,   185,   198,
      93,     0,   186,   198,    93,     0,   186,    97,   198,    93,
       0,   177,    99,    99,   190,    93,     0,   178,    99,    99,
     198,    93,     0,   181,     0,   181,    97,   179,     0,   185,
       0,   185,    97,   179,     0,   186,     0,   186,    97,   179,
       0,   180,     0,   179,    97,   180,     0,    41,    96,   153,
      95,     0,    82,     0,    58,     0,    68,     0,    73,     0,
     182,     0,   182,    98,   397,     0,   182,   189,   184,    95,
       0,   183,     0,    67,     0,    79,     0,    37,     0,    69,
       0,    44,     0,    43,     0,    30,     0,   365,     0,   393,
     101,   365,     0,    34,     0,   185,   187,     0,    98,   215,
       0,     0,   189,   188,   216,    95,     0,    25,     0,   191,
       0,   190,    97,   191,     0,   197,     0,     0,     0,   197,
     100,   192,   246,   193,   100,     0,     0,   197,   194,   101,
     364,     0,   152,     0,     0,     0,   152,   100,   195,   246,
     196,   100,     0,   393,     0,   199,     0,   198,    97,   199,
       0,   206,     0,     0,     0,   206,   100,   200,   246,   201,
     100,     0,   206,   101,   364,     0,   152,     0,   152,    98,
     215,     0,     0,     0,   152,   100,   202,   246,   203,   100,
       0,     0,     0,   152,    98,   215,   100,   204,   246,   205,
     100,     0,   393,     0,   393,    98,   215,     0,    64,     0,
     207,   209,    93,     0,   207,    71,    93,     0,   211,     0,
       0,   209,    97,   210,   211,     0,     0,   141,    96,   212,
     213,    95,     0,   214,     0,   213,    97,   214,     0,    16,
       0,    16,   102,    16,     0,   397,     0,    96,   218,    95,
       0,   217,     0,   216,    97,   217,     0,   218,     0,   393,
     101,   218,     0,    98,     0,   380,     0,   220,     0,   221,
       0,    73,    96,   224,    95,    93,     0,     0,     0,    73,
     222,   224,   223,    93,     0,   225,     0,   224,    97,   225,
       0,     0,   393,   226,   101,   364,     0,    58,   228,    93,
       0,   393,     0,   228,    97,   393,     0,    68,   230,    93,
       0,   393,     0,   230,    97,   393,     0,    75,   232,    93,
       0,   233,     0,   232,    97,   233,     0,    96,   234,    97,
     235,    95,     0,   393,     0,   393,     0,   152,     0,    82,
      93,     0,    82,   237,    93,     0,   238,     0,   237,    97,
     238,     0,   393,     0,   100,   393,   100,     0,    40,   240,
      93,     0,   241,     0,   240,   241,     0,   240,    97,   241,
       0,     0,     0,   244,   100,   242,   246,   243,   100,     0,
     245,     0,   244,    97,   245,     0,   257,     0,   252,     0,
     247,     0,   246,    97,   247,     0,   249,     0,   248,    98,
     249,     0,   397,     0,   393,     0,   396,     0,   393,     0,
     251,     0,   250,    97,   251,     0,   382,     0,   252,     0,
      96,   250,    97,   393,   101,   253,    95,     0,   380,    97,
     380,     0,   380,    97,   380,    97,   380,     0,     0,     0,
     257,   101,   255,   365,   256,    93,     0,   390,     0,   382,
       0,   387,     0,   348,     0,    27,   398,   399,    87,   390,
      93,     0,   262,   398,   399,    93,     0,   262,    96,   263,
      95,   378,    93,     0,   262,    96,   263,    95,    97,   378,
      93,     0,   262,   393,    93,     0,   262,   393,    96,   263,
      95,    93,     0,   262,   393,    97,    96,   263,    95,    93,
       0,    62,     0,   398,   399,     0,   263,    97,   398,   399,
       0,   267,   398,   399,    97,   398,   399,    97,   398,   399,
      93,     0,   267,   124,     0,   267,    86,    93,     0,   268,
       0,   394,   268,     0,     0,    63,    96,   269,   365,    95,
       0,     0,     0,    46,    63,    96,   271,   365,    95,   272,
     273,     0,    86,    93,     0,    86,   395,    93,     0,    46,
      93,     0,    46,   395,    93,     0,    52,    93,     0,    52,
     395,    93,     0,     0,   278,    96,   277,   365,    95,    93,
       0,    83,     0,   394,    83,     0,   280,    93,     0,   280,
     395,    93,     0,    32,    96,   281,    95,     0,   282,     0,
     281,    97,   282,     0,   283,     0,   283,    99,   283,     0,
      99,   283,     0,   283,    99,     0,   365,     0,    33,    93,
       0,    33,   395,    93,     0,    54,    93,     0,    54,   395,
      93,     0,   289,   390,   101,   291,    93,     0,     0,    45,
      96,   287,   365,    95,    93,     0,     0,   289,    89,    96,
     288,   365,    95,    93,     0,   289,    93,     0,   290,     0,
     394,   290,     0,    42,   398,   399,     0,    42,   398,   399,
      97,     0,    42,   398,     0,   379,    97,   379,     0,   379,
      97,   379,    97,   379,     0,    49,    93,     0,    49,   395,
      93,     0,    38,    93,     0,   295,     0,   296,     0,    39,
      93,     0,    39,   395,    93,     0,    59,    93,     0,    59,
     395,    93,     0,    84,   299,    93,     0,    74,   299,    93,
       0,     0,     6,     0,   393,     0,    13,     0,     0,   303,
     301,    93,     0,     0,   303,   318,   302,    93,     0,     0,
      90,   304,    96,   314,    95,     0,   306,    96,   314,    95,
      93,     0,   306,    96,   314,    95,   318,    93,     0,   306,
      96,   314,    95,    97,   318,    93,     0,   306,   334,    93,
       0,   306,   334,    97,   318,    93,     0,    78,     0,    26,
     334,    93,     0,    26,   334,    97,   318,    93,     0,    76,
     334,    93,     0,     0,     0,    76,   334,    97,   309,   318,
     310,    93,     0,    88,   334,    93,     0,     0,     0,    88,
     334,    97,   312,   318,   313,    93,     0,   315,     0,   314,
      97,   315,     0,   393,   101,   333,     0,   333,     0,   333,
       0,   393,   101,   333,     0,   316,    97,   317,     0,   393,
     101,   333,     0,   393,     0,   319,     0,   318,    97,   319,
       0,   365,     0,   320,     0,    96,   318,    97,   390,   101,
     291,    95,     0,     0,    72,   322,    96,   316,    95,    93,
       0,     0,    35,   324,    96,   314,    95,    93,     0,     0,
      66,   326,    96,   314,    95,    93,     0,   328,   333,    93,
       0,   328,    96,   314,    95,    93,     0,    28,     0,   330,
     333,    93,     0,   330,    96,   314,    95,    93,     0,    50,
       0,   332,   333,    93,     0,   332,    96,   314,    95,    93,
       0,    81,     0,   365,     0,    98,     0,   374,     0,    98,
       0,     0,    60,   336,    96,   337,    95,    93,     0,     0,
     338,     0,   339,     0,   338,   339,     0,   340,     0,   341,
       0,   342,     0,    96,   338,    95,     0,    15,     0,    13,
       0,    14,     0,   344,     0,   345,     0,    97,     0,   100,
       0,    24,     0,    99,     0,   103,     0,   343,     0,   104,
       0,     6,     0,   102,     6,     0,   105,     6,     0,     0,
       0,   106,   346,   378,   347,   107,     0,   391,    96,   349,
      95,     0,     0,   350,     0,   351,     0,   350,    97,   351,
       0,   390,     0,     0,   356,   353,    93,     0,     0,   356,
      96,    95,   354,    93,     0,     0,   356,    96,   357,    95,
     355,    93,     0,    31,   393,     0,   358,     0,   357,    97,
     358,     0,   365,     0,    98,   398,   399,     0,    80,    93,
       0,    80,   378,    93,     0,   361,    96,   362,    95,     0,
     391,     0,     0,   363,     0,   365,     0,   363,    97,   365,
       0,   365,     0,   366,     0,   367,     0,   365,    20,   367,
       0,   365,    21,   367,     0,   368,     0,   367,    19,   368,
       0,   369,     0,   368,    18,   369,     0,   370,     0,    22,
     370,     0,   371,     0,   370,    17,   370,     0,   372,     0,
     102,   372,     0,   105,   372,     0,   371,   105,   372,     0,
     371,   102,   372,     0,   373,     0,   372,   100,   373,     0,
     372,    98,   373,     0,   374,     0,   374,    23,   373,     0,
     375,     0,   374,    24,   375,     0,   390,     0,   383,     0,
     360,     0,   386,     0,   376,     0,    96,   365,    95,     0,
     377,     0,    13,     0,    14,     0,    12,     0,     6,     0,
       7,     0,     8,     0,     9,     0,    10,     0,    11,     0,
     371,     0,   371,     0,   371,     0,   371,     0,   392,    96,
     384,    95,     0,   392,    96,   384,    95,     0,   385,     0,
     384,    97,   385,     0,   365,     0,   361,   388,     0,   360,
     388,     0,   383,   388,     0,   391,   388,     0,   382,   388,
       0,    96,    99,    95,     0,    96,   389,    99,    95,     0,
      96,    99,   389,    95,     0,    96,   389,    99,   389,    95,
       0,   371,     0,   391,     0,   392,     0,     3,     0,     4,
       0,     3,     0,     4,     0,   395,    99,     0,     3,     0,
       4,     0,   377,     0,   102,   377,     0,   105,   377,     0,
      12,     0,    13,     0,    14,     0,     6,     0,     0,     6,
       0
};

#endif

#if YYDEBUG
/* YYRLINE[YYN] -- source line where rule number YYN was defined. */
static const short yyrline[] =
{
       0,   362,   363,   366,   367,   371,   428,   429,   437,   438,
     445,   483,   502,   511,   516,   532,   540,   557,   561,   565,
     569,   575,   576,   593,   598,   601,   602,   605,   611,   612,
     613,   614,   617,   636,   646,   662,   668,   681,   689,   694,
     700,   701,   702,   703,   704,   705,   706,   707,   708,   709,
     714,   720,   726,   727,   734,   741,   746,   747,   750,   751,
     752,   753,   754,   755,   756,   762,   772,   773,   780,   781,
     782,   797,   798,   799,   806,   807,   808,   811,   816,   821,
     826,   833,   835,   844,   863,   878,   883,   888,   894,   899,
     904,   911,   911,   929,   934,   946,   950,   967,   983,  1000,
    1018,  1024,  1027,  1033,  1034,  1035,  1042,  1046,  1057,  1075,
    1081,  1085,  1088,  1092,  1098,  1103,  1116,  1133,  1146,  1153,
    1156,  1157,  1161,  1167,  1174,  1182,  1190,  1202,  1207,  1216,
    1216,  1220,  1221,  1224,  1228,  1235,  1239,  1243,  1249,  1253,
    1254,  1258,  1272,  1280,  1298,  1302,  1310,  1323,  1328,  1332,
    1338,  1343,  1356,  1361,  1368,  1373,  1386,  1398,  1399,  1405,
    1412,  1418,  1422,  1435,  1441,  1450,  1451,  1452,  1453,  1457,
    1465,  1475,  1476,  1479,  1480,  1481,  1482,  1486,  1487,  1490,
    1497,  1501,  1505,  1509,  1516,  1523,  1552,  1561,  1567,  1572,
    1577,  1582,  1589,  1596,  1606,  1622,  1631,  1653,  1666,  1689,
    1700,  1700,  1729,  1736,  1737,  1742,  1752,  1752,  1752,  1770,
    1770,  1791,  1802,  1802,  1802,  1823,  1833,  1834,  1837,  1845,
    1845,  1845,  1859,  1873,  1883,  1896,  1896,  1896,  1915,  1915,
    1915,  1939,  1949,  1966,  1969,  1982,  1998,  1999,  1999,  2005,
    2005,  2009,  2010,  2013,  2033,  2062,  2069,  2100,  2104,  2110,
    2127,  2153,  2160,  2176,  2177,  2180,  2183,  2183,  2183,  2193,
    2194,  2197,  2197,  2208,  2211,  2215,  2222,  2225,  2229,  2236,
    2244,  2245,  2248,  2251,  2258,  2265,  2273,  2277,  2280,  2281,
    2284,  2288,  2296,  2299,  2300,  2301,  2304,  2304,  2304,  2311,
    2312,  2315,  2319,  2322,  2326,  2332,  2336,  2348,  2349,  2355,
    2356,  2363,  2364,  2367,  2371,  2374,  2381,  2382,  2387,  2387,
    2387,  2429,  2430,  2431,  2432,  2438,  2453,  2462,  2463,  2467,
    2471,  2475,  2481,  2492,  2496,  2503,  2520,  2530,  2549,  2553,
    2564,  2564,  2579,  2579,  2579,  2597,  2601,  2611,  2615,  2625,
    2629,  2646,  2646,  2663,  2674,  2683,  2684,  2687,  2690,  2691,
    2694,  2695,  2708,  2716,  2726,  2743,  2744,  2747,  2748,  2760,
    2809,  2809,  2823,  2823,  2833,  2840,  2849,  2860,  2867,  2873,
    2881,  2885,  2891,  2895,  2899,  2904,  2911,  2920,  2921,  2924,
    2925,  2929,  2933,  2942,  2943,  2944,  2948,  2952,  2952,  2954,
    2954,  2958,  2958,  2967,  2968,  2969,  2970,  2974,  2979,  2982,
    2988,  2997,  3001,  3001,  3001,  3009,  3015,  3015,  3015,  3026,
    3030,  3052,  3056,  3119,  3131,  3136,  3142,  3146,  3153,  3154,
    3157,  3179,  3183,  3198,  3198,  3203,  3203,  3208,  3208,  3213,
    3221,  3223,  3227,  3235,  3237,  3241,  3249,  3251,  3259,  3260,
    3264,  3282,  3286,  3286,  3293,  3294,  3298,  3299,  3302,  3303,
    3304,  3307,  3308,  3311,  3312,  3319,  3320,  3323,  3324,  3325,
    3326,  3327,  3328,  3335,  3338,  3339,  3340,  3344,  3344,  3344,
    3354,  3369,  3373,  3376,  3380,  3387,  3391,  3391,  3397,  3397,
    3403,  3403,  3414,  3420,  3425,  3431,  3438,  3447,  3451,  3487,
    3515,  3525,  3531,  3534,  3539,  3548,  3577,  3592,  3594,  3599,
    3606,  3608,  3615,  3617,  3624,  3626,  3632,  3634,  3642,  3644,
    3648,  3652,  3657,  3664,  3666,  3676,  3683,  3685,  3692,  3694,
    3701,  3705,  3707,  3709,  3711,  3721,  3745,  3747,  3752,  3761,
    3768,  3773,  3778,  3783,  3792,  3797,  3805,  3819,  3839,  3863,
    3897,  3913,  3929,  3933,  3939,  3953,  3968,  3976,  3985,  3991,
    4000,  4014,  4024,  4034,  4046,  4067,  4072,  4075,  4082,  4091,
    4092,  4096,  4104,  4108,  4116,  4117,  4118,  4119,  4124,  4129,
    4139,  4158,  4165
};
#endif


#if (YYDEBUG) || defined YYERROR_VERBOSE

/* YYTNAME[TOKEN_NUM] -- String name of the token TOKEN_NUM. */
static const char *const yytname[] =
{
  "$", "error", "$undefined.", "identifier", "array identifier", 
  "label", "integer const", "real const", "dp const", 
  "quad const", "complex const", "dcomplex const", 
  "logical const", "string", "hollerith", 
  "edit descriptor", "letter", "relop", "AND", "OR", 
  "EQV", "NEQV", "NOT", "power", "concat", 
  "lparen", "ACCEPT", "ASSIGN", "BACKSPACE", 
  "BLOCKDATA", "BYTE", "CALL", "CASE", "CASEDEFAULT", 
  "CHARACTER", "CLOSE", "COMMON", "COMPLEX", 
  "CONTINUE", "CYCLE", "DATA", "DIMENSION", "DO", 
  "DOUBLECOMPLEX", "DOUBLEPRECISION", "DOWHILE", "ELSE", 
  "END", "ENDBLOCKDATA", "ENDDO", "ENDFILE", 
  "ENDFUNCTION", "ENDIF", "ENDPROGRAM", "ENDSELECT", 
  "ENDSUBROUTINE", "ENTRY", "EQUIVALENCE", "EXTERNAL", 
  "EXIT", "FORMAT", "FUNCTION", "GOTO", "IF", 
  "IMPLICIT", "INCLUDE", "INQUIRE", "INTEGER", 
  "INTRINSIC", "LOGICAL", "NAMELIST", "NONE", "OPEN", 
  "PARAMETER", "PAUSE", "POINTER", "PRINT", "PROGRAM", 
  "READ", "REAL", "RETURN", "REWIND", "SAVE", 
  "SELECTCASE", "STOP", "SUBROUTINE", "THEN", "TO", 
  "TYPE", "WHILE", "WRITE", "illegal", "empty", "end of statement", 
  "REDUCE", "')'", "'('", "','", "'*'", "':'", "'/'", "'='", "'-'", "'.'", 
  "'$'", "'+'", "'<'", "'>'", "prog body", "stmt list", "stmt list item", 
  "ordinary stmt", "stmt", "unlabeled stmt", "subprogram header", 
  "end stmt", "unlabeled end stmt", "unnamed end stmt", "named end stmt", 
  "end subprog token", "include stmt", "specification stmt", 
  "anywhere stmt", "specif stmt", "executable stmt", "transfer stmt", 
  "nontransfer stmt", "io stmt", "io positioning stmt", "restricted stmt", 
  "restricted nontransfer stmt", "else or endif stmt", 
  "case or endselect stmt", "prog stmt", "@1", "entry stmt", 
  "function stmt", "unlabeled function stmt", "typed function handle", 
  "plain function handle", "function keyword", "type name", 
  "subroutine stmt", "unlabeled subroutine stmt", "subroutine handle", 
  "dummy argument list", "non empty arg list", "dummy argument", 
  "block data stmt", "block data handle", "dimension stmt", 
  "array declarator list", "array declarator", "dim bound list", 
  "dim bound item", "equivalence stmt", "@2", "equivalence list", 
  "equivalence list item", "equiv entity", "array equiv name", 
  "substring equiv name", "common stmt", "common block list", 
  "labeled common block", "common block name", "common variable list", 
  "common list item", "common entity", "namelist stmt", "namelist list", 
  "namelist decl", "namelist name", "namelist var list", "namelist item", 
  "type stmt", "attrbased type stmt", "arith attrbased type handle", 
  "char attrbased type handle", "attr list", "type attr", 
  "arith type name", "sizeable type name", "unsizeable type name", 
  "kind selector", "plain char type name", "char type name", 
  "char selector", "@3", "left paren", "arith type decl list", 
  "arith type decl item", "@4", "@5", "@6", "@7", "@8", 
  "scalar type decl entity", "char type decl list", "char type decl item", 
  "@9", "@10", "@11", "@12", "@13", "@14", "char type decl entity", 
  "implicit handle", "implicit stmt", "implicit decl list", "@15", 
  "implicit decl item", "@16", "letter list", "letter list item", 
  "len specification", "len spec list", "len spec item", "len spec expr", 
  "parameter stmt", "std parameter stmt", "parenless parameter stmt", 
  "@17", "@18", "parameter defn list", "parameter defn item", "@19", 
  "external stmt", "external name list", "intrinsic stmt", 
  "intrinsic name list", "pointer stmt", "pointer item list", 
  "pointer item", "pointer name", "pointee name", "save stmt", 
  "save list", "save item", "data stmt", "data defn list", 
  "data defn item", "@20", "@21", "data defn assignee list", 
  "data defn assignee", "data value list", "data value", 
  "data repeat factor", "data constant value", "data dlist", 
  "data dlist item", "data implied do list", "data do loop bounds", 
  "assignment stmt", "@22", "@23", "lvalue", "assign stmt", 
  "unconditional goto", "computed goto", "assigned goto", "goto", 
  "goto list", "arithmetic if stmt", "logical if stmt", "block if stmt", 
  "if handle", "f77 if handle", "@24", "else if stmt", "@25", "@26", 
  "else if then", "else stmt", "end if stmt", "select case stmt", "@27", 
  "select handle", "case stmt", "case handle", "case values", 
  "case value", "case value primary", "case default stmt", 
  "end select stmt", "do stmt", "@28", "@29", "do handle", 
  "f77 do handle", "do loop bounds", "enddo stmt", "continue stmt", 
  "cycle or exit stmt", "cycle stmt", "exit stmt", "stop stmt", 
  "pause stmt", "stop info", "write stmt", "@30", "@31", "write handle", 
  "@32", "read stmt", "read handle", "accept stmt", "print stmt", "@33", 
  "@34", "type output stmt", "@35", "@36", "control info list", 
  "control info item", "open info list", "open info item", "io list", 
  "io item", "io implied do list", "open stmt", "@37", "close stmt", 
  "@38", "inquire stmt", "@39", "backspace stmt", "backspace handle", 
  "endfile stmt", "endfile handle", "rewind stmt", "rewind handle", 
  "unit id", "format id", "format stmt", "@40", "format spec", 
  "nonempty format spec", "fmt spec item", "repeatable fmt item", 
  "unrepeatable fmt item", "fmt item separator", "nonstandard fmt item", 
  "repeat spec", "variable fmt item", "@41", "@42", 
  "stmt function handle", "stmt function dummy list", 
  "nonempty stmt fun dummy list", "stmt function dummy arg", "call stmt", 
  "@43", "@44", "@45", "call handle", "subr arg list", "subr arg", 
  "return stmt", "function reference", "fun or substr handle", 
  "fun arg list", "nonempty fun arg list", "parameter expr", "expr", 
  "log expr", "log disjunct", "log term", "log factor", "log primary", 
  "arith expr", "term", "factor", "char expr", "primary", "literal const", 
  "numeric const", "integer expr", "int real dp expr", 
  "int constant expr", "dim bound expr", "array element lvalue", 
  "array element name", "subscript list", "subscript", "substring name", 
  "substring lvalue", "substring interval", "substr index expr", 
  "variable name", "scalar name", "array name", "symbolic name", 
  "construct spec", "construct name", "data constant", 
  "nonzero unsigned int const", "pre label", "label", 0
};
#endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives. */
static const short yyr1[] =
{
       0,   108,   108,   109,   109,   110,   110,   110,   111,   111,
     112,   112,   113,   113,   113,   113,   113,   114,   114,   114,
     114,   115,   115,   116,   116,   117,   117,   118,   119,   119,
     119,   119,   120,   121,   121,   121,   121,   121,   122,   122,
     123,   123,   123,   123,   123,   123,   123,   123,   123,   123,
     124,   124,   125,   125,   125,   125,   125,   125,   126,   126,
     126,   126,   126,   126,   126,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   128,   128,   128,   129,   129,   129,
     129,   130,   130,   130,   130,   131,   131,   131,   132,   132,
     132,   134,   133,   135,   135,   136,   137,   137,   137,   137,
     138,   139,   140,   141,   141,   141,   142,   143,   143,   144,
     145,   145,   146,   146,   147,   147,   148,   148,   149,   150,
     151,   151,   152,   153,   153,   154,   154,   154,   154,   156,
     155,   157,   157,   158,   158,   159,   159,   159,   160,   161,
     161,   162,   162,   162,   163,   163,   164,   165,   165,   165,
     166,   166,   167,   167,   168,   168,   169,   170,   170,   171,
     172,   173,   173,   174,   174,   175,   175,   175,   175,   176,
     176,   177,   177,   178,   178,   178,   178,   179,   179,   180,
     180,   180,   180,   180,   181,   181,   181,   181,   182,   182,
     182,   182,   183,   183,   183,   184,   184,   185,   186,   187,
     188,   187,   189,   190,   190,   191,   192,   193,   191,   194,
     191,   191,   195,   196,   191,   197,   198,   198,   199,   200,
     201,   199,   199,   199,   199,   202,   203,   199,   204,   205,
     199,   206,   206,   207,   208,   208,   209,   210,   209,   212,
     211,   213,   213,   214,   214,   215,   215,   216,   216,   217,
     217,   218,   218,   219,   219,   220,   222,   223,   221,   224,
     224,   226,   225,   227,   228,   228,   229,   230,   230,   231,
     232,   232,   233,   234,   235,   235,   236,   236,   237,   237,
     238,   238,   239,   240,   240,   240,   242,   243,   241,   244,
     244,   245,   245,   246,   246,   247,   247,   248,   248,   249,
     249,   250,   250,   251,   251,   252,   253,   253,   255,   256,
     254,   257,   257,   257,   257,   258,   259,   260,   260,   261,
     261,   261,   262,   263,   263,   264,   265,   266,   267,   267,
     269,   268,   271,   272,   270,   273,   273,   274,   274,   275,
     275,   277,   276,   278,   278,   279,   279,   280,   281,   281,
     282,   282,   282,   282,   283,   284,   284,   285,   285,   286,
     287,   286,   288,   286,   286,   289,   289,   290,   290,   290,
     291,   291,   292,   292,   293,   294,   294,   295,   295,   296,
     296,   297,   298,   299,   299,   299,   299,   301,   300,   302,
     300,   304,   303,   305,   305,   305,   305,   305,   306,   307,
     307,   308,   309,   310,   308,   311,   312,   313,   311,   314,
     314,   315,   315,   316,   316,   316,   317,   317,   318,   318,
     319,   319,   320,   322,   321,   324,   323,   326,   325,   327,
     327,   328,   329,   329,   330,   331,   331,   332,   333,   333,
     334,   334,   336,   335,   337,   337,   338,   338,   339,   339,
     339,   340,   340,   341,   341,   341,   341,   342,   342,   342,
     342,   342,   342,   343,   344,   344,   344,   346,   347,   345,
     348,   349,   349,   350,   350,   351,   353,   352,   354,   352,
     355,   352,   356,   357,   357,   358,   358,   359,   359,   360,
     361,   362,   362,   363,   363,   364,   365,   366,   366,   366,
     367,   367,   368,   368,   369,   369,   370,   370,   371,   371,
     371,   371,   371,   372,   372,   372,   373,   373,   374,   374,
     375,   375,   375,   375,   375,   375,   376,   376,   376,   376,
     377,   377,   377,   377,   377,   377,   378,   379,   380,   381,
     382,   383,   384,   384,   385,   386,   386,   386,   387,   387,
     388,   388,   388,   388,   389,   390,   390,   391,   392,   393,
     393,   394,   395,   395,   396,   396,   396,   396,   396,   396,
     397,   398,   399
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN. */
static const short yyr2[] =
{
       0,     1,     0,     1,     2,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     2,     1,     1,     2,     2,     3,     1,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     3,     6,     1,     3,     6,     3,     6,
       2,     1,     1,     1,     1,     1,     1,     3,     6,     1,
       0,     1,     1,     3,     1,     1,     2,     3,     1,     3,
       1,     3,     4,     1,     3,     1,     3,     1,     3,     0,
       4,     3,     5,     3,     3,     1,     1,     1,     4,     2,
       2,     3,     3,     4,     1,     2,     2,     3,     2,     1,
       1,     2,     1,     2,     1,     1,     3,     1,     2,     2,
       3,     1,     2,     1,     2,     3,     3,     3,     4,     5,
       5,     1,     3,     1,     3,     1,     3,     1,     3,     4,
       1,     1,     1,     1,     1,     3,     4,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     2,     2,
       0,     4,     1,     1,     3,     1,     0,     0,     6,     0,
       4,     1,     0,     0,     6,     1,     1,     3,     1,     0,
       0,     6,     3,     1,     3,     0,     0,     6,     0,     0,
       8,     1,     3,     1,     3,     3,     1,     0,     4,     0,
       5,     1,     3,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     1,     1,     1,     5,     0,     0,     5,     1,
       3,     0,     4,     3,     1,     3,     3,     1,     3,     3,
       1,     3,     5,     1,     1,     1,     2,     3,     1,     3,
       1,     3,     3,     1,     2,     3,     0,     0,     6,     1,
       3,     1,     1,     1,     3,     1,     3,     1,     1,     1,
       1,     1,     3,     1,     1,     7,     3,     5,     0,     0,
       6,     1,     1,     1,     1,     6,     4,     6,     7,     3,
       6,     7,     1,     2,     4,    10,     2,     3,     1,     2,
       0,     5,     0,     0,     8,     2,     3,     2,     3,     2,
       3,     0,     6,     1,     2,     2,     3,     4,     1,     3,
       1,     3,     2,     2,     1,     2,     3,     2,     3,     5,
       0,     6,     0,     7,     2,     1,     2,     3,     4,     2,
       3,     5,     2,     3,     2,     1,     1,     2,     3,     2,
       3,     3,     3,     0,     1,     1,     1,     0,     3,     0,
       4,     0,     5,     5,     6,     7,     3,     5,     1,     3,
       5,     3,     0,     0,     7,     3,     0,     0,     7,     1,
       3,     3,     1,     1,     3,     3,     3,     1,     1,     3,
       1,     1,     7,     0,     6,     0,     6,     0,     6,     3,
       5,     1,     3,     5,     1,     3,     5,     1,     1,     1,
       1,     1,     0,     6,     0,     1,     1,     2,     1,     1,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     0,     0,     5,
       4,     0,     1,     1,     3,     1,     0,     3,     0,     5,
       0,     6,     2,     1,     3,     1,     3,     2,     3,     4,
       1,     0,     1,     1,     3,     1,     1,     1,     3,     3,
       1,     3,     1,     3,     1,     2,     1,     3,     1,     2,
       2,     3,     3,     1,     3,     3,     1,     3,     1,     3,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     4,     1,     3,     1,     2,     2,     2,     2,     2,
       3,     4,     4,     5,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     2,     2,     1,     1,     1,
       1,     0,     1
};

/* YYDEFACT[S] -- default rule to reduce with in state S when YYTABLE
   doesn't specify something else to do.  Zero means the default is an
   error. */
static const short yydefact[] =
{
       0,     0,   557,   558,     0,     0,   571,   431,   118,   194,
       0,     0,     0,   197,   425,     0,   190,     0,     0,     0,
       0,   571,   193,   192,     0,     0,     0,    28,     0,   434,
      29,     0,    30,     0,    31,     0,   129,     0,     0,   442,
     102,   322,     0,   233,     0,   427,   188,     0,   191,     0,
     423,   256,   383,     0,     0,    91,   398,   189,     0,   437,
       0,   343,   383,   109,     0,   391,     7,     0,     3,     5,
       8,    11,    12,     9,    21,    23,    24,     0,     6,    13,
      33,    37,    14,    50,    51,    64,    73,    15,    77,    78,
      80,    17,    38,    18,    95,     0,     0,   101,     0,    19,
     106,     0,    20,     0,    40,    41,    42,    43,    44,    45,
       0,     0,   103,   184,   187,   104,   105,     0,    35,    34,
     253,   254,    46,    47,    49,    48,    36,    58,     0,    59,
      52,    61,    53,   571,    54,    81,    82,   571,   328,    85,
      86,    87,    79,     0,    88,     0,    89,    90,    83,     0,
     365,    84,    62,    55,   375,   376,    56,    63,    67,   387,
      65,     0,    66,    68,    69,    70,    71,    72,    75,     0,
      76,     0,    74,     0,    39,   314,    60,   476,    57,   312,
     313,   311,   555,   556,     0,     0,    16,    10,    22,   557,
     558,   530,   531,   532,   533,   534,   535,   529,   527,   528,
       0,   441,     0,   522,     0,   440,   518,   524,   526,   521,
     523,   520,   555,   556,     0,   559,   560,   482,     0,   562,
     563,   355,     0,     0,   149,     0,   155,     0,   144,     0,
       0,   150,   152,   154,   374,   377,     0,     0,     0,   283,
       0,   289,   292,   291,     0,   120,     0,   369,   360,     0,
     337,     0,    25,   372,     0,   339,     0,   357,     0,     0,
       0,     0,   264,   379,     0,     0,   330,     0,     0,     0,
     267,     0,     0,   157,     0,     0,     0,     0,   384,   386,
       0,   385,     0,     0,   270,     0,     0,   487,     0,     0,
     536,   508,   513,   516,     0,   276,     0,     0,   278,   280,
       0,     0,     0,     4,    26,     0,     0,     0,   100,     0,
     116,     0,     0,     0,     0,   211,     0,   203,   205,   215,
     202,     0,     0,     0,     0,   223,   198,   200,     0,   216,
     218,   231,     0,     0,     0,     0,   103,   104,   105,     0,
     236,   308,   571,     0,     0,     0,   326,   571,     0,     0,
     341,   345,     0,     0,   364,     0,   555,   556,     0,     0,
       0,   389,   418,   421,   420,   496,   497,   500,   502,   504,
     506,     0,     0,     0,   439,     0,   438,     0,     0,     0,
       0,     0,     0,     0,   549,   471,   548,     0,   344,   329,
     366,   561,     0,   399,     0,   546,   491,   545,     0,   547,
       0,   572,     0,     0,     0,   348,   350,   354,   356,     0,
     148,     0,   142,   145,   146,   141,     0,   151,   153,     0,
     378,     0,   301,   304,   303,     0,   282,     0,   284,     0,
     286,   119,     0,   367,     0,   332,   338,   373,   340,   358,
      93,   110,     0,     0,   263,     0,   380,   444,     0,    32,
       0,   266,     0,     0,   156,   158,   159,   161,   163,     0,
       0,   259,   261,   257,   382,     0,   273,   269,     0,   401,
     402,     0,   509,   510,     0,     0,     0,     0,     0,   488,
       0,   277,     0,   381,   405,   406,     0,    27,    96,   110,
      98,   110,   107,   110,   117,     0,     0,     0,   181,   182,
     183,   180,   172,   177,   212,   165,     0,   206,     0,   570,
     185,   557,   558,     0,   195,     0,   174,     0,   199,   245,
       0,   225,     0,   166,     0,   219,     0,     0,   176,     0,
     167,   235,   239,   234,   237,     0,     0,     0,   319,   571,
       0,     0,   327,     0,     0,   346,   362,     0,   505,     0,
     420,   388,     0,     0,     0,     0,     0,     0,     0,     0,
     409,   412,   438,     0,   396,     0,     0,   429,     0,   432,
       0,   435,   478,   571,     0,   483,   485,   477,     0,   554,
       0,     0,   472,   473,   520,   544,     0,   542,   525,     0,
       0,   492,   493,   506,   519,     0,     0,   352,   347,     0,
     353,     0,   147,   143,   127,     0,   123,   539,   125,     0,
     285,   290,     0,   121,   368,     0,     0,   115,     0,   111,
     112,   114,     0,     0,   136,   137,   135,   130,     0,   265,
     464,   453,   454,   452,   459,     0,   457,   460,   458,     0,
     461,   463,     0,   467,     0,   445,   446,   448,   449,   450,
     462,   455,   456,     0,     0,   268,   160,   162,   164,     0,
     413,     0,     0,     0,     0,     0,     0,   271,     0,    92,
     512,   511,   515,   514,   517,   281,   279,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   204,     0,     0,
     186,     0,   251,     0,   538,   252,   224,     0,     0,   247,
     249,     0,   217,     0,   222,   495,   232,   168,     0,     0,
     309,     0,   571,   323,     0,   571,   316,   571,     0,     0,
       0,   537,     0,     0,   419,   390,   498,   499,   501,   503,
     507,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     480,     0,   550,     0,     0,   470,     0,   540,     0,   400,
     489,     0,   541,     0,   349,   351,     0,   122,     0,     0,
     302,     0,   530,   567,   568,   569,     0,     0,   287,   293,
       0,   295,   564,   300,   299,   297,     0,     0,     0,     0,
     131,     0,     0,   140,     0,   139,     0,     0,   465,   466,
       0,     0,   447,   331,     0,     0,     0,     0,   255,   260,
       0,   258,   275,     0,   274,   403,   407,   392,     0,     0,
       0,   169,   170,     0,   178,   213,   207,   210,   196,   246,
     228,   226,   201,     0,     0,   220,   243,     0,   241,   238,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   359,
       0,   520,   393,     0,     0,   410,   411,   397,   430,   433,
     436,   479,   486,     0,   484,   552,   551,     0,   474,   475,
     543,   494,   315,   426,   124,   128,   126,     0,   565,   566,
       0,     0,     0,   361,   333,    94,   113,   134,   133,     0,
       0,   451,   468,   443,   428,   424,   415,   417,   414,   262,
     272,     0,     0,    97,    99,   108,   179,     0,     0,     0,
       0,   248,   250,     0,     0,   240,     0,   310,     0,   317,
     324,   320,     0,     0,   342,     0,   370,     0,     0,   394,
     481,   553,     0,     0,   294,   288,   296,   300,     0,   138,
     132,     0,     0,   404,   408,   214,   208,   229,   227,   221,
     244,   242,   318,   321,   571,   363,     0,     0,   395,   305,
       0,     0,   334,   469,   416,     0,     0,   371,   422,   306,
     335,     0,   230,     0,     0,   336,   325,   307,     0,     0,
       0
};

static const short yydefgoto[] =
{
     968,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,   286,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   618,   619,   620,
     102,   103,   104,   244,   325,   605,   606,   105,   260,   443,
     622,   623,   624,   625,   106,   227,   228,   229,   230,   231,
     232,   107,   272,   273,   274,   456,   457,   108,   109,   110,
     111,   502,   503,   112,   113,   114,   513,   115,   116,   326,
     522,   327,   316,   317,   688,   898,   508,   686,   897,   318,
     328,   329,   703,   903,   697,   900,   899,   955,   330,   117,
     118,   339,   709,   340,   708,   827,   828,   518,   698,   699,
     700,   119,   120,   121,   277,   665,   460,   461,   664,   122,
     261,   123,   269,   124,   283,   284,   465,   803,   125,   297,
     298,   126,   238,   239,   612,   871,   240,   241,   768,   769,
     770,   771,   421,   422,   242,   922,   127,   535,   830,   128,
     129,   130,   131,   132,   133,   536,   134,   135,   136,   137,
     138,   448,   139,   616,   928,   952,   140,   141,   142,   544,
     143,   144,   145,   404,   405,   406,   146,   147,   148,   434,
     719,   149,   150,   720,   151,   152,   153,   154,   155,   156,
     157,   280,   158,   360,   553,   159,   302,   160,   161,   162,
     163,   668,   891,   164,   677,   892,   559,   560,   659,   886,
     361,   362,   363,   165,   275,   166,   223,   167,   268,   168,
     169,   170,   171,   172,   173,   561,   202,   174,   265,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   790,   931,
     175,   581,   582,   583,   176,   382,   738,   853,   177,   574,
     575,   178,   203,   204,   590,   591,   704,   376,   365,   366,
     367,   368,   369,   370,   291,   292,   293,   206,   207,   208,
     294,   722,   695,   608,   179,   209,   586,   587,   210,   180,
     384,   580,   211,   212,   213,   563,   184,   185,   774,   775,
     537,   402
};

static const short yypact[] =
{
    1417,    23,    41,    98,  1596,   265,-32768,-32768,-32768,-32768,
     160,   152,   149,-32768,-32768,    91,-32768,   172,   295,    97,
     160,-32768,-32768,-32768,   186,   164,   200,-32768,   298,-32768,
  -32768,   304,-32768,   306,-32768,   160,-32768,   160,   311,-32768,
  -32768,-32768,   207,-32768,   177,-32768,-32768,   160,-32768,   225,
  -32768,   264,   425,   277,   265,-32768,-32768,-32768,   516,-32768,
      89,-32768,   425,-32768,   265,-32768,-32768,  1508,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,-32768,   313,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,   160,   160,-32768,   273,-32768,
  -32768,   160,-32768,   315,-32768,-32768,-32768,-32768,-32768,-32768,
     296,   334,    87,    52,-32768,   114,   120,   804,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   353,-32768,
  -32768,-32768,-32768,   132,-32768,-32768,-32768,  1679,-32768,-32768,
  -32768,-32768,-32768,   378,-32768,   317,-32768,-32768,-32768,   171,
  -32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1008,
  -32768,   533,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   553,
  -32768,   576,-32768,   734,-32768,-32768,-32768,   383,-32768,   437,
  -32768,-32768,   452,   462,   301,   372,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
    1025,-32768,    11,   437,   482,   464,-32768,-32768,-32768,   437,
  -32768,-32768,   485,   509,   586,-32768,-32768,-32768,   368,-32768,
  -32768,-32768,   478,   515,-32768,    85,-32768,    61,-32768,   160,
      79,-32768,   504,   521,-32768,-32768,   527,    75,   156,-32768,
     -16,-32768,-32768,-32768,    49,-32768,   521,   586,-32768,   575,
  -32768,   530,-32768,-32768,   543,-32768,   590,-32768,   595,    35,
     583,   292,-32768,-32768,   599,   609,-32768,   618,   622,   308,
  -32768,   160,    62,-32768,   160,   626,   160,   160,-32768,-32768,
     623,-32768,   160,   324,-32768,   330,   160,-32768,  1511,  1511,
      71,   268,-32768,   379,   631,-32768,   160,   339,-32768,-32768,
     633,   352,   636,-32768,-32768,   641,   133,   187,-32768,   254,
  -32768,   643,   654,   660,   357,   655,   360,-32768,   546,   521,
  -32768,   754,  1055,   357,    96,   294,-32768,-32768,   375,-32768,
     649,   122,   612,   387,   668,   666,-32768,   112,-32768,   389,
  -32768,-32768,-32768,   290,   586,   670,-32768,-32768,   701,   586,
  -32768,-32768,   672,   673,-32768,   669,-32768,-32768,  1267,  1008,
     678,   676,-32768,-32768,   497,-32768,   757,   759,-32768,   762,
      71,   855,   416,   855,-32768,   687,   497,   855,   689,   855,
     691,   345,   692,   988,-32768,   988,-32768,  1025,-32768,-32768,
  -32768,-32768,   235,-32768,  1008,-32768,   888,-32768,  1511,-32768,
    1025,-32768,   699,  1025,   325,-32768,   688,   497,-32768,   855,
  -32768,   690,-32768,-32768,   160,-32768,   212,-32768,-32768,  1088,
  -32768,   697,-32768,-32768,-32768,   462,-32768,    97,-32768,    97,
  -32768,-32768,   160,   700,  1025,-32768,-32768,-32768,-32768,-32768,
  -32768,   117,   160,   417,-32768,   160,-32768,   715,  1025,-32768,
     855,-32768,   160,   698,-32768,-32768,   160,-32768,   702,   855,
     351,-32768,-32768,   703,-32768,   704,-32768,-32768,   277,-32768,
  -32768,   695,   268,   268,  1511,  1511,  1511,  1511,  1511,-32768,
     705,-32768,    94,-32768,-32768,-32768,   855,-32768,-32768,   117,
  -32768,   117,-32768,   117,-32768,   160,   160,   706,-32768,-32768,
  -32768,-32768,   707,-32768,-32768,-32768,   160,-32768,   708,-32768,
  -32768,   712,   721,   713,   497,   723,   707,  1125,-32768,-32768,
      96,-32768,  1155,-32768,   160,-32768,  1025,    96,   707,   418,
  -32768,-32768,-32768,-32768,-32768,  1025,   392,   586,-32768,-32768,
     711,   710,-32768,   719,  1025,-32768,-32768,  1267,   762,   728,
     235,-32768,  1008,   733,  1025,  1025,  1025,  1025,  1267,   479,
  -32768,-32768,   497,   727,-32768,  1008,   496,-32768,   500,-32768,
     505,-32768,-32768,-32768,   513,-32768,   497,-32768,  1167,    71,
     730,   736,   740,-32768,   529,   497,   537,-32768,-32768,   438,
     745,   746,   497,   236,-32768,   540,   610,-32768,-32768,   368,
    1025,   555,-32768,-32768,-32768,   562,-32768,    71,   743,   145,
  -32768,-32768,  1300,-32768,-32768,   238,  1025,-32768,   749,   752,
  -32768,-32768,   565,   753,   437,-32768,   750,-32768,   755,-32768,
  -32768,-32768,-32768,-32768,-32768,   715,-32768,-32768,-32768,   847,
  -32768,-32768,   848,-32768,   761,   715,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,   241,   566,-32768,-32768,-32768,-32768,   569,
  -32768,   756,   777,   160,   771,   781,   160,-32768,  1008,-32768,
     268,   268,-32768,-32768,-32768,-32768,-32768,  1008,   570,   783,
     784,   786,   456,   457,  1088,   357,  1300,-32768,  1300,  1025,
  -32768,  1025,-32768,   789,    71,-32768,   776,  1300,   580,-32768,
  -32768,   785,-32768,  1300,-32768,   497,-32768,-32768,   869,   656,
     497,  1188,-32768,-32768,   587,-32768,-32768,-32768,   270,  1025,
     794,    71,   793,  1008,-32768,-32768,   757,   757,   759,-32768,
     886,   192,   855,   908,   458,   812,   813,   815,   820,   586,
  -32768,   955,-32768,   828,  1200,-32768,   610,-32768,  1025,-32768,
  -32768,  1025,-32768,   831,-32768,-32768,   832,-32768,  1088,  1225,
  -32768,   825,   829,-32768,-32768,-32768,   634,   634,   834,-32768,
     830,-32768,-32768,   835,-32768,-32768,   836,   349,   839,   117,
  -32768,   160,   160,-32768,   888,-32768,   160,  1336,-32768,-32768,
    1267,   841,-32768,-32768,   842,   843,   160,   908,-32768,-32768,
    1025,-32768,-32768,   845,   521,   676,   676,-32768,   844,   849,
     850,-32768,-32768,   594,-32768,   834,   834,-32768,   497,-32768,
  -32768,   834,-32768,  1155,  1125,   834,   846,   600,-32768,-32768,
     851,  1267,   852,   586,   853,   611,   586,   856,   398,-32768,
    1267,   837,-32768,  1008,   475,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,   857,-32768,-32768,-32768,   859,-32768,-32768,
  -32768,   497,-32768,-32768,-32768,-32768,-32768,  1267,-32768,-32768,
    1300,   870,  1325,-32768,-32768,-32768,-32768,-32768,-32768,   615,
     620,-32768,-32768,-32768,-32768,-32768,-32768,   854,-32768,-32768,
  -32768,   863,   878,-32768,-32768,-32768,-32768,   872,   873,  1300,
     874,-32768,-32768,   875,   936,-32768,   869,-32768,   883,-32768,
  -32768,-32768,   885,   882,-32768,   887,   884,  1267,   480,-32768,
  -32768,-32768,   890,   889,-32768,-32768,-32768,-32768,   896,-32768,
  -32768,   876,   908,-32768,-32768,-32768,-32768,   834,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,  1267,   893,-32768,-32768,
    1267,   320,-32768,-32768,-32768,   903,   586,-32768,-32768,   892,
  -32768,   912,-32768,   914,  1267,-32768,-32768,-32768,  1009,  1023,
  -32768
};

static const short yypgoto[] =
{
  -32768,-32768,   941,-32768,-32768,  1020,-32768,-32768,  1021,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,   904,-32768,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,   928,  -115,-32768,-32768,-32768,  -162,-32768,   248,
  -32768,-32768,-32768,-32768,     2,   356,   284,-32768,-32768,-32768,
     257,   -30,-32768,-32768,-32768,   814,  -192,-32768,   816,  -181,
  -32768,-32768,-32768,   774,-32768,-32768,   592,-32768,-32768,-32768,
  -32768,  -191,   364,  -106,-32768,-32768,-32768,  -103,  -102,-32768,
  -32768,   937,   557,   548,-32768,-32768,-32768,-32768,-32768,-32768,
     -85,   531,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,   347,-32768,-32768,   165,  -362,-32768,   247,
    -479,-32768,-32768,-32768,-32768,-32768,   795,   410,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,   606,-32768,-32768,-32768,-32768,
     593,-32768,-32768,  -184,-32768,-32768,-32768,   647,  -645,   208,
  -32768,   209,-32768,   470,  -213,-32768,-32768,-32768,-32768,   -15,
  -32768,-32768,-32768,-32768,-32768,  -493,-32768,-32768,-32768,   943,
    -118,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,-32768,   483,  -363,-32768,-32768,-32768,-32768,
  -32768,-32768,   899,   168,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,  1024,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,  -299,   371,-32768,-32768,
    -331,  -502,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,  -164,    22,-32768,-32768,-32768,
     453,  -578,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,-32768,   343,-32768,-32768,-32768,-32768,-32768,-32768,
     365,-32768,-32768,-32768,-32768,-32768,  -620,   104,-32768,   203,
     549,   550,  -314,   -57,  -224,    18,    55,   714,-32768,  -559,
    -643,  -769,  -794,   350,  -200,-32768,  -377,   363,-32768,-32768,
    -148,  -519,     8,    32,    29,    10,   971,   -12,-32768,  -282,
      -3,  -237
};


#define	YYLAST		1769


static const short yytable[] =
{
     222,   290,   335,   214,   243,   375,   236,   378,   181,   380,
     433,   336,   181,   251,   337,   338,   254,   226,   247,   256,
     217,   258,   245,   595,   423,   233,   264,   181,   549,   183,
     246,   333,   182,   183,   386,   413,   182,   424,   693,   510,
     597,   815,   519,   816,   548,   259,   714,   262,   183,   417,
     724,   182,   821,   772,   428,   395,   397,   270,   825,   743,
     205,   399,   281,   589,   472,   473,   389,   792,   832,   817,
     299,   916,   281,   923,   566,   181,   285,   320,   568,   190,
     570,   429,   215,   216,   430,   224,   301,   305,   215,   216,
     215,   216,   215,   216,   215,   216,   183,   215,   216,   182,
     189,   190,   509,   224,   393,   306,   307,   541,   394,   205,
     601,   309,   543,   311,   315,   224,   186,   215,   216,   205,
     215,   216,   319,   215,   216,   331,   331,   772,   440,   772,
     344,   441,   516,   352,   349,   215,   216,   320,   772,   320,
    -562,   528,   431,   343,   772,   181,   432,   882,   215,   512,
     321,   654,   219,   220,   412,   454,   959,   355,   696,   189,
     190,   225,   271,   215,   216,   706,   183,   219,   220,   182,
     967,   237,   415,   474,   189,   190,   475,   957,   357,   225,
     889,   356,   295,   372,   314,   410,  -171,   678,   908,   296,
     267,   225,   517,   237,   296,   189,   190,  -563,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   868,   869,   792,
     324,   323,   324,  -173,   358,   617,   205,   332,   419,  -175,
     527,   724,   835,   243,   413,   857,   488,   249,   342,   489,
     389,   226,   226,   417,   734,   411,   224,   755,   519,   233,
     233,   237,   221,   610,   730,   519,   181,   529,   218,   426,
     670,   671,   237,   427,   937,   554,   555,   250,   554,   555,
     353,   554,   555,   364,   354,   234,   425,   183,   189,   190,
     182,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     490,   453,   248,   491,   458,   842,   462,   462,   359,   843,
     554,   555,   466,   252,   288,   660,   471,   289,   219,   220,
     713,   219,   220,   266,   392,   603,   480,   219,   220,   219,
     220,   772,   225,   772,   219,   220,   215,   216,   215,   216,
     219,   220,   407,   219,   220,   271,   579,   679,   579,   680,
     588,   681,   515,   776,    40,  -554,   793,   805,   474,   593,
     772,   475,   331,    21,   349,   902,   806,   492,   189,   190,
     493,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     276,   200,   607,   201,    42,   837,   476,   358,   477,   554,
     555,   189,   190,   282,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   538,   388,   444,   539,   540,   235,   445,
     358,   253,   520,   584,   521,   312,   423,   255,   497,   257,
     844,   451,   478,   398,   263,   452,   304,   879,   310,   424,
     351,   683,   243,   960,   243,   498,   226,   467,   554,   555,
     598,   468,   599,   469,   233,   499,   514,   470,   215,   216,
     500,   278,   481,   313,   613,   181,   482,   181,   279,   501,
     572,   200,   246,   573,   874,   484,   662,   288,   663,   485,
     289,   621,   626,   505,   341,   629,   183,   506,   183,   182,
     694,   182,   655,   550,   200,   694,   458,   403,   523,   661,
     288,   391,   524,   289,   350,   562,   783,   562,   785,   381,
     530,   562,   533,   562,   524,   576,   534,   711,   398,   712,
     721,   585,   299,   915,   672,   673,   674,   315,   364,   621,
     592,   621,   852,   621,   585,   319,   331,   407,   315,   564,
     627,   707,   918,   565,   628,   524,   319,   554,   555,   189,
     190,   579,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   749,   701,   383,   331,   552,   189,   190,   615,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   385,   811,
     812,   847,   653,   506,   524,   552,   189,   190,   387,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   919,   846,
     739,   408,   552,   948,   731,   358,   732,   552,   396,   189,
     190,  -490,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   735,   401,   732,   335,   736,   910,   732,   358,   913,
     737,   418,   732,   336,   753,   400,   337,   338,   740,   287,
     741,   409,   200,   189,   190,   215,   216,   419,   288,   761,
     420,   289,   773,   436,  -475,   357,  -475,   607,   356,   371,
     705,   201,   747,   888,   748,   752,   437,   748,   425,   710,
     191,   192,   193,   194,   195,   196,   507,  -209,   718,   373,
     756,   374,   732,   497,   290,   288,   364,   757,   289,   758,
     780,   794,   781,   732,   795,   807,   796,   732,   802,   364,
     498,   435,   377,   462,   374,   822,   804,   823,   288,   442,
     499,   289,   834,   438,   712,   500,     9,   579,   439,   896,
      13,   758,   446,    16,   501,   905,   773,   906,   773,    22,
      23,   607,   607,   407,   407,   447,   912,   773,   712,   833,
     929,   449,   748,   773,   836,   930,   464,   781,   450,   963,
     777,   630,   459,    46,   479,    48,   483,   593,   631,   632,
     633,   841,   486,   290,   487,    57,   494,   189,   190,   634,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   525,
     526,   877,   878,   495,   859,   504,   358,   726,   727,   496,
     509,   531,   532,   542,    42,   545,   694,   694,   954,   546,
     547,   551,   364,   552,   290,   357,   556,   557,   356,   558,
     567,   364,   569,   721,   571,   577,   596,   600,   669,   621,
     602,   626,   626,   705,   609,   818,   626,   614,   656,   658,
     663,   666,   684,   716,   685,   675,   887,   715,   690,   689,
     694,   635,   636,  -559,   637,   638,   717,   639,   640,   641,
     642,   643,  -560,   838,   691,   723,   725,   364,   733,   744,
     379,   745,   374,   701,     9,   364,   288,   746,    13,   289,
     750,    16,   759,   751,   778,   576,   784,    22,    23,   779,
     782,   786,   585,   788,   789,   861,   791,   797,   511,   512,
     721,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     798,    46,   800,    48,   801,   334,   820,   358,   808,   809,
     773,   810,   927,    57,   819,   826,   824,   839,   585,   721,
     840,   189,   190,   694,   191,   192,   193,   194,   195,   196,
     197,   198,   199,-32768,   705,   848,   849,   694,   850,   773,
     358,   189,   190,   851,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   855,   862,   863,   867,  -570,   872,   873,
     358,   870,   875,  -298,   883,   884,   885,   893,   917,   961,
     890,   956,   894,   895,   907,   909,   911,   364,   904,   914,
     920,   200,   940,   374,   921,   932,   933,   288,   189,   190,
     289,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     925,   934,   935,   936,   938,   939,   942,   358,   943,   944,
     945,   946,   951,   953,   200,   949,   950,   578,   958,   964,
     288,   189,   190,   289,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   962,   200,   965,   374,   966,   303,   969,
     288,   189,   190,   289,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   970,   187,   188,   308,   876,   189,   190,
     358,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     813,   346,   864,   880,   416,   414,   455,   358,   657,   814,
     322,   200,   682,   573,   687,   702,   829,   288,   511,   512,
     289,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     901,   941,   463,   799,   667,   676,   611,   358,   924,   760,
     347,   926,   754,   390,   200,   947,   300,   578,   787,   858,
     288,   189,   190,   289,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   845,   359,   728,   854,   729,   348,   866,
     288,   860,   594,   289,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,     0,     0,     0,   288,   189,   190,
     289,   191,   192,   193,   194,   195,   196,   197,   198,   199,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,     0,     0,     0,   288,   511,   512,
     289,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     189,   190,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,     0,     0,   200,     0,   604,     0,     0,     0,
     288,   189,   190,   289,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   189,   190,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,     0,     0,     0,     0,
       0,   200,     0,   692,     0,     0,     0,   288,   189,   190,
     289,   191,   192,   193,   194,   195,   196,   197,   198,   199,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,   692,     0,     0,     0,   288,     0,     0,
     289,     0,   742,   200,     0,     0,     0,     0,     0,   288,
     189,   190,   289,   191,   192,   193,   194,   195,   196,   197,
     198,   199,     0,     0,   200,   831,     0,     0,     0,     0,
     288,     0,     0,   289,     0,   856,   200,     0,     0,     0,
       0,     0,   288,   215,   216,   289,   762,   192,   193,   194,
     195,   196,   763,   764,   765,     0,     0,     0,     0,     0,
       0,   200,     0,   865,     0,     0,     0,   288,   215,   216,
     289,   191,   192,   193,   194,   195,   196,   763,   764,   765,
       0,     0,   630,     0,     0,     0,     0,     0,     0,   631,
     632,   633,     0,     0,     0,     0,     0,     0,     0,     0,
     634,     0,     0,   200,     0,     0,     0,     0,     0,   288,
       0,     0,   289,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   766,     0,     0,   767,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     1,     0,
       2,     3,     4,     0,     0,     0,     0,   766,     0,     0,
     767,   881,   635,   636,     0,   637,   638,     0,   639,   640,
     641,   642,   643,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,     0,     0,    64,     0,    65,    -1,     1,
      66,     2,     3,     4,   189,   190,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,     0,     0,    64,     1,    65,     2,
       3,    66,     0,     0,     0,     0,     0,   200,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,     0,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,     2,     3,    64,     0,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     0,
      10,     0,     0,     0,    14,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    29,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,    41,    42,     0,     0,    45,     0,     0,     0,     0,
       0,    50,     0,    52,     0,    54,     0,    56,     0,    58,
      59,     0,     0,    62,     0,   345,     0,    64,     0,    65
};

static const short yycheck[] =
{
      12,    58,   117,     6,    19,   169,    18,   171,     0,   173,
     247,   117,     4,    25,   117,   117,    28,    15,    21,    31,
      10,    33,    20,   400,   237,    15,    38,    19,   359,     0,
      20,   116,     0,     4,   182,   227,     4,   237,   517,   321,
     403,   686,   324,   688,   358,    35,   539,    37,    19,   230,
     552,    19,   697,   612,   238,   203,   204,    47,   703,   578,
       5,   209,    52,   394,   288,   289,   184,   645,   711,   689,
      60,   840,    62,   867,   373,    67,    54,    25,   377,     4,
     379,    97,     3,     4,   100,    24,    64,    77,     3,     4,
       3,     4,     3,     4,     3,     4,    67,     3,     4,    67,
       3,     4,     6,    24,    93,    95,    96,   344,    97,    54,
     409,   101,   349,   103,   112,    24,    93,     3,     4,    64,
       3,     4,   112,     3,     4,   115,   116,   686,    93,   688,
     133,    96,   323,   145,   137,     3,     4,    25,   697,    25,
      99,   332,    93,   133,   703,   137,    97,   790,     3,     4,
      98,   450,     3,     4,    93,    93,   950,   149,   520,     3,
       4,   100,   100,     3,     4,   527,   137,     3,     4,   137,
     964,    96,    93,   102,     3,     4,   105,   946,   149,   100,
     800,   149,    93,   161,    97,   100,    99,   486,   831,   100,
      13,   100,    96,    96,   100,     3,     4,    99,     6,     7,
       8,     9,    10,    11,    12,    13,    14,   766,   767,   787,
      98,    97,    98,    99,    22,    98,   161,    97,    96,    99,
      98,   723,   715,   238,   416,   744,    93,    63,    96,    96,
     348,   229,   230,   414,   565,   225,    24,   600,   520,   229,
     230,    96,    93,   427,   558,   527,   238,   332,    96,    93,
     474,   475,    96,    97,   899,    20,    21,    93,    20,    21,
      89,    20,    21,   159,    93,    93,   237,   238,     3,     4,
     238,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      93,   271,    96,    96,   274,    93,   276,   277,    96,    97,
      20,    21,   282,    93,   102,   459,   286,   105,     3,     4,
     537,     3,     4,    96,   200,    93,   296,     3,     4,     3,
       4,   870,   100,   872,     3,     4,     3,     4,     3,     4,
       3,     4,   218,     3,     4,   100,   383,   489,   385,   491,
      95,   493,   322,    95,    61,    99,    95,   668,   102,   396,
     899,   105,   332,    42,   347,   824,   677,    93,     3,     4,
      96,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      96,    96,   419,    98,    63,    95,    98,    22,   100,    20,
      21,     3,     4,    96,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    93,    83,    93,    96,    97,    93,    97,
      22,    93,    98,   385,   100,    99,   609,    93,    41,    93,
     731,    93,    23,    24,    93,    97,    93,   784,    93,   609,
      93,   496,   427,    93,   429,    58,   414,    93,    20,    21,
      95,    97,    97,    93,   414,    68,   322,    97,     3,     4,
      73,     6,    93,    99,   432,   427,    97,   429,    13,    82,
      95,    96,   432,    98,    95,    93,    95,   102,    97,    97,
     105,   441,   442,    93,   101,   445,   427,    97,   429,   427,
     517,   429,   452,   359,    96,   522,   456,    99,    93,   459,
     102,    99,    97,   105,    96,   371,   624,   373,   626,    96,
      93,   377,    93,   379,    97,   381,    97,    95,    24,    97,
     547,   387,   482,    95,   476,   477,   478,   495,   394,   489,
     396,   491,   739,   493,   400,   495,   496,   403,   506,    93,
      93,    93,   843,    97,    97,    97,   506,    20,    21,     3,
       4,   578,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    93,   522,    96,   524,    97,     3,     4,   434,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    96,    93,
      93,    93,   448,    97,    97,    97,     3,     4,    96,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    93,   733,
     573,    93,    97,    93,    95,    22,    97,    97,    96,     3,
       4,    96,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    95,     6,    97,   709,    95,   833,    97,    22,   836,
      95,    97,    97,   709,   596,    96,   709,   709,    95,    93,
      97,    96,    96,     3,     4,     3,     4,    96,   102,   609,
      93,   105,   612,    93,    95,   596,    97,   684,   596,    96,
     526,    98,    95,   797,    97,    95,    93,    97,   609,   535,
       6,     7,     8,     9,    10,    11,   100,   101,   544,    96,
      95,    98,    97,    41,   711,   102,   552,    95,   105,    97,
      95,    95,    97,    97,    95,    95,    97,    97,   666,   565,
      58,    96,    96,   663,    98,    95,   666,    97,   102,    96,
      68,   105,    95,    93,    97,    73,    30,   744,    93,    95,
      34,    97,    93,    37,    82,    95,   686,    97,   688,    43,
      44,   758,   759,   599,   600,    96,    95,   697,    97,   712,
      95,    93,    97,   703,   717,    95,    93,    97,    96,   956,
     616,     6,    96,    67,    93,    69,    93,   784,    13,    14,
      15,   723,    96,   790,    93,    79,    93,     3,     4,    24,
       6,     7,     8,     9,    10,    11,    12,    13,    14,   100,
     101,   781,   782,    99,   746,   100,    22,   554,   555,    99,
       6,    93,    96,    93,    63,    93,   823,   824,   932,    96,
     101,    93,   668,    97,   831,   746,    19,    18,   746,    17,
      93,   677,    93,   840,    93,    93,    87,    99,    93,   779,
     100,   781,   782,   689,    97,   691,   786,    97,   100,    97,
      97,    97,    96,    93,    97,   100,   796,    96,    95,   101,
     867,    96,    97,   101,    99,   100,    97,   102,   103,   104,
     105,   106,   101,   719,   101,    97,    93,   723,   101,    99,
      96,    95,    98,   823,    30,   731,   102,    97,    34,   105,
      95,    37,    99,    97,    95,   741,    96,    43,    44,    97,
      97,    96,   748,     6,     6,   751,    95,   101,     3,     4,
     917,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      93,    67,   101,    69,    93,    71,   100,    22,    95,    95,
     870,    95,   872,    79,    95,    16,   101,    93,   784,   946,
      97,     3,     4,   950,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    17,   800,    93,    93,   964,    93,   899,
      22,     3,     4,    93,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    95,    93,    93,   101,    98,    98,    93,
      22,    97,    93,    98,    93,    93,    93,    93,   101,   951,
      95,   944,    93,    93,    93,    93,    93,   843,   102,    93,
      93,    96,    16,    98,    95,   101,    93,   102,     3,     4,
     105,     6,     7,     8,     9,    10,    11,    12,    13,    14,
     100,    93,   100,   100,   100,   100,    93,    22,    93,    97,
      93,    97,    86,   107,    96,    95,    97,    99,    95,    97,
     102,     3,     4,   105,     6,     7,     8,     9,    10,    11,
      12,    13,    14,   100,    96,    93,    98,    93,    67,     0,
     102,     3,     4,   105,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     4,     4,    98,   779,     3,     4,
      22,     6,     7,     8,     9,    10,    11,    12,    13,    14,
     684,   137,   758,   786,   230,   229,   272,    22,   456,   685,
     113,    96,   495,    98,   506,   524,   709,   102,     3,     4,
     105,     6,     7,     8,     9,    10,    11,    12,    13,    14,
     823,   906,   277,   663,   468,   482,   429,    22,   870,   609,
     137,   872,   599,   184,    96,   917,    62,    99,   635,   746,
     102,     3,     4,   105,     6,     7,     8,     9,    10,    11,
      12,    13,    14,   732,    96,   556,   741,   557,   137,   759,
     102,   748,   398,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    -1,    -1,    -1,   102,     3,     4,
     105,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    -1,    -1,    -1,   102,     3,     4,
     105,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       3,     4,    -1,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    -1,    -1,    96,    -1,    98,    -1,    -1,    -1,
     102,     3,     4,   105,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     3,     4,    -1,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    98,    -1,    -1,    -1,   102,     3,     4,
     105,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    98,    -1,    -1,    -1,   102,    -1,    -1,
     105,    -1,    95,    96,    -1,    -1,    -1,    -1,    -1,   102,
       3,     4,   105,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    -1,    -1,    96,    97,    -1,    -1,    -1,    -1,
     102,    -1,    -1,   105,    -1,    95,    96,    -1,    -1,    -1,
      -1,    -1,   102,     3,     4,   105,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    98,    -1,    -1,    -1,   102,     3,     4,
     105,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      -1,    -1,     6,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    96,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,     1,    -1,
       3,     4,     5,    -1,    -1,    -1,    -1,   102,    -1,    -1,
     105,    95,    96,    97,    -1,    99,   100,    -1,   102,   103,
     104,   105,   106,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    -1,    -1,    88,    -1,    90,     0,     1,
      93,     3,     4,     5,     3,     4,    -1,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    -1,    -1,    88,     1,    90,     3,
       4,    93,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    66,    67,    68,    69,    70,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,     3,     4,    88,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    26,    27,    28,    -1,    -1,
      31,    -1,    -1,    -1,    35,    -1,    -1,    38,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,
      -1,    62,    63,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      81,    -1,    -1,    84,    -1,    86,    -1,    88,    -1,    90
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison/bison.simple"

/* Skeleton output parser for bison,

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software
   Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser when
   the %semantic_parser declaration is not specified in the grammar.
   It was written by Richard Stallman by simplifying the hairy parser
   used when %semantic_parser is specified.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

#if ! defined (yyoverflow) || defined (YYERROR_VERBOSE)

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || defined (YYERROR_VERBOSE) */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYLTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
# if YYLSP_NEEDED
  YYLTYPE yyls;
# endif
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAX (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# if YYLSP_NEEDED
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE) + sizeof (YYLTYPE))	\
      + 2 * YYSTACK_GAP_MAX)
# else
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAX)
# endif

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAX;	\
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif


#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");			\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).

   When YYLLOC_DEFAULT is run, CURRENT is set the location of the
   first token.  By default, to implement support for ranges, extend
   its range to the last symbol.  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)       	\
   Current.last_line   = Rhs[N].last_line;	\
   Current.last_column = Rhs[N].last_column;
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#if YYPURE
# if YYLSP_NEEDED
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, &yylloc, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval, &yylloc)
#  endif
# else /* !YYLSP_NEEDED */
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval)
#  endif
# endif /* !YYLSP_NEEDED */
#else /* !YYPURE */
# define YYLEX			yylex ()
#endif /* !YYPURE */


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)
/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif

#ifdef YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif
#endif

#line 315 "/usr/share/bison/bison.simple"


/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
#  define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL
# else
#  define YYPARSE_PARAM_ARG YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
# endif
#else /* !YYPARSE_PARAM */
# define YYPARSE_PARAM_ARG
# define YYPARSE_PARAM_DECL
#endif /* !YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
# ifdef YYPARSE_PARAM
int yyparse (void *);
# else
int yyparse (void);
# endif
#endif

/* YY_DECL_VARIABLES -- depending whether we use a pure parser,
   variables are global, or local to YYPARSE.  */

#define YY_DECL_NON_LSP_VARIABLES			\
/* The lookahead symbol.  */				\
int yychar;						\
							\
/* The semantic value of the lookahead symbol. */	\
YYSTYPE yylval;						\
							\
/* Number of parse errors so far.  */			\
int yynerrs;

#if YYLSP_NEEDED
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES			\
						\
/* Location data for the lookahead symbol.  */	\
YYLTYPE yylloc;
#else
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES
#endif


/* If nonreentrant, generate the variables here. */

#if !YYPURE
YY_DECL_VARIABLES
#endif  /* !YYPURE */

int
yyparse (YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  /* If reentrant, generate the variables here. */
#if YYPURE
  YY_DECL_VARIABLES
#endif  /* !YYPURE */

  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yychar1 = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack. */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;

#if YYLSP_NEEDED
  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
#endif

#if YYLSP_NEEDED
# define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
# define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  YYSIZE_T yystacksize = YYINITDEPTH;


  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
#if YYLSP_NEEDED
  YYLTYPE yyloc;
#endif

  /* When reducing, the number of symbols on the RHS of the reduced
     rule. */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
#if YYLSP_NEEDED
  yylsp = yyls;
#endif
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  */
# if YYLSP_NEEDED
	YYLTYPE *yyls1 = yyls;
	/* This used to be a conditional around just the two extra args,
	   but that might be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
# else
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);
# endif
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
# if YYLSP_NEEDED
	YYSTACK_RELOCATE (yyls);
# endif
# undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
#if YYLSP_NEEDED
      yylsp = yyls + yysize - 1;
#endif

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

#if YYDEBUG
     /* We have to keep this `#if YYDEBUG', since we use variables
	which are defined only if `YYDEBUG' is set.  */
      if (yydebug)
	{
	  YYFPRINTF (stderr, "Next token is %d (%s",
		     yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise
	     meaning of a token, for further debugging info.  */
# ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
# endif
	  YYFPRINTF (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %d (%s), ",
	      yychar, yytname[yychar1]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to the semantic value of
     the lookahead token.  This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

#if YYLSP_NEEDED
  /* Similarly for the default location.  Let the user run additional
     commands if for instance locations are ranges.  */
  yyloc = yylsp[1-yylen];
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
#endif

#if YYDEBUG
  /* We have to keep this `#if YYDEBUG', since we use variables which
     are defined only if `YYDEBUG' is set.  */
  if (yydebug)
    {
      int yyi;

      YYFPRINTF (stderr, "Reducing via rule %d (line %d), ",
		 yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (yyi = yyprhs[yyn]; yyrhs[yyi] > 0; yyi++)
	YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
      YYFPRINTF (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif

  switch (yyn) {

case 5:
#line 372 "fortran.y"
{
				/* Create id token for prog if unnamed.  NOTE:
				   this clobbers $1.class, value, src_text.
				 */
			  if(current_module_hash == -1) {
			    implied_id_token(&(yyvsp[0]),unnamed_prog);
			    def_function(
					 type_PROGRAM,	/* type */
					 size_DEFAULT,	/* size */
					 (char *)NULL,	/* size text */
					 &(yyvsp[0]),		/* name */
					 (Token*)NULL);	/* args */
			    current_module_hash =
			      def_curr_module(&(yyvsp[0]));
			    current_module_type = type_PROGRAM;

				/* Pretend this is a PROGRAM statement */
			    if(style_req_prog_stmt) {
				warning(yyvsp[0].line_num,yyvsp[0].col_num,
			"Program does not start with a PROGRAM statement");
			    }
			    push_block(&(yyval),tok_PROGRAM,subprog,
				       hashtab[current_module_hash].name,
				       NO_LABEL);
				/* It is possible for a block construct to
				   be the initial statement, and if so it
				   has earlier been pushed onto stack.  Detect
				   this situation and swap stack entries to
				   make them nest correctly.
				 */
			    if(block_depth > 1 &&
			       block_stack[block_depth-2].first_line == yyvsp[0].line_num) {
				BlockStack temp = block_stack[block_depth-1];
				block_stack[block_depth-1] = block_stack[block_depth-2];
				block_stack[block_depth-2] = temp;
			    }
			  }

					/* Handle END statement.  Note that
					   curr_stmt_class of structured END
					   stmts have been merged into tok_END.
					 */
			  if(curr_stmt_class == tok_END) {
			    if(prev_stmt_class != tok_RETURN)
			      (void)do_RETURN(current_module_hash,&(yyvsp[0]));
			    pop_block(&(yyval),yyval.tclass,
				      curr_stmt_name,NO_LABEL);

			    END_processing(&(yyval));
			    goto_flag = prev_goto = FALSE;
			  }
			  prev_stmt_class = curr_stmt_class;
			  integer_context = FALSE;
			  in_attrbased_typedecl = FALSE;
			  true_prev_stmt_line_num = yyval.line_num;
			}
    break;
case 9:
#line 439 "fortran.y"
{
				/* Treat END PROGRAM et al as plain END */
			    curr_stmt_class = tok_END;
			}
    break;
case 10:
#line 446 "fortran.y"
{
				/* Put definition of label into table, and
				   if it marks end of a DO range, pop block.
				 */
			  int do_label = def_label(&(yyvsp[-1]),labeled_stmt_type);
			  if( do_label || yyvsp[0].tclass == tok_ENDDO ) {
			    if(is_true(NOT_DO_TERMINAL_STMT,yyvsp[0].TOK_flags)) {
			      syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
			"statement cannot be terminal statement of a DO loop");
			    }
			    else {
			      pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,
				      do_label?
					(LABEL_t)(yyvsp[-1].value.integer):
					NO_LABEL);
			    }
			  }

				/* Issue picky warnings about labeled
				   statements.  FORMAT has an excuse
				   for existing, so warnings for it
				   are separately controlled.  */
			  if( style_labeled_exec &&
			      curr_stmt_class != tok_FORMAT ) {
			    warning(yyvsp[-1].line_num,yyvsp[-1].col_num,
				    "obsolescent feature: labeled statement");
			  }
			  else if( style_labeled_format &&
			      curr_stmt_class == tok_FORMAT ) {
			    warning(yyvsp[0].line_num,yyvsp[0].col_num,
				    "obsolescent feature: FORMAT statement");
			  }

			  if(executable_stmt)
			    prev_goto = goto_flag;
			  yyval = yyvsp[0];
			}
    break;
case 11:
#line 484 "fortran.y"
{
			  if(executable_stmt) {
			    if(prev_goto)
				syntax_error(yyvsp[0].line_num, NO_COL_NUM,
					"No path to this statement");
			    prev_goto = goto_flag;
			  }

			  if( curr_stmt_class == tok_FORMAT &&
			      misc_warn ) {
			      syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
				      "FORMAT statement has no label");
			  }
			  if( yyvsp[0].tclass == tok_ENDDO )
			    pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,NO_LABEL);
			}
    break;
case 12:
#line 503 "fortran.y"
{
			    exec_stmt_count = 0;
			    executable_stmt = FALSE;
			    labeled_stmt_type = LAB_SPECIFICATION;
			    push_block(&(yyvsp[0]),yyvsp[0].tclass,subprog,
				       hashtab[current_module_hash].name,
				       NO_LABEL);
			}
    break;
case 13:
#line 512 "fortran.y"
{
			    executable_stmt = FALSE;
			/* labeled_stmt_type set in lower productions */
			}
    break;
case 14:
#line 517 "fortran.y"
{	/* handle statement functions correctly */
			  if(is_true(STMT_FUNCTION_EXPR, yyvsp[0].TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN) {
			    stmt_sequence_no = SEQ_STMT_FUN;
			    f90_stmt_sequence_no = F90_SEQ_SPECIF;
			    executable_stmt = FALSE;
			  }
			  else {
			    stmt_sequence_no = SEQ_EXEC;
			    f90_stmt_sequence_no = F90_SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			  }
			  labeled_stmt_type = LAB_EXECUTABLE;
			}
    break;
case 15:
#line 533 "fortran.y"
{
			    stmt_sequence_no = SEQ_EXEC;
			    f90_stmt_sequence_no = F90_SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			    labeled_stmt_type = LAB_EXECUTABLE;
			}
    break;
case 16:
#line 541 "fortran.y"
{
			    executable_stmt = TRUE;
			    if(stmt_sequence_no == 0)
			      stmt_sequence_no = SEQ_HEADER;
			    if(f90_stmt_sequence_no == 0)
			      f90_stmt_sequence_no = SEQ_HEADER;
			    complex_const_allowed = FALSE; /* turn off flags */
			    inside_format=FALSE;
			    integer_context = FALSE;
			    in_assignment_stmt = FALSE;
			    yyval.line_num = prev_stmt_line_num; /* best guess */
			    labeled_stmt_type = LAB_EXECUTABLE;
			    yyerrok; /* (error message already given) */
			}
    break;
case 17:
#line 558 "fortran.y"
{
			    current_module_type = type_PROGRAM;
			}
    break;
case 18:
#line 562 "fortran.y"
{
			    current_module_type = type_SUBROUTINE;
			}
    break;
case 19:
#line 566 "fortran.y"
{
			    current_module_type = type_SUBROUTINE;
			}
    break;
case 20:
#line 570 "fortran.y"
{
			    current_module_type = type_BLOCK_DATA;
			}
    break;
case 22:
#line 578 "fortran.y"
{
			  if( def_label(&(yyvsp[-1]),labeled_stmt_type) ) {
			    syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
			"statement cannot be terminal statement of a DO loop");
				/* Pop it anyway to keep stack consistent */
			    pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,
				      (LABEL_t)(yyvsp[-1].value.integer));
			  }
			  yyval = yyvsp[0];
			}
    break;
case 23:
#line 594 "fortran.y"
{
			    curr_stmt_name = NULL;
			}
    break;
case 27:
#line 606 "fortran.y"
{
			    curr_stmt_name = hashtab[yyvsp[-1].value.integer].name;
			}
    break;
case 32:
#line 618 "fortran.y"
{
#ifdef ALLOW_INCLUDE
			  if(f77_include) {
			      nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,0,0);
			  }
 			  open_include_file(yyvsp[-1].value.string,yyvsp[-2].line_num);
#else
			  syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
				"statement not permitted");
#endif
 			}
    break;
case 33:
#line 637 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     if(f90_stmt_sequence_no < F90_SEQ_IMPLICIT_NONE) {
				stmt_sequence_no = F90_SEQ_IMPLICIT_NONE;
			     }
			     /* labeled_stmt_type set below  */
			}
    break;
case 34:
#line 647 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				   stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     else if(stmt_sequence_no > SEQ_SPECIF) {
			       check_stmt_sequence(&(yyvsp[0]),SEQ_SPECIF);
			     }
			     if(f90_stmt_sequence_no < F90_SEQ_IMPLICIT) {
				   f90_stmt_sequence_no = F90_SEQ_IMPLICIT;
			     }
			     else if(f90_stmt_sequence_no > F90_SEQ_SPECIF) {
			       check_f90_stmt_sequence(&(yyvsp[0]),F90_SEQ_SPECIF);
			     }
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
    break;
case 35:
#line 663 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[0]),SEQ_IMPLICIT);
			/* f90 seq checks done at implicit_stmt */
			  labeled_stmt_type = LAB_SPECIFICATION;
			}
    break;
case 36:
#line 669 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_STMT_FUN) {
				stmt_sequence_no = SEQ_STMT_FUN;
		 	     }
			     if(f90_stmt_sequence_no <= F90_SEQ_SPECIF) {
				f90_stmt_sequence_no = F90_SEQ_SPECIF;
		 	     }
			     else {
			       check_f90_stmt_sequence(&(yyvsp[0]),F90_SEQ_EXEC);
			     }
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
    break;
case 37:
#line 682 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[0]),SEQ_SPECIF);
			  check_f90_stmt_sequence(&(yyvsp[0]),F90_SEQ_SPECIF);
			  labeled_stmt_type = LAB_SPECIFICATION;
			}
    break;
case 38:
#line 690 "fortran.y"
{
			     goto_flag = prev_goto = FALSE;
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
    break;
case 39:
#line 695 "fortran.y"
{
			     labeled_stmt_type = LAB_FORMAT;
			}
    break;
case 50:
#line 716 "fortran.y"
{
			    goto_flag=TRUE;
			    make_true(NOT_DO_TERMINAL_STMT,yyval.TOK_flags);
			}
    break;
case 51:
#line 721 "fortran.y"
{
			    goto_flag=FALSE;
			}
    break;
case 53:
#line 728 "fortran.y"
{
			     if( f95_assign ) {
			       nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,f95_assign);
			       msg_tail(": assigned GOTO");
			     }
			}
    break;
case 54:
#line 735 "fortran.y"
{
			    if(style_req_block_if) {
				warning(if_line_num, if_col_num,
					"non-structured IF statement");
			    }
			}
    break;
case 55:
#line 742 "fortran.y"
{
			  check_construct_name_match(&(yyvsp[0]),
				     curr_stmt_name);
			}
    break;
case 64:
#line 757 "fortran.y"
{
			   record_io_usage(&(yyvsp[0]));
			}
    break;
case 65:
#line 763 "fortran.y"
{
			     /* If form not defined by now, READ is unformatted.
				If no REC=num was seen, then it is sequential.
			      */
			   if(current_io_form == IO_FORM_DEFAULT)
			      current_io_form = IO_FORM_UNFORMATTED;
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			}
    break;
case 67:
#line 774 "fortran.y"
{
			   if(current_io_form == IO_FORM_DEFAULT)
			      current_io_form = IO_FORM_UNFORMATTED;
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			}
    break;
case 70:
#line 783 "fortran.y"
{
			  /* In OPEN, default ACCESS is SEQUENTIAL, and default FORM
			     is FORMATTED for ACCESS=SEQUENTIAL, UNFORMATTED for
			     ACCESS=DIRECT.
			   */
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			   if(current_io_form == IO_FORM_DEFAULT) {
			     if(current_io_access == IO_ACCESS_SEQUENTIAL)
			       current_io_form = IO_FORM_FORMATTED;
			     else
			       current_io_form = IO_FORM_UNFORMATTED;
			   }
			}
    break;
case 73:
#line 800 "fortran.y"
{
			     /* These statements only apply to sequential access */
			   current_io_access = IO_ACCESS_SEQUENTIAL;
			}
    break;
case 77:
#line 813 "fortran.y"
{
			    goto_flag=FALSE;
			}
    break;
case 78:
#line 817 "fortran.y"
{
			    prev_goto = goto_flag = FALSE;
			    make_true(NOT_DO_TERMINAL_STMT,yyval.TOK_flags);
			}
    break;
case 79:
#line 822 "fortran.y"
{
			    goto_flag = TRUE;
			    make_true(NOT_DO_TERMINAL_STMT,yyval.TOK_flags);
			}
    break;
case 80:
#line 827 "fortran.y"
{
			    prev_goto = goto_flag = FALSE;
			    make_true(NOT_DO_TERMINAL_STMT,yyval.TOK_flags);
			}
    break;
case 82:
#line 836 "fortran.y"
{
			/* Note that $1 at this point is expr, not tok_IF.
			   This is undesirable for our purpose here, but
			   needed for more important stuff elsewhere.
			 */
			    push_block(&(yyvsp[0]),tok_IF,construct,curr_stmt_name,NO_LABEL);
			    make_true(NOT_DO_TERMINAL_STMT,yyval.TOK_flags);
			}
    break;
case 83:
#line 845 "fortran.y"
{	/* Flag DO w/o label or DO WHILE forms here */
			  if(is_true(NONSTD_USAGE_FLAG,yyvsp[0].TOK_flags))
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
#else
			    syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
				    "Nonstandard syntax");
#endif
			  push_block(&(yyvsp[0]),tok_DO,construct,curr_stmt_name,
				     (LABEL_t)(yyvsp[0].tsubclass));
			  make_true(NOT_DO_TERMINAL_STMT,yyval.TOK_flags);
				/* Record hash index of DO variable in the
				   block stack entry for this statement.
				 */
			  block_stack[block_depth-1].do_var_hash = yyvsp[0].value.integer;
			}
    break;
case 84:
#line 864 "fortran.y"
{
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
#else
			    syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
				    "Nonstandard syntax");
#endif
				/* pop_block is done at stmt production, where
				   optional label can be checked for match.
				 */
			}
    break;
case 85:
#line 879 "fortran.y"
{
			    pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,NO_LABEL);
			    push_block(&(yyvsp[0]),yyvsp[0].tclass,construct,curr_stmt_name,NO_LABEL);
			}
    break;
case 86:
#line 884 "fortran.y"
{
			    pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,NO_LABEL);
			    push_block(&(yyvsp[0]),yyvsp[0].tclass,construct,curr_stmt_name,NO_LABEL);
			}
    break;
case 87:
#line 889 "fortran.y"
{
			    pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,NO_LABEL);
			}
    break;
case 88:
#line 895 "fortran.y"
{
			    pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,NO_LABEL);
			    push_block(&(yyvsp[0]),yyvsp[0].tclass,construct,curr_stmt_name,NO_LABEL);
			}
    break;
case 89:
#line 900 "fortran.y"
{
			    pop_block(&(yyvsp[0]),tok_CASE,curr_stmt_name,NO_LABEL);
			    push_block(&(yyvsp[0]),tok_CASE,construct,curr_stmt_name,NO_LABEL);
			}
    break;
case 90:
#line 905 "fortran.y"
{
			    pop_block(&(yyvsp[0]),yyvsp[0].tclass,curr_stmt_name,NO_LABEL);
			}
    break;
case 91:
#line 911 "fortran.y"
{check_seq_header(&(yyvsp[0]));}
    break;
case 92:
#line 913 "fortran.y"
{
			     def_function(
					  type_PROGRAM,	/* type */
					  size_DEFAULT,	/* size */
					  (char *)NULL,	/* size text */
					  &(yyvsp[-1]),	/* name */
					  (Token*)NULL);/* args */
			     current_module_hash =
			       def_curr_module(&(yyvsp[-1]));
			}
    break;
case 93:
#line 930 "fortran.y"
{
			  do_ENTRY(&(yyvsp[-1]),(Token*)NULL
				   ,current_module_hash);
			}
    break;
case 94:
#line 935 "fortran.y"
{
			  do_ENTRY(&(yyvsp[-4]),&(yyvsp[-2])
				   ,current_module_hash);
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("entry stmt",&(yyvsp[-2]));
#endif
			}
    break;
case 96:
#line 951 "fortran.y"
{
			     if(f77_function_noparen || f90_function_noparen) {
				nonstandard(yyvsp[-1].line_num,
			     (unsigned)(yyvsp[-1].col_num+strlen(token_name(&yyvsp[-1]))),
					    f90_function_noparen,0);
				msg_tail(": parentheses required");
			     }
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &(yyvsp[-1]),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&(yyvsp[-1]));
			}
    break;
case 97:
#line 969 "fortran.y"
{
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &(yyvsp[-4]),
				      &(yyvsp[-2]));
			 current_module_hash=
			   def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&(yyvsp[-2]));
#endif
			}
    break;
case 98:
#line 984 "fortran.y"
{
			     if(f77_function_noparen || f90_function_noparen) {
				nonstandard(yyvsp[-1].line_num,
			      (unsigned)(yyvsp[-1].col_num+strlen(token_name(&yyvsp[-1]))),
					    f90_function_noparen,0);
				msg_tail(": parentheses required");
			     }
			 def_function(
				      type_UNDECL,
				      size_DEFAULT,
				      (char *)NULL,
				      &(yyvsp[-1]),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&(yyvsp[-1]));
			}
    break;
case 99:
#line 1002 "fortran.y"
{
			 def_function(
				      type_UNDECL,	/* type */
				      size_DEFAULT,	/* size */
				      (char *)NULL,	/* size text */
				      &(yyvsp[-4]),		/* name */
				      &(yyvsp[-2]));		/* args */
			 current_module_hash=
			   def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&(yyvsp[-2]));
#endif
			}
    break;
case 100:
#line 1019 "fortran.y"
{
			    yyval = yyvsp[0]; /* needed for block opener info */
			}
    break;
case 102:
#line 1028 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			}
    break;
case 107:
#line 1047 "fortran.y"
{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyvsp[-1]),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyvsp[-1]));
			}
    break;
case 108:
#line 1059 "fortran.y"
{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyvsp[-4]),
				       &(yyvsp[-2]));
			  current_module_hash=
			    def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&(yyvsp[-2]));
#endif
			}
    break;
case 109:
#line 1076 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			}
    break;
case 110:
#line 1082 "fortran.y"
{
			    yyval.next_token = (Token*)NULL;
			}
    break;
case 112:
#line 1089 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			}
    break;
case 113:
#line 1093 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			}
    break;
case 114:
#line 1099 "fortran.y"
{
			     def_arg_name(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			}
    break;
case 115:
#line 1104 "fortran.y"
{
			     yyval.TOK_type = type_byte(class_LABEL,type_LABEL);
			     yyval.size = size_DEFAULT;
			     yyval.TOK_flags = 0;
			     yyval.left_token = (Token *)NULL;
			     label_dummy_arg_count++;
			}
    break;
case 116:
#line 1117 "fortran.y"
{
				  /* form name %DATnn */
			  ++block_data_number;
			  (void)sprintf(unnamed_block_data+4,"%02d",
					block_data_number%100);
			  implied_id_token(&(yyvsp[-1]),unnamed_block_data);

			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyvsp[-1]),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyvsp[-1]));
			}
    break;
case 117:
#line 1134 "fortran.y"
{
			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyvsp[-1]),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyvsp[-1]));
			}
    break;
case 118:
#line 1147 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			}
    break;
case 122:
#line 1162 "fortran.y"
{
			     def_array_dim(&(yyvsp[-3]),&(yyvsp[-1]));
			}
    break;
case 123:
#line 1169 "fortran.y"
{
			     yyval.TOK_dims = 1;
			     yyval.TOK_elts = yyvsp[0].TOK_elts;
			     yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			}
    break;
case 124:
#line 1175 "fortran.y"
{
			     yyval.TOK_dims = yyvsp[-2].TOK_dims + 1; /* one more dimension */
			     yyval.TOK_elts = yyvsp[-2].TOK_elts * yyvsp[0].TOK_elts;
			     yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			}
    break;
case 125:
#line 1183 "fortran.y"
{
			      if( datatype_of(yyvsp[0].TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[0].TOK_flags) )
				yyval.TOK_elts = yyvsp[0].value.integer;
			      else
				yyval.TOK_elts = 0;
			}
    break;
case 126:
#line 1191 "fortran.y"
{	/* avoid getting 0 - 0 + 1 = 1 if bounds nonconstant */
			      if( datatype_of(yyvsp[-2].TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[-2].TOK_flags)
				 && datatype_of(yyvsp[0].TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[0].TOK_flags) )
				yyval.TOK_elts = yyvsp[0].value.integer - yyvsp[-2].value.integer + 1;
			      else
				yyval.TOK_elts = 0;

			      yyval.left_token = add_tree_node(&(yyvsp[-1]),&(yyvsp[-2]),&(yyvsp[0]));
			}
    break;
case 127:
#line 1203 "fortran.y"
{
			     yyval.TOK_elts = 0;
			     yyval.left_token = (Token *)NULL;
			}
    break;
case 128:
#line 1208 "fortran.y"
{
			     yyval.TOK_elts = 0;
			     yyvsp[0].left_token = (Token *)NULL;
			     yyval.left_token = add_tree_node(&(yyvsp[-1]),&(yyvsp[-2]),&(yyvsp[0]));
			}
    break;
case 129:
#line 1216 "fortran.y"
{equivalence_flag = TRUE;}
    break;
case 130:
#line 1217 "fortran.y"
{equivalence_flag = FALSE;}
    break;
case 133:
#line 1225 "fortran.y"
{
			  equivalence(&(yyvsp[-2]), &(yyvsp[0]));
			}
    break;
case 134:
#line 1229 "fortran.y"
{
			  equivalence(&(yyvsp[-2]), &(yyvsp[0]));
			}
    break;
case 135:
#line 1236 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			}
    break;
case 136:
#line 1240 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			}
    break;
case 137:
#line 1244 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			}
    break;
case 141:
#line 1259 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			     def_com_block(&(yyval), &(yyvsp[-1]));
			     if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
			   	syntax_error(
					     yyvsp[-1].line_num,yyvsp[-1].col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&(yyvsp[-1]));
#endif

			}
    break;
case 142:
#line 1273 "fortran.y"
{
			     if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					     yyvsp[-1].line_num,yyvsp[-1].col_num,
					     "trailing comma");

			}
    break;
case 143:
#line 1281 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			     def_com_block(&(yyval),&(yyvsp[-2]));
			     if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					     yyvsp[-1].line_num,yyvsp[-1].col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&(yyvsp[-2]));
#endif
			}
    break;
case 144:
#line 1299 "fortran.y"
{
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			}
    break;
case 145:
#line 1303 "fortran.y"
{
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			     yyval.line_num = yyvsp[0].line_num;
			     yyval.col_num = yyvsp[0].col_num;
			}
    break;
case 146:
#line 1311 "fortran.y"
{
			     def_com_block(&(yyvsp[-1]),&(yyvsp[0]));
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			     yyval.line_num = yyvsp[0].line_num;
			     yyval.col_num = yyvsp[0].col_num;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("labeled common",&(yyvsp[0]));
#endif
			}
    break;
case 147:
#line 1324 "fortran.y"
{
			     yyval = yyvsp[-1];
			}
    break;
case 148:
#line 1329 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			}
    break;
case 149:
#line 1333 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			}
    break;
case 150:
#line 1339 "fortran.y"
{
			    yyval.TOK_flags = yyvsp[0].TOK_flags;
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			}
    break;
case 151:
#line 1344 "fortran.y"
{
			    if(!is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num-1,
					"missing comma");
			    yyval.TOK_flags = yyvsp[0].TOK_flags;
			    yyval.line_num = yyvsp[0].line_num;
			    yyval.col_num = yyvsp[0].col_num;
			    yyval.next_token = append_token(yyvsp[-1].next_token,&(yyvsp[0]));
			}
    break;
case 152:
#line 1357 "fortran.y"
{			   /* no comma */
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			     make_false(COMMA_FLAG,yyval.TOK_flags);
			}
    break;
case 153:
#line 1362 "fortran.y"
{			   /* has comma */
			     yyval.TOK_flags = yyvsp[-1].TOK_flags;
			     make_true(COMMA_FLAG,yyval.TOK_flags);
   			}
    break;
case 154:
#line 1369 "fortran.y"
{
			     def_com_variable(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			}
    break;
case 155:
#line 1374 "fortran.y"
{
			     def_com_variable(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			}
    break;
case 156:
#line 1387 "fortran.y"
{
			    if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(yyvsp[-1].line_num,
				 (unsigned)(yyvsp[-1].col_num+strlen(token_name(&yyvsp[-1]))),
					"trailing comma");
			    if(f77_namelist) {
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,0,0);
			    }
			}
    break;
case 158:
#line 1400 "fortran.y"
{
			    yyval = yyvsp[0];
			}
    break;
case 159:
#line 1406 "fortran.y"
{
			     def_namelist(&(yyvsp[-1]),&(yyvsp[0]));
			     yyval = yyvsp[0];
			}
    break;
case 160:
#line 1413 "fortran.y"
{
			    yyval = yyvsp[-1];
			}
    break;
case 161:
#line 1419 "fortran.y"
{
			     yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			}
    break;
case 162:
#line 1423 "fortran.y"
{
			    if(!is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num-1,
					"missing comma");
			    yyval.TOK_flags = yyvsp[0].TOK_flags;
			    yyval.line_num = yyvsp[0].line_num;
			    yyval.col_num = yyvsp[0].col_num;
			    yyval.next_token = append_token(yyvsp[-1].next_token,&(yyvsp[0]));
			}
    break;
case 163:
#line 1436 "fortran.y"
{			   /* no comma */
			     def_namelist_item(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			     make_false(COMMA_FLAG,yyval.TOK_flags);
			}
    break;
case 164:
#line 1442 "fortran.y"
{			   /* has comma */
			     def_namelist_item(&(yyvsp[-1]));
			     primary_id_expr(&(yyvsp[-1]),&(yyval));
			     make_true(COMMA_FLAG,yyval.TOK_flags);
			}
    break;
case 169:
#line 1459 "fortran.y"
{
			  if(f77_attrbased_typedecl) {
			    nonstandard(yyvsp[-3].line_num, yyvsp[-3].col_num,0,0);
			    msg_tail(": attribute-based variable declaration");
			  }
			}
    break;
case 170:
#line 1467 "fortran.y"
{
			  if(f77_attrbased_typedecl) {
			    nonstandard(yyvsp[-3].line_num, yyvsp[-3].col_num,0,0);
			    msg_tail(": attribute-based variable declaration");
			  }
			}
    break;
case 179:
#line 1491 "fortran.y"
{
				/* turn back on flags turned off by punct  */
			  in_attrbased_typedecl = initial_flag = TRUE;
			  dim_bound_token = yyvsp[-1];	/* save copy of header */
			  current_dim_bound_list = &dim_bound_token;
			}
    break;
case 180:
#line 1498 "fortran.y"
{
			     current_save_attr = TRUE;
			}
    break;
case 181:
#line 1502 "fortran.y"
{
			     current_external_attr = TRUE;
			}
    break;
case 182:
#line 1506 "fortran.y"
{
			     current_intrinsic_attr = TRUE;
			}
    break;
case 183:
#line 1510 "fortran.y"
{
			     current_parameter_attr = TRUE;
			}
    break;
case 184:
#line 1517 "fortran.y"
{
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			  reset_type_attrs();
			}
    break;
case 185:
#line 1524 "fortran.y"
{
			    current_typesize = yyvsp[0].value.integer;
			    current_len_text = NULL;
			    reset_type_attrs();
#if 0 /* defunct feature */
			    if(local_wordsize > 0) {
			      /*  recognize REAL*2w as DOUBLE PRECISION */
			      if(current_datatype == type_REAL
				 && yyvsp[0].value.integer == type_size[type_DP])
				current_datatype = type_DP;
			      /*  recognize COMPLEX*4w as DOUBLE COMPLEX */
			      if(current_datatype == type_COMPLEX
				 && yyvsp[0].value.integer==type_size[type_DCOMPLEX])
				current_datatype = type_DCOMPLEX;
			    }
#endif
			     if(f77_typesize || f90_typesize) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,f90_typesize,0);
			     }

				/* Give hint to lexer to continue taking attrs
				   as keywords despite non-initial position */
			     if(see_double_colon())
				 in_attrbased_typedecl = TRUE;
			}
    break;
case 186:
#line 1553 "fortran.y"
{
				/* Treat all KINDs as default */
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			  reset_type_attrs();
			}
    break;
case 187:
#line 1562 "fortran.y"
{
			    reset_type_attrs();
			}
    break;
case 188:
#line 1568 "fortran.y"
{
			     current_datatype = type_INTEGER;
			     integer_context = TRUE;
			}
    break;
case 189:
#line 1573 "fortran.y"
{
			     current_datatype = type_REAL;
			     integer_context = TRUE;
			}
    break;
case 190:
#line 1578 "fortran.y"
{
			     current_datatype = type_COMPLEX;
			     integer_context = TRUE;
			}
    break;
case 191:
#line 1583 "fortran.y"
{
			     current_datatype = type_LOGICAL;
			     integer_context = TRUE;
			}
    break;
case 192:
#line 1590 "fortran.y"
{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     reset_type_attrs();
			}
    break;
case 193:
#line 1597 "fortran.y"
{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     reset_type_attrs();
			     if(f77_double_complex || f90_double_complex) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,f90_double_complex,0);
			     }
			}
    break;
case 194:
#line 1607 "fortran.y"
{
			     current_datatype = type_INTEGER;
			     current_typesize = 1;
			     current_len_text = NULL;
			     reset_type_attrs();
			     if(f77_byte || f90_byte)
			       nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,f90_byte,0);
			}
    break;
case 195:
#line 1623 "fortran.y"
{
			  if(!kind_warning_given)
			      give_kind_warning(&(yyvsp[0]));
			  if(f77_attrbased_typedecl) {
			    nonstandard(yyvsp[0].line_num, yyvsp[0].col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }
			}
    break;
case 196:
#line 1632 "fortran.y"
{
			  int erroneous=FALSE;
			  if( strcmp(hashtab[yyvsp[-2].value.integer].name,"KIND")
			      == 0 ) {
			    if(!kind_warning_given)
			      give_kind_warning(&(yyvsp[-2]));
			  }
			  else {
			    syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
					 "unrecognized keyword");
			    msg_tail(hashtab[yyvsp[-2].value.integer].name);
			    erroneous=TRUE;
			  }
			  if(!erroneous && f77_attrbased_typedecl) {
			    nonstandard(yyvsp[-2].line_num, yyvsp[-2].col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }
			}
    break;
case 197:
#line 1654 "fortran.y"
{
			     current_datatype = type_STRING;
			     current_typesize = 1;
			     current_len_text = NULL;
			     current_size_is_adjustable = 0;
			     current_size_is_expression = 0;
			     reset_type_attrs();
			     integer_context = TRUE;
			     len_selector_given = FALSE;
			}
    break;
case 198:
#line 1667 "fortran.y"
{
			     current_typesize = yyvsp[0].value.integer;
			     current_size_is_adjustable = yyvsp[0].size_is_adjustable;
			     current_size_is_expression = yyvsp[0].size_is_expression;
				/* Save length spec text if expression */
			     if(current_size_is_expression) {
			       if(yyvsp[0].left_token == NULL)
				 current_len_text = new_tree_text(&(yyvsp[0]));
			       else
				 current_len_text = new_tree_text(yyvsp[0].left_token);
			     }
			     else
			       current_len_text = NULL;

			     reset_type_attrs();
				/* Give hint to lexer to continue taking attrs
				   as keywords despite non-initial position */
			     if(see_double_colon())
				 in_attrbased_typedecl = TRUE;
			}
    break;
case 199:
#line 1690 "fortran.y"
{
			  yyval = yyvsp[0];
			}
    break;
case 200:
#line 1700 "fortran.y"
{len_spec_item_count = 0;}
    break;
case 201:
#line 1701 "fortran.y"
{
			  if( len_selector_given ) {
			    yyval = len_spec_token; /* Recover spec saved below */
				/* Store as a parenthesized expr tree */
			    yyval.left_token = add_tree_node(&(yyvsp[-3]),
							  &len_spec_token,
							  (Token*)NULL);
			  }
				/* If len_spec_list does not specify a LEN,
				   use the current default values.
				 */
			  else {
			    yyval.left_token = (Token *)NULL;
			    yyval.value.integer = current_typesize;
			    yyval.size_is_adjustable = current_size_is_adjustable;
			    yyval.size_is_expression = current_size_is_expression;
			  }
			  if(f77_attrbased_typedecl) {
			    nonstandard(yyvsp[-3].line_num, yyvsp[-3].col_num,0,0);
			    msg_tail(": F90-style variable declaration");
			  }
			}
    break;
case 202:
#line 1730 "fortran.y"
{
			  yyval.tclass = '('; /* make it a regular paren */
			}
    break;
case 205:
#line 1743 "fortran.y"
{
			     if( current_parameter_attr) {
				syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					     "PARAMETER lacks initializer");
			     }
			}
    break;
case 206:
#line 1753 "fortran.y"
{integer_context=FALSE;complex_const_allowed=TRUE;}
    break;
case 207:
#line 1755 "fortran.y"
{integer_context=TRUE;complex_const_allowed=FALSE;}
    break;
case 208:
#line 1756 "fortran.y"
{
			    if(f77_initializers || f90_initializers) {
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    primary_id_expr(&(yyvsp[-5]),&(yyvsp[-5]));
			    check_initializer_type(&(yyvsp[-5]),&(yyvsp[-4]),&(yyvsp[-2]));
			}
    break;
case 209:
#line 1770 "fortran.y"
{integer_context=FALSE;complex_const_allowed = TRUE;}
    break;
case 210:
#line 1772 "fortran.y"
{
			    if(current_parameter_attr)
				def_parameter(&(yyvsp[-3]),&(yyvsp[0]),FALSE);
			    else
				use_lvalue(&(yyvsp[-3]));
			    if(f77_initializers) {
				nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,
					    0,0);
				msg_tail(": F90-style initializer");
			    }
			    primary_id_expr(&(yyvsp[-3]),&(yyvsp[-3]));
			    check_initializer_type(&(yyvsp[-3]),&(yyvsp[-1]),&(yyvsp[0]));
			    integer_context=TRUE;
			    complex_const_allowed = FALSE;
			}
    break;
case 211:
#line 1792 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&(yyvsp[0]),(Token *)NULL);
			}
    break;
case 212:
#line 1803 "fortran.y"
{integer_context=FALSE;complex_const_allowed=TRUE;}
    break;
case 213:
#line 1805 "fortran.y"
{integer_context=TRUE;complex_const_allowed=FALSE;}
    break;
case 214:
#line 1806 "fortran.y"
{
			    declare_type(&(yyvsp[-5]),
					 current_datatype,
					 current_typesize,
					 current_len_text);
			    process_attrs(&(yyvsp[-5]),(Token *)NULL);
			    if(f77_initializers || f90_initializers) {
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    use_lvalue(&(yyvsp[-5]));
			    primary_id_expr(&(yyvsp[-5]),&(yyvsp[-5]));
			    check_initializer_type(&(yyvsp[-5]),&(yyvsp[-4]),&(yyvsp[-2]));
			}
    break;
case 215:
#line 1824 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&(yyvsp[0]),current_dim_bound_list);
			}
    break;
case 218:
#line 1838 "fortran.y"
{
			     if( current_parameter_attr) {
				syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					     "PARAMETER lacks initializer");
			     }
			}
    break;
case 219:
#line 1846 "fortran.y"
{integer_context=FALSE;complex_const_allowed=TRUE;}
    break;
case 220:
#line 1848 "fortran.y"
{integer_context=TRUE;complex_const_allowed=FALSE;}
    break;
case 221:
#line 1849 "fortran.y"
{
			    if(f77_initializers || f90_initializers) {
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    primary_id_expr(&(yyvsp[-5]),&(yyvsp[-5]));
			    check_initializer_type(&(yyvsp[-5]),&(yyvsp[-4]),&(yyvsp[-2]));
			}
    break;
case 222:
#line 1860 "fortran.y"
{
			    if(current_parameter_attr)
				def_parameter(&(yyvsp[-2]),&(yyvsp[0]),FALSE);
			    else
				use_lvalue(&(yyvsp[-2]));
			    primary_id_expr(&(yyvsp[-2]),&(yyvsp[-2]));
			    if(f77_initializers) {
				nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,
					    0,0);
				msg_tail(": F90-style initializer");
			    }
			    check_initializer_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0]));
			}
    break;
case 223:
#line 1874 "fortran.y"
{
			     yyvsp[0].size_is_adjustable = current_size_is_adjustable;
			     yyvsp[0].size_is_expression = current_size_is_expression;
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&(yyvsp[0]),(Token *)NULL);
			}
    break;
case 224:
#line 1884 "fortran.y"
{
			     yyvsp[-2].size_is_adjustable = yyvsp[0].size_is_adjustable;
			     yyvsp[-2].size_is_expression = yyvsp[0].size_is_expression;
			     declare_type(&(yyvsp[-2]),
					  current_datatype,
					  yyvsp[0].value.integer,
					  new_tree_text(
					     yyvsp[0].left_token == NULL?
					     &(yyvsp[0]): yyvsp[0].left_token )
					  );
			     process_attrs(&(yyvsp[-2]),(Token *)NULL);
			}
    break;
case 225:
#line 1897 "fortran.y"
{integer_context=FALSE;complex_const_allowed=TRUE;}
    break;
case 226:
#line 1899 "fortran.y"
{integer_context=TRUE;complex_const_allowed=FALSE;}
    break;
case 227:
#line 1900 "fortran.y"
{
			    yyvsp[-5].size_is_adjustable = current_size_is_adjustable;
			    yyvsp[-5].size_is_expression = current_size_is_expression;
			    declare_type(&(yyvsp[-5]),
					 current_datatype,
					 current_typesize,
					 current_len_text);
			    process_attrs(&(yyvsp[-5]),(Token *)NULL);
			    if(f77_initializers || f90_initializers) {
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    use_lvalue(&(yyvsp[-5]));
			}
    break;
case 228:
#line 1916 "fortran.y"
{integer_context=FALSE;complex_const_allowed=TRUE;}
    break;
case 229:
#line 1918 "fortran.y"
{integer_context=TRUE;complex_const_allowed=FALSE;}
    break;
case 230:
#line 1919 "fortran.y"
{
			    yyvsp[-7].size_is_adjustable = yyvsp[-5].size_is_adjustable;
			    yyvsp[-7].size_is_expression = yyvsp[-5].size_is_expression;
			    declare_type(&(yyvsp[-7]),
					 current_datatype,
					 yyvsp[-5].value.integer,
					 new_tree_text(
					     yyvsp[-5].left_token == NULL?
					     &(yyvsp[-5]): yyvsp[-5].left_token )
				);
			    process_attrs(&(yyvsp[-7]),(Token *)NULL);
			    if(f77_initializers || f90_initializers) {
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    use_lvalue(&(yyvsp[-7]));
			}
    break;
case 231:
#line 1940 "fortran.y"
{
			     yyvsp[0].size_is_adjustable = current_size_is_adjustable;
			     yyvsp[0].size_is_expression = current_size_is_expression;
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&(yyvsp[0]),current_dim_bound_list);
			}
    break;
case 232:
#line 1950 "fortran.y"
{
			     yyvsp[-2].size_is_adjustable = yyvsp[0].size_is_adjustable;
			     yyvsp[-2].size_is_expression = yyvsp[0].size_is_expression;
			     declare_type(&(yyvsp[-2]),
					  current_datatype,
					  yyvsp[0].value.integer,
					  new_tree_text(
					     yyvsp[0].left_token == NULL?
					     &(yyvsp[0]): yyvsp[0].left_token )
					  );
			     process_attrs(&(yyvsp[-2]),current_dim_bound_list);
			}
    break;
case 233:
#line 1966 "fortran.y"
{implicit_flag=TRUE;}
    break;
case 234:
#line 1970 "fortran.y"
{
			    implicit_flag=FALSE;
			    if(implicit_none) {
				syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
				     "conflicts with IMPLICIT NONE");
			    }
			    else {
				implicit_type_given = TRUE;
			    }
			    check_f90_stmt_sequence(&(yyvsp[-2]),F90_SEQ_IMPLICIT);
			}
    break;
case 235:
#line 1983 "fortran.y"
{
			    implicit_flag=FALSE;
			    if(implicit_type_given) {
			      syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
				   "conflicts with IMPLICIT statement");
			    }
			    else {
			      if(f77_implicit_none)
				      nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,0,0);
			      implicit_none = TRUE;
			    }
			    check_f90_stmt_sequence(&(yyvsp[-2]),F90_SEQ_IMPLICIT_NONE);
			}
    break;
case 237:
#line 1999 "fortran.y"
{initial_flag = TRUE;}
    break;
case 239:
#line 2005 "fortran.y"
{implicit_letter_flag = TRUE;}
    break;
case 240:
#line 2006 "fortran.y"
{implicit_letter_flag = FALSE;}
    break;
case 243:
#line 2014 "fortran.y"
{
			  int c1 = (int)yyvsp[0].tsubclass;
				/* kluge to allow other non-alpha chars:
				   treate anything except _ as $.
				 */
			  if( !isalpha(c1) && c1 != '_' ) c1 = '$';

			  if( ((f77_dollarsigns||f90_dollarsigns) && c1=='$')
			   || (f77_underscores && c1=='_') ) {
			    nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,
				f90_dollarsigns&&c1=='$',0);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c1);
			}
    break;
case 244:
#line 2034 "fortran.y"
{
			  int c1 = (int)yyvsp[-2].tsubclass,
			      c2 = (int)yyvsp[0].tsubclass;

			  if( !isalpha(c1) && c1 != '_' ) c1 = '$';
			  if( !isalpha(c2) && c2 != '_' ) c2 = '$';

			  if( ((f77_dollarsigns||f90_dollarsigns) && (c1 == '$' || c2 == '$'))
			   || (f77_underscores && (c1 == '_' || c2 == '_')))
			  {
			    if(!isalpha(c1))
			      nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,
				  f90_dollarsigns&&(c1=='$'||c2=='$'),0);
			    else
			      nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,
				  f90_dollarsigns&&(c1=='$'||c2=='$'),0);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c2);
			}
    break;
case 245:
#line 2063 "fortran.y"
{
			     yyval.value.integer = yyvsp[0].value.integer;
			     yyval.size_is_adjustable = 0;
			     yyval.size_is_expression = 0;
			}
    break;
case 246:
#line 2070 "fortran.y"
{
			    yyval.value.integer = yyvsp[-1].value.integer;
			    yyval.size_is_adjustable = yyvsp[-1].size_is_adjustable;
			    yyval.size_is_expression = yyvsp[-1].size_is_expression;
				/* Store as a parenthesized expr tree */
			    yyval.left_token = add_tree_node(&(yyvsp[-2]),
							  &(yyvsp[-1]),
							  (Token*)NULL);
			}
    break;
case 247:
#line 2101 "fortran.y"
{
			  ++len_spec_item_count;
			}
    break;
case 248:
#line 2105 "fortran.y"
{
			  ++len_spec_item_count;
			}
    break;
case 249:
#line 2111 "fortran.y"
{
				/* Non-keyword form: 1st item is LEN */
			  if(len_spec_item_count == 0) {
			    len_spec_token = yyvsp[0];
			    len_selector_given = TRUE;
			  }
				/* 2nd item is KIND */
			  else if(len_spec_item_count == 1) {
			    if(!kind_warning_given)
			      give_kind_warning(&(yyvsp[0]));
			  }
			  else if(len_spec_item_count == 2) {
			    syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					 "too many specifiers in list");
			  }
			}
    break;
case 250:
#line 2128 "fortran.y"
{
			  int erroneous=FALSE;
			  if( strcmp(hashtab[yyvsp[-2].value.integer].name,"LEN")
			      == 0 ) {
			    len_spec_token = yyvsp[0];
			    len_selector_given = TRUE;
			  }
			  else if( strcmp(hashtab[yyvsp[-2].value.integer].name,"KIND")
			      == 0 ) {
			    if(!kind_warning_given)
			      give_kind_warning(&(yyvsp[-2]));
			  }
			  else {
			    syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
					 "unrecognized keyword");
			    msg_tail(hashtab[yyvsp[-2].value.integer].name);
			    erroneous=TRUE;
			  }
			  if(!erroneous && f77_attrbased_typedecl) {
			    nonstandard(yyvsp[-1].line_num, yyvsp[-1].col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }
			}
    break;
case 251:
#line 2154 "fortran.y"
{
			     yyval.left_token = (Token *)NULL;
			     yyval.value.integer = size_ADJUSTABLE;
			     yyval.size_is_adjustable = 1;
			     yyval.size_is_expression = 0;
			}
    break;
case 252:
#line 2161 "fortran.y"
{
			     yyval.size_is_adjustable = 0;
			     yyval.size_is_expression = 1;
			     if( yyval.value.integer <= 0 ){
			       if(misc_warn) {
				 warning(yyvsp[0].line_num,yyvsp[0].col_num,
					"invalid length specification");
				 msg_tail(": substituting 1");
			       }
			       yyval.value.integer = 1;
			     }
			}
    break;
case 256:
#line 2183 "fortran.y"
{param_noparen=TRUE;}
    break;
case 257:
#line 2184 "fortran.y"
{param_noparen=FALSE;}
    break;
case 258:
#line 2185 "fortran.y"
{
			  if(f77_param_noparen || f90_param_noparen) {
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,f90_param_noparen,0);
				msg_tail(" : PARAMETER declaration without parentheses");
			  }
			}
    break;
case 261:
#line 2197 "fortran.y"
{complex_const_allowed = TRUE;}
    break;
case 262:
#line 2199 "fortran.y"
{
			     def_parameter(&(yyvsp[-3]),&(yyvsp[0]),param_noparen);
			     primary_id_expr(&(yyvsp[-3]),&(yyvsp[-3]));
			     check_initializer_type(&(yyvsp[-3]),&(yyvsp[-1]),&(yyvsp[0]));
			     complex_const_allowed = FALSE;
			}
    break;
case 264:
#line 2212 "fortran.y"
{
			     def_ext_name(&(yyvsp[0]));
			}
    break;
case 265:
#line 2216 "fortran.y"
{
			     def_ext_name(&(yyvsp[0]));
			}
    break;
case 267:
#line 2226 "fortran.y"
{
			     def_intrins_name(&(yyvsp[0]));
			}
    break;
case 268:
#line 2230 "fortran.y"
{
			     def_intrins_name(&(yyvsp[0]));
			}
    break;
case 269:
#line 2237 "fortran.y"
{
		  if(f77_cray_pointers || f90_cray_pointers) {
		    nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,f90_cray_pointers,0);
		  }
		}
    break;
case 273:
#line 2252 "fortran.y"
{
			     declare_type(&(yyvsp[0]),type_INTEGER,local_ptrsize,
					  NULL );
			}
    break;
case 274:
#line 2259 "fortran.y"
{
				/* Suppress set/used warnings since
				   often is accessed only via pointer */
		             use_lvalue(&(yyvsp[0]));
		             use_variable(&(yyvsp[0]));
		        }
    break;
case 275:
#line 2266 "fortran.y"
{
		             use_lvalue(&(yyvsp[0]));
		             use_variable(&(yyvsp[0]));
		        }
    break;
case 276:
#line 2274 "fortran.y"
{
			  global_save = TRUE;
			}
    break;
case 280:
#line 2285 "fortran.y"
{
			     save_variable(&(yyvsp[0]));
			}
    break;
case 281:
#line 2289 "fortran.y"
{
/***			     def_com_block(&($2),(Token*)NULL);***/
			     save_com_block(&(yyvsp[-1]));
			}
    break;
case 286:
#line 2305 "fortran.y"
{complex_const_allowed=TRUE;}
    break;
case 287:
#line 2307 "fortran.y"
{complex_const_allowed=FALSE;}
    break;
case 291:
#line 2316 "fortran.y"
{
			     use_lvalue(&(yyvsp[0]));
			}
    break;
case 293:
#line 2323 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			}
    break;
case 294:
#line 2327 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			}
    break;
case 295:
#line 2333 "fortran.y"
{
			    yyval.left_token = (Token*)NULL;
			}
    break;
case 296:
#line 2337 "fortran.y"
{
				/* Save data repeat factor in a permanent token
				   pointed to by left_token.
				 */
			    Token *tcopy = new_token();
			    *tcopy = yyvsp[-2]; /* copy the repeat factor token */
			    yyval = yyvsp[0]; /* pass data_value up the parse tree */
			    yyval.left_token = tcopy;
			}
    break;
case 298:
#line 2350 "fortran.y"
{
			     use_parameter(&(yyvsp[0]));
			}
    break;
case 300:
#line 2357 "fortran.y"
{
			     use_parameter(&(yyvsp[0]));
			}
    break;
case 303:
#line 2368 "fortran.y"
{
			     use_lvalue(&(yyvsp[0]));
			}
    break;
case 305:
#line 2376 "fortran.y"
{
			    use_implied_do_index(&(yyvsp[-3]));
			}
    break;
case 308:
#line 2387 "fortran.y"
{complex_const_allowed = TRUE;
				    in_assignment_stmt = TRUE;}
    break;
case 309:
#line 2389 "fortran.y"
{
			  if( ! (is_true(LVALUE_EXPR,yyvsp[-3].TOK_flags)
			       || is_true(STMT_FUNCTION_EXPR,yyvsp[-3].TOK_flags) )) {
			    syntax_error(yyvsp[-3].line_num,yyvsp[-3].col_num,
					 "left side is not assignable");
			    if(is_true(CONST_EXPR,yyvsp[-3].TOK_flags))
				msg_tail(": it is a constant");
			  }
			  else {
			    int array_lhs, array_rhs;
			    array_lhs =
			      ((yyvsp[-3].TOK_flags&(ARRAY_ID_EXPR|ARRAY_ELEMENT_EXPR)) == ARRAY_ID_EXPR);
			    array_rhs =
			      ((yyvsp[0].TOK_flags&(ARRAY_ID_EXPR|ARRAY_ELEMENT_EXPR)) == ARRAY_ID_EXPR);
			    if( array_lhs || array_rhs ) {
			      if( (! array_lhs) && misc_warn) {
				warning(yyvsp[-3].line_num,yyvsp[-3].col_num,
					"array assigned to scalar");
			      }
			      else if( f77_assignment ) {
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,0,0);
				msg_tail(": assignment involving whole array");
			      }
			    }

			    assignment_stmt_type(&(yyvsp[-3]),&(yyvsp[-2]),
					&(yyvsp[0]));
			  }
			  complex_const_allowed = FALSE;
			  in_assignment_stmt = FALSE;
			}
    break;
case 310:
#line 2421 "fortran.y"
{
				/* Clear u-b-s flags spuriously set */
			  if(is_true(STMT_FUNCTION_EXPR, yyvsp[-5].TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN)
			     stmt_function_stmt(&(yyvsp[-5]));
		        }
    break;
case 315:
#line 2439 "fortran.y"
{
			    do_ASSIGN(&(yyvsp[-1]));
			    if( f95_assign ) {
			      nonstandard(yyvsp[-5].line_num,yyvsp[-5].col_num,0,f95_assign);
			      msg_tail(": ASSIGN statement");
			    }

			    ref_label(&(yyvsp[-3]),LAB_ASSIGN);

			}
    break;
case 316:
#line 2454 "fortran.y"
{

			  ref_label(&(yyvsp[-1]),LAB_GOTO);

			}
    break;
case 319:
#line 2468 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-1]));
			}
    break;
case 320:
#line 2472 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-4]));
			}
    break;
case 321:
#line 2476 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-5]));
			}
    break;
case 322:
#line 2482 "fortran.y"
{
			    integer_context=TRUE;
				/* Warn if GOTO considered harmful */
			    if( style_goto ) {
			      warning(yyvsp[0].line_num,yyvsp[0].col_num,
				      "obsolescent feature: GOTO statement");
			    }
			}
    break;
case 323:
#line 2493 "fortran.y"
{
                            ref_label(&(yyvsp[0]), LAB_GOTO);
			}
    break;
case 324:
#line 2497 "fortran.y"
{
			    ref_label(&(yyvsp[0]), LAB_GOTO);
			}
    break;
case 325:
#line 2505 "fortran.y"
{
			  int t=datatype_of(yyvsp[-9].TOK_type);
			  if(t != type_INTEGER && t != type_REAL
			     && t != type_DP && t != type_ERROR ) {
			    syntax_error(yyvsp[-9].line_num,yyvsp[-9].col_num,
		  "integer, real, or double precision expression required");
			  }
			  ref_label(&(yyvsp[-7]), LAB_GOTO);
			  ref_label(&(yyvsp[-4]), LAB_GOTO);
			  ref_label(&(yyvsp[-1]), LAB_GOTO);

			}
    break;
case 326:
#line 2521 "fortran.y"
{
			  int t=datatype_of(yyvsp[-1].TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error(yyvsp[-1].line_num,yyvsp[-1].col_num,
					  "logical expression required");
			}
    break;
case 327:
#line 2531 "fortran.y"
{
			  int t=datatype_of(yyvsp[-2].TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
					  "logical expression required");

		/* In picky mode warn if no name tag on block construct.
		   By this time $1 is the expr, not tok_IF, so line and
		   column must be those saved at lower-level productions.
		 */
			  if(curr_stmt_name == NULL &&
			     style_req_construct_name) {
			      warning(if_line_num,if_col_num,
				"Construct name missing from IF statement");
			  }
			}
    break;
case 328:
#line 2550 "fortran.y"
{
			    curr_stmt_name = NULL;
			}
    break;
case 329:
#line 2554 "fortran.y"
{
			  if(f77_construct_name) {
			    nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			  construct_name_seen=FALSE;
			  yyval = yyvsp[0];
			}
    break;
case 330:
#line 2564 "fortran.y"
{complex_const_allowed = TRUE;}
    break;
case 331:
#line 2565 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;	/* for is_keyword */
			    if_line_num = yyvsp[-4].line_num; /* save location */
			    if_col_num = yyvsp[-4].col_num; /* for picky warnings */
			    yyval = yyvsp[-1]; /* Inherit expr for type checking above */
			}
    break;
case 332:
#line 2579 "fortran.y"
{complex_const_allowed = TRUE;}
    break;
case 333:
#line 2580 "fortran.y"
{
			    int t=datatype_of(yyvsp[-1].TOK_type);
			    if(t != type_LOGICAL && t != type_ERROR)
				syntax_error(yyvsp[-1].line_num,yyvsp[-1].col_num,
					  "logical expression required");

			    if(is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }

			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;
			}
    break;
case 335:
#line 2598 "fortran.y"
{
			    curr_stmt_name = NULL;
			}
    break;
case 336:
#line 2602 "fortran.y"
{
			  if(f77_construct_name) {
			    nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
    break;
case 337:
#line 2612 "fortran.y"
{
			    curr_stmt_name = NULL;
			}
    break;
case 338:
#line 2616 "fortran.y"
{
			  if(f77_construct_name) {
			    nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
    break;
case 339:
#line 2626 "fortran.y"
{
			    curr_stmt_name = NULL;
			}
    break;
case 340:
#line 2630 "fortran.y"
{
			  if(f77_construct_name) {
			    nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
    break;
case 341:
#line 2646 "fortran.y"
{complex_const_allowed = TRUE;}
    break;
case 342:
#line 2647 "fortran.y"
{
			    int t = datatype_of (yyvsp[-2].TOK_type);
			    if (t != type_ERROR) {
			        if (!is_const_type(t) || is_float_type(t)) {
			            syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
			"integer, character, or logical expression required");
			        }
			    }
			    if(is_true(ID_EXPR,yyvsp[-2].TOK_flags)){
				use_variable(&(yyvsp[-2]));
			    }
			    complex_const_allowed = FALSE;
			    push_block(&(yyvsp[-5]),yyvsp[-5].tclass,construct,curr_stmt_name,NO_LABEL);
			}
    break;
case 343:
#line 2664 "fortran.y"
{
			    curr_stmt_name = NULL;
			    if (f77_case_construct) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
			    }
			    if( style_req_construct_name ) {
			      warning(yyvsp[0].line_num,yyvsp[0].col_num,
			       "Construct name missing from SELECT statement");
			    }
			}
    break;
case 344:
#line 2675 "fortran.y"
{
			    if (f77_case_construct) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
			    }
			    yyval = yyvsp[0];
			}
    break;
case 351:
#line 2696 "fortran.y"
{
			    int t1 = datatype_of(yyvsp[-2].TOK_type),
			        t2 = datatype_of(yyvsp[0].TOK_type);
			    if (t1 == type_LOGICAL || t2 == type_LOGICAL) {
			        syntax_error(yyvsp[-1].line_num,yyvsp[-1].col_num,
			            "ranges of type LOGICAL not allowed here");
			    }
			    if (t1 != t2) {
			        syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
			            "range boundaries must have the same type");
			    }
			}
    break;
case 352:
#line 2709 "fortran.y"
{
			    int t = datatype_of(yyvsp[0].TOK_type);
			    if (t == type_LOGICAL) {
			        syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
			            "ranges may not have type LOGICAL bounds");
			    }
			}
    break;
case 353:
#line 2717 "fortran.y"
{
			    int t = datatype_of(yyvsp[0].TOK_type);
			    if (t == type_LOGICAL) {
			        syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
			            "ranges may not have type LOGICAL bounds");
			    }
			}
    break;
case 354:
#line 2727 "fortran.y"
{
			    int t = datatype_of(yyvsp[0].TOK_type);
			    if (t != type_ERROR) {
			        if (!is_const_type(t) || is_float_type(t)) {
			            syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
			"integer, character, or logical expression required");
			        }
			    }
			    if (!is_true(CONST_EXPR, yyvsp[0].TOK_flags)) {
			        syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
			"expression must evaluate to a compile-time constant");
			    }
			    yyval = yyvsp[0];
			}
    break;
case 359:
#line 2762 "fortran.y"
{
			  if( ! is_true(LVALUE_EXPR,yyvsp[-3].TOK_flags) ) {
			    syntax_error(yyvsp[-3].line_num,yyvsp[-3].col_num,
					 "index is not assignable");
			    if(is_true(CONST_EXPR,yyvsp[-3].TOK_flags))
				msg_tail(": it is a constant");
			    yyval.value.integer = -1; /* no hash entry */
			  }
			  else {
			     def_do_variable(&(yyvsp[-3]));
				/* Store hash index of DO index in token for
				   use when pushing block on stack. The
				   value field is not used by keywords, so it
				   is OK to use it this way. */
			     yyval.value.integer = yyvsp[-3].value.integer;
			  }

				/* Check for non-integer DO index or bounds */
			     if(datatype_of(yyvsp[-3].TOK_type) == type_INTEGER
				&& datatype_of(yyvsp[-1].TOK_type) != type_INTEGER) {
			       if( f95_real_do ) {
				 nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,0,f95_real_do);
				 msg_tail(": DO loop bounds not integer");
			       }
			       else if(misc_warn) {
				 warning(yyvsp[-2].line_num,yyvsp[-2].col_num,
				  "type mismatch between DO index and bounds");
			       }
			     }
			     else if(datatype_of(yyvsp[-3].TOK_type) != type_INTEGER) {
			       if( f95_real_do ) {
				 nonstandard(yyvsp[-3].line_num,yyvsp[-3].col_num,0,f95_real_do);
				 msg_tail(": DO index is not integer");
			       }
			       else if(datatype_of(yyvsp[-1].TOK_type) != type_INTEGER) {
				 if(port_real_do)
				   nonportable(yyvsp[-1].line_num,yyvsp[-1].col_num,
					       "non-integer DO loop");
			       }
			       else {
				 if(trunc_real_do_index) {
				   warning(yyvsp[-3].line_num,yyvsp[-3].col_num,
					   "DO index is not integer");
				 }
			       }
			     }
			}
    break;
case 360:
#line 2810 "fortran.y"
{complex_const_allowed=TRUE;}
    break;
case 361:
#line 2811 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-2].TOK_flags)){
				use_variable(&(yyvsp[-2]));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,yyval.TOK_flags);
			    yyval.value.integer = -1; /* no DO index */
			    curr_stmt_name = NULL;
			}
    break;
case 362:
#line 2824 "fortran.y"
{complex_const_allowed=TRUE;}
    break;
case 363:
#line 2825 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-2].TOK_flags)){
				use_variable(&(yyvsp[-2]));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,yyval.TOK_flags);
			    yyval.value.integer = -1; /* no DO index */
			}
    break;
case 364:
#line 2834 "fortran.y"
{
			    make_true(NONSTD_USAGE_FLAG,yyval.TOK_flags);
			    yyval.value.integer = -1; /* no DO index */
			}
    break;
case 365:
#line 2841 "fortran.y"
{
		/* In picky mode warn if no name tag on block construct. */
			  if( style_req_construct_name ) {
			      warning(yyvsp[0].line_num,yyvsp[0].col_num,
				"Construct name missing from DO statement");
			  }
			  curr_stmt_name = NULL;
			}
    break;
case 366:
#line 2850 "fortran.y"
{
			  if(f77_construct_name) {
			    nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num,0,0);
			    msg_tail(": DO construct name");
			  }
			  construct_name_seen=FALSE;
			  yyval = yyvsp[0];
			}
    break;
case 367:
#line 2861 "fortran.y"
{
			    ref_label(&(yyvsp[0]), LAB_DO);
			    def_do_label(&(yyvsp[0]));
				/* Save label in subclass for push_block */
			    yyval.tsubclass = yyvsp[0].value.integer;
			}
    break;
case 368:
#line 2868 "fortran.y"
{
                            ref_label(&(yyvsp[-1]), LAB_DO);
			    def_do_label(&(yyvsp[-1]));
			    yyval.tsubclass = yyvsp[-1].value.integer;
			}
    break;
case 369:
#line 2874 "fortran.y"
{
			    make_true(NONSTD_USAGE_FLAG,yyval.TOK_flags);
			    integer_context=FALSE;
			    yyval.tsubclass = (long)NO_LABEL;
			}
    break;
case 370:
#line 2882 "fortran.y"
{
			    yyval.TOK_type=do_bounds_type(&(yyvsp[-2]),&(yyvsp[0]),&(yyvsp[0]));
			}
    break;
case 371:
#line 2886 "fortran.y"
{
			    yyval.TOK_type=do_bounds_type(&(yyvsp[-4]),&(yyvsp[-2]),&(yyvsp[0]));
			}
    break;
case 372:
#line 2892 "fortran.y"
{
			  curr_stmt_name = NULL;
			}
    break;
case 375:
#line 2905 "fortran.y"
{
			   if( f77_cycle_exit ) {
			     nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
			     msg_tail(": CYCLE statement");
			   }
		}
    break;
case 376:
#line 2912 "fortran.y"
{
			   if( f77_cycle_exit ) {
			     nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
			     msg_tail(": EXIT statement");
			   }
		}
    break;
case 382:
#line 2934 "fortran.y"
{
			  if( f95_pause ) {
			    nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,0,f95_pause);
			    msg_tail(": PAUSE statement");
			  }
			}
    break;
case 385:
#line 2945 "fortran.y"
{
			     use_variable(&(yyvsp[0]));
			}
    break;
case 387:
#line 2953 "fortran.y"
{complex_const_allowed = FALSE;}
    break;
case 389:
#line 2955 "fortran.y"
{complex_const_allowed = FALSE;}
    break;
case 391:
#line 2958 "fortran.y"
{init_io_ctrl_list();}
    break;
case 392:
#line 2960 "fortran.y"
{complex_const_allowed = TRUE;}
    break;
case 396:
#line 2971 "fortran.y"
{
			    record_default_io();
			}
    break;
case 397:
#line 2975 "fortran.y"
{
			    record_default_io();
			}
    break;
case 398:
#line 2979 "fortran.y"
{init_io_ctrl_list();}
    break;
case 399:
#line 2983 "fortran.y"
{
			    if(f77_accept_type || f90_accept_type)
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,f90_accept_type,0);
			    record_default_io();
			}
    break;
case 400:
#line 2989 "fortran.y"
{
			    if(f77_accept_type || f90_accept_type)
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,f90_accept_type,0);
			    record_default_io();
			}
    break;
case 401:
#line 2998 "fortran.y"
{
			    record_default_io();
			}
    break;
case 402:
#line 3002 "fortran.y"
{complex_const_allowed = TRUE;}
    break;
case 403:
#line 3003 "fortran.y"
{complex_const_allowed = FALSE;}
    break;
case 404:
#line 3004 "fortran.y"
{
			    record_default_io();
			}
    break;
case 405:
#line 3010 "fortran.y"
{
			    if(f77_accept_type || f90_accept_type)
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num,f90_accept_type,0);
			    record_default_io();
			}
    break;
case 406:
#line 3016 "fortran.y"
{complex_const_allowed = TRUE;}
    break;
case 407:
#line 3017 "fortran.y"
{complex_const_allowed = FALSE;}
    break;
case 408:
#line 3018 "fortran.y"
{
			    if(f77_accept_type || f90_accept_type)
				nonstandard(yyvsp[-6].line_num,yyvsp[-6].col_num,f90_accept_type,0);
			    record_default_io();
			}
    break;
case 409:
#line 3027 "fortran.y"
{
			    ++control_item_count;
			}
    break;
case 410:
#line 3031 "fortran.y"
{
			    ++control_item_count;
			    if(! io_warning_given) {
			      if( io_internal_file ) {
				if( (curr_stmt_class == tok_WRITE ||
				     curr_stmt_class == tok_READ) &&
				    io_list_directed ) {
				  if(f77_internal_list_io) {
				    nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
	    msg_tail(": internal file cannot be used with list-directed I/O");
				  }
				  io_warning_given = TRUE;
				}
			      }
			    }
			}
    break;
case 411:
#line 3053 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			}
    break;
case 412:
#line 3057 "fortran.y"
{
			  if(control_item_count == 0) /* unit id */
			  {
					/* Handle special cases */
			    if( datatype_of(yyvsp[0].TOK_type) == type_STRING ) {
					/* unit id=char variable is
					   an internal file.  I/O goes in
					   and out of the variable. */
			      if( is_true(ID_EXPR,yyvsp[0].TOK_flags) ) {
				 io_internal_file = TRUE;
				 if(curr_stmt_class == tok_WRITE) {
				      use_lvalue(&(yyvsp[0]));
				 }
			      }
			      else { /* internal unit must be a variable */
				syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					"internal file must be a variable");
			      }
			    }
			    else { /* Otherwise it is a normal external file unit id */
				 record_io_unit_id(&yyvsp[0]);
			    }
			  }
			  else if(control_item_count == 1) /* format id */
			  {
			    if( yyvsp[0].tclass == '*' )
			    {
				 io_list_directed = TRUE;
			    }
			    else if( is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				 if(datatype_of(yyvsp[0].TOK_type) == type_NAMELIST) {
				   ref_namelist(&(yyvsp[0]),curr_stmt_class);
				 }
				 else
				     /* format id=integer variable is assigned format */
				   if( datatype_of(yyvsp[0].TOK_type) == type_INTEGER) {
				     if( f95_assign ) {
				       nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,f95_assign);
				       msg_tail(": assigned format");
				     }
				   }
			    }
				/* An integer at this point is a format label */
			    else if ( is_true(LIT_CONST,yyvsp[0].TOK_flags) &&
				      yyvsp[0].TOK_type == type_byte(class_VAR,type_INTEGER))
			    {
				 ref_label(&(yyvsp[0]),LAB_IO);
			    }
			    current_io_form = IO_FORM_FORMATTED;
  			  }
					/* Handle use of variable */
			  if( is_true(ID_EXPR,yyvsp[0].TOK_flags)){
			       use_variable(&(yyvsp[0]));
			  }
			}
    break;
case 413:
#line 3120 "fortran.y"
{
			    if( yyvsp[0].tclass != '*'
			       && is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    if(control_item_count == 0)
			    {
			       record_io_unit_id(&(yyvsp[0]));
			    }
			    ++control_item_count;
			}
    break;
case 414:
#line 3132 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			    ++control_item_count;
			}
    break;
case 415:
#line 3137 "fortran.y"
{
			    ++control_item_count;
			}
    break;
case 416:
#line 3143 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			}
    break;
case 417:
#line 3147 "fortran.y"
{
			    use_special_open_keywd(&(yyvsp[0]));
			}
    break;
case 420:
#line 3158 "fortran.y"
{
			    if( curr_stmt_class == tok_READ ||
				curr_stmt_class == tok_ACCEPT ) { /* Input */
				if(is_true(LVALUE_EXPR,yyvsp[0].TOK_flags)) {
				    use_lvalue(&(yyvsp[0]));
				}
				else {
				    syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
						 "item is not assignable");
				/* Give hint if it is a parameter */
				    if(is_true(ID_EXPR,yyvsp[0].TOK_flags) &&
				       is_true(CONST_EXPR,yyvsp[0].TOK_flags))
					msg_tail(": it is a constant");
				}
			    }
			    else {				 /* Output */
				if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				    use_variable(&(yyvsp[0]));
				}
			    }
			}
    break;
case 422:
#line 3184 "fortran.y"
{
			  if( ! is_true(LVALUE_EXPR,yyvsp[-3].TOK_flags) ) {
			    syntax_error(yyvsp[-3].line_num,yyvsp[-3].col_num,
					 "index is not assignable");
			    if(is_true(CONST_EXPR,yyvsp[-3].TOK_flags))
				msg_tail(": it is a constant");
			  }
			  else {
			     use_implied_do_index(&(yyvsp[-3]));
			  }
			}
    break;
case 423:
#line 3198 "fortran.y"
{init_io_ctrl_list();}
    break;
case 425:
#line 3203 "fortran.y"
{init_io_ctrl_list();}
    break;
case 427:
#line 3208 "fortran.y"
{init_io_ctrl_list();}
    break;
case 429:
#line 3214 "fortran.y"
{
			    if( yyvsp[-1].tclass != '*'
			       && is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			    record_io_unit_id(&yyvsp[-1]);
			}
    break;
case 431:
#line 3223 "fortran.y"
{init_io_ctrl_list();}
    break;
case 432:
#line 3228 "fortran.y"
{
			    if( yyvsp[-1].tclass != '*'
			       && is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			    record_io_unit_id(&yyvsp[-1]);
			}
    break;
case 434:
#line 3237 "fortran.y"
{init_io_ctrl_list();}
    break;
case 435:
#line 3242 "fortran.y"
{
			    if( yyvsp[-1].tclass != '*'
			       && is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			    record_io_unit_id(&yyvsp[-1]);
			}
    break;
case 437:
#line 3251 "fortran.y"
{init_io_ctrl_list();}
    break;
case 440:
#line 3265 "fortran.y"
{
			  if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
			    use_variable(&(yyvsp[0]));
				/* If integer, format_id is assigned format */
			    if( datatype_of(yyvsp[0].TOK_type) == type_INTEGER ) {
			      if( f95_assign ) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,f95_assign);
				msg_tail(": assigned format");
			      }
			    }
			  }
			     /* A format label appears here as integer const */
			  else if(is_true(LIT_CONST,yyvsp[0].TOK_flags) &&
			    yyvsp[0].TOK_type == type_byte(class_VAR,type_INTEGER)){
			      ref_label(&(yyvsp[0]),LAB_IO);
			  }
			}
    break;
case 442:
#line 3286 "fortran.y"
{inside_format=TRUE;}
    break;
case 443:
#line 3287 "fortran.y"
{
			  inside_format=FALSE;
			}
    break;
case 454:
#line 3313 "fortran.y"
{
			  if( f95_Hedit ) {
			    nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,f95_Hedit);
			    msg_tail(": H edit descriptor");
			  }
			}
    break;
case 462:
#line 3329 "fortran.y"
{
			  if(f77_format_dollarsigns || f90_format_dollarsigns)
			     nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,f90_format_dollarsigns,0);
			}
    break;
case 467:
#line 3344 "fortran.y"
{inside_format=FALSE;}
    break;
case 468:
#line 3345 "fortran.y"
{inside_format=TRUE;}
    break;
case 469:
#line 3346 "fortran.y"
{
			  if(f77_variable_format || f90_variable_format)
			     nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num,f90_variable_format,0);
			}
    break;
case 470:
#line 3355 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[-3]),SEQ_STMT_FUN);
			  check_f90_stmt_sequence(&(yyvsp[-3]),F90_SEQ_SPECIF);

				def_stmt_function(&(yyvsp[-3]),&(yyvsp[-1]));
					/* make token info */
				primary_id_expr(&(yyvsp[-3]),&(yyval));
#ifdef DEBUG_PARSER
				if(debug_parser)
				  print_exprlist("stmt function",&(yyvsp[-1]));
#endif
			}
    break;
case 471:
#line 3370 "fortran.y"
{
			    yyval.next_token = (Token*)NULL;
			}
    break;
case 473:
#line 3377 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			}
    break;
case 474:
#line 3382 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			}
    break;
case 476:
#line 3392 "fortran.y"
{
			     call_subr(&(yyvsp[0]),(Token*)NULL);
			     complex_const_allowed = FALSE;
			}
    break;
case 478:
#line 3398 "fortran.y"
{
			     call_subr(&(yyvsp[-2]),(Token*)NULL);
			     complex_const_allowed = FALSE;
			}
    break;
case 480:
#line 3404 "fortran.y"
{
			     call_subr(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("call stmt",&(yyvsp[-1]));
#endif
			     complex_const_allowed = FALSE;
			}
    break;
case 482:
#line 3415 "fortran.y"
{
			     complex_const_allowed = TRUE;
			     yyval = yyvsp[0];
			}
    break;
case 483:
#line 3421 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			    yyval.left_token = (Token *)NULL;
			}
    break;
case 484:
#line 3426 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			}
    break;
case 485:
#line 3432 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				 use_actual_arg(&(yyvsp[0]));
				 use_variable(&(yyvsp[0]));
			    }
			}
    break;
case 486:
#line 3439 "fortran.y"
{
			    ref_label(&(yyvsp[0]), LAB_CALL);  
			  yyval = yyvsp[0];
			  yyval.left_token = (Token *)NULL;
			}
    break;
case 487:
#line 3448 "fortran.y"
{
			  (void)do_RETURN(current_module_hash,&(yyvsp[-1]));
			}
    break;
case 488:
#line 3452 "fortran.y"
{
			  if( do_RETURN(current_module_hash,&(yyvsp[-2])) ) {

				/* Warn if alternate return value is a constant
				   that is not between 0 and the number of
				   labels that are dummy-arguments.
				 */
			     if( pretty_alt_return &&
				(is_true(EVALUATED_EXPR,yyvsp[-1].TOK_flags) &&
			        (yyvsp[-1].value.integer < 1 ||
				  yyvsp[-1].value.integer > label_dummy_arg_count)) ){
				 warning(yyvsp[-1].line_num,yyvsp[-1].col_num,
					 "alternate return value");
				 msg_tail(ulongtostr(yyvsp[-1].value.integer));
				 if( yyvsp[-1].value.integer < 0 ) {
				   msg_tail("is negative");
				 }
				 else {
				   msg_tail("exceeds");
				   msg_tail(ulongtostr(label_dummy_arg_count));
				   msg_tail("= number of alternative return points");
				 }
			     }
			     else {
				/* Style warning is under goto rubric */
			       if( style_goto ) {
				 warning(yyvsp[-2].line_num,yyvsp[-2].col_num,
				     "obsolescent feature: alternate return");
			       }
			     }
			  }
			}
    break;
case 489:
#line 3488 "fortran.y"
{
				   /* restore context */
				if(!is_true(COMPLEX_FLAG,yyvsp[-3].TOK_flags))
				  complex_const_allowed=FALSE;
				if(is_true(IN_ASSIGN,yyvsp[-3].TOK_flags))
				  in_assignment_stmt = TRUE;

				  /* Change empty arg list to no arg list */
				if(yyvsp[-1].next_token == NULL)
				  call_func(&(yyvsp[-3]),(Token *)NULL);
				else
				  call_func(&(yyvsp[-3]),&(yyvsp[-1]));
							/* make token info */
				func_ref_expr(&(yyvsp[-3]),&(yyvsp[-1]),&(yyval));
				/* Substitute empty token for null arglist */
				yyval.left_token = add_tree_node(
						   &(yyvsp[-2]),&(yyvsp[-3]),
						   (yyvsp[-1].next_token == NULL?
						    empty_token(&(yyvsp[-1])) :
						    yyvsp[-1].next_token) );
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("function",&(yyvsp[-1]));
#endif
			}
    break;
case 490:
#line 3516 "fortran.y"
{
			  if(complex_const_allowed)/* save context */
			    make_true(COMPLEX_FLAG,yyval.TOK_flags);
			  complex_const_allowed=TRUE;
			  if(in_assignment_stmt)
			    make_true(IN_ASSIGN,yyval.TOK_flags);
			  in_assignment_stmt = FALSE;
			}
    break;
case 491:
#line 3526 "fortran.y"
{
				yyval.tclass = 0;
				yyval.next_token = (Token *)NULL;
				yyval.left_token = (Token *)NULL;
			}
    break;
case 493:
#line 3535 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			    yyval.left_token = (Token *)NULL;
			}
    break;
case 494:
#line 3540 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			}
    break;
case 495:
#line 3549 "fortran.y"
{
			  int t=datatype_of(yyvsp[0].TOK_type);
			  if( t != type_ERROR){
			    if( ! is_const_type(t) ) {
			      syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
		      "arithmetic, char, or logical expression expected");
			    }
			    else {
			      if( !is_true(PARAMETER_EXPR,yyvsp[0].TOK_flags) ) {
				syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					   "constant expression expected");
			      }
			    /* Here we allow, with some warnings, expr
			       containing intrins func or **REAL in
			       PARAMETER defn. */
			      else if( !is_true(CONST_EXPR,yyvsp[0].TOK_flags) ) {
				if(f77_param_intrinsic) {
				  nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
				  msg_tail(
			 "intrinsic function or **REAL in PARAMETER defn");
				}
			      }
			    }
			  }
			}
    break;
case 496:
#line 3578 "fortran.y"
{
				/* Fix it up in case it is used in expr list */
			  yyval.next_token = (Token *) NULL;
#ifdef DEBUG_PARSER
			    if(debug_parser) {
				(void)fprintf(list_fd,
					"\nexpr: class=0x%lx subclass=0x%lx",
					yyvsp[0].tclass,
					yyvsp[0].tsubclass);
			    }
#endif
			}
    break;
case 498:
#line 3595 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 499:
#line 3600 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 501:
#line 3609 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 503:
#line 3618 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 505:
#line 3627 "fortran.y"
{
			    do_unexpr(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			}
    break;
case 507:
#line 3635 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 509:
#line 3645 "fortran.y"
{
			    do_unexpr(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			}
    break;
case 510:
#line 3649 "fortran.y"
{
			    do_unexpr(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			}
    break;
case 511:
#line 3653 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 512:
#line 3658 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 514:
#line 3667 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			    if(div_check &&
			       !is_true(CONST_EXPR,yyvsp[0].TOK_flags)){
				warning(yyvsp[-1].line_num,yyvsp[-1].col_num,
					"Possible division by zero");
			    }
			}
    break;
case 515:
#line 3677 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 517:
#line 3686 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 519:
#line 3695 "fortran.y"
{
			  do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			}
    break;
case 520:
#line 3702 "fortran.y"
{
			    make_true(DIM_BOUND_EXPR,yyval.TOK_flags);
			}
    break;
case 524:
#line 3712 "fortran.y"
{
			    yyval.TOK_flags = 0;
			    yyval.left_token = (Token *)NULL;
			    make_true(CONST_EXPR,yyval.TOK_flags);
			    make_true(PARAMETER_EXPR,yyval.TOK_flags);
			    make_true(LIT_CONST,yyval.TOK_flags);
			    make_true(EVALUATED_EXPR,yyval.TOK_flags);
			    make_true(DIM_BOUND_EXPR,yyval.TOK_flags);
			}
    break;
case 525:
#line 3722 "fortran.y"
{
			    yyval = yyvsp[-1];
				/* (identifier) becomes a non-identifier */
			    if(is_true(LVALUE_EXPR,yyvsp[-1].TOK_flags)) {
				if(pretty_parens) {
				  ugly_code(yyvsp[-1].line_num,yyvsp[-1].col_num,
					  "Extraneous parentheses");
				}
				use_variable(&(yyvsp[-1]));
				make_false(LVALUE_EXPR,yyval.TOK_flags);
				make_false(ARRAY_ID_EXPR,yyval.TOK_flags);
				make_false(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
				make_false(ID_EXPR,yyval.TOK_flags);
				make_false(DO_VARIABLE,yyval.TOK_flags);
			    }
				/* (expr) becomes tree node with root = '(' */
			    yyval.left_token = add_tree_node(&(yyvsp[-2]),&(yyvsp[-1]),
							  (Token*)NULL);
			}
    break;
case 527:
#line 3748 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_STRING);
			    /* (size is set in get_string) */
			}
    break;
case 528:
#line 3753 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_HOLLERITH);
			    /* (size is set in get_hollerith) */
			    if(port_hollerith) {
				warning(yyvsp[0].line_num,yyvsp[0].col_num,
				"hollerith constant may not be portable");
			    }
			}
    break;
case 529:
#line 3762 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_LOGICAL);
			    yyval.size = size_DEFAULT;
			}
    break;
case 530:
#line 3769 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_INTEGER);
			    yyval.size = size_DEFAULT;
			}
    break;
case 531:
#line 3774 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_REAL);
			    yyval.size = size_DEFAULT;
			}
    break;
case 532:
#line 3779 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_DP);
			    yyval.size = size_DEFAULT;
			}
    break;
case 533:
#line 3784 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_QUAD);
			    yyval.size = size_QUAD;
                            if(f77_quad_constants || f90_quad_constants) {
                              nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,f90_quad_constants,0);
                              msg_tail(": quad precision constant");
                            }
			}
    break;
case 534:
#line 3793 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_COMPLEX);
			    yyval.size = size_DEFAULT;
			}
    break;
case 535:
#line 3798 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_DCOMPLEX);
			    yyval.size = size_DEFAULT;
			}
    break;
case 536:
#line 3806 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    if(datatype_of(yyvsp[0].TOK_type) != type_INTEGER) {
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"expression must be integer type");
			    }
			}
    break;
case 537:
#line 3820 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    {
				int t=datatype_of(yyvsp[0].TOK_type);
				    if(t != type_INTEGER && t != type_REAL
					&& t != type_DP ) {
					syntax_error(
					  yyvsp[0].line_num,yyvsp[0].col_num,
		"expression must be integer, real, or double precision type");
			    	    }
			    }
			}
    break;
case 538:
#line 3840 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    if( ! is_true(CONST_EXPR,yyvsp[0].TOK_flags) ) {
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"constant expression expected");
			    }
			    else {
			      if(datatype_of(yyvsp[0].TOK_type) != type_INTEGER){
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"integer expression expected");
			      }
			      else {
				yyval.value.integer = int_expr_value(&(yyvsp[0]));
			      }
			    }
			}
    break;
case 539:
#line 3864 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }

			    if(f77_array_bounds) {	/* Section 5.1.1.1 */
			      if( !is_true(DIM_BOUND_EXPR,yyvsp[0].TOK_flags) ) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num,0,0);
				msg_tail(
		": array bounds expr cannot have array or function reference");
			      }
			    }

			    if( datatype_of(yyvsp[0].TOK_type) != type_INTEGER ){
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"integer dimension expected");
				yyval.value.integer = 0;
			    }
			    else {
			      if( is_true(EVALUATED_EXPR,yyvsp[0].TOK_flags) )
				yyval.value.integer =
				  int_expr_value(&(yyvsp[0]));
			      else		/* must be dummy */
				yyval.value.integer = 0;
			    }
			}
    break;
case 540:
#line 3898 "fortran.y"
{
				ref_array(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array lvalue",&(yyvsp[-1]));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,yyval.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
				yyval.left_token = add_tree_node(
						   &(yyvsp[-2]),&(yyvsp[-3]),yyvsp[-1].next_token);
				yyval.next_token = (Token *) NULL;
			}
    break;
case 541:
#line 3914 "fortran.y"
{
				ref_array(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array",&(yyvsp[-1]));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,yyval.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
				yyval.left_token = add_tree_node(
						   &(yyvsp[-2]),&(yyvsp[-3]),yyvsp[-1].next_token);
				yyval.next_token = (Token *) NULL;
			}
    break;
case 542:
#line 3930 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			}
    break;
case 543:
#line 3934 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			}
    break;
case 544:
#line 3940 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				 use_variable(&(yyvsp[0]));
			    }
				/* check subscript exprs for integer type */
			    if(datatype_of(yyvsp[0].TOK_type) != type_INTEGER)
			      if(trunc_real_subscript)
			         warning(yyvsp[0].line_num,yyvsp[0].col_num,
					 "subscript is not integer");
			}
    break;
case 545:
#line 3954 "fortran.y"
{
				   /* restore status of complex flag */
			    if(!is_true(COMPLEX_FLAG,yyvsp[-1].TOK_flags))
				  complex_const_allowed=FALSE;
				/* set flag to keep more than just id for
				   arg list text */
			    if(is_true(ID_EXPR,yyvsp[-1].TOK_flags))
			       make_true(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.left_token = add_tree_node(
					       &save_token,&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.next_token = (Token *) NULL;
			}
    break;
case 546:
#line 3969 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.left_token = add_tree_node(
					       &save_token,&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.next_token = (Token *) NULL;
			}
    break;
case 547:
#line 3977 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.left_token = add_tree_node(
					       &save_token,&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.next_token = (Token *) NULL;
			}
    break;
case 548:
#line 3986 "fortran.y"
{
			    ref_variable(&(yyvsp[-1]));
			    yyval.TOK_flags = yyvsp[-1].TOK_flags;
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			}
    break;
case 549:
#line 3992 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			}
    break;
case 550:
#line 4001 "fortran.y"
{
			    yyval.TOK_start=1;
			    yyval.TOK_end=0; /* 0 means LEN */

			    save_token = yyvsp[-2]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-1]),
				     empty_token(&(yyvsp[-2])),empty_token(&(yyvsp[0])));
				/* Nullify next_token so it looks like
				   a tokenlist */
			    yyval.next_token = (Token *)NULL;
			}
    break;
case 551:
#line 4015 "fortran.y"
{
			    yyval.TOK_start=yyvsp[-2].value.integer;
			    yyval.TOK_end=0; /* 0 means LEN */

			    save_token = yyvsp[-3]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-1]),&(yyvsp[-2]),empty_token(&(yyvsp[0])));
			    yyval.next_token = (Token *)NULL;
			}
    break;
case 552:
#line 4025 "fortran.y"
{
			    yyval.TOK_start=1;
			    yyval.TOK_end=yyvsp[-1].value.integer;

			    save_token = yyvsp[-3]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-2]),empty_token(&(yyvsp[-3])),&(yyvsp[-1]));
			    yyval.next_token = (Token *)NULL;
			}
    break;
case 553:
#line 4035 "fortran.y"
{
			    yyval.TOK_start=yyvsp[-3].value.integer;
			    yyval.TOK_end=yyvsp[-1].value.integer;

			    save_token = yyvsp[-4]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-2]),&(yyvsp[-3]),&(yyvsp[-1]));
			    yyval.next_token = (Token *)NULL;
			}
    break;
case 554:
#line 4047 "fortran.y"
{
			  if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
			    use_variable(&(yyvsp[0]));
			  }
				/* check validity and replace nonconst
				   value by size_UNKNOWN. */
			  if(is_true(CONST_EXPR,yyvsp[0].TOK_flags)) {
			    if( (yyval.value.integer=int_expr_value(&(yyvsp[0]))) < 1) {
			      syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					   "invalid substring index");
			    }
			  }
			  else  /* (no longer need ID hash index) */
			    yyval.value.integer=size_UNKNOWN;
			}
    break;
case 555:
#line 4068 "fortran.y"
{
			    ref_variable(&(yyvsp[0]));
			    yyval.TOK_flags = yyvsp[0].TOK_flags;
			}
    break;
case 557:
#line 4076 "fortran.y"
{
			    ref_identifier(&(yyvsp[0]));
			    primary_id_expr(&(yyvsp[0]),&(yyval));
			}
    break;
case 558:
#line 4083 "fortran.y"
{
			    ref_variable(&(yyvsp[0]));
			    primary_id_expr(&(yyvsp[0]),&(yyval));
			}
    break;
case 561:
#line 4097 "fortran.y"
{
			  construct_name_seen=TRUE;
				/* remember the name for block balancing */
			  curr_stmt_name = hashtab[yyvsp[-1].value.integer].name;
			}
    break;
case 562:
#line 4105 "fortran.y"
{
			  curr_stmt_name = hashtab[yyvsp[0].value.integer].name;
			}
    break;
case 563:
#line 4109 "fortran.y"
{
			  curr_stmt_name = hashtab[yyvsp[0].value.integer].name;
			}
    break;
case 565:
#line 4117 "fortran.y"
{ yyval = yyvsp[0]; }
    break;
case 566:
#line 4118 "fortran.y"
{ yyval = yyvsp[0]; }
    break;
case 567:
#line 4120 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_LOGICAL);
			    yyval.size = size_DEFAULT;
			}
    break;
case 568:
#line 4125 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_STRING);
			    yyval.size = size_DEFAULT;
			}
    break;
case 569:
#line 4130 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_HOLLERITH);
			    yyval.size = size_DEFAULT;
			}
    break;
case 570:
#line 4141 "fortran.y"
{
			  if(yyvsp[0].value.integer == 0) {
			    if(misc_warn) {
			      warning(yyvsp[0].line_num,yyvsp[0].col_num,
				    "nonzero integer expected");
			      msg_tail(": substituting 1");
			    }
			    yyval.value.integer = 1;
			  }
			  yyval.left_token = (Token *)NULL;
			}
    break;
case 571:
#line 4159 "fortran.y"
{
			    integer_context=TRUE;
			}
    break;
case 572:
#line 4166 "fortran.y"
{
			    if( yyval.value.integer > 99999 && misc_warn) {
				syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
				      "statement label exceeds 5 digits");
			    }
				integer_context=FALSE;
				yyval.TOK_type = type_byte(class_LABEL,type_LABEL);
				yyval.size = size_DEFAULT;
				yyval.TOK_flags = 0;
			}
    break;
}

#line 705 "/usr/share/bison/bison.simple"


  yyvsp -= yylen;
  yyssp -= yylen;
#if YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;
#if YYLSP_NEEDED
  *++yylsp = yyloc;
#endif

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("parse error, unexpected ") + 1;
	  yysize += yystrlen (yytname[YYTRANSLATE (yychar)]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "parse error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[YYTRANSLATE (yychar)]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exhausted");
	}
      else
#endif /* defined (YYERROR_VERBOSE) */
	yyerror ("parse error");
    }
  goto yyerrlab1;


/*--------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action |
`--------------------------------------------------*/
yyerrlab1:
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;
      YYDPRINTF ((stderr, "Discarding token %d (%s).\n",
		  yychar, yytname[yychar1]));
      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;


/*-------------------------------------------------------------------.
| yyerrdefault -- current state does not do anything special for the |
| error token.                                                       |
`-------------------------------------------------------------------*/
yyerrdefault:
#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */

  /* If its default is to accept any token, ok.  Otherwise pop it.  */
  yyn = yydefact[yystate];
  if (yyn)
    goto yydefault;
#endif


/*---------------------------------------------------------------.
| yyerrpop -- pop the current state because it cannot handle the |
| error token                                                    |
`---------------------------------------------------------------*/
yyerrpop:
  if (yyssp == yyss)
    YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#if YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "Error: state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

/*--------------.
| yyerrhandle.  |
`--------------*/
yyerrhandle:
  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

/*---------------------------------------------.
| yyoverflowab -- parser overflow comes here.  |
`---------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}
#line 4180 "fortran.y"


void
init_parser(VOID)			/* Initialize various flags & counters */
{
	initial_flag = TRUE;	/* set flag for keyword test */
	implicit_flag=FALSE;	/* clear flags for IMPLICIT stmt */
	implicit_letter_flag = FALSE;
	implicit_type_given = FALSE;
	implicit_none = FALSE;
	global_save = FALSE;
	prev_token_class = EOS;
	complex_const_allowed = FALSE;
	stmt_sequence_no = 0;
	f90_stmt_sequence_no = 0;
	true_prev_stmt_line_num = 0;
	{
	  int i;		/* Reset *_this_file flags for project files */
	  for(i=0; i<glob_symtab_top; i++) {
	    glob_symtab[i].used_this_file =
	      glob_symtab[i].set_this_file =
	      glob_symtab[i].invoked_as_func_this_file =
	      glob_symtab[i].declared_external_this_file = FALSE;
	  }
	}
}

				/* Handle unary expressions: link
				   into a tree and propagate type.
				 */
PRIVATE void
#if HAVE_STDC
do_unexpr(Token *op, Token *expr, Token *result)
#else /* K&R style */
do_unexpr(op,expr,result)
     Token *op,*expr,*result;
#endif /* HAVE_STDC */
{
  unexpr_type(op,expr,result);

  result->left_token = add_tree_node(op, expr, (Token*)NULL);
}
				/* Handle binary expressions: link
				   into a tree and propagate type.
				 */
PRIVATE void
#if HAVE_STDC
do_binexpr(Token *l_expr, Token *op, Token *r_expr, Token *result)
#else /* K&R style */
do_binexpr(l_expr,op,r_expr,result)
     Token *l_expr,*op,*r_expr,*result;
#endif /* HAVE_STDC */
{
  binexpr_type(l_expr,op,r_expr,result); /* Propagate the type */

  result->left_token = add_tree_node(op, l_expr, r_expr);
}


			/* Changes a token to empty and replaces
			   src_text by null string, value by 0.  Other
			   info (line, col, etc.)  unchanged. */

PRIVATE Token *
#if HAVE_STDC
empty_token(Token *t)
#else /* K&R style */
empty_token(t)
     Token *t;
#endif /* HAVE_STDC */
{
#ifdef DEBUG_EMPTY_TOKEN
  static char *nullstring="(empty)"; /* for debugging.  */
#else
  static char *nullstring=""; /* for operation.  */
#endif
  t->tclass = tok_empty;
  t->tsubclass = 0;
  t->value.integer = 0;
  t->left_token = (Token *) NULL;
  t->src_text = nullstring;

  return t;
}

		/* Propagate non-integer type if any of DO loop
		   bounds are non-integer. */
PRIVATE int
#if HAVE_STDC
do_bounds_type(Token *t1, Token *t2, Token *t3)
#else /* K&R style */
do_bounds_type(t1,t2,t3)
     Token *t1, *t2, *t3;
#endif /* HAVE_STDC */
{
  int result_type;
       if(datatype_of(t1->TOK_type) != type_INTEGER)result_type = t1->TOK_type;
  else if(datatype_of(t2->TOK_type) != type_INTEGER)result_type = t2->TOK_type;
  else if(datatype_of(t3->TOK_type) != type_INTEGER)result_type = t3->TOK_type;
  else result_type = t1->TOK_type;
  return result_type;
}


/* Debugging routine: prints the expression list of various productions */
#ifdef DEBUG_PARSER
PRIVATE void
print_exprlist(s,t)
	char *s;
	Token *t;
{

	(void)fprintf(list_fd,"\n%s arglist: ",s);

	if(t == NULL)
		(void)fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->TOK_type)]);
		  if( is_true(ID_EXPR,t->TOK_flags) )
			(void)fprintf(list_fd,"(%s) ",token_name(t));
	    }
	}
}

PRIVATE void
print_comlist(s,t)
	char *s;
	Token *t;
{

	(void)fprintf(list_fd,"\n%s varlist: ",s);

	if(t == NULL)
		(void)fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->TOK_type)]);
		  if( is_true(ID_EXPR,t->TOK_flags) )
			(void)fprintf(list_fd,"(%s) ",token_name(t));
		}
	  }
}
#endif

/* After having parsed prog_stmt, function_stmt, subroutine_stmt,
   block_data_stmt, the stmt_sequence_no is set to the value SEQ_HEADER.
*/

void
#if HAVE_STDC
check_seq_header(Token *t)
#else /* K&R style */
check_seq_header(t)
     Token *t;
#endif /* HAVE_STDC */
{
	if(stmt_sequence_no >= SEQ_HEADER) {
	   syntax_error( (t == (Token *) NULL? line_num: t->line_num),
			NO_COL_NUM,
			"missing END statement inserted");
	   msg_tail( (t == (Token *) NULL? "at end of file":
		      "prior to statement") );

	   if( t != (Token *) NULL )
	     pop_block(t,tok_END,(char *)NULL,NO_LABEL);

	   END_processing(t);
	}
	stmt_sequence_no = SEQ_HEADER;
	f90_stmt_sequence_no = F90_SEQ_HEADER;
}

PRIVATE void
#if HAVE_STDC
check_stmt_sequence(Token *t, int seq_num)
#else /* K&R style */
check_stmt_sequence(t,seq_num)
     Token *t;
     int seq_num;
#endif /* HAVE_STDC */
{
    if(stmt_sequence_no > seq_num) {
      if(f77_stmt_order) {
	nonstandard(t->line_num, NO_COL_NUM,0,0);
	msg_tail(": Statement out of order.");
      }
    }
			/* If no error, sequence number is updated to new
			   value.  If error, it is rolled back to prevent
			   cascades of error messages.  */
    stmt_sequence_no = seq_num;
}

PRIVATE void
#if HAVE_STDC
check_f90_stmt_sequence(Token *t, int f90_seq_num)
#else /* K&R style */
check_f90_stmt_sequence(t,f90_seq_num)
     Token *t;
     int f90_seq_num;
#endif /* HAVE_STDC */
{
    if(f90_stmt_sequence_no > f90_seq_num) {
      if(f90_stmt_order) {
	nonstandard(t->line_num, NO_COL_NUM,f90_stmt_order,0);
	msg_tail(": Statement out of order.");
      }
    }
			/* If no error, sequence number is updated to new
			   value.  If error, it is rolled back to prevent
			   cascades of error messages.  */
    f90_stmt_sequence_no = f90_seq_num;
}

PRIVATE void
init_io_ctrl_list(VOID)
{
  control_item_count = 0;
  io_internal_file = FALSE;
  io_list_directed = FALSE;
  io_warning_given = FALSE;
  current_io_unit_no = IO_UNIT_UNKNOWN;
  current_io_unit_id = IO_UNIT_UNKNOWN;
  current_io_access = IO_ACCESS_DEFAULT;
  current_io_form = IO_FORM_DEFAULT;
}


		/* Remember the name or number of unit_id for current I/O stmt.
		   This routine is only called by parser productions that have a
		   bare unit_id, so less checking is done than for unit=unit_id
		   specifiers that can refer to other than external files.
		 */
void
record_io_unit_id(Token *id)
{
     if( id->tclass == '*' )
     {
	  current_io_unit_id = IO_UNIT_DEFAULT;
     }
     else if( is_true(ID_EXPR,id->TOK_flags)){
	  current_io_unit_id = id->value.integer; /* get hash code of identifier */
     }
     else if( is_true(LIT_CONST,id->TOK_flags) &&
	      id->TOK_type == type_byte(class_VAR,type_INTEGER))
     {
	  current_io_unit_no = id->value.integer; /* get literal int value */
     }
}

		/* Set I/O usage parameters for default formatted sequential I/O
		   statement like READ *, X  */
PRIVATE void
record_default_io(VOID)
{
  current_io_unit_no = IO_UNIT_UNKNOWN;
  current_io_unit_id = IO_UNIT_DEFAULT;
  current_io_access = IO_ACCESS_SEQUENTIAL;
  current_io_form = IO_FORM_FORMATTED;
}

		/* This routine applies the attributes of attr-based type
		   declaration.  Set dim_bounds to current_dim_bound_list
		   or to NULL if item has its own array bounds declarator.
		 */
PRIVATE void
process_attrs(Token *t,Token *dim_bounds)
{
    if(current_save_attr)
	save_variable(t);
    if(current_external_attr)
	def_ext_name(t);
    if(current_intrinsic_attr)
	def_intrins_name(t);
    if(dim_bounds != NULL)
	def_array_dim(t,dim_bounds);
}

	/* After having parsed end_stmt, common block lists and
	   subprogram argument lists are copied over into global symbol
	   table, the local symbol table is printed out and then cleared,
	   and stmt_sequence_no is set to zero for start of next module.
	*/

PRIVATE void
#if HAVE_STDC
END_processing(Token *t)
#else /* K&R style */
END_processing(t)
	Token *t;
#endif /* HAVE_STDC */
{
  ++tot_module_count;
  if(current_module_hash != -1) {
        if(exec_stmt_count == 0 &&
	   current_module_type != type_BLOCK_DATA) {
	  if(misc_warn)
	    warning(t == (Token *)NULL? line_num: t->line_num, NO_COL_NUM,
		  "Module contains no executable statements");
	}
	if(do_list && t != (Token *)NULL)
	    (void)flush_line_out(t->line_num);

	doing_end_proc = TRUE;	/* Set flag for special error message mode */

			/* Catch things that had to wait till now */
	check_loose_ends(current_module_hash);
			/* Put arg and com lists into global table */
	process_lists(current_module_hash);
			/* Print symbol table for debug */
	debug_symtabs();
			/* Print local symbol table and do local warnings */
	print_loc_symbols();
			/* Reset local symbol table */
	init_symtab();

	doing_end_proc = FALSE;
  }
  exec_stmt_count = 0;
  stmt_sequence_no = 0;
  f90_stmt_sequence_no = 0;
  current_module_hash = -1;
  implicit_type_given = FALSE;
  implicit_none = FALSE;
  true_prev_stmt_line_num = 0;
  integer_context = FALSE;
  global_save = FALSE;
  label_dummy_arg_count = 0;
  num_io_unit_usages = 0;
}

		/* Routine to create a node for an expr tree.  Returns
		   a pointer to the newly created node.
		 */
PRIVATE Token *
#if HAVE_STDC
add_tree_node(Token *node, Token *left, Token *right)
#else /* K&R style */
add_tree_node(node,left,right)
     Token *node,*left,*right;
#endif /* HAVE_STDC */
{
  Token *new_node, *new_left, *new_right;

  new_node=new_token();

  *new_node = *node;		/* Make a permanent copy of root */

		/* Add the children.  If child's left_token pointer is
		   null, then that expression is a primary.  Otherwise
		   it is the root node of a subtree.
		 */
  if(left->left_token == (Token *)NULL) {
    new_left=new_token();
    *new_left = *left;			/* Copy primary to permanent space */
  }
  else {
    new_left = left->left_token;	/* No copying needed in this case */
  }

  if(right == (Token *)NULL) {
    new_right = (Token *)NULL;		/* No right child */
  }
  else if(right->left_token == (Token *)NULL
	  || node->tclass == '(') { /* Paren means right child is expr list */
    new_right=new_token();
    *new_right = *right;		/* Copy primary to permanent space */
  }
  else {
    new_right = right->left_token;	/* No copying needed in this case */
  }

  new_node->left_token = new_left;	/* Link children onto the new root */
  new_node->next_token = new_right;
  return new_node;
}

		/* Routine to add token t to the front of a token list. */
PRIVATE Token *
#if HAVE_STDC
append_token(Token *tlist, Token *t)
#else /* K&R style */
append_token(tlist,t)
     Token *tlist, *t;
#endif /* HAVE_STDC */
{
	Token *tcopy;

	tcopy=new_token();

	*tcopy = *t;		/* make permanent copy of token */
	tcopy->next_token = tlist; /* link it onto front of list */
	return tcopy;		/* return it as new tlist */
}


			/* Routine to pop closing statement of block off
			   the stack.  Note: label should be NO_LABEL even
			   if the statement has a label, except for terminal
			   statement of a labeled DO loop.
			 */
PRIVATE void pop_block(Token *t, int stmt_class, char *name, LABEL_t label)
{

	/* Define lookup table for block matches.  This table is generated
	   from tokdefs.h by the Perl script make_blockmatch.pl.  This script
	   also generates 4 macro definitions that go here.  It
	   defines MIN_BLOCK_TOKEN and MAX_BLOCK_TOKEN used to bound range
	   of valid keytok_name arguments for error messages.  It also defines
	   MIN_CLOSER and MAX_CLOSER giving the range of token numbers for
	   closing tokens.  The array block_match contains the token numbers
	   for the matching openers.  Look up the matching opener class as
	     block_match[closer_class-MIN_CLOSER]
	   Plain END is handled specially since it matches many things.
	   Likewise closing labeled-DO range is handled specially.  (ENDDO
	   is handled there if loop is labeled, handled with structured
	   block closers otherwise.)
	*/

  static int block_match[] = {
#include "block_match.h"
  };

		/* Macro for names of all block keyword tokens.  Although no
		   error messages should print "identifier" (only occurs
		   as DO terminator, and mismatch is handled specially),
		   include it for possible debug use.  Any other
		   token classes in block_stack mean a bug in ftnchek.
		 */
#define TOKEN_NAME(CLASS) ((CLASS)>=MIN_BLOCK_TOKEN && (CLASS)<=MAX_BLOCK_TOKEN? \
	keytok_name(CLASS):((CLASS) == tok_identifier?"identifier":"bogus"))

		/* Define DBG_TOKNAME for debugging.  If possible,
		   use yytname to allow for bogus tokens showing up.
		   This works only for bison parser generator.  It would
		   be easy to fix up for byacc too if need be.
		*/
#ifdef DEBUG_BLOCKCHECK
#ifdef USE_YYTNAME
#define DBG_TOKNAME(CLASS) (char *)(yytname[YYTRANSLATE(CLASS)])
#else
#define DBG_TOKNAME(CLASS) TOKEN_NAME(CLASS)
#endif
#endif

#ifdef DEBUG_BLOCKCHECK
  if(debug_latest) {
    fprintf(list_fd,"\npopping stmt class %s name %s label %d at line %d",
	    DBG_TOKNAME(stmt_class),name,label,t->line_num);
  }
#endif

  if(block_depth == 0) {
      syntax_error(t->line_num,t->col_num,"no construct to end here");
  }
  else {
    int opener_class, must_check_name=FALSE;
    BLOCK_TYPE blocktype;

    --block_depth;

    opener_class = block_stack[block_depth].sclass;
    blocktype = block_stack[block_depth].blocktype;

#ifdef DEBUG_BLOCKCHECK
    if(debug_latest) {
      fprintf(list_fd,"\n  opener was class %s name %s label %d at line %d",
	      DBG_TOKNAME(opener_class),
	      block_stack[block_depth].name,
	      block_stack[block_depth].label,
	      block_stack[block_depth].first_line);
    }
#endif
				/* DO loop terminator */
    if( label != NO_LABEL) {
      int shared_warning = TRUE;
				/* Mark DO index variable as inactive */
      int h=block_stack[block_depth].do_var_hash;
      if(h != -1)
	undef_do_variable(h);

      if( stmt_class == tok_ENDDO ) {
	must_check_name = TRUE;
      }
      else {
			/* issue picky warnings if not modern DO construct */
	if(style_req_enddo) {
	  warning(t->line_num,t->col_num,
		  "DO loop not terminated by ENDDO");
	}
	else if( stmt_class != tok_CONTINUE ) {
	  if(style_req_do_construct) {
	    warning(t->line_num,t->col_num,
		    "DO loop not terminated by CONTINUE or ENDDO");
	  }
	}
      }
      if( opener_class != tok_DO ) {
	syntax_error(t->line_num,t->col_num,keytok_name(opener_class));
	msg_tail("block not closed when DO loop terminating statement encountered");
      }
      else if( label != block_stack[block_depth].label ) {
	syntax_error(t->line_num,t->col_num,"Label");
	msg_tail(ulongtostr(label));
	msg_tail("on DO loop terminator does not match");
	if(block_stack[block_depth].label == NO_LABEL ) {
	  msg_tail("label-less DO statement");
	}
	else {
	  msg_tail("corresponding DO label");
	  msg_tail(ulongtostr(block_stack[block_depth].label));
	}
	msg_tail("above");
      }
		/* If terminator is shared by other DOs, pop them too */
      else {
	while(block_depth > 0 && label == block_stack[block_depth-1].label) {
	  --block_depth;

				/* Mark DO index variable as inactive */
	  {
	    int hh=block_stack[block_depth].do_var_hash;
	    if(hh != -1)
	      undef_do_variable(hh);
	  }
#ifdef DEBUG_BLOCKCHECK
	  if(debug_latest) {
	      fprintf(list_fd,"\n  opener was class %s name %s label %d at line %d",
		      DBG_TOKNAME(opener_class),
		      block_stack[block_depth].name,
		      block_stack[block_depth].label,
		      block_stack[block_depth].first_line);
	  }
#endif
	  if(style_shared_do_terminator && shared_warning) {
	    warning(t->line_num,t->col_num,
		    "Obsolescent feature: shared DO terminator");
	    shared_warning = FALSE; /* don't repeat the warning */
	  }
	}
      }
    }
    else if( stmt_class == tok_END ) {	/* plain END statement */
      if(style_req_structured_end) {
	  warning(t->line_num,t->col_num,"Old-style END statement");
      }
			/* Check that END statement ends subprog block */
      if(opener_class != tok_SUBROUTINE
	 && opener_class != tok_FUNCTION
	 && opener_class != tok_PROGRAM
	 && opener_class != tok_BLOCKDATA) {
	  syntax_error(t->line_num,t->col_num,
		       "Block not closed when END statement encountered");
      }
    }
    else {			/* structured block closers */
      int matching_class;

      must_check_name = TRUE;

				/* Look up the correct matching opener class */
      if( stmt_class < MIN_CLOSER || stmt_class > MAX_CLOSER ||
	  (matching_class = block_match[stmt_class-MIN_CLOSER]) == 0 ) {
	fprintf(list_fd,"%d=%s ",stmt_class,keytok_name(stmt_class));
	oops_message(OOPS_FATAL,t->line_num,t->col_num,
		     "pop_block called for unrecognized closing token");
      }
      else {			/* check if opener matches expected */
	if( opener_class != matching_class ) {
		/* If possible, use token names for better error messages */
	  syntax_error(t->line_num,t->col_num,
		       keytok_name(stmt_class));
	  msg_tail("statement does not match");
	  msg_tail(keytok_name(opener_class));
	  msg_tail("block it closes");

	  must_check_name = FALSE; /* name mismatch probably bogus here */
	}
				/* If unlabeled END DO of loop with index
				   variable, mark it inactive.
				 */
	if( stmt_class == tok_ENDDO ) {
	  int h=block_stack[block_depth].do_var_hash;
	  if(h != -1)
	    undef_do_variable(h);
	}
      }
    }
		/* Issue syntax error if name missing from a component of
		   a named construct.  In picky mode warn if no name tag on
		   structured END.
		*/
    if( must_check_name ) {
      if( name == (char *)NULL ) {
	if( blocktype == construct ) {	/* IF, DO, SELECT */
	  if( block_stack[block_depth].name != (char *)NULL ) {
	    syntax_error(t->line_num,t->col_num,
			 "Construct name");
	    msg_tail(block_stack[block_depth].name);
	    msg_tail("missing");
	  }
	}
	else {		/* structured END of subprogram */
	  if(style_req_end_name) {
	    warning(t->line_num,t->col_num,
		    "Name of subprogram missing from");
	    msg_tail(keytok_name(stmt_class));
	    msg_tail("statement");
	  }
	}
      }
				/* Otherwise check name tag if present */
      else {
			/* OK to compare pointers due to name storage method */
	if(name != block_stack[block_depth].name) {
			/* Opener name can only be missing from a construct,
			   but handle subprog case anyway.
			*/
	  if(block_stack[block_depth].name == (char *)NULL) {
	    syntax_error(t->line_num,t->col_num,
			   "name found on closing statement of unnamed");
	    msg_tail(blocktype == construct? "construct": "subprogram");
	  }
	  else {
	    syntax_error(t->line_num,t->col_num,
			 "Name");
	    msg_tail(name);
	    msg_tail("does not match");
	    msg_tail(blocktype == construct? "construct": "subprogram");
	    msg_tail("name");
	    msg_tail(block_stack[block_depth].name);
	  }
	}
      }
    }
  }
}

			/* Check CYCLE and EXIT statements for agreement
			   with enclosing DO loop.
			*/

PRIVATE void check_construct_name_match(Token *t, char *name)
{

				/* If no name on statement, it must at
				   least be inside range of a DO.
				 */
  if( name == (char *)NULL ) {
    int depth;
    for(depth = block_depth-1; depth >= 0; depth--) {
      if( block_stack[depth].sclass == tok_DO )
	return;
    }
    syntax_error(t->line_num,t->col_num,
		 "statement not within range of any DO loop");
  }
				/* If name tag present, it must belong to
				   an enclosing DO.
				*/
  else {
    int depth;
    for(depth = block_depth-1; depth >= 0; depth--) {
      if( block_stack[depth].sclass == tok_DO &&
	  block_stack[depth].name == name )
	  return;
    }
    syntax_error(t->line_num,t->col_num,
		 "construct name");
    msg_tail(name);
    msg_tail("does not match name of any enclosing DO construct");
  }
}

PRIVATE void push_block(Token *t, int stmt_class, BLOCK_TYPE blocktype,
			char *name, LABEL_t label)
{
  if(block_depth == MAX_BLOCK_DEPTH) {
    oops_message(OOPS_FATAL,t->line_num,t->col_num,
		     "blocks nested too deeply");
  }
  else {
#ifdef DEBUG_BLOCKCHECK
    if(debug_latest) {
      fprintf(list_fd,"\npushing stmt class %s name %s label %d at line %d",
	      DBG_TOKNAME(stmt_class),name,label,t->line_num);
    }
#endif
				/* ELSE and ELSE IF masquerade here as IF,
				   and CASE and CASEDEFAULT as SELECT, to
				   simplify match code in pop_block. */
    block_stack[block_depth].sclass = 
      ((stmt_class == tok_ELSE)? tok_IF:
       ((stmt_class == tok_CASE)? tok_SELECTCASE: stmt_class));
    block_stack[block_depth].name = name;
    block_stack[block_depth].label = label;
    block_stack[block_depth].first_line = t->line_num;
    block_stack[block_depth].blocktype = blocktype;
    block_stack[block_depth].do_var_hash = -1; /* undefined at this time */
    ++block_depth;

  }
}

PRIVATE void give_kind_warning(Token *t)
{
  warning(t->line_num,t->col_num,
"I do not yet support KIND selectors.  \
All KINDs are treated as default KIND.  \
Checking of type agreement may be incorrect as a result.  \
(This message is only given once.)");
  kind_warning_given = TRUE;
}
