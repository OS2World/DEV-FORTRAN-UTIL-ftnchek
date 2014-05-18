/* $Id: fortran.y,v 1.56 2003/02/14 20:42:10 moniot Exp $

    fortran.y:

	  Yacc grammar for Fortran program checker.  Uses the yylex()
	  in file FORLEX.C

*/

%{

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


%}

%token tok_identifier
%token tok_array_identifier
%token tok_label
%token tok_integer_const
%token tok_real_const
%token tok_dp_const
%token tok_quad_const
%token tok_complex_const
%token tok_dcomplex_const
%token tok_logical_const
%token tok_string
%token tok_hollerith
%token tok_edit_descriptor
%token tok_letter
%token tok_relop	/* .EQ. .NE. .LT. .LE. .GT. .GE. */
%token tok_AND
%token tok_OR
%token tok_EQV
%token tok_NEQV
%token tok_NOT
%token tok_power	/*   **   */
%token tok_concat	/*   //   */
%token tok_lparen	/* special left paren to avoid s/r conflicts */
%token tok_ACCEPT
%token tok_ASSIGN
%token tok_BACKSPACE
%token tok_BLOCKDATA
%token tok_BYTE
%token tok_CALL
%token tok_CASE
%token tok_CASEDEFAULT
%token tok_CHARACTER
%token tok_CLOSE
%token tok_COMMON
%token tok_COMPLEX
%token tok_CONTINUE
%token tok_CYCLE
%token tok_DATA
%token tok_DIMENSION
%token tok_DO
%token tok_DOUBLECOMPLEX
%token tok_DOUBLEPRECISION
%token tok_DOWHILE
%token tok_ELSE
%token tok_END
%token tok_ENDBLOCKDATA
%token tok_ENDDO
%token tok_ENDFILE
%token tok_ENDFUNCTION
%token tok_ENDIF
%token tok_ENDPROGRAM
%token tok_ENDSELECT
%token tok_ENDSUBROUTINE
%token tok_ENTRY
%token tok_EQUIVALENCE
%token tok_EXTERNAL
%token tok_EXIT
%token tok_FORMAT
%token tok_FUNCTION
%token tok_GOTO
%token tok_IF
%token tok_IMPLICIT
%token tok_INCLUDE
%token tok_INQUIRE
%token tok_INTEGER
%token tok_INTRINSIC
%token tok_LOGICAL
%token tok_NAMELIST
%token tok_NONE
%token tok_OPEN
%token tok_PARAMETER
%token tok_PAUSE
%token tok_POINTER
%token tok_PRINT
%token tok_PROGRAM
%token tok_READ
%token tok_REAL
%token tok_RETURN
%token tok_REWIND
%token tok_SAVE
%token tok_SELECTCASE
%token tok_STOP
%token tok_SUBROUTINE
%token tok_THEN
%token tok_TO
%token tok_TYPE
%token tok_WHILE
%token tok_WRITE

%token tok_illegal  /* Illegal token unused in grammar: induces syntax error */

%token tok_empty    /* For empty tokens used to fill gaps in expr trees */

%token EOS	127	/* Character for end of statement.  */

%nonassoc tok_relop

%left REDUCE ')'	/* Used at unit_io to force a reduction */


%%
	/*  The following grammar is based on the ANSI manual, diagrams
	 *  of section F.  Numbers in the comments refer to the diagram
	 *  corresponding to the grammar rule.
	 */


/* 1-5 */

prog_body	:	stmt_list
		|	/* empty file */
		;

stmt_list	:	stmt_list_item
		|	stmt_list stmt_list_item
		;


stmt_list_item	:	ordinary_stmt
			{
				/* Create id token for prog if unnamed.  NOTE:
				   this clobbers $1.class, value, src_text.
				 */
			  if(current_module_hash == -1) {
			    implied_id_token(&($1),unnamed_prog);
			    def_function(
					 type_PROGRAM,	/* type */
					 size_DEFAULT,	/* size */
					 (char *)NULL,	/* size text */
					 &($1),		/* name */
					 (Token*)NULL);	/* args */
			    current_module_hash =
			      def_curr_module(&($1));
			    current_module_type = type_PROGRAM;

				/* Pretend this is a PROGRAM statement */
			    if(style_req_prog_stmt) {
				warning($1.line_num,$1.col_num,
			"Program does not start with a PROGRAM statement");
			    }
			    push_block(&($$),tok_PROGRAM,subprog,
				       hashtab[current_module_hash].name,
				       NO_LABEL);
				/* It is possible for a block construct to
				   be the initial statement, and if so it
				   has earlier been pushed onto stack.  Detect
				   this situation and swap stack entries to
				   make them nest correctly.
				 */
			    if(block_depth > 1 &&
			       block_stack[block_depth-2].first_line == $1.line_num) {
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
			      (void)do_RETURN(current_module_hash,&($1));
			    pop_block(&($$),$$.tclass,
				      curr_stmt_name,NO_LABEL);

			    END_processing(&($$));
			    goto_flag = prev_goto = FALSE;
			  }
			  prev_stmt_class = curr_stmt_class;
			  integer_context = FALSE;
			  in_attrbased_typedecl = FALSE;
			  true_prev_stmt_line_num = $$.line_num;
			}
 		|	include_stmt
		|	EOS	/* "sticky" EOF for needed delay */
		;

			/* Statements: note that ordering by category
			   of statement is not enforced in the grammar
			   but is deferred to semantic processing.
			 */

ordinary_stmt	:	stmt
		|	end_stmt
			{
				/* Treat END PROGRAM et al as plain END */
			    curr_stmt_class = tok_END;
			}
		;

stmt		:	tok_label unlabeled_stmt
			{
				/* Put definition of label into table, and
				   if it marks end of a DO range, pop block.
				 */
			  int do_label = def_label(&($1),labeled_stmt_type);
			  if( do_label || $2.tclass == tok_ENDDO ) {
			    if(is_true(NOT_DO_TERMINAL_STMT,$2.TOK_flags)) {
			      syntax_error($2.line_num,$2.col_num,
			"statement cannot be terminal statement of a DO loop");
			    }
			    else {
			      pop_block(&($2),$2.tclass,curr_stmt_name,
				      do_label?
					(LABEL_t)($1.value.integer):
					NO_LABEL);
			    }
			  }

				/* Issue picky warnings about labeled
				   statements.  FORMAT has an excuse
				   for existing, so warnings for it
				   are separately controlled.  */
			  if( style_labeled_exec &&
			      curr_stmt_class != tok_FORMAT ) {
			    warning($1.line_num,$1.col_num,
				    "obsolescent feature: labeled statement");
			  }
			  else if( style_labeled_format &&
			      curr_stmt_class == tok_FORMAT ) {
			    warning($2.line_num,$2.col_num,
				    "obsolescent feature: FORMAT statement");
			  }

			  if(executable_stmt)
			    prev_goto = goto_flag;
			  $$ = $2;
			}
		|	unlabeled_stmt
			{
			  if(executable_stmt) {
			    if(prev_goto)
				syntax_error($1.line_num, NO_COL_NUM,
					"No path to this statement");
			    prev_goto = goto_flag;
			  }

			  if( curr_stmt_class == tok_FORMAT &&
			      misc_warn ) {
			      syntax_error($1.line_num,$1.col_num,
				      "FORMAT statement has no label");
			  }
			  if( $1.tclass == tok_ENDDO )
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			}
		;

unlabeled_stmt	:	subprogram_header
			{
			    exec_stmt_count = 0;
			    executable_stmt = FALSE;
			    labeled_stmt_type = LAB_SPECIFICATION;
			    push_block(&($1),$1.tclass,subprog,
				       hashtab[current_module_hash].name,
				       NO_LABEL);
			}
		|	specification_stmt
			{
			    executable_stmt = FALSE;
			/* labeled_stmt_type set in lower productions */
			}
		|	executable_stmt
			{	/* handle statement functions correctly */
			  if(is_true(STMT_FUNCTION_EXPR, $1.TOK_flags)
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
		|	restricted_stmt
			{
			    stmt_sequence_no = SEQ_EXEC;
			    f90_stmt_sequence_no = F90_SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			    labeled_stmt_type = LAB_EXECUTABLE;
			}
		|	error EOS
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
			    $$.line_num = prev_stmt_line_num; /* best guess */
			    labeled_stmt_type = LAB_EXECUTABLE;
			    yyerrok; /* (error message already given) */
			}
		;

subprogram_header:	prog_stmt
			{
			    current_module_type = type_PROGRAM;
			}
		|	function_stmt
			{
			    current_module_type = type_SUBROUTINE;
			}
		|	subroutine_stmt
			{
			    current_module_type = type_SUBROUTINE;
			}
		|	block_data_stmt
			{
			    current_module_type = type_BLOCK_DATA;
			}
		;

end_stmt	:	unlabeled_end_stmt
		|	tok_label
			unlabeled_end_stmt
			{
			  if( def_label(&($1),labeled_stmt_type) ) {
			    syntax_error($2.line_num,$2.col_num,
			"statement cannot be terminal statement of a DO loop");
				/* Pop it anyway to keep stack consistent */
			    pop_block(&($2),$2.tclass,curr_stmt_name,
				      (LABEL_t)($1.value.integer));
			  }
			  $$ = $2;
			}
		;

				/* Various END statements not checked for
				   balance
				 */
unlabeled_end_stmt:	unnamed_end_stmt
			{
			    curr_stmt_name = NULL;
			}

		|	named_end_stmt
		;

unnamed_end_stmt:	tok_END EOS
		|	end_subprog_token EOS
		;

named_end_stmt:		end_subprog_token symbolic_name EOS
			{
			    curr_stmt_name = hashtab[$2.value.integer].name;
			}
		;

end_subprog_token:	tok_ENDBLOCKDATA
		|	tok_ENDFUNCTION
		|	tok_ENDPROGRAM
		|	tok_ENDSUBROUTINE
		;

include_stmt	:	tok_INCLUDE tok_string EOS
 			{
#ifdef ALLOW_INCLUDE
			  if(f77_include) {
			      nonstandard($1.line_num,$1.col_num,0,0);
			  }
 			  open_include_file($2.value.string,$1.line_num);
#else
			  syntax_error($1.line_num,$1.col_num,
				"statement not permitted");
#endif
 			}
 		;

/* 5,6 */
		/* Note that stmt_function_stmt is not distinguished from
		   assignment_stmt, but assign (label to variable) is.
		   Also, format_stmt w/o label is accepted here.
		   ANSI standard for statement sequencing is enforced here. */
specification_stmt:	anywhere_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     if(f90_stmt_sequence_no < F90_SEQ_IMPLICIT_NONE) {
				stmt_sequence_no = F90_SEQ_IMPLICIT_NONE;
			     }
			     /* labeled_stmt_type set below  */
			}
		|	parameter_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				   stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     else if(stmt_sequence_no > SEQ_SPECIF) {
			       check_stmt_sequence(&($1),SEQ_SPECIF);
			     }
			     if(f90_stmt_sequence_no < F90_SEQ_IMPLICIT) {
				   f90_stmt_sequence_no = F90_SEQ_IMPLICIT;
			     }
			     else if(f90_stmt_sequence_no > F90_SEQ_SPECIF) {
			       check_f90_stmt_sequence(&($1),F90_SEQ_SPECIF);
			     }
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	implicit_stmt
			{
			  check_stmt_sequence(&($1),SEQ_IMPLICIT);
			/* f90 seq checks done at implicit_stmt */
			  labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	data_stmt
			{
			     if(stmt_sequence_no < SEQ_STMT_FUN) {
				stmt_sequence_no = SEQ_STMT_FUN;
		 	     }
			     if(f90_stmt_sequence_no <= F90_SEQ_SPECIF) {
				f90_stmt_sequence_no = F90_SEQ_SPECIF;
		 	     }
			     else {
			       check_f90_stmt_sequence(&($1),F90_SEQ_EXEC);
			     }
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	specif_stmt
			{
			  check_stmt_sequence(&($1),SEQ_SPECIF);
			  check_f90_stmt_sequence(&($1),F90_SEQ_SPECIF);
			  labeled_stmt_type = LAB_SPECIFICATION;
			}
		;

anywhere_stmt	:	entry_stmt
			{
			     goto_flag = prev_goto = FALSE;
			     labeled_stmt_type = LAB_SPECIFICATION;
			}
		|	format_stmt
			{
			     labeled_stmt_type = LAB_FORMAT;
			}
		;

specif_stmt	:	dimension_stmt
		|	equivalence_stmt
		|	common_stmt
		|	namelist_stmt
		|	type_stmt
		|	attrbased_type_stmt
		|	external_stmt
		|	intrinsic_stmt
		|	save_stmt
		|       pointer_stmt
		;


/* 7 */
executable_stmt:		/* Allowed in logical IF */
			transfer_stmt
			{
			    goto_flag=TRUE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	nontransfer_stmt
			{
			    goto_flag=FALSE;
			}
		;

transfer_stmt	:	unconditional_goto
		|	assigned_goto
			{
			     if( f95_assign ) {
			       nonstandard($1.line_num,$1.col_num,0,f95_assign);
			       msg_tail(": assigned GOTO");
			     }
			}
		|	arithmetic_if_stmt
			{
			    if(style_req_block_if) {
				warning(if_line_num, if_col_num,
					"non-structured IF statement");
			    }
			}
		|	cycle_or_exit_stmt
			{
			  check_construct_name_match(&($1),
				     curr_stmt_name);
			}
		|	stop_stmt
		|	return_stmt
		;

nontransfer_stmt:	assignment_stmt
		|	assign_stmt
		|	call_stmt
		|	computed_goto	/* fallthru allowed */
		|	continue_stmt
		|	pause_stmt	
		|	io_stmt
			{
			   record_io_usage(&($1));
			}
		;

io_stmt:		read_stmt
			{
			     /* If form not defined by now, READ is unformatted.
				If no REC=num was seen, then it is sequential.
			      */
			   if(current_io_form == IO_FORM_DEFAULT)
			      current_io_form = IO_FORM_UNFORMATTED;
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			}
		|	accept_stmt
		|	write_stmt
			{
			   if(current_io_form == IO_FORM_DEFAULT)
			      current_io_form = IO_FORM_UNFORMATTED;
			   if(current_io_access == IO_ACCESS_DEFAULT)
			      current_io_access = IO_ACCESS_SEQUENTIAL;
			}
		|	print_stmt
		|       type_output_stmt
		|	open_stmt
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
		|	close_stmt
		|	inquire_stmt
		|	io_positioning_stmt
			{
			     /* These statements only apply to sequential access */
			   current_io_access = IO_ACCESS_SEQUENTIAL;
			}
		;

io_positioning_stmt:	rewind_stmt
		|	backspace_stmt
		|	endfile_stmt
		;

restricted_stmt:		/* Disallowed in logical IF */
			restricted_nontransfer_stmt
			{
			    goto_flag=FALSE;
			}
		|	else_or_endif_stmt
			{
			    prev_goto = goto_flag = FALSE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	select_case_stmt
			{
			    goto_flag = TRUE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	case_or_endselect_stmt
			{
			    prev_goto = goto_flag = FALSE;
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		;

restricted_nontransfer_stmt:
			logical_if_stmt
		|	block_if_stmt
			{
			/* Note that $1 at this point is expr, not tok_IF.
			   This is undesirable for our purpose here, but
			   needed for more important stuff elsewhere.
			 */
			    push_block(&($1),tok_IF,construct,curr_stmt_name,NO_LABEL);
			    make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
			}
		|	do_stmt
			{	/* Flag DO w/o label or DO WHILE forms here */
			  if(is_true(NONSTD_USAGE_FLAG,$1.TOK_flags))
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard($1.line_num,$1.col_num,0,0);
#else
			    syntax_error($1.line_num,$1.col_num,
				    "Nonstandard syntax");
#endif
			  push_block(&($1),tok_DO,construct,curr_stmt_name,
				     (LABEL_t)($1.tsubclass));
			  make_true(NOT_DO_TERMINAL_STMT,$$.TOK_flags);
				/* Record hash index of DO variable in the
				   block stack entry for this statement.
				 */
			  block_stack[block_depth-1].do_var_hash = $1.value.integer;
			}

		|	enddo_stmt
			{
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard($1.line_num,$1.col_num,0,0);
#else
			    syntax_error($1.line_num,$1.col_num,
				    "Nonstandard syntax");
#endif
				/* pop_block is done at stmt production, where
				   optional label can be checked for match.
				 */
			}
		;

else_or_endif_stmt:	else_if_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    push_block(&($1),$1.tclass,construct,curr_stmt_name,NO_LABEL);
			}
		|	else_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    push_block(&($1),$1.tclass,construct,curr_stmt_name,NO_LABEL);
			}
		|	end_if_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			}
		;

case_or_endselect_stmt:	case_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			    push_block(&($1),$1.tclass,construct,curr_stmt_name,NO_LABEL);
			}
		|	case_default_stmt
			{
			    pop_block(&($1),tok_CASE,curr_stmt_name,NO_LABEL);
			    push_block(&($1),tok_CASE,construct,curr_stmt_name,NO_LABEL);
			}
		|	end_select_stmt
			{
			    pop_block(&($1),$1.tclass,curr_stmt_name,NO_LABEL);
			}
		;

/* 8 */
prog_stmt	:	tok_PROGRAM {check_seq_header(&($1));}
				 symbolic_name EOS
			{
			     def_function(
					  type_PROGRAM,	/* type */
					  size_DEFAULT,	/* size */
					  (char *)NULL,	/* size text */
					  &($3),	/* name */
					  (Token*)NULL);/* args */
			     current_module_hash =
			       def_curr_module(&($3));
			}
		;

			/* Note that function & subroutine entry not
			 * distinguished in this grammar.
			 */
/* 9 */
entry_stmt	:	tok_ENTRY symbolic_name EOS
			{
			  do_ENTRY(&($2),(Token*)NULL
				   ,current_module_hash);
			}
		|	tok_ENTRY symbolic_name '(' dummy_argument_list ')' EOS
			{
			  do_ENTRY(&($2),&($4)
				   ,current_module_hash);
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("entry stmt",&($4));
#endif
			}
		;

/* 10 */
function_stmt	:	unlabeled_function_stmt
		;

unlabeled_function_stmt
		:	typed_function_handle symbolic_name EOS
			{
			     if(f77_function_noparen || f90_function_noparen) {
				nonstandard($2.line_num,
			     (unsigned)($2.col_num+strlen(token_name(&$2))),
					    f90_function_noparen,0);
				msg_tail(": parentheses required");
			     }
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &($2),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&($2));
			}
		|	typed_function_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &($2),
				      &($4));
			 current_module_hash=
			   def_curr_module(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif
			}
		|	plain_function_handle symbolic_name EOS
			{
			     if(f77_function_noparen || f90_function_noparen) {
				nonstandard($2.line_num,
			      (unsigned)($2.col_num+strlen(token_name(&$2))),
					    f90_function_noparen,0);
				msg_tail(": parentheses required");
			     }
			 def_function(
				      type_UNDECL,
				      size_DEFAULT,
				      (char *)NULL,
				      &($2),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&($2));
			}
		|	plain_function_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			 def_function(
				      type_UNDECL,	/* type */
				      size_DEFAULT,	/* size */
				      (char *)NULL,	/* size text */
				      &($2),		/* name */
				      &($4));		/* args */
			 current_module_hash=
			   def_curr_module(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif
			}
		;

typed_function_handle:	type_name function_keyword
			{
			    $$ = $2; /* needed for block opener info */
			}
		;

plain_function_handle:	function_keyword
		;

function_keyword:	tok_FUNCTION
			{
			  check_seq_header(&($1));
			}
		;

type_name	:	arith_type_name
		|	plain_char_type_name
		|	char_type_name
		;


/* 11 not present: see 9 */

/* 12 */
subroutine_stmt	:	unlabeled_subroutine_stmt
		;

unlabeled_subroutine_stmt
		:	subroutine_handle symbolic_name EOS
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &($2),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($2));
			}
		|	subroutine_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &($2),
				       &($4));
			  current_module_hash=
			    def_curr_module(&($2));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&($4));
#endif
			}
		;

subroutine_handle:	tok_SUBROUTINE
			{
			  check_seq_header(&($1));
			}
		;

dummy_argument_list:	/* empty */
			{
			    $$.next_token = (Token*)NULL;
			}
		|	non_empty_arg_list
		;

non_empty_arg_list:	dummy_argument
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	non_empty_arg_list ',' dummy_argument
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

dummy_argument	:	symbolic_name
			{
			     def_arg_name(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	'*'
			{
			     $$.TOK_type = type_byte(class_LABEL,type_LABEL);
			     $$.size = size_DEFAULT;
			     $$.TOK_flags = 0;
			     $$.left_token = (Token *)NULL;
			     label_dummy_arg_count++;
			}
		;

/* 13 not present: see 9 */

/* 14 */
block_data_stmt	:	block_data_handle EOS
			{
				  /* form name %DATnn */
			  ++block_data_number;
			  (void)sprintf(unnamed_block_data+4,"%02d",
					block_data_number%100);
			  implied_id_token(&($1),unnamed_block_data);

			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &($1),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($1));
			}
		|	block_data_handle symbolic_name EOS
			{
			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &($2),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($2));
			}
		;

block_data_handle:	tok_BLOCKDATA
			{
			  check_seq_header(&($1));
			}

		;
/* 15 */
dimension_stmt	:	tok_DIMENSION array_declarator_list EOS
		;

array_declarator_list:	array_declarator
		|	array_declarator_list ',' array_declarator
		;

/* 16 */
array_declarator:	symbolic_name '(' dim_bound_list ')'
			{
			     def_array_dim(&($1),&($3));
			}
		;

dim_bound_list	:	dim_bound_item      /* token class = no. of dimensions,
					       subclass = no. of elements */
			{
			     $$.TOK_dims = 1;
			     $$.TOK_elts = $1.TOK_elts;
			     $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	dim_bound_list ',' dim_bound_item
			{
			     $$.TOK_dims = $1.TOK_dims + 1; /* one more dimension */
			     $$.TOK_elts = $1.TOK_elts * $3.TOK_elts;
			     $$.next_token = append_token($1.next_token,&($3));
			}
		;

dim_bound_item	:	dim_bound_expr
			{
			      if( datatype_of($1.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.TOK_flags) )
				$$.TOK_elts = $1.value.integer;
			      else
				$$.TOK_elts = 0;
			}
		|	dim_bound_expr ':' dim_bound_expr
			{	/* avoid getting 0 - 0 + 1 = 1 if bounds nonconstant */
			      if( datatype_of($1.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.TOK_flags)
				 && datatype_of($3.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$3.TOK_flags) )
				$$.TOK_elts = $3.value.integer - $1.value.integer + 1;
			      else
				$$.TOK_elts = 0;

			      $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	'*'
			{
			     $$.TOK_elts = 0;
			     $$.left_token = (Token *)NULL;
			}
		|	dim_bound_expr ':' '*'
			{
			     $$.TOK_elts = 0;
			     $3.left_token = (Token *)NULL;
			     $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		;

/* 17 */
equivalence_stmt:	tok_EQUIVALENCE {equivalence_flag = TRUE;}
			equivalence_list EOS {equivalence_flag = FALSE;}
		;

equivalence_list:	'(' equivalence_list_item ')'
		|	equivalence_list ',' '(' equivalence_list_item ')'
		;

equivalence_list_item:	equiv_entity ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		|	equivalence_list_item ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		;

/* 17 */
equiv_entity	:	symbolic_name
			{
			     def_equiv_name(&($1));
			}
		|	array_equiv_name
			{
			     def_equiv_name(&($1));
			}
		|	substring_equiv_name
			{
			     def_equiv_name(&($1));
			}
		;

array_equiv_name:	symbolic_name '(' subscript_list ')'
				/* should check */
		;

substring_equiv_name:	symbolic_name substring_interval
		|	array_equiv_name substring_interval
		;

/* 19 */
common_stmt	:	tok_COMMON common_variable_list EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$), &($2));
			     if(is_true(COMMA_FLAG,$2.TOK_flags))
			   	syntax_error(
					     $2.line_num,$2.col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif

			}
		|	tok_COMMON common_block_list EOS
			{
			     if(is_true(COMMA_FLAG,$2.TOK_flags))
				syntax_error(
					     $2.line_num,$2.col_num,
					     "trailing comma");

			}
		|	tok_COMMON common_variable_list common_block_list EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$),&($2));
			     if(is_true(COMMA_FLAG,$3.TOK_flags))
				syntax_error(
					     $3.line_num,$3.col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif
			}
		;

	/*  The following defns allow trailing commas and missing commas in
	    order to tolerate the optional comma before /blockname/.  The
	    token TOK_flags holds comma status to allow errors to be caught. */
common_block_list:	labeled_common_block
			{
			     $$.TOK_flags = $1.TOK_flags;
			}
		|	common_block_list labeled_common_block
			{
			     $$.TOK_flags = $2.TOK_flags;
			     $$.line_num = $2.line_num;
			     $$.col_num = $2.col_num;
			}
		;

labeled_common_block:	common_block_name common_variable_list
			{
			     def_com_block(&($1),&($2));
			     $$.TOK_flags = $2.TOK_flags;
			     $$.line_num = $2.line_num;
			     $$.col_num = $2.col_num;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("labeled common",&($2));
#endif
			}
		;

common_block_name:	'/' symbolic_name '/'
			{
			     $$ = $2;
			}

		|	'/'  '/'		/* block with no name */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		|	tok_concat		/* "//" becomes this */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		;

common_variable_list:	common_list_item
			{
			    $$.TOK_flags = $1.TOK_flags;
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	common_variable_list common_list_item
			{
			    if(!is_true(COMMA_FLAG,$1.TOK_flags))
				syntax_error(
					$2.line_num,$2.col_num-1,
					"missing comma");
			    $$.TOK_flags = $2.TOK_flags;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

common_list_item:	common_entity
			{			   /* no comma */
			     $$.TOK_flags = $1.TOK_flags;
			     make_false(COMMA_FLAG,$$.TOK_flags);
			}
		|	common_entity ','
			{			   /* has comma */
			     $$.TOK_flags = $1.TOK_flags;
			     make_true(COMMA_FLAG,$$.TOK_flags);
   			}
		;

common_entity	:	symbolic_name
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	array_declarator
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		;


/* NAMELIST : Not Standard
   Syntax is:
	NAMELIST /group/ var [,var...] [[,] /group/ var [,var...]...]
*/

namelist_stmt	:	tok_NAMELIST namelist_list EOS
			{
			    if(is_true(COMMA_FLAG,$2.TOK_flags))
				syntax_error($2.line_num,
				 (unsigned)($2.col_num+strlen(token_name(&$2))),
					"trailing comma");
			    if(f77_namelist) {
				nonstandard($1.line_num,$1.col_num,0,0);
			    }
			}
		;

namelist_list	:	namelist_decl
		|	namelist_list namelist_decl
			{
			    $$ = $2;
			}
		;

namelist_decl	:	namelist_name namelist_var_list
			{
			     def_namelist(&($1),&($2));
			     $$ = $2;
			}
		;

namelist_name	:	'/' symbolic_name '/'
			{
			    $$ = $2;
			}
		;

namelist_var_list:	namelist_item
			{
			     $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	namelist_var_list namelist_item
			{
			    if(!is_true(COMMA_FLAG,$1.TOK_flags))
				syntax_error(
					$2.line_num,$2.col_num-1,
					"missing comma");
			    $$.TOK_flags = $2.TOK_flags;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

namelist_item	:	symbolic_name
			{			   /* no comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_false(COMMA_FLAG,$$.TOK_flags);
			}
		|	symbolic_name ','
			{			   /* has comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_true(COMMA_FLAG,$$.TOK_flags);
			}
		;

/* 20 */
type_stmt	:	arith_type_name arith_type_decl_list EOS
		|	plain_char_type_name char_type_decl_list EOS
		|	char_type_name char_type_decl_list EOS
		|	char_type_name ',' char_type_decl_list EOS
		;

				/* Attribute-based type declarations */
attrbased_type_stmt:	arith_attrbased_type_handle
			    ':' ':' arith_type_decl_list EOS
			{
			  if(f77_attrbased_typedecl) {
			    nonstandard($2.line_num, $2.col_num,0,0);
			    msg_tail(": attribute-based variable declaration");
			  }
			}
		|	char_attrbased_type_handle
			    ':' ':' char_type_decl_list EOS
			{
			  if(f77_attrbased_typedecl) {
			    nonstandard($2.line_num, $2.col_num,0,0);
			    msg_tail(": attribute-based variable declaration");
			  }
			}
		;

arith_attrbased_type_handle: arith_type_name
		|	arith_type_name ',' attr_list
		;

char_attrbased_type_handle: plain_char_type_name
		|	plain_char_type_name ',' attr_list
		|	char_type_name
		|	char_type_name ',' attr_list
		;


attr_list	:	type_attr
		|	attr_list ',' type_attr
		;

type_attr	:	tok_DIMENSION '(' dim_bound_list ')'
			{
				/* turn back on flags turned off by punct  */
			  in_attrbased_typedecl = initial_flag = TRUE;
			  dim_bound_token = $3;	/* save copy of header */
			  current_dim_bound_list = &dim_bound_token;
			}
		|	tok_SAVE
			{
			     current_save_attr = TRUE;
			}
		|	tok_EXTERNAL
			{
			     current_external_attr = TRUE;
			}
		|	tok_INTRINSIC
			{
			     current_intrinsic_attr = TRUE;
			}
		|	tok_PARAMETER
			{
			     current_parameter_attr = TRUE;
			}
		;


arith_type_name	:	sizeable_type_name
			{
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			  reset_type_attrs();
			}
				/* Allow *len to modify some arith types */
		|	sizeable_type_name '*' nonzero_unsigned_int_const
			{
			    current_typesize = $3.value.integer;
			    current_len_text = NULL;
			    reset_type_attrs();
#if 0 /* defunct feature */
			    if(local_wordsize > 0) {
			      /*  recognize REAL*2w as DOUBLE PRECISION */
			      if(current_datatype == type_REAL
				 && $3.value.integer == type_size[type_DP])
				current_datatype = type_DP;
			      /*  recognize COMPLEX*4w as DOUBLE COMPLEX */
			      if(current_datatype == type_COMPLEX
				 && $3.value.integer==type_size[type_DCOMPLEX])
				current_datatype = type_DCOMPLEX;
			    }
#endif
			     if(f77_typesize || f90_typesize) {
				nonstandard($3.line_num,$3.col_num,f90_typesize,0);
			     }

				/* Give hint to lexer to continue taking attrs
				   as keywords despite non-initial position */
			     if(see_double_colon())
				 in_attrbased_typedecl = TRUE;
			}
				/* Parse KIND selectors although we don't
				   yet support them.
				 */
		|	sizeable_type_name left_paren kind_selector ')'
			{
				/* Treat all KINDs as default */
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			  reset_type_attrs();
			}

				/* Other type disallow *len modifier */
		|	unsizeable_type_name
			{
			    reset_type_attrs();
			}
		;

sizeable_type_name:	tok_INTEGER
			{
			     current_datatype = type_INTEGER;
			     integer_context = TRUE;
			}
		|	tok_REAL
			{
			     current_datatype = type_REAL;
			     integer_context = TRUE;
			}
		|	tok_COMPLEX
			{
			     current_datatype = type_COMPLEX;
			     integer_context = TRUE;
			}
		|	tok_LOGICAL
			{
			     current_datatype = type_LOGICAL;
			     integer_context = TRUE;
			}
		;

unsizeable_type_name:	tok_DOUBLEPRECISION
			{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     reset_type_attrs();
			}
		|	tok_DOUBLECOMPLEX
			{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     reset_type_attrs();
			     if(f77_double_complex || f90_double_complex) {
				nonstandard($1.line_num,$1.col_num,f90_double_complex,0);
			     }
			}
		|	tok_BYTE /* treate BYTE as a form of integer for now */
			{
			     current_datatype = type_INTEGER;
			     current_typesize = 1;
			     current_len_text = NULL;
			     reset_type_attrs();
			     if(f77_byte || f90_byte)
			       nonstandard($1.line_num,$1.col_num,f90_byte,0);
			}
		;

				/* When F90 kind intrinsics are supported,
				   expr should become int_constant_expr.
				   For now, keep it lax to avoid spurious
				   warnings.
				 */
kind_selector	:	expr
			{
			  if(!kind_warning_given)
			      give_kind_warning(&($1));
			  if(f77_attrbased_typedecl) {
			    nonstandard($1.line_num, $1.col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }
			}
		|	symbolic_name '=' expr
			{
			  int erroneous=FALSE;
			  if( strcmp(hashtab[$1.value.integer].name,"KIND")
			      == 0 ) {
			    if(!kind_warning_given)
			      give_kind_warning(&($1));
			  }
			  else {
			    syntax_error($1.line_num,$1.col_num,
					 "unrecognized keyword");
			    msg_tail(hashtab[$1.value.integer].name);
			    erroneous=TRUE;
			  }
			  if(!erroneous && f77_attrbased_typedecl) {
			    nonstandard($1.line_num, $1.col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }
			}
		;


plain_char_type_name:	tok_CHARACTER
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
		;

char_type_name	:	plain_char_type_name char_selector
			{
			     current_typesize = $2.value.integer;
			     current_size_is_adjustable = $2.size_is_adjustable;
			     current_size_is_expression = $2.size_is_expression;
				/* Save length spec text if expression */
			     if(current_size_is_expression) {
			       if($2.left_token == NULL)
				 current_len_text = new_tree_text(&($2));
			       else
				 current_len_text = new_tree_text($2.left_token);
			     }
			     else
			       current_len_text = NULL;

			     reset_type_attrs();
				/* Give hint to lexer to continue taking attrs
				   as keywords despite non-initial position */
			     if(see_double_colon())
				 in_attrbased_typedecl = TRUE;
			}
		;

char_selector	:	'*' len_specification
			{
			  $$ = $2;
			}

				/* This production uses a special left paren
				   to avoid a shift/reduce conflict with
				     IMPLICIT CHARACTER(letter_list)
				   The lexer knows when to produce this
				   special left paren.
				 */
		|	left_paren {len_spec_item_count = 0;} len_spec_list ')'
			{
			  if( len_selector_given ) {
			    $$ = len_spec_token; /* Recover spec saved below */
				/* Store as a parenthesized expr tree */
			    $$.left_token = add_tree_node(&($1),
							  &len_spec_token,
							  (Token*)NULL);
			  }
				/* If len_spec_list does not specify a LEN,
				   use the current default values.
				 */
			  else {
			    $$.left_token = (Token *)NULL;
			    $$.value.integer = current_typesize;
			    $$.size_is_adjustable = current_size_is_adjustable;
			    $$.size_is_expression = current_size_is_expression;
			  }
			  if(f77_attrbased_typedecl) {
			    nonstandard($1.line_num, $1.col_num,0,0);
			    msg_tail(": F90-style variable declaration");
			  }
			}
		;

			/* This production simply turns the special left paren
			   back into a regular paren in case it matters
			   somewhere.  The grammar still knows it's special.
			 */
left_paren	:	tok_lparen
			{
			  $$.tclass = '('; /* make it a regular paren */
			}
		;


arith_type_decl_list:	arith_type_decl_item
		|	arith_type_decl_list ',' arith_type_decl_item
		;

			/* Allow the combined type declaration and data value form.
			 */
arith_type_decl_item: scalar_type_decl_entity
			{
			     if( current_parameter_attr) {
				syntax_error($1.line_num,$1.col_num,
					     "PARAMETER lacks initializer");
			     }
			}
				/* Handle bastard initializers (combined type decl
				   and data statement) here.
				 */
		|	 scalar_type_decl_entity '/'
				{integer_context=FALSE;complex_const_allowed=TRUE;}
					data_value_list
				{integer_context=TRUE;complex_const_allowed=FALSE;}  '/'
			{
			    if(f77_initializers || f90_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    primary_id_expr(&($1),&($1));
			    check_initializer_type(&($1),&($2),&($4));
			}
				/* Handle F90 initializers here.  Note that
				   this production will not be reached in
				   non attribute-based type declarations since
				   it will be lexed as an assignment statement.
				 */
		|	scalar_type_decl_entity {integer_context=FALSE;complex_const_allowed = TRUE;}
				'=' parameter_expr
			{
			    if(current_parameter_attr)
				def_parameter(&($1),&($4),FALSE);
			    else
				use_lvalue(&($1));
			    if(f77_initializers) {
				nonstandard($3.line_num,$3.col_num,
					    0,0);
				msg_tail(": F90-style initializer");
			    }
			    primary_id_expr(&($1),&($1));
			    check_initializer_type(&($1),&($3),&($4));
			    integer_context=TRUE;
			    complex_const_allowed = FALSE;
			}
				/* Note: array initializers not supported
				   since syntax for array constructors
				   is not yet implemented.
				 */
		|	array_declarator
			{
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),(Token *)NULL);
			}
				/* Handle bastard initializers here.  Not checked
				   for assignment compatibility.
				 */
		|	array_declarator '/'
				{integer_context=FALSE;complex_const_allowed=TRUE;}
					data_value_list
				{integer_context=TRUE;complex_const_allowed=FALSE;}  '/'
			{
			    declare_type(&($1),
					 current_datatype,
					 current_typesize,
					 current_len_text);
			    process_attrs(&($1),(Token *)NULL);
			    if(f77_initializers || f90_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    use_lvalue(&($1));
			    primary_id_expr(&($1),&($1));
			    check_initializer_type(&($1),&($2),&($4));
			}
		;

scalar_type_decl_entity:symbolic_name
			{
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),current_dim_bound_list);
			}
		;

char_type_decl_list:	char_type_decl_item
		|	char_type_decl_list ',' char_type_decl_item
		;

char_type_decl_item: char_type_decl_entity
			{
			     if( current_parameter_attr) {
				syntax_error($1.line_num,$1.col_num,
					     "PARAMETER lacks initializer");
			     }
			}
				/* Handle bastard initializers here */
		|	 char_type_decl_entity '/'
				{integer_context=FALSE;complex_const_allowed=TRUE;}
					data_value_list
				{integer_context=TRUE;complex_const_allowed=FALSE;}  '/'
			{
			    if(f77_initializers || f90_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    primary_id_expr(&($1),&($1));
			    check_initializer_type(&($1),&($2),&($4));
			}
				/* Handle F90 initializers here */
		|	char_type_decl_entity '=' parameter_expr
			{
			    if(current_parameter_attr)
				def_parameter(&($1),&($3),FALSE);
			    else
				use_lvalue(&($1));
			    primary_id_expr(&($1),&($1));
			    if(f77_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    0,0);
				msg_tail(": F90-style initializer");
			    }
			    check_initializer_type(&($1),&($2),&($3));
			}
		|	array_declarator
			{
			     $1.size_is_adjustable = current_size_is_adjustable;
			     $1.size_is_expression = current_size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),(Token *)NULL);
			}
		|	array_declarator '*' len_specification
			{
			     $1.size_is_adjustable = $3.size_is_adjustable;
			     $1.size_is_expression = $3.size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  $3.value.integer,
					  new_tree_text(
					     $3.left_token == NULL?
					     &($3): $3.left_token )
					  );
			     process_attrs(&($1),(Token *)NULL);
			}
		|	array_declarator '/'
				{integer_context=FALSE;complex_const_allowed=TRUE;}
					data_value_list
				{integer_context=TRUE;complex_const_allowed=FALSE;}  '/'
			{
			    $1.size_is_adjustable = current_size_is_adjustable;
			    $1.size_is_expression = current_size_is_expression;
			    declare_type(&($1),
					 current_datatype,
					 current_typesize,
					 current_len_text);
			    process_attrs(&($1),(Token *)NULL);
			    if(f77_initializers || f90_initializers) {
				nonstandard($2.line_num,$2.col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    use_lvalue(&($1));
			}
		|	array_declarator '*' len_specification '/'
				{integer_context=FALSE;complex_const_allowed=TRUE;}
					data_value_list
				{integer_context=TRUE;complex_const_allowed=FALSE;}  '/'
			{
			    $1.size_is_adjustable = $3.size_is_adjustable;
			    $1.size_is_expression = $3.size_is_expression;
			    declare_type(&($1),
					 current_datatype,
					 $3.value.integer,
					 new_tree_text(
					     $3.left_token == NULL?
					     &($3): $3.left_token )
				);
			    process_attrs(&($1),(Token *)NULL);
			    if(f77_initializers || f90_initializers) {
				nonstandard($4.line_num,$4.col_num,
					    f90_initializers,0);
				msg_tail(": combined type declaration and data-style initializer");
			    }
			    use_lvalue(&($1));
			}
   		;

char_type_decl_entity:symbolic_name
			{
			     $1.size_is_adjustable = current_size_is_adjustable;
			     $1.size_is_expression = current_size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			     process_attrs(&($1),current_dim_bound_list);
			}
		|	symbolic_name '*' len_specification
			{
			     $1.size_is_adjustable = $3.size_is_adjustable;
			     $1.size_is_expression = $3.size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  $3.value.integer,
					  new_tree_text(
					     $3.left_token == NULL?
					     &($3): $3.left_token )
					  );
			     process_attrs(&($1),current_dim_bound_list);
			}
		;

/* 21 */
				/* implicit_flag helps is_keyword's work */
implicit_handle	:	tok_IMPLICIT {implicit_flag=TRUE;}
		;

implicit_stmt	:	implicit_handle implicit_decl_list EOS
			{
			    implicit_flag=FALSE;
			    if(implicit_none) {
				syntax_error($1.line_num,$1.col_num,
				     "conflicts with IMPLICIT NONE");
			    }
			    else {
				implicit_type_given = TRUE;
			    }
			    check_f90_stmt_sequence(&($1),F90_SEQ_IMPLICIT);
			}
				/* IMPLICIT NONE statement */
		|	implicit_handle tok_NONE EOS
			{
			    implicit_flag=FALSE;
			    if(implicit_type_given) {
			      syntax_error($1.line_num,$1.col_num,
				   "conflicts with IMPLICIT statement");
			    }
			    else {
			      if(f77_implicit_none)
				      nonstandard($2.line_num,$2.col_num,0,0);
			      implicit_none = TRUE;
			    }
			    check_f90_stmt_sequence(&($1),F90_SEQ_IMPLICIT_NONE);
			}
		;

implicit_decl_list:	implicit_decl_item
		|	implicit_decl_list ',' {initial_flag = TRUE;}
				       implicit_decl_item
		;

		/* implicit_letter_flag tells lexer to treat letters as letters,
			   not as identifiers */
implicit_decl_item:	type_name '('  {implicit_letter_flag = TRUE;}
				letter_list ')'  {implicit_letter_flag = FALSE;}
		;

letter_list	:	letter_list_item
		|	letter_list ',' letter_list_item
		;

letter_list_item:	tok_letter
			{
			  int c1 = (int)$1.tsubclass;
				/* kluge to allow other non-alpha chars:
				   treate anything except _ as $.
				 */
			  if( !isalpha(c1) && c1 != '_' ) c1 = '$';

			  if( ((f77_dollarsigns||f90_dollarsigns) && c1=='$')
			   || (f77_underscores && c1=='_') ) {
			    nonstandard($1.line_num,$1.col_num,
				f90_dollarsigns&&c1=='$',0);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c1);
			}
		|	tok_letter '-' tok_letter
			{
			  int c1 = (int)$1.tsubclass,
			      c2 = (int)$3.tsubclass;

			  if( !isalpha(c1) && c1 != '_' ) c1 = '$';
			  if( !isalpha(c2) && c2 != '_' ) c2 = '$';

			  if( ((f77_dollarsigns||f90_dollarsigns) && (c1 == '$' || c2 == '$'))
			   || (f77_underscores && (c1 == '_' || c2 == '_')))
			  {
			    if(!isalpha(c1))
			      nonstandard($1.line_num,$1.col_num,
				  f90_dollarsigns&&(c1=='$'||c2=='$'),0);
			    else
			      nonstandard($3.line_num,$3.col_num,
				  f90_dollarsigns&&(c1=='$'||c2=='$'),0);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c2);
			}
		;


/* 22 */
len_specification:	nonzero_unsigned_int_const
			{
			     $$.value.integer = $1.value.integer;
			     $$.size_is_adjustable = 0;
			     $$.size_is_expression = 0;
			}

		|	'(' len_spec_expr ')'
			{
			    $$.value.integer = $2.value.integer;
			    $$.size_is_adjustable = $2.size_is_adjustable;
			    $$.size_is_expression = $2.size_is_expression;
				/* Store as a parenthesized expr tree */
			    $$.left_token = add_tree_node(&($1),
							  &($2),
							  (Token*)NULL);
			}
		;

		/* To keep grammar simple, the syntax rules for CHARACTER
		   length-selector are relaxed, leaving checking to the
		   semantic processing.  Legal variations are
		      CHARACTER*(length)
		      CHARACTER*(LEN=length)
		      CHARACTER*(length,kind)
		      CHARACTER*(length,KIND=kind)
		      CHARACTER*(LEN=length,KIND=kind)
		      CHARACTER*(KIND=kind,LEN=length)
		   where length is * or a specification-expr, and kind is
		   a scalar-int-initialization-expr.  The * can be omitted
		   in all of these.
		   The grammar rules here accept anything of the form
		      CHARACTER*([KEYWORD=]value[, ...])
		   where value is * or int_constant_expr.  When the LEN
		   keyword is seen, the len_spec_item token is copied to
		   len_spec_token to be used in higher-level productions.
		 */

len_spec_list	:	len_spec_item
			{
			  ++len_spec_item_count;
			}
		|	len_spec_list ',' len_spec_item
			{
			  ++len_spec_item_count;
			}
		;

len_spec_item	:	len_spec_expr
			{
				/* Non-keyword form: 1st item is LEN */
			  if(len_spec_item_count == 0) {
			    len_spec_token = $1;
			    len_selector_given = TRUE;
			  }
				/* 2nd item is KIND */
			  else if(len_spec_item_count == 1) {
			    if(!kind_warning_given)
			      give_kind_warning(&($1));
			  }
			  else if(len_spec_item_count == 2) {
			    syntax_error($1.line_num,$1.col_num,
					 "too many specifiers in list");
			  }
			}
		|	symbolic_name '=' len_spec_expr
			{
			  int erroneous=FALSE;
			  if( strcmp(hashtab[$1.value.integer].name,"LEN")
			      == 0 ) {
			    len_spec_token = $3;
			    len_selector_given = TRUE;
			  }
			  else if( strcmp(hashtab[$1.value.integer].name,"KIND")
			      == 0 ) {
			    if(!kind_warning_given)
			      give_kind_warning(&($1));
			  }
			  else {
			    syntax_error($1.line_num,$1.col_num,
					 "unrecognized keyword");
			    msg_tail(hashtab[$1.value.integer].name);
			    erroneous=TRUE;
			  }
			  if(!erroneous && f77_attrbased_typedecl) {
			    nonstandard($2.line_num, $2.col_num,0,0);
			    msg_tail(": F90-style declaration");
			  }
			}
		;

len_spec_expr	:	'*'
			{
			     $$.left_token = (Token *)NULL;
			     $$.value.integer = size_ADJUSTABLE;
			     $$.size_is_adjustable = 1;
			     $$.size_is_expression = 0;
			}
		|	int_constant_expr
			{
			     $$.size_is_adjustable = 0;
			     $$.size_is_expression = 1;
			     if( $$.value.integer <= 0 ){
			       if(misc_warn) {
				 warning($1.line_num,$1.col_num,
					"invalid length specification");
				 msg_tail(": substituting 1");
			       }
			       $$.value.integer = 1;
			     }
			}
		;

/* 23 */
parameter_stmt	:	std_parameter_stmt
		|	parenless_parameter_stmt
		;

std_parameter_stmt:	tok_PARAMETER '(' parameter_defn_list ')' EOS
		;

parenless_parameter_stmt:tok_PARAMETER {param_noparen=TRUE;}
			parameter_defn_list {param_noparen=FALSE;} EOS
			{
			  if(f77_param_noparen || f90_param_noparen) {
				nonstandard($1.line_num,$1.col_num,f90_param_noparen,0);
				msg_tail(" : PARAMETER declaration without parentheses");
			  }
			}
   		;

parameter_defn_list:	parameter_defn_item
		|	parameter_defn_list ',' parameter_defn_item
		;

parameter_defn_item:	symbolic_name {complex_const_allowed = TRUE;}
				'=' parameter_expr
			{
			     def_parameter(&($1),&($4),param_noparen);
			     primary_id_expr(&($1),&($1));
			     check_initializer_type(&($1),&($3),&($4));
			     complex_const_allowed = FALSE;
			}
		;

/* 24 */
external_stmt	:	tok_EXTERNAL external_name_list EOS
		;

external_name_list:	symbolic_name
			{
			     def_ext_name(&($1));
			}
		|	external_name_list ',' symbolic_name
			{
			     def_ext_name(&($3));
			}
		;

/* 25 */
intrinsic_stmt	:	tok_INTRINSIC intrinsic_name_list EOS
		;

intrinsic_name_list:	symbolic_name
			{
			     def_intrins_name(&($1));
			}
		|	intrinsic_name_list ',' symbolic_name
			{
			     def_intrins_name(&($3));
			}
		;

        /* constructs for POINTER(pointer=pointee) statement */
pointer_stmt    :       tok_POINTER pointer_item_list EOS
		{
		  if(f77_cray_pointers || f90_cray_pointers) {
		    nonstandard($1.line_num,$1.col_num,f90_cray_pointers,0);
		  }
		}
		;

pointer_item_list:      pointer_item
		|       pointer_item_list ',' pointer_item
		;

pointer_item    :       '(' pointer_name ',' pointee_name ')'
		;

pointer_name    :       symbolic_name
			{
			     declare_type(&($1),type_INTEGER,local_ptrsize,
					  NULL );
			}
		;

pointee_name    :       symbolic_name
		        {
				/* Suppress set/used warnings since
				   often is accessed only via pointer */
		             use_lvalue(&($1));
		             use_variable(&($1));
		        }
		|       array_declarator
		        {
		             use_lvalue(&($1));
		             use_variable(&($1));
		        }
		;

/* 26 */
save_stmt	:	tok_SAVE EOS
			{
			  global_save = TRUE;
			}
		|	tok_SAVE save_list EOS
		;

save_list	:	save_item
		|	save_list ',' save_item
		;

save_item	:	symbolic_name
			{
			     save_variable(&($1));
			}
		|	'/' symbolic_name '/'
			{
/***			     def_com_block(&($2),(Token*)NULL);***/
			     save_com_block(&($2));
			}
		;

/* 27 */
data_stmt	:	tok_DATA data_defn_list EOS
   		;

data_defn_list	:	data_defn_item
		|	data_defn_list data_defn_item
		|	data_defn_list ',' data_defn_item
		;

data_defn_item	:	data_defn_assignee_list '/'
				{complex_const_allowed=TRUE;}
					data_value_list
				{complex_const_allowed=FALSE;}  '/'
		;

data_defn_assignee_list
		:	data_defn_assignee
		|	data_defn_assignee_list ',' data_defn_assignee
		;

data_defn_assignee:	lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_value_list:	data_value
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	data_value_list ',' data_value
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

data_value	:	data_constant_value
			{
			    $$.left_token = (Token*)NULL;
			}
		|	data_repeat_factor '*' data_constant_value
			{
				/* Save data repeat factor in a permanent token
				   pointed to by left_token.
				 */
			    Token *tcopy = new_token();
			    *tcopy = $1; /* copy the repeat factor token */
			    $$ = $3; /* pass data_value up the parse tree */
			    $$.left_token = tcopy;
			}
		;

data_repeat_factor:	nonzero_unsigned_int_const
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;

data_constant_value:	data_constant
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;


data_dlist	:	data_dlist_item
		|	data_dlist ',' data_dlist_item
		;

data_dlist_item	:	array_element_lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_implied_do_list:  '(' data_dlist ',' symbolic_name
				'=' data_do_loop_bounds ')'
			{
			    use_implied_do_index(&($4));
			}
		;

data_do_loop_bounds:	int_constant_expr ',' int_constant_expr
		| int_constant_expr ',' int_constant_expr ',' int_constant_expr
		;


/* 29 */
assignment_stmt	:	lvalue '=' {complex_const_allowed = TRUE;
				    in_assignment_stmt = TRUE;} expr
			{
			  if( ! (is_true(LVALUE_EXPR,$1.TOK_flags)
			       || is_true(STMT_FUNCTION_EXPR,$1.TOK_flags) )) {
			    syntax_error($1.line_num,$1.col_num,
					 "left side is not assignable");
			    if(is_true(CONST_EXPR,$1.TOK_flags))
				msg_tail(": it is a constant");
			  }
			  else {
			    int array_lhs, array_rhs;
			    array_lhs =
			      (($1.TOK_flags&(ARRAY_ID_EXPR|ARRAY_ELEMENT_EXPR)) == ARRAY_ID_EXPR);
			    array_rhs =
			      (($4.TOK_flags&(ARRAY_ID_EXPR|ARRAY_ELEMENT_EXPR)) == ARRAY_ID_EXPR);
			    if( array_lhs || array_rhs ) {
			      if( (! array_lhs) && misc_warn) {
				warning($1.line_num,$1.col_num,
					"array assigned to scalar");
			      }
			      else if( f77_assignment ) {
				nonstandard($2.line_num,$2.col_num,0,0);
				msg_tail(": assignment involving whole array");
			      }
			    }

			    assignment_stmt_type(&($1),&($2),
					&($4));
			  }
			  complex_const_allowed = FALSE;
			  in_assignment_stmt = FALSE;
			}
				 EOS
			{
				/* Clear u-b-s flags spuriously set */
			  if(is_true(STMT_FUNCTION_EXPR, $1.TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN)
			     stmt_function_stmt(&($1));
		        }
		;

lvalue		:	variable_name
		|	array_element_lvalue
		|	substring_lvalue
		|	stmt_function_handle
		;


/* array-element_lvalue is at 88 */

assign_stmt	:    	tok_ASSIGN pre_label label tok_TO variable_name EOS
			{
			    do_ASSIGN(&($5));
			    if( f95_assign ) {
			      nonstandard($1.line_num,$1.col_num,0,f95_assign);
			      msg_tail(": ASSIGN statement");
			    }

			    ref_label(&($3),LAB_ASSIGN);

			}
		;


/* 31 */
unconditional_goto:	goto pre_label label EOS
			{

			  ref_label(&($3),LAB_GOTO);

			}
		;

/* 32 */
computed_goto	:	goto '(' goto_list ')' integer_expr EOS
		|	goto '(' goto_list ')' ',' integer_expr EOS
		;

/* 33 */
assigned_goto	:	goto symbolic_name EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name ',' '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		;

goto		:	tok_GOTO
			{
			    integer_context=TRUE;
				/* Warn if GOTO considered harmful */
			    if( style_goto ) {
			      warning($1.line_num,$1.col_num,
				      "obsolescent feature: GOTO statement");
			    }
			}
		;

goto_list	:	pre_label label
                        {
                            ref_label(&($2), LAB_GOTO);
			}
		|	goto_list ',' pre_label label
                        {
			    ref_label(&($4), LAB_GOTO);
			}
		;

/* 34 */
arithmetic_if_stmt:	if_handle pre_label label ',' pre_label label
				 ',' pre_label label EOS
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_INTEGER && t != type_REAL
			     && t != type_DP && t != type_ERROR ) {
			    syntax_error($1.line_num,$1.col_num,
		  "integer, real, or double precision expression required");
			  }
			  ref_label(&($3), LAB_GOTO);
			  ref_label(&($6), LAB_GOTO);
			  ref_label(&($9), LAB_GOTO);

			}
		;

/* 35 */
logical_if_stmt	:	if_handle executable_stmt
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error($1.line_num,$1.col_num,
					  "logical expression required");
			}
		;

/* 36 */
block_if_stmt	:	if_handle tok_THEN EOS
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error($1.line_num,$1.col_num,
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
		;

if_handle	:	f77_if_handle
			{
			    curr_stmt_name = NULL;
			}
		|	construct_spec f77_if_handle
			{
			  if(f77_construct_name) {
			    nonstandard($1.line_num,$1.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			  construct_name_seen=FALSE;
			  $$ = $2;
			}
		;

f77_if_handle	:	tok_IF '(' {complex_const_allowed = TRUE;}  expr ')'
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;	/* for is_keyword */
			    if_line_num = $1.line_num; /* save location */
			    if_col_num = $1.col_num; /* for picky warnings */
			    $$ = $4; /* Inherit expr for type checking above */
			}
		;

/* 37 */
else_if_stmt	:	tok_ELSE tok_IF '(' {complex_const_allowed = TRUE;} expr ')'
			{
			    int t=datatype_of($5.TOK_type);
			    if(t != type_LOGICAL && t != type_ERROR)
				syntax_error($5.line_num,$5.col_num,
					  "logical expression required");

			    if(is_true(ID_EXPR,$5.TOK_flags)){
				use_variable(&($5));
			    }

			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;
			}
			else_if_then
		;

else_if_then	:	tok_THEN EOS
			{
			    curr_stmt_name = NULL;
			}
		|	tok_THEN construct_name EOS
			{
			  if(f77_construct_name) {
			    nonstandard($2.line_num,$2.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
		;

/* 38 */
else_stmt	:	tok_ELSE EOS
			{
			    curr_stmt_name = NULL;
			}
		|	tok_ELSE construct_name EOS
			{
			  if(f77_construct_name) {
			    nonstandard($2.line_num,$2.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
		;

/* 39 */
end_if_stmt	:	tok_ENDIF EOS
			{
			    curr_stmt_name = NULL;
			}
		|	tok_ENDIF construct_name EOS
			{
			  if(f77_construct_name) {
			    nonstandard($2.line_num,$2.col_num,0,0);
			    msg_tail(": IF construct name");
			  }
			}
		;

                      /* F90 CASE construct:
                         SELECT CASE ( expr )
                         [ CASE ( case-value [, case-value ...] ) 
                            [ ... ] ]
                         [ CASE DEFAULT
                            [ ... ] ]
                         END SELECT
                      */
select_case_stmt:	select_handle '(' {complex_const_allowed = TRUE;} expr ')' EOS
			{
			    int t = datatype_of ($4.TOK_type);
			    if (t != type_ERROR) {
			        if (!is_const_type(t) || is_float_type(t)) {
			            syntax_error($4.line_num,$4.col_num,
			"integer, character, or logical expression required");
			        }
			    }
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;
			    push_block(&($1),$1.tclass,construct,curr_stmt_name,NO_LABEL);
			}
		;

select_handle	:	tok_SELECTCASE
			{
			    curr_stmt_name = NULL;
			    if (f77_case_construct) {
				nonstandard($1.line_num,$1.col_num,0,0);
			    }
			    if( style_req_construct_name ) {
			      warning($1.line_num,$1.col_num,
			       "Construct name missing from SELECT statement");
			    }
			}
		|	construct_spec tok_SELECTCASE
			{
			    if (f77_case_construct) {
				nonstandard($2.line_num,$2.col_num,0,0);
			    }
			    $$ = $2;
			}
		;

case_stmt	:	case_handle EOS
		|	case_handle construct_name EOS
		;

case_handle	:	tok_CASE '(' case_values ')'
		;

case_values	:	case_value
		|	case_values ',' case_value
		;

case_value	:	case_value_primary
		|	case_value_primary ':' case_value_primary
			{
			    int t1 = datatype_of($1.TOK_type),
			        t2 = datatype_of($3.TOK_type);
			    if (t1 == type_LOGICAL || t2 == type_LOGICAL) {
			        syntax_error($2.line_num,$2.col_num,
			            "ranges of type LOGICAL not allowed here");
			    }
			    if (t1 != t2) {
			        syntax_error($3.line_num,$3.col_num,
			            "range boundaries must have the same type");
			    }
			}
		|	':' case_value_primary
			{
			    int t = datatype_of($2.TOK_type);
			    if (t == type_LOGICAL) {
			        syntax_error($2.line_num,$2.col_num,
			            "ranges may not have type LOGICAL bounds");
			    }
			}
		|	case_value_primary ':'
			{
			    int t = datatype_of($2.TOK_type);
			    if (t == type_LOGICAL) {
			        syntax_error($2.line_num,$2.col_num,
			            "ranges may not have type LOGICAL bounds");
			    }
			}
		;

case_value_primary:	expr
			{
			    int t = datatype_of($1.TOK_type);
			    if (t != type_ERROR) {
			        if (!is_const_type(t) || is_float_type(t)) {
			            syntax_error($1.line_num,$1.col_num,
			"integer, character, or logical expression required");
			        }
			    }
			    if (!is_true(CONST_EXPR, $1.TOK_flags)) {
			        syntax_error($1.line_num,$1.col_num,
			"expression must evaluate to a compile-time constant");
			    }
			    $$ = $1;
			}
		;

case_default_stmt:	tok_CASEDEFAULT EOS
		|	tok_CASEDEFAULT construct_name EOS
		;

end_select_stmt	:	tok_ENDSELECT EOS
		|	tok_ENDSELECT construct_name EOS
		;


/* 40 */
			/* Allow F90 extensions:
			   DO [label [,]] var = expr , expr [,expr]
			   DO [label [,]] WHILE ( expr )
			      ...
			   ENDDO
			*/

do_stmt		:	do_handle variable_name
				'=' do_loop_bounds EOS
			{
			  if( ! is_true(LVALUE_EXPR,$2.TOK_flags) ) {
			    syntax_error($2.line_num,$2.col_num,
					 "index is not assignable");
			    if(is_true(CONST_EXPR,$2.TOK_flags))
				msg_tail(": it is a constant");
			    $$.value.integer = -1; /* no hash entry */
			  }
			  else {
			     def_do_variable(&($2));
				/* Store hash index of DO index in token for
				   use when pushing block on stack. The
				   value field is not used by keywords, so it
				   is OK to use it this way. */
			     $$.value.integer = $2.value.integer;
			  }

				/* Check for non-integer DO index or bounds */
			     if(datatype_of($2.TOK_type) == type_INTEGER
				&& datatype_of($4.TOK_type) != type_INTEGER) {
			       if( f95_real_do ) {
				 nonstandard($4.line_num,$4.col_num,0,f95_real_do);
				 msg_tail(": DO loop bounds not integer");
			       }
			       else if(misc_warn) {
				 warning($3.line_num,$3.col_num,
				  "type mismatch between DO index and bounds");
			       }
			     }
			     else if(datatype_of($2.TOK_type) != type_INTEGER) {
			       if( f95_real_do ) {
				 nonstandard($2.line_num,$2.col_num,0,f95_real_do);
				 msg_tail(": DO index is not integer");
			       }
			       else if(datatype_of($4.TOK_type) != type_INTEGER) {
				 if(port_real_do)
				   nonportable($4.line_num,$4.col_num,
					       "non-integer DO loop");
			       }
			       else {
				 if(trunc_real_do_index) {
				   warning($2.line_num,$2.col_num,
					   "DO index is not integer");
				 }
			       }
			     }
			}
		|	tok_DOWHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    $$.value.integer = -1; /* no DO index */
			    curr_stmt_name = NULL;
			}
		/* Normally, the lexer glues DO WHILE together and yields
		   tok_DOWHILE.  The following production is needed, however,
		   for e.g. DO 100 WHILE and constructname : DOWHILE */
		|	do_handle tok_WHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$5.TOK_flags)){
				use_variable(&($5));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    $$.value.integer = -1; /* no DO index */
			}
		|	do_handle EOS
			{
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    $$.value.integer = -1; /* no DO index */
			}
		;

do_handle	:	f77_do_handle
			{
		/* In picky mode warn if no name tag on block construct. */
			  if( style_req_construct_name ) {
			      warning($1.line_num,$1.col_num,
				"Construct name missing from DO statement");
			  }
			  curr_stmt_name = NULL;
			}
		|	construct_spec f77_do_handle
			{
			  if(f77_construct_name) {
			    nonstandard($1.line_num,$1.col_num,0,0);
			    msg_tail(": DO construct name");
			  }
			  construct_name_seen=FALSE;
			  $$ = $2;
			}
		;

f77_do_handle	:	tok_DO pre_label label
                        {
			    ref_label(&($3), LAB_DO);
			    def_do_label(&($3));
				/* Save label in subclass for push_block */
			    $$.tsubclass = $3.value.integer;
			}
		|	tok_DO pre_label label ','
                        {
                            ref_label(&($3), LAB_DO);
			    def_do_label(&($3));
			    $$.tsubclass = $3.value.integer;
			}
		|	tok_DO pre_label
			{
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    integer_context=FALSE;
			    $$.tsubclass = (long)NO_LABEL;
			}
		;

do_loop_bounds	:	int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.TOK_type=do_bounds_type(&($1),&($3),&($3));
			}
		|   int_real_dp_expr ',' int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.TOK_type=do_bounds_type(&($1),&($3),&($5));
			}
		;

enddo_stmt	:	tok_ENDDO EOS
			{
			  curr_stmt_name = NULL;
			}
		|	tok_ENDDO construct_name EOS
		;

/* 41 */
continue_stmt	:	tok_CONTINUE EOS
		;

/* F90 CYCLE and EXIT statements.  Note: at this time, the
   optional do-construct-name is not supported. */
cycle_or_exit_stmt:	cycle_stmt
		{
			   if( f77_cycle_exit ) {
			     nonstandard($1.line_num,$1.col_num,0,0);
			     msg_tail(": CYCLE statement");
			   }
		}
		|	exit_stmt
		{
			   if( f77_cycle_exit ) {
			     nonstandard($1.line_num,$1.col_num,0,0);
			     msg_tail(": EXIT statement");
			   }
		}
		;

cycle_stmt	:	tok_CYCLE EOS
		|	tok_CYCLE construct_name EOS
		;

exit_stmt	:	tok_EXIT EOS
		|	tok_EXIT construct_name EOS
		;

/* 42 */
stop_stmt	:	tok_STOP stop_info EOS
		;

/* 43 */
pause_stmt	:	tok_PAUSE stop_info EOS
			{
			  if( f95_pause ) {
			    nonstandard($1.line_num,$1.col_num,0,f95_pause);
			    msg_tail(": PAUSE statement");
			  }
			}
		;

stop_info	:	/* empty */
		|	tok_integer_const
		|	symbolic_name
			{
			     use_variable(&($1));
			}
		|	tok_string
		;

/* 44 */
write_stmt	:	write_handle
				{complex_const_allowed = FALSE;} EOS
		|	write_handle io_list
				{complex_const_allowed = FALSE;} EOS
		;

write_handle	:	tok_WRITE {init_io_ctrl_list();}
				'(' control_info_list ')'
				{complex_const_allowed = TRUE;}
		;

/* 45 */
		/* Note that parenthesized format_id's will end up in
		   control_info_list. Disambiguation left to semantic phase.
		   This is why we need the optional comma */
read_stmt	:	read_handle '(' control_info_list ')' EOS
		|	read_handle '(' control_info_list ')' io_list EOS
		|	read_handle '(' control_info_list ')' ',' io_list EOS
		|	read_handle format_id EOS
			{
			    record_default_io();
			}
		|	read_handle format_id ',' io_list EOS
			{
			    record_default_io();
			}
		;
read_handle	:	tok_READ {init_io_ctrl_list();}
		;

accept_stmt	:	tok_ACCEPT format_id EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
		|	tok_ACCEPT format_id ',' io_list EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
		;

/* 46 */
print_stmt	:	tok_PRINT format_id EOS
			{
			    record_default_io();
			}
   		|	tok_PRINT format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
			{
			    record_default_io();
			}
		;

type_output_stmt:	tok_TYPE format_id EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
   		|	tok_TYPE format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
			{
			    if(f77_accept_type || f90_accept_type)
				nonstandard($1.line_num,$1.col_num,f90_accept_type,0);
			    record_default_io();
			}
		;

/* 47 */
control_info_list:	control_info_item
			{
			    ++control_item_count;
			}
		|	control_info_list ',' control_info_item
			{
			    ++control_item_count;
			    if(! io_warning_given) {
			      if( io_internal_file ) {
				if( (curr_stmt_class == tok_WRITE ||
				     curr_stmt_class == tok_READ) &&
				    io_list_directed ) {
				  if(f77_internal_list_io) {
				    nonstandard($3.line_num,$3.col_num,0,0);
	    msg_tail(": internal file cannot be used with list-directed I/O");
				  }
				  io_warning_given = TRUE;
				}
			      }
			    }
			}
		;

	/* Note that unit id is not distinguished from format id
	   by the grammar. Use sequence no. to tell which is which.
	 */
control_info_item:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	unit_id
			{
			  if(control_item_count == 0) /* unit id */
			  {
					/* Handle special cases */
			    if( datatype_of($1.TOK_type) == type_STRING ) {
					/* unit id=char variable is
					   an internal file.  I/O goes in
					   and out of the variable. */
			      if( is_true(ID_EXPR,$1.TOK_flags) ) {
				 io_internal_file = TRUE;
				 if(curr_stmt_class == tok_WRITE) {
				      use_lvalue(&($1));
				 }
			      }
			      else { /* internal unit must be a variable */
				syntax_error($1.line_num,$1.col_num,
					"internal file must be a variable");
			      }
			    }
			    else { /* Otherwise it is a normal external file unit id */
				 record_io_unit_id(&$1);
			    }
			  }
			  else if(control_item_count == 1) /* format id */
			  {
			    if( $1.tclass == '*' )
			    {
				 io_list_directed = TRUE;
			    }
			    else if( is_true(ID_EXPR,$1.TOK_flags)){
				 if(datatype_of($1.TOK_type) == type_NAMELIST) {
				   ref_namelist(&($1),curr_stmt_class);
				 }
				 else
				     /* format id=integer variable is assigned format */
				   if( datatype_of($1.TOK_type) == type_INTEGER) {
				     if( f95_assign ) {
				       nonstandard($1.line_num,$1.col_num,0,f95_assign);
				       msg_tail(": assigned format");
				     }
				   }
			    }
				/* An integer at this point is a format label */
			    else if ( is_true(LIT_CONST,$1.TOK_flags) &&
				      $1.TOK_type == type_byte(class_VAR,type_INTEGER))
			    {
				 ref_label(&($1),LAB_IO);
			    }
			    current_io_form = IO_FORM_FORMATTED;
  			  }
					/* Handle use of variable */
			  if( is_true(ID_EXPR,$1.TOK_flags)){
			       use_variable(&($1));
			  }
			}
		;

			/* OPEN stmt needs its own control list defn to
			   allow for VMS READONLY and similar keywords.
			   Special prodn for unit_id as optional 1st item
			   needed to avoid reduce/reduce conflict with
			   later-occurring symbolic_name items.   */
open_info_list	:	unit_id
			{
			    if( $1.tclass != '*'
			       && is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if(control_item_count == 0)
			    {
			       record_io_unit_id(&($1));
			    }
			    ++control_item_count;
			}
		|	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			    ++control_item_count;
			}
		|	open_info_list ',' open_info_item
			{
			    ++control_item_count;
			}
		;

open_info_item	:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	symbolic_name	/* NOSPANBLOCKS, READONLY or SHARED */
			{
			    use_special_open_keywd(&($1));
			}
		;

/* 48 */
io_list		:	io_item
		|	io_list ',' io_item
		;

io_item		:	expr
			{
			    if( curr_stmt_class == tok_READ ||
				curr_stmt_class == tok_ACCEPT ) { /* Input */
				if(is_true(LVALUE_EXPR,$1.TOK_flags)) {
				    use_lvalue(&($1));
				}
				else {
				    syntax_error($1.line_num,$1.col_num,
						 "item is not assignable");
				/* Give hint if it is a parameter */
				    if(is_true(ID_EXPR,$1.TOK_flags) &&
				       is_true(CONST_EXPR,$1.TOK_flags))
					msg_tail(": it is a constant");
				}
			    }
			    else {				 /* Output */
				if(is_true(ID_EXPR,$1.TOK_flags)){
				    use_variable(&($1));
				}
			    }
			}
		|	io_implied_do_list
		;

/* 49 */
io_implied_do_list:	'(' io_list ',' variable_name '=' do_loop_bounds ')'
			{
			  if( ! is_true(LVALUE_EXPR,$4.TOK_flags) ) {
			    syntax_error($4.line_num,$4.col_num,
					 "index is not assignable");
			    if(is_true(CONST_EXPR,$4.TOK_flags))
				msg_tail(": it is a constant");
			  }
			  else {
			     use_implied_do_index(&($4));
			  }
			}
		;

/* 50 */
open_stmt	:	tok_OPEN {init_io_ctrl_list();}
				 '(' open_info_list ')' EOS
		;

/* 51 */
close_stmt	:	tok_CLOSE {init_io_ctrl_list();}
				'(' control_info_list ')' EOS
		;

/* 52 */
inquire_stmt	:	tok_INQUIRE {init_io_ctrl_list();}
				'(' control_info_list ')' EOS
		;

/* 53 */
backspace_stmt	:	backspace_handle unit_id EOS
			{
			    if( $2.tclass != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			    record_io_unit_id(&$2);
			}
		|	backspace_handle '(' control_info_list ')' EOS
		;
backspace_handle:	tok_BACKSPACE {init_io_ctrl_list();}
		;

/* 54 */
endfile_stmt	:	endfile_handle unit_id EOS
			{
			    if( $2.tclass != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			    record_io_unit_id(&$2);
			}
		|	endfile_handle '(' control_info_list ')' EOS
		;
endfile_handle	:	tok_ENDFILE {init_io_ctrl_list();}
		;

/* 55 */
rewind_stmt	:	rewind_handle unit_id EOS
			{
			    if( $2.tclass != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			    record_io_unit_id(&$2);
			}
		|	rewind_handle '(' control_info_list ')' EOS
		;
rewind_handle	:	tok_REWIND {init_io_ctrl_list();}
		;


/* 56 */
		/* "expr" causes shift/reduce conflict on ')' between
		   red'n  unit_id: expr_  and shift  primary: ( expr_ ).
		   Use "associativity" rule to force reduction */
unit_id		:	expr		%prec REDUCE
		|	'*'
		;

/* 57 */
format_id	:	char_expr
			{
			  if(is_true(ID_EXPR,$1.TOK_flags)){
			    use_variable(&($1));
				/* If integer, format_id is assigned format */
			    if( datatype_of($1.TOK_type) == type_INTEGER ) {
			      if( f95_assign ) {
				nonstandard($1.line_num,$1.col_num,0,f95_assign);
				msg_tail(": assigned format");
			      }
			    }
			  }
			     /* A format label appears here as integer const */
			  else if(is_true(LIT_CONST,$1.TOK_flags) &&
			    $1.TOK_type == type_byte(class_VAR,type_INTEGER)){
			      ref_label(&($1),LAB_IO);
			  }
			}
		|	'*'
		;

/* 58,59 */
format_stmt	:	tok_FORMAT {inside_format=TRUE;} '(' format_spec ')' EOS
			{
			  inside_format=FALSE;
			}
		;

/* 60-69 */
format_spec	:		/* EMPTY */
		|	nonempty_format_spec
		;


nonempty_format_spec:	fmt_spec_item
		|	nonempty_format_spec fmt_spec_item
		;

fmt_spec_item	:	repeatable_fmt_item
		|	unrepeatable_fmt_item
		|	fmt_item_separator
		;

repeatable_fmt_item:	'(' nonempty_format_spec ')'
		|	tok_edit_descriptor
		;

unrepeatable_fmt_item:	tok_string
		|	tok_hollerith
			{
			  if( f95_Hedit ) {
			    nonstandard($1.line_num,$1.col_num,0,f95_Hedit);
			    msg_tail(": H edit descriptor");
			  }
			}
		|	repeat_spec
		|	variable_fmt_item
		;

fmt_item_separator:	','
		|	'/'
		|	tok_concat	/* since lexer spots "//" */
		|	':'
		|	'.'		/* Occurs when variable w.d is used */
		|	nonstandard_fmt_item
			{
			  if(f77_format_dollarsigns || f90_format_dollarsigns)
			     nonstandard($1.line_num,$1.col_num,f90_format_dollarsigns,0);
			}
		;

nonstandard_fmt_item: '$'	/* VMS uses this */
		;

repeat_spec	:	tok_integer_const
		|	'-' tok_integer_const	/* for kP descriptor */
		|	'+' tok_integer_const	/* for +kP descriptor */
		;

		/* VMS-style variable format size or repeat spec*/
variable_fmt_item:	'<' {inside_format=FALSE;} integer_expr
				{inside_format=TRUE;} '>'
			{
			  if(f77_variable_format || f90_variable_format)
			     nonstandard($1.line_num,$1.col_num,f90_variable_format,0);
			}
		;

/* 70 handle only: complete defn handled as assignment stmt */

stmt_function_handle:	scalar_name '(' stmt_function_dummy_list ')'
			{
			  check_stmt_sequence(&($1),SEQ_STMT_FUN);
			  check_f90_stmt_sequence(&($1),F90_SEQ_SPECIF);

				def_stmt_function(&($1),&($3));
					/* make token info */
				primary_id_expr(&($1),&($$));
#ifdef DEBUG_PARSER
				if(debug_parser)
				  print_exprlist("stmt function",&($3));
#endif
			}
		;

stmt_function_dummy_list: /* empty list */
			{
			    $$.next_token = (Token*)NULL;
			}
		| nonempty_stmt_fun_dummy_list
		;

nonempty_stmt_fun_dummy_list:	  stmt_function_dummy_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	  nonempty_stmt_fun_dummy_list ','
					stmt_function_dummy_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

stmt_function_dummy_arg:  variable_name	/* for now: later, handle correctly */
		;

/* 71 */
call_stmt	:	call_handle
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' ')'
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' subr_arg_list ')'
			{
			     call_subr(&($1),&($3));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("call stmt",&($3));
#endif
			     complex_const_allowed = FALSE;
			} EOS
		;

call_handle	:	tok_CALL symbolic_name
			{
			     complex_const_allowed = TRUE;
			     $$ = $2;
			}
		;
subr_arg_list:		subr_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			    $$.left_token = (Token *)NULL;
			}
		|	subr_arg_list ',' subr_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

subr_arg	:	expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				 use_actual_arg(&($1));
				 use_variable(&($1));
			    }
			}
		|	'*' pre_label label
			{
			    ref_label(&($3), LAB_CALL);  
			  $$ = $3;
			  $$.left_token = (Token *)NULL;
			}
		;

/* 72 */
return_stmt	:	tok_RETURN EOS
			{
			  (void)do_RETURN(current_module_hash,&($1));
			}
		|	tok_RETURN integer_expr EOS
			{
			  if( do_RETURN(current_module_hash,&($1)) ) {

				/* Warn if alternate return value is a constant
				   that is not between 0 and the number of
				   labels that are dummy-arguments.
				 */
			     if( pretty_alt_return &&
				(is_true(EVALUATED_EXPR,$2.TOK_flags) &&
			        ($2.value.integer < 1 ||
				  $2.value.integer > label_dummy_arg_count)) ){
				 warning($2.line_num,$2.col_num,
					 "alternate return value");
				 msg_tail(ulongtostr($2.value.integer));
				 if( $2.value.integer < 0 ) {
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
				 warning($1.line_num,$1.col_num,
				     "obsolescent feature: alternate return");
			       }
			     }
			  }
			}
		;

/* 73 */
function_reference:	fun_or_substr_handle '(' fun_arg_list ')'
			{
				   /* restore context */
				if(!is_true(COMPLEX_FLAG,$1.TOK_flags))
				  complex_const_allowed=FALSE;
				if(is_true(IN_ASSIGN,$1.TOK_flags))
				  in_assignment_stmt = TRUE;

				  /* Change empty arg list to no arg list */
				if($3.next_token == NULL)
				  call_func(&($1),(Token *)NULL);
				else
				  call_func(&($1),&($3));
							/* make token info */
				func_ref_expr(&($1),&($3),&($$));
				/* Substitute empty token for null arglist */
				$$.left_token = add_tree_node(
						   &($2),&($1),
						   ($3.next_token == NULL?
						    empty_token(&($3)) :
						    $3.next_token) );
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("function",&($3));
#endif
			}
		;

fun_or_substr_handle:	scalar_name
			{
			  if(complex_const_allowed)/* save context */
			    make_true(COMPLEX_FLAG,$$.TOK_flags);
			  complex_const_allowed=TRUE;
			  if(in_assignment_stmt)
			    make_true(IN_ASSIGN,$$.TOK_flags);
			  in_assignment_stmt = FALSE;
			}
		;
fun_arg_list	:	/* empty */
			{
				$$.tclass = 0;
				$$.next_token = (Token *)NULL;
				$$.left_token = (Token *)NULL;
			}
		|	nonempty_fun_arg_list
		;

nonempty_fun_arg_list:	expr
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			    $$.left_token = (Token *)NULL;
			}
		|	nonempty_fun_arg_list ',' expr
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}

		;
/* 74 not present: type checking not done at this level */

/* 75 was constant_expr, but only used by PARAMETER */
parameter_expr	:	/* arith, char, or logical */ expr
			{
			  int t=datatype_of($1.TOK_type);
			  if( t != type_ERROR){
			    if( ! is_const_type(t) ) {
			      syntax_error($1.line_num,$1.col_num,
		      "arithmetic, char, or logical expression expected");
			    }
			    else {
			      if( !is_true(PARAMETER_EXPR,$1.TOK_flags) ) {
				syntax_error($1.line_num,$1.col_num,
					   "constant expression expected");
			      }
			    /* Here we allow, with some warnings, expr
			       containing intrins func or **REAL in
			       PARAMETER defn. */
			      else if( !is_true(CONST_EXPR,$1.TOK_flags) ) {
				if(f77_param_intrinsic) {
				  nonstandard($1.line_num,$1.col_num,0,0);
				  msg_tail(
			 "intrinsic function or **REAL in PARAMETER defn");
				}
			      }
			    }
			  }
			}
		;

/* 76 following the text of the standard, not the diagrams */
expr		:	log_expr
			{
				/* Fix it up in case it is used in expr list */
			  $$.next_token = (Token *) NULL;
#ifdef DEBUG_PARSER
			    if(debug_parser) {
				(void)fprintf(list_fd,
					"\nexpr: class=0x%lx subclass=0x%lx",
					$1.tclass,
					$1.tsubclass);
			    }
#endif
			}
		;

log_expr	:	log_disjunct

		|	expr tok_EQV log_disjunct
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		|	expr tok_NEQV log_disjunct
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_disjunct	:	log_term

		|	log_disjunct tok_OR log_term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_term	:	log_factor

		|	log_term tok_AND log_factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_factor	:	log_primary

		|	tok_NOT log_primary
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		;

log_primary	:	arith_expr

		|	log_primary tok_relop log_primary
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;


arith_expr	:	term

		|	'-' term
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		|	'+' term
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		|	arith_expr '+' term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		|	arith_expr '-' term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

term		:	factor

		|	term '/' factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			    if(div_check &&
			       !is_true(CONST_EXPR,$3.TOK_flags)){
				warning($2.line_num,$2.col_num,
					"Possible division by zero");
			    }
			}
		|	term '*' factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

factor		:	char_expr

		|	char_expr tok_power factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

char_expr	:	primary

		|	char_expr tok_concat primary
			{
			  do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

primary		:	variable_name
			{
			    make_true(DIM_BOUND_EXPR,$$.TOK_flags);
			}
		|	array_element_name

		|	function_reference

		|	substring_name

		|	literal_const
			{
			    $$.TOK_flags = 0;
			    $$.left_token = (Token *)NULL;
			    make_true(CONST_EXPR,$$.TOK_flags);
			    make_true(PARAMETER_EXPR,$$.TOK_flags);
			    make_true(LIT_CONST,$$.TOK_flags);
			    make_true(EVALUATED_EXPR,$$.TOK_flags);
			    make_true(DIM_BOUND_EXPR,$$.TOK_flags);
			}
		|	'(' expr ')'
			{
			    $$ = $2;
				/* (identifier) becomes a non-identifier */
			    if(is_true(LVALUE_EXPR,$2.TOK_flags)) {
				if(pretty_parens) {
				  ugly_code($2.line_num,$2.col_num,
					  "Extraneous parentheses");
				}
				use_variable(&($2));
				make_false(LVALUE_EXPR,$$.TOK_flags);
				make_false(ARRAY_ID_EXPR,$$.TOK_flags);
				make_false(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				make_false(ID_EXPR,$$.TOK_flags);
				make_false(DO_VARIABLE,$$.TOK_flags);
			    }
				/* (expr) becomes tree node with root = '(' */
			    $$.left_token = add_tree_node(&($1),&($2),
							  (Token*)NULL);
			}
		;

				/* Literal constants are numbers, strings
				   holleriths, and logical constants */
literal_const	:	numeric_const
			    /* (class, size set in numeric_const productions) */
		|	tok_string
			{
			    $$.TOK_type = type_byte(class_VAR,type_STRING);
			    /* (size is set in get_string) */
			}
		|	tok_hollerith
			{
			    $$.TOK_type = type_byte(class_VAR,type_HOLLERITH);
			    /* (size is set in get_hollerith) */
			    if(port_hollerith) {
				warning($1.line_num,$1.col_num,
				"hollerith constant may not be portable");
			    }
			}
		|	tok_logical_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_LOGICAL);
			    $$.size = size_DEFAULT;
			}
		;

numeric_const	:	tok_integer_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_INTEGER);
			    $$.size = size_DEFAULT;
			}
		|	tok_real_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_REAL);
			    $$.size = size_DEFAULT;
			}
		|	tok_dp_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_DP);
			    $$.size = size_DEFAULT;
			}
		|	tok_quad_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_QUAD);
			    $$.size = size_QUAD;
                            if(f77_quad_constants || f90_quad_constants) {
                              nonstandard($1.line_num,$1.col_num,f90_quad_constants,0);
                              msg_tail(": quad precision constant");
                            }
			}
		|	tok_complex_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_COMPLEX);
			    $$.size = size_DEFAULT;
			}
		|	tok_dcomplex_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_DCOMPLEX);
			    $$.size = size_DEFAULT;
			}
		;

/* 77 */
integer_expr	:	/* integer */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if(datatype_of($1.TOK_type) != type_INTEGER) {
				syntax_error(
					$1.line_num,$1.col_num,
					"expression must be integer type");
			    }
			}
		;

/* 78 */
int_real_dp_expr:	/* integer, real, or double */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    {
				int t=datatype_of($1.TOK_type);
				    if(t != type_INTEGER && t != type_REAL
					&& t != type_DP ) {
					syntax_error(
					  $1.line_num,$1.col_num,
		"expression must be integer, real, or double precision type");
			    	    }
			    }
			}
		;

/* 79 absent */

/* 80 */
int_constant_expr:	/* integer const */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if( ! is_true(CONST_EXPR,$1.TOK_flags) ) {
				syntax_error(
					$1.line_num,$1.col_num,
					"constant expression expected");
			    }
			    else {
			      if(datatype_of($1.TOK_type) != type_INTEGER){
				syntax_error(
					$1.line_num,$1.col_num,
					"integer expression expected");
			      }
			      else {
				$$.value.integer = int_expr_value(&($1));
			      }
			    }
			}
		;

/* 81 */
dim_bound_expr	:       /* integer */  arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }

			    if(f77_array_bounds) {	/* Section 5.1.1.1 */
			      if( !is_true(DIM_BOUND_EXPR,$1.TOK_flags) ) {
				nonstandard($1.line_num,$1.col_num,0,0);
				msg_tail(
		": array bounds expr cannot have array or function reference");
			      }
			    }

			    if( datatype_of($1.TOK_type) != type_INTEGER ){
				syntax_error(
					$1.line_num,$1.col_num,
					"integer dimension expected");
				$$.value.integer = 0;
			    }
			    else {
			      if( is_true(EVALUATED_EXPR,$1.TOK_flags) )
				$$.value.integer =
				  int_expr_value(&($1));
			      else		/* must be dummy */
				$$.value.integer = 0;
			    }
			}
		;

/* 82-85 absent: no type checking here */
/* 86-87 absent: see 76 */

/* 88 */
array_element_lvalue:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array lvalue",&($3));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,$$.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				$$.left_token = add_tree_node(
						   &($2),&($1),$3.next_token);
				$$.next_token = (Token *) NULL;
			}
		;

array_element_name:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array",&($3));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,$$.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				$$.left_token = add_tree_node(
						   &($2),&($1),$3.next_token);
				$$.next_token = (Token *) NULL;
			}
		;

subscript_list	:	subscript
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	subscript_list ',' subscript
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		     ;

subscript	:	expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				 use_variable(&($1));
			    }
				/* check subscript exprs for integer type */
			    if(datatype_of($1.TOK_type) != type_INTEGER)
			      if(trunc_real_subscript)
			         warning($1.line_num,$1.col_num,
					 "subscript is not integer");
			}
		;

/* 89 */
substring_name	:	fun_or_substr_handle  substring_interval
			{
				   /* restore status of complex flag */
			    if(!is_true(COMPLEX_FLAG,$1.TOK_flags))
				  complex_const_allowed=FALSE;
				/* set flag to keep more than just id for
				   arg list text */
			    if(is_true(ID_EXPR,$1.TOK_flags))
			       make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
			    $$.size=substring_size(&($1),&($2));
			    $$.left_token = add_tree_node(
					       &save_token,&($1),&($2));
			    $$.next_token = (Token *) NULL;
			}

		|	function_reference  substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			    $$.left_token = add_tree_node(
					       &save_token,&($1),&($2));
			    $$.next_token = (Token *) NULL;
			}

		|	array_element_name substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			    $$.left_token = add_tree_node(
					       &save_token,&($1),&($2));
			    $$.next_token = (Token *) NULL;
			}
		;

substring_lvalue:	scalar_name substring_interval
			{
			    ref_variable(&($1));
			    $$.TOK_flags = $1.TOK_flags;
			    $$.size=substring_size(&($1),&($2));
			}
		|	array_element_lvalue substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			}
		;

			/* substring interval: limits go into
			   TOK_start, TOK_end.  */

substring_interval:	'(' ':' ')'
			{
			    $$.TOK_start=1;
			    $$.TOK_end=0; /* 0 means LEN */

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($2),
				     empty_token(&($1)),empty_token(&($3)));
				/* Nullify next_token so it looks like
				   a tokenlist */
			    $$.next_token = (Token *)NULL;
			}

		  |	'(' substr_index_expr ':' ')'
			{
			    $$.TOK_start=$2.value.integer;
			    $$.TOK_end=0; /* 0 means LEN */

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($3),&($2),empty_token(&($4)));
			    $$.next_token = (Token *)NULL;
			}
		  |	'(' ':' substr_index_expr ')'
			{
			    $$.TOK_start=1;
			    $$.TOK_end=$3.value.integer;

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($2),empty_token(&($1)),&($3));
			    $$.next_token = (Token *)NULL;
			}
		  |	'(' substr_index_expr ':' substr_index_expr ')'
			{
			    $$.TOK_start=$2.value.integer;
			    $$.TOK_end=$4.value.integer;

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($3),&($2),&($4));
			    $$.next_token = (Token *)NULL;
			}
		  ;

substr_index_expr:	arith_expr
			{
			  if(is_true(ID_EXPR,$1.TOK_flags)){
			    use_variable(&($1));
			  }
				/* check validity and replace nonconst
				   value by size_UNKNOWN. */
			  if(is_true(CONST_EXPR,$1.TOK_flags)) {
			    if( ($$.value.integer=int_expr_value(&($1))) < 1) {
			      syntax_error($1.line_num,$1.col_num,
					   "invalid substring index");
			    }
			  }
			  else  /* (no longer need ID hash index) */
			    $$.value.integer=size_UNKNOWN;
			}
		;

/* 90-98 absent: name categories not distinguished */

/* 99 */
variable_name	:	scalar_name
			{
			    ref_variable(&($1));
			    $$.TOK_flags = $1.TOK_flags;
			}
		|	array_name
		;

scalar_name	:	tok_identifier
			{
			    ref_identifier(&($1));
			    primary_id_expr(&($1),&($$));
			}
		;

array_name	:	tok_array_identifier
			{
			    ref_variable(&($1));
			    primary_id_expr(&($1),&($$));
			}
		;


/* symbolic_name refers to a name without making it into an id expr */
symbolic_name	:	tok_identifier
		|	tok_array_identifier
		;


construct_spec	:	construct_name ':'
			{
			  construct_name_seen=TRUE;
				/* remember the name for block balancing */
			  curr_stmt_name = hashtab[$1.value.integer].name;
			}
		;

construct_name	:	tok_identifier
			{
			  curr_stmt_name = hashtab[$1.value.integer].name;
			}
		|	tok_array_identifier
			{
			  curr_stmt_name = hashtab[$1.value.integer].name;
			}
		;


/* 100 */
data_constant	:	numeric_const
		|	'-' numeric_const { $$ = $2; }
		|	'+' numeric_const { $$ = $2; }
		|	tok_logical_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_LOGICAL);
			    $$.size = size_DEFAULT;
			}
   		|	tok_string
			{
			    $$.TOK_type = type_byte(class_VAR,type_STRING);
			    $$.size = size_DEFAULT;
			}
		|	tok_hollerith
			{
			    $$.TOK_type = type_byte(class_VAR,type_HOLLERITH);
			    $$.size = size_DEFAULT;
			}
		;

/* 101-102 absent */

/* 103 */
nonzero_unsigned_int_const:
			tok_integer_const
			{
			  if($1.value.integer == 0) {
			    if(misc_warn) {
			      warning($1.line_num,$1.col_num,
				    "nonzero integer expected");
			      msg_tail(": substituting 1");
			    }
			    $$.value.integer = 1;
			  }
			  $$.left_token = (Token *)NULL;
			}
		;

/* 104-109 absent: lexer handles these */
	/* pre_label prepares for an expected label by setting flag
	   so that lexer won't look for E-format number.  All grammar
	   rules that have "label" precede it with "pre_label" */
pre_label	:	/* NOTHING */
			{
			    integer_context=TRUE;
			}
		;

/* 110 */
label		:	tok_integer_const
			{
			    if( $$.value.integer > 99999 && misc_warn) {
				syntax_error($1.line_num,$1.col_num,
				      "statement label exceeds 5 digits");
			    }
				integer_context=FALSE;
				$$.TOK_type = type_byte(class_LABEL,type_LABEL);
				$$.size = size_DEFAULT;
				$$.TOK_flags = 0;
			}
		;

/* 111-116 absent: lexer handles these */

%%

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
