/* $Id: symtab.c,v 1.42 2003/03/26 01:09:22 moniot Exp $

  Definitions of symbol table maintenance routines and
  hash table functions


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

/*
  I. Symtab


		Symbol table routines for Fortran program checker.

	  Shared functions defined:


	   call_func(id,arg)	 Handles function invocations.
	   call_subr(id,arg)	 Handles CALL statements.
	   declare_type(id,datatype,size) Handles TYPE statements.
	   def_arg_name(id)	 Handles func/subr argument lists.
	   def_array_dim(id,arg) Handles dimensioning declarations.
	   def_com_block(id)	 Handles common blocks and SAVE stmts.
	   def_com_variable(id)	 Handles common block lists.
       int def_curr_module(id)	 Identifies symbol as current module.
     	   def_equiv_name(id)	 Initializes equivalence list items.
	   def_ext_name(id)	 Handles external lists.
	   def_function(datatype,size,size_text,id,args)
	   		Installs function name in global table.
	   def_intrins_name(id)  Handles intrinsic lists.
	   def_parameter(id,value) Handles parameter_defn_item
	   def_stmt_function(id) Declares a statement function.
	   do_ASSIGN(id)	 Handles ASSIGN stmts.
	   do_assigned_GOTO(id)	 Handles assigned GOTO.
	   do_ENTRY(id,args,hashno) Processes ENTRY statement.
	   do_RETURN(hashno,keyword) Processes RETURN statement.
	   equivalence(id1,id2)	 equivalences two variables
       int get_type(symt)	 Finds out data type of symbol, or uses implicit
				 typing to establish its type.
       int get_size(symt,type)	 Finds out size of symbol's datatype.
	unsigned hash_lookup(s)	 Looks up identifier in hashtable.
	   init_globals()	 Initializes global symbol info.
	   init_symtab()	 Clears local symbol table & removes locals
				 from stringspace. Also restores default
				 implicit data typing.
 Gsymtab* install_global(t,datatype,storage_class) Installs indentifier in
				global symbol table.
 Lsymtab* install_local(t,datatype,storage_class) Installs indentifier in
				local symbol table.
	   ref_array(id,subscrs) Handles array references
	   ref_variable(id)	 Handles accessing variable name.
	   set_implicit_type(type,size,c1,c2) Processes IMPLICIT statement.
	   stmt_function_stmt(id) Finishes processing stmt func defn.
    char * token_name(t)	 Returns ptr to token's symbol's name.
	   use_actual_arg(id)	 Handles using a variable as actual arg.
	   use_io_keyword(id_keywd,id_val,class) Handles i/o control specifier.
	   use_len_arg(id)	 Handles arguments passed to LEN.
	   use_lvalue(id)	 Handles assignment to a variable.
	   use_parameter(id)	 Handles data_constant_value &
				 data_repeat_factor.
	   use_variable(id)	 Sets used-flag for a variable used in expr.

*/

/*  private functions defined:
 call_external(symt,id,arg)	places token list of args into local symtab
 check_intrins_args(arg, defn) Checks call seq of intrinsic functions
 check_stmt_function_args(symt,id,arg)  ditto for statement functions
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#define SYMTAB
#include "ftnchek.h"
#include "symtab.h"
#include "symspace.h"
#include "symutils.h"
#include "intrins.h"
#include "tokdefs.h"

#ifdef DEVELOPMENT		/* for maintaining the program */
#define DEBUG_SIZES
#endif



PROTO(PRIVATE void call_external,( Lsymtab *symt, Token *id, Token *arg ));
PROTO(PRIVATE void check_intrins_args,( Token *id, Token *arg ));
PROTO(PRIVATE void check_stmt_function_args,( const Lsymtab *symt, Token *id, Token *arg ));
PROTO(PRIVATE Lsymtab* install_local,( int h, int datatype, int storage_class ));
PROTO(PRIVATE void use_function_arg,( Token *id ));
PROTO(PRIVATE void use_len_arg,( Token *id ));



#ifdef DEBUG_SIZES
PROTO(extern void print_sizeofs,( void ));	/* in symtab.c */
#endif



			/* This routine handles the saving of arg lists which
			   is done by call_func and call_subr.  Also called
			   by def_namelist to save its variable list. */
PRIVATE void
#if HAVE_STDC
call_external(Lsymtab *symt, Token *id, Token *arg)
#else /* K&R style */
call_external(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
#endif /* HAVE_STDC */
{
       	TokenListHeader *TH_ptr;

		/* Insert the new list onto linked list of token lists */
      	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (arg == NULL ? NULL: arg->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;
#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\nSubprogram %s :: ",symt->name);
	  if(arg != NULL)
	    print_expr_list(arg->next_token);
	}
#endif
} /*call_external*/

void
#if HAVE_STDC
call_func(Token *id, Token *arg)	/* Process function invocation */
#else /* K&R style */
call_func(id,arg)	/* Process function invocation */
	Token *id, *arg;
#endif /* HAVE_STDC */
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	IntrinsInfo *defn;

	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
       	   symt->info.toklist = NULL;
	}
	else {			/* protect ourself against nonsense */
	   if( symt->array_var || symt->parameter ) {
	      syntax_error(id->line_num,id->col_num,
		   "identifier was previously declared a non-function:");
	      msg_tail(symt->name);
	      symt->array_var = symt->parameter = FALSE;
	      symt->info.toklist = NULL;
	   }
	}


	t = datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if(storage_class_of(symt->type) == class_VAR) {
	    symt->type = type_byte(class_SUBPROGRAM,t);
	    symt->info.toklist = NULL;
	}


		/* See if intrinsic.  If so, set flag, save info */
	if(!symt->external && !symt->intrinsic && !symt->argument
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
	}

		/* Update set/used status of variables in arg list.  This
		   is deferred to now to allow intrinsics to be treated
		   as pure functions regardless of pure_function flag. */

	if(arg != NULL) {
	    Token *a=arg;
	    intrins_flags_t
	        nonpure,	/* flag if function may modify arg */
	        i_len;		/* special handling for intrinsic LEN */
	    if(symt->intrinsic) {
	      nonpure = symt->info.intrins_info->intrins_flags&I_NONPURE;
	      i_len = symt->info.intrins_info->intrins_flags&I_LEN;
	    }
	    else {
	      nonpure = ! pure_functions;
	      i_len = FALSE;
	    }

			/* Token list is in reverse order.  Restore
			   args to original order. */
	    arg->next_token = reverse_tokenlist(arg->next_token);

  	    while( (a=a->next_token) != NULL) {
	      if(is_true(ID_EXPR,a->TOK_flags)){
		if( nonpure ) {
			     /* Treat impure function like subroutine call */
		  use_actual_arg(a);
		  use_variable(a);
		}
		else {
		  if(i_len)
		    use_len_arg(a); /* LEN is sui generis */
		  else
			     /* Pure-function invocation checks u-b-s */
		    use_function_arg(a);
		}
	      }
	    }
	}

		/* If intrinsic, do checking now.  Otherwise, save arg list
		   to be checked later. */

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	if(misc_warn)
	  check_intrins_args(id,arg);
    }
    else {		/* It is not intrinsic: install in global table */
      switch(storage_class_of(symt->type)) {
	case class_SUBPROGRAM:
	  symt->external = TRUE;
	  if((!symt->argument) && (gsymt=(hashtab[h].glob_symtab)) == NULL) {
		gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
		gsymt->info.arglist = NULL;
	  }
			/* store arg list in local table */
	  call_external(symt,id,arg);
	  break;
	case class_STMT_FUNCTION:
	  symt->external = TRUE;
	  if(misc_warn)
	    check_stmt_function_args(symt,id,arg);
	  break;
      }
    }

    if(! symt->used_flag) { /* record first line where used */
	symt->line_used = id->line_num;
	symt->file_used = inctable_index;
    }

    symt->used_flag = TRUE;
    symt->invoked_as_func = TRUE;

} /*call_func*/


void
#if HAVE_STDC
call_subr(Token *id, Token *arg)	/* Process call statements */
#else /* K&R style */
call_subr(id,arg)	/* Process call statements */
	Token *id, *arg;
#endif /* HAVE_STDC */
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
#ifndef STANDARD_INTRINSICS
	IntrinsInfo *defn;
#endif
	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(h,type_SUBROUTINE,class_SUBPROGRAM);
   	   symt->info.toklist = NULL;
	}
	else {			/* protect ourself against nonsense */
	   if( symt->array_var || symt->parameter ) {
	      syntax_error(id->line_num,id->col_num,
		   "identifier was previously declared a non-subroutine:");
	      msg_tail(symt->name);
	      symt->array_var = symt->parameter = FALSE;
	      symt->info.toklist = NULL;
	   }
	}


	t=datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if( (storage_class_of(symt->type) == class_VAR
	     || symt->external ) && t == type_UNDECL) {
		t = type_SUBROUTINE;
		symt->info.toklist = NULL;
	}
	symt->type = type_byte(class_SUBPROGRAM,t);

	/* Since nonstandard intrinsics include some subroutines,
	   see if it is in intrinsic list.  Or
	   if declared intrinsic, then accept it as such and
	   do checking now.  Otherwise, save arg list
	   to be checked later. */
#ifndef STANDARD_INTRINSICS
    if(!symt->external && !symt->intrinsic
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
    }
#endif

			/* Token list is in reverse order.  Restore
			   args to original order. */
    if(arg != NULL)
	arg->next_token = reverse_tokenlist(arg->next_token);

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	if(misc_warn)
	  check_intrins_args(id,arg);
    }
    else {		/* It is not intrinsic: install in global table */
	symt->external = TRUE;
	if((!symt->argument) && (gsymt=(hashtab[h].glob_symtab)) == NULL) {
		gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
		gsymt->info.arglist = NULL;
	}
			/* store arg list in local table */
	call_external(symt,id,arg);
    }

    if(! symt->used_flag) { /* record first line where used */
	symt->line_used = id->line_num;
	symt->file_used = inctable_index;
    }

    symt->used_flag = TRUE;

}/*call_subr*/

		/* check out consistency of intrinsic argument list */
PRIVATE
void
#if HAVE_STDC
check_intrins_args(Token *id, Token *arg)
#else /* K&R style */
check_intrins_args(id, arg)
	Token *id;
	Token *arg;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	IntrinsInfo *defn=symt->info.intrins_info;
	unsigned args_given = ((arg == NULL)?0:arg_count(arg->next_token));
	int numargs;
	unsigned short flags;
	Token *t;

	numargs = defn->num_args;
	flags = defn->intrins_flags;

			/* positive numargs: must agree */
	if( (numargs >= 0 && (args_given != (unsigned)numargs))
			/* 1 or 2 arguments allowed */
	 || (numargs == I_1or2 && (args_given != 1 && args_given != 2))
			/* numargs == -2: 2 or more */
	 || (numargs == I_2up && (args_given < 2))
			/* 0 or 1 argument allowed */
	 || (numargs == I_0or1 && (args_given != 0 && args_given != 1)) ){
		LINENO_t linenum;
		COLNO_t colnum;
		if(arg==NULL) {linenum=id->line_num; colnum=id->col_num;}
		else {linenum = arg->line_num; colnum = arg->col_num;}

		syntax_error(linenum,colnum,
		  "wrong number of arguments for intrinsic function");
		msg_tail(defn->name);
	}
#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\nIntrinsic %s :: ",defn->name);
	  if(arg != NULL)
	    print_expr_list(arg->next_token);
	}
#endif
	if(arg != NULL && numargs != 0) {

	  Token *prev_t,	/* one operand in type propagation  */
	         fake_op;	/* operator in binexpr_type call */

	  t = arg->next_token;
				/* Copy type & size info into result */
	  arg->tclass = t->tclass;
	  arg->tsubclass = t->tsubclass;
#ifndef TOK_type
	  arg->TOK_type = t->TOK_type;
#endif
#ifndef TOK_flags
	  arg->TOK_flags = t->TOK_flags;
#endif
	  arg->size = t->size;
	  prev_t = t;

	  while(t != NULL) {
	    if(intrins_arg_cmp(defn,t)) {
				/* Propagate data type thru the list.
				   Resulting type info is stored in
				   args token.  */
	      if(prev_t != t && ! (flags & I_MIXED_ARGS) ) {
				/* Set up a pretend expr term for binexpr */
		fake_op.tclass = ',';
		fake_op.line_num = prev_t->line_num;
		fake_op.col_num = prev_t->col_num;
		fake_op.src_text = ",";

		binexpr_type(prev_t,&fake_op,t,arg);
	      }
	      prev_t = t;
	    }
	    t = t->next_token;
	  }/* end while */

	}/* end arg != NULL */
}/* check_intrins_args */


PRIVATE
void
#if HAVE_STDC
check_stmt_function_args(const Lsymtab *symt, Token *id, Token *arg)
#else /* K&R style */
check_stmt_function_args(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
#endif /* HAVE_STDC */
{
	unsigned n1,n2,n;
	unsigned i;
	Token *t1,*t2;

	t1 = symt->info.toklist->tokenlist;
	t2 = ((arg==NULL)? NULL: arg->next_token);

	n1 = arg_count(t1);
	n2 = arg_count(t2);

	if(n1 != n2) {
	    syntax_error(id->line_num,id->col_num,
		"function invoked with incorrect number of arguments");
	}

	n = (n1 < n2? n1: n2);
	for(i=0; i<n; i++) {
#ifdef OLDSTUFF
	    if( t1->TOK_type != t2->TOK_type) {
		syntax_error(t2->line_num,t2->col_num,
		  "function argument is of incorrect datatype");
	    }
#else
	    stmt_fun_arg_cmp(symt,t1,t2);
#endif
	    t1 = t1->next_token;
	    t2 = t2->next_token;
	}
}


void
#if HAVE_STDC
declare_type(Token *id, int datatype, long int size, char *size_text)
#else /* K&R style */
declare_type(id,datatype,size,size_text)
	Token *id;
	int datatype;
	long size;
	char *size_text;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

	if( (symt) == NULL) {
	   symt = install_local(h,datatype,class_VAR);
	   symt->size = size;
	   symt->size_is_adjustable = id->size_is_adjustable;
	   symt->size_is_expression = id->size_is_expression;
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {           /* Symbol has been seen before: check it */

			/* Intrinsic: see if type is consistent */
	  if( symt->intrinsic ) {
	    IntrinsInfo *defn = symt->info.intrins_info;
	    int rettype = defn->result_type,
		argtype = defn->arg_type;
			/* N.B. this test catches many but not all errors */
	    if( (rettype != type_GENERIC && datatype != rettype)
	     || (rettype == type_GENERIC && !((1<<datatype) & argtype)) ){
	      if(misc_warn) {
		    warning(id->line_num,id->col_num,
				"Declared type ");
		    msg_tail(type_name[datatype]);
		    msg_tail(" is invalid for intrinsic function: ");
		    msg_tail(symt->name);
	      }
	    }
	  }

	  if(datatype_of(symt->type) != type_UNDECL) {
	      syntax_error(id->line_num,id->col_num,
		"symbol redeclared: ");
	  	msg_tail(symt->name);
	  }
	  else {
			/* Now give it the declared type */
	      symt->type = type_byte(storage_class_of(symt->type),datatype);
	      symt->size = size;
	      symt->size_is_adjustable = id->size_is_adjustable;
	      symt->size_is_expression = id->size_is_expression;
				/* Type declaration overrides implicit
				   declarations as line where declared.
				 */
	      symt->line_declared = id->line_num;
	      symt->file_declared = inctable_index;
	  }

			/* Issue error if already defined as a parameter */
	  if( symt->parameter ) {
	    syntax_error(id->line_num,id->col_num,
			 "type declaration must precede PARAMETER definition");
	  }
	}

		/* If character type, save the source text of the size
		   specification.  If it is an array already
		   dimensioned, add size_text to tail of src.textvec,
		   otherwise place size_text in src.text if it is
		   character type, except for parameter, which
		   shouldn't happen.
		 */

	if( datatype_of(symt->type) == type_STRING ) {
	  if(symt->array_var) {
	    int i, dims = array_dims(symt->info.array_dim);
	    char **tvec = new_textvec(dims+1);

	    for(i=0; i<dims; i++)	/* Copy the old list over */
	      tvec[i] = symt->src.textvec[i];

	    tvec[dims] = size_text; /* Copy size text to new last element */

	    free_textvec(symt->src.textvec); /* Free the old list */

	    symt->src.textvec = tvec; /* Replace old list with new */
	  }
	  else if( ! symt->parameter ) {
	    symt->src.text = size_text;
	  }
	}

#ifdef DEBUG_EXPRTREES
	      if(debug_latest) {
		fprintf(list_fd,"\n      %s",type_table[datatype]);
		size_text = get_size_text(symt,0);
		if(size_text != NULL) {
		  fprintf(list_fd," * %s",size_text);
		}
		else {
		  if(symt->size != size_DEFAULT)
		  fprintf(list_fd," * %d",symt->size);
		}
		fprintf(list_fd," %s",symt->name);
	      }
#endif

			/* Under -port=long-string warn if char size > 255 */
	if(port_long_string) {
	  if(datatype == type_STRING && size > 255)
	    nonportable(id->line_num,id->col_num,
			"character variable length exceeds 255");
	}
}/*declare_type*/


void
#if HAVE_STDC
def_arg_name(Token *id)		/* Process items in argument list */
#else /* K&R style */
def_arg_name(id)		/* Process items in argument list */
#endif /* HAVE_STDC */

#if HAVE_STDC
#else /* K&R style */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {           /* Symbol has been seen before: check it */

	}
	symt->argument = TRUE;
}/*def_arg_name*/


void
#if HAVE_STDC
def_array_dim(Token *id, Token *arg)	/* Process dimension lists */
	               	     /* arg previously defined as int */
#else /* K&R style */
def_array_dim(id,arg)	/* Process dimension lists */
	Token *id,*arg;	     /* arg previously defined as int */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;


	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {           /* Symbol has been seen before: check it */
	   if(storage_class_of(symt->type) != class_VAR ||
	       symt->parameter || symt->entry_point) {
	      syntax_error(id->line_num,id->col_num,
		"Entity cannot be dimensioned: ");
		msg_tail(symt->name);
	      return;
	   }
	}

	symt->array_var = TRUE;
	if(!equivalence_flag){      /* some checking should be done here */
	   if(symt->info.array_dim != 0)
	      syntax_error(id->line_num,id->col_num,
		"Array redimensioned");
	   else
	      symt->info.array_dim = array_dim_info(arg->TOK_dims,
						    arg->TOK_elts);

	}

		/* Save text of dimension exprs in a list of strings
		   in symtab entry.  If array is of type character,
		   the text of size expression is already in src.text,
		   and is saved at tail of the list of dim strings. */

	{
	  int i, dims=arg->TOK_dims,
	      is_char = (datatype_of(symt->type) == type_STRING);
	  char **tvec;
	  char *size_text=symt->src.text;
	  Token *t;
	  int auto_array=FALSE;	/* records whether automatic array */
				/* Restore dim list to correct order */
	  arg->next_token = reverse_tokenlist(arg->next_token);

	  symt->src.textvec = tvec = new_textvec(is_char?dims+1:dims);

				/* Store dimension expr text in list */
	  for(i=0, t=arg->next_token; i<dims; i++, t=t->next_token) {
	    tvec[i] = ( t->left_token == NULL ?
		       new_tree_text(t):
		       new_tree_text(t->left_token) );

				/* Do some -f77 checking while we're here */
	    if( !symt->argument && !is_true(PARAMETER_EXPR,t->TOK_flags) ) {
				/* Section 5.1.2.1 */
		auto_array = TRUE;
		if(f77_automatic_array ) {
		    nonstandard(t->line_num,t->col_num,0,0);
		    msg_tail(": local array cannot have variable size");
		}
	    }
	  }
				/* Novices sometimes put FUNCTION decl late.
				   Only likely in a type declaration stmt.
				 */
	  if(auto_array && novice_help && curr_stmt_class != tok_DIMENSION
	     && curr_stmt_class != tok_COMMON
	     && strncmp(symt->name,"FUNCTION",8) == 0) {
	      warning(id->line_num,id->col_num,
		"Possible intended function declaration is not first line of module");
	  }
				/* If character type, store size expr
				   text in tail of list. */
	  if(is_char)
	    tvec[dims] = size_text;

#ifdef DEBUG_EXPRTREES
	  if(debug_latest) {
	    int type=datatype_of(symt->type);
	    fprintf(list_fd,"\n      %s",
		    (type == type_UNDECL)?"DIMENSION":type_table[type]);
	    if(is_char)
	      fprintf(list_fd," * %s",symt->src.textvec[dims]);

	    fprintf(list_fd," %s ( ",symt->name);
	    for(i=0; i<dims; i++) {
	      fprintf(list_fd,"%s",symt->src.textvec[i]);
	      if(i < dims-1)
		fprintf(list_fd," , ");
	    }
	    fprintf(list_fd," )");
	  }
#endif

	}

}/*def_array_dim*/


void
#if HAVE_STDC
def_com_block(Token *id, Token *comlist)	/* Process common blocks and save_stmt */
#else /* K&R style */
def_com_block(id,comlist)	/* Process common blocks and save_stmt */
	Token *id, *comlist;
#endif /* HAVE_STDC */

{
	int h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
   	TokenListHeader *TH_ptr;
	extern LINENO_t true_prev_stmt_line_num;/* set by fortran.y */

		/* Install name in global symbol table */
	if( (gsymt=hashtab[h].com_glob_symtab) == NULL) {
	   gsymt = install_global(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   gsymt->info.comlist = NULL;
	}


	if( (symt = hashtab[h].com_loc_symtab) == NULL){
	   symt = install_local(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
	}

				/* Record 1st location of declaration */
	if( symt->info.toklist == NULL ) {
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}

	if(pretty_multiple_common) {

		/* Flag declarations of same block in separate statements
		   unless separated only by comments. Use front token
		   of previous tokenlist which is last token of decl. */
	  if( symt->info.toklist != NULL
	   && symt->info.toklist->tokenlist->line_num < true_prev_stmt_line_num) {
	    ugly_code(id->line_num,id->col_num,
		"Common block declared in more than one statement");
	  }
	}

		/* Insert the new list onto linked list of token lists */
	if(comlist != NULL) {
	  	/* Will be NULL only for SAVE, in which case skip */
	    TH_ptr= make_TL_head(id);

 	    TH_ptr->tokenlist = comlist->next_token;
	    TH_ptr->next = symt->info.toklist;
            symt->info.toklist = TH_ptr;
	    {
			/* For each variable in the list, record a pointer
			   to this common block and variable's index in
			   the block. Note that token list is still in
			   reverse order at this time, so we count backwards.
			*/
	      Token *c = comlist->next_token;
	      int indx;
				/* Add to the block's total count, and
				   start indexing there. */
	      indx = (symt->common_index += arg_count(c));
	      while(c != NULL) {
		Lsymtab *com_var = hashtab[c->value.integer].loc_symtab;
		com_var->common_block = gsymt;
		com_var->common_index = indx--;
		c = c->next_token;
	      }
	    }
	}

	if(! symt->used_flag) { /* record first line where used */
	    symt->line_used = id->line_num;
	    symt->file_used = inctable_index;
	}

	if(! symt->set_flag) { /* record first line where set */
	    symt->line_set = id->line_num;
	    symt->file_set = inctable_index;
	}

   	symt->set_flag = TRUE;
	symt->used_flag = TRUE;
}/*def_com_block*/


void
#if HAVE_STDC
def_com_variable(Token *id)		/* Process items in common block list */
#else /* K&R style */
def_com_variable(id)		/* Process items in common block list */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {           /* Symbol has been seen before: check it */
	    if(symt->common_var	/* Already in common? */
				/* But if it is equivalenced, suppress
				   the warning.  Equivs in common are not
				   handled in present version. */
	       && symt->equiv_link == symt ) {
		syntax_error(id->line_num,id->col_num,
		     "Variable cannot be in common twice");
	    }
	    else if(symt->entry_point || symt->parameter ||
		    symt->argument || symt->external || symt->intrinsic) {
		syntax_error(id->line_num,id->col_num,
		     "Item cannot be placed in common");
		return;
	    }
	    if(symt->size == size_ADJUSTABLE) {	/* CHARACTER *(*) */
	      syntax_error(id->line_num,id->col_num,
		    "Common variable cannot have adjustable size");
	      symt->size = 1;
	    }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->common_var = TRUE; /* set the flag even if not legit */
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*def_com_variable*/


	/* This guy sets the flag in symbol table saying the id is the
	   current module.  It returns the hash code for later reference.
	 */
int
#if HAVE_STDC
def_curr_module(Token *id)
#else /* K&R style */
def_curr_module(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int hashno = id->value.integer;
	hashtab[hashno].loc_symtab->is_current_module = TRUE;

	return hashno;
}/*def_curr_module*/


void
def_do_variable(Token *id)	/* Treat DO index variable in DO stmt */
{
	int h=id->value.integer;
	Lsymtab *symt;
	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}
	else {
	   if(symt->active_do_var) {
	      syntax_error(id->line_num,id->col_num,
		   "DO variable is already in use in an enclosing DO loop");
	   }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	if(! equiv->used_flag) { /* record first line where used */
	    equiv->line_used = id->line_num;
	    equiv->file_used = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv->assigned_flag = TRUE;
	equiv->used_flag = TRUE;
	equiv->active_do_var = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}


void
#if HAVE_STDC
def_equiv_name(Token *id)		/* Process equivalence list elements */
#else /* K&R style */
def_equiv_name(id)		/* Process equivalence list elements */
	Token *id;
#endif /* HAVE_STDC */
{
  ref_variable(id);		/* Put it in symtab */
	/* No other action needed: processing of equiv pairs is
	   done by equivalence() */
}/*def_equiv_name*/



void
#if HAVE_STDC
def_ext_name(Token *id)		/* Process external lists */
#else /* K&R style */
def_ext_name(id)		/* Process external lists */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
	   symt->info.toklist = NULL;
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
        }
	else if(symt->entry_point){ /* protect ourself from nonsense */
	    syntax_error(id->line_num,id->col_num,
		"Subprogram cannot declare itself external:");
	    msg_tail(symt->name);
	    return;
	}
	else if(symt->array_var || symt->parameter){ /* worse nonsense */
	    syntax_error(id->line_num,id->col_num,
		"Identifier was previously declared non-external:");
	    msg_tail(symt->name);
	    return;
	}
	else {
			/* Symbol seen before: check it & change class */

	    if(storage_class_of(symt->type) == class_VAR) {
	      symt->info.toklist = NULL;
	    }
	    symt->type = type_byte(class_SUBPROGRAM,datatype_of(symt->type));
	}

	if(symt->intrinsic){
	    syntax_error(id->line_num,id->col_num,
		"Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	    symt->external = TRUE;
	    if(!symt->argument){
	        TokenListHeader *TH_ptr;
		Gsymtab *gsymt;
		if( (gsymt=hashtab[h].glob_symtab) == NULL) {
	   	    gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
	   	    gsymt->info.arglist = NULL;
		}
		TH_ptr=make_TL_head(id);

		TH_ptr->external_decl = TRUE;
		TH_ptr->next = symt->info.toklist;
		symt->info.toklist = TH_ptr;
	    }
	}
	symt->declared_external = TRUE;

}/*def_ext_name*/



void
#if HAVE_STDC
def_function(int datatype, long int size, char *size_text, Token *id, Token *args)
#else /* K&R style */
def_function(datatype,size,size_text,id,args)
#endif /* HAVE_STDC */
				/* Installs function or subroutine name */
#if HAVE_STDC
	                                  /* in global table */
#else /* K&R style */
	int datatype;                     /* in global table */
	long size;
	char *size_text;
	Token *id,*args;
#endif /* HAVE_STDC */
{
	int storage_class;
	int h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	TokenListHeader *TH_ptr;
   	storage_class = class_SUBPROGRAM;

   	if((symt = (hashtab[h].loc_symtab)) == NULL) {
			/* Symbol is new to local symtab: install it.
			   Since this is the current routine, it has
			   storage class of a variable. */
	   symt = install_local(h,datatype,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	   symt->size = size;
	   symt->src.text = size_text;
	}

	if(! symt->entry_point)	/* seen before but not as entry */
	   symt->info.toklist = NULL;

	if(symt->external) {	/* warn if entry point was declared external */
	    syntax_error(id->line_num,id->col_num,
		"Entry point was declared external:");
	    msg_tail(symt->name);
				/* try to undo the damage */
	    symt->type = type_byte(class_VAR,datatype_of(symt->type));
	    symt->external = FALSE;
	}

	if((gsymt = (hashtab[h].glob_symtab)) == NULL) {
			/* Symbol is new to global symtab: install it */
	  gsymt = install_global(h,datatype,storage_class);
	  gsymt->size = size;
	  gsymt->info.arglist = NULL;
	}
	else {
			/* Symbol is already in global symtab. Put the
			   declared datatype into symbol table. */
	  gsymt->type = type_byte(storage_class,datatype);
	  gsymt->size = size;
	}

				/* Restore args list to original order */
	if(args != NULL)
	  args->next_token = reverse_tokenlist(args->next_token);

		/* Insert the new list onto linked list of token lists */
   	TH_ptr=make_TL_head(id);

			/* If this is an implied PROGRAM statement it may
			   occur in an include file, which we do not want
			   to appear in diagnostic messages about it. */
	if(top_filename != current_filename && datatype == type_PROGRAM) {
	  TH_ptr->filename = top_filename;
	  TH_ptr->line_num = top_file_line_num;
	}

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;

	symt->entry_point = TRUE;

		/* library mode: set the flag so no complaint will
		   be issued if function never invoked. */
	if(library_mode)
		symt->library_module = TRUE;
	if(datatype == type_PROGRAM) {
#ifdef VCG_SUPPORT		/* Get name of file containing main module */
		main_filename = top_filename;
#endif
	}

}/*def_function*/



void
#if HAVE_STDC
def_intrins_name(Token *id)		/* Process intrinsic lists */
#else /* K&R style */
def_intrins_name(id)		/* Process intrinsic lists */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	   symt->info.toklist = NULL;
        }
	else {
			/* Symbol seen before: check it & change class */
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }

	  symt->type = type_byte(class_SUBPROGRAM,datatype_of(symt->type));
	}

		/* Place info about intrinsic datatype in local symtab.
		   If not found, it will be treated as external.
		 */

	if(symt->external){
	    syntax_error(id->line_num,id->col_num,
	       "Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	  IntrinsInfo *defn;
	  symt->declared_intrinsic = TRUE;
	  if( (defn=find_intrinsic(symt->name)) == NULL ) {
	     if(misc_warn) {
	       warning(id->line_num,id->col_num,
			"Unknown intrinsic function: ");
	       msg_tail(symt->name);
	       msg_tail("Treated as if user-defined");
	     }
				/* Here treat as if EXTERNAL declaration */
	     def_ext_name(id);
	     return;
	   }
	   else {
			/* Found in info table: set intrins flag and store
			   pointer to definition info. */
	     symt->intrinsic = TRUE;
	     symt->info.intrins_info = defn;
	   }
	}
	symt->declared_external = TRUE;
}/*def_intrins_name*/

void
#if HAVE_STDC
def_namelist(Token *id, Token *list)		/* Process NAMELIST declaration */
#else /* K&R style */
def_namelist(id,list)		/* Process NAMELIST declaration */
     Token *id,*list;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;
	extern LINENO_t true_prev_stmt_line_num;/* set by fortran.y */

	if( (symt=hashtab[h].loc_symtab) == NULL) {
				/* First encounter: install in local symtab */
	  symt = install_local(h,type_NAMELIST,class_NAMELIST);
	  symt->line_declared = id->line_num;
	  symt->file_declared = inctable_index;
	  symt->info.toklist = NULL;
	}
			/* protect ourself against nonsense */
	else if( symt->array_var || symt->parameter || symt->entry_point ) {
	  syntax_error(id->line_num,id->col_num,
		       "identifier was previously declared a non-namelist");
	  return;
	}
	else if(pretty_multiple_namelist) {

		/* Flag declarations of same namelist in separate statements
		   unless separated only by comments. Use front token
		   of previous tokenlist which is last token of decl. */
	  if((symt->info.toklist != NULL) && 
	    (symt->info.toklist->tokenlist->line_num < true_prev_stmt_line_num)) {
	    ugly_code(id->line_num,id->col_num,
		"Namelist declared in more than one statement");
	  }
	}

	call_external(symt,id,list); /* attach list to symt->info.toklist */

}/*def_namelist*/


void
#if HAVE_STDC
def_namelist_item(Token *id)		/* Process NAMELIST list elements */
#else /* K&R style */
def_namelist_item(id)		/* Process NAMELIST list elements */
	Token *id;
#endif /* HAVE_STDC */
{
  ref_variable(id);		/* Put it in symtab */
}/*def_namelist_name*/


void
#if HAVE_STDC
def_parameter(Token *id, Token *val, int noparen)/* Process parameter_defn_item */
#else /* K&R style */
def_parameter(id,val,noparen)	/* Process parameter_defn_item */
	Token *id,*val;
	int noparen;		/* parenthesis-less form */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {			/* protect ourself against nonsense */
	   if( symt->array_var || symt->external || symt->intrinsic 
	       || symt->entry_point ) {
	      syntax_error(id->line_num,id->col_num,
		   "identifier cannot be a parameter:");
	      msg_tail(symt->name);
	      return;
	   }
	}

	if(! symt->set_flag) { /* record first line where set */
	    symt->line_set = id->line_num;
	    symt->file_set = inctable_index;
	}

	symt->set_flag = TRUE;
	symt->parameter = TRUE;
	symt->info.param = new_param_info();
	symt->info.param->seq_num = ++parameter_count;

		/* If parameter type is not declared, then if it is DEC
		   parenthesis-less form (and -source=dec-param not given)
		   or if standard form and -source=parameter-implicit option
		   is given, get type from value.  Warn about it under -f77,
		   or under -port if the data type is not same as F77 default.
		*/
	if( ((noparen && !source_dec_param_std_type)
	   ||(!noparen && source_param_implicit)) &&
	    (datatype_of(symt->type) == type_UNDECL) ) {
	  int val_type = datatype_of(val->TOK_type);
	  if( f77_param_implicit_type || f90_param_implicit_type ) {
	    nonstandard(id->line_num,id->col_num,f90_param_implicit_type,0);
	    msg_tail(": PARAMETER implicitly typed");
	    if( get_type(symt) != val_type )
	      msg_tail("differently from default type");
	  }
	  else if( port_param_implicit_type && 
		   get_type(symt) != val_type ) {
	    nonportable(id->line_num,id->col_num,
	      ": PARAMETER implicitly typed differently from default type");
	  }
	  symt->type = type_byte(class_VAR,val_type);
	  symt->size = val->size;
	}

		/* Integer parameters: save value in symtab entry.  Other
		   types not saved.  Need these since used in array dims */
	switch(get_type(symt)) {
		case type_INTEGER:
			symt->info.param->value.integer = int_expr_value(val);
#ifdef DEBUG_PARAMETERS
if(debug_latest)
(void)fprintf(list_fd,"\nPARAMETER %s = %d",
	      symt->name,symt->info.param->value.integer);
#endif
			break;
			/* Character parameter: if declared adjustable
			   i.e. *(*) then inherit size of const */
		case type_STRING:
			if(symt->size == size_ADJUSTABLE
			   && datatype_of(val->TOK_type) == type_STRING)
			  symt->size = val->size;
			symt->info.param->value.string = char_expr_value(val);
			break;
		case type_REAL:
		case type_DP:
		case type_COMPLEX:
			symt->info.param->value.dbl = float_expr_value(val);
		default:
			break;
	}

			/* Save the source text of value for declaration */

	symt->info.param->src_text = new_tree_text(
		(val->left_token == NULL?
			val:			/* Primary */
			val->left_token)	/* Expr tree */
	        );

#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\n      PARAMETER ( %s = %s ) ",
		  symt->name,
		  symt->info.param->src_text);
	}
#endif

}/*def_parameter*/



void    	       /* Installs statement function name in local table */
#if HAVE_STDC
def_stmt_function(Token *id, Token *args)
#else /* K&R style */
def_stmt_function(id, args)
	Token *id, *args;
#endif /* HAVE_STDC */
{
	int t,h=id->value.integer;
	Lsymtab *symt;
   	TokenListHeader *TH_ptr;

   	if((symt = (hashtab[h].loc_symtab)) == NULL) {
			/* Symbol is new to local symtab: install it. */

	   symt = install_local(h,type_UNDECL,class_STMT_FUNCTION);
	   symt->info.toklist = NULL;
	}
	else {
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }
	}
	symt->line_declared = id->line_num;
	symt->file_declared = inctable_index;

		/* Restore args to original order for sake of checking phase */
	if(args != NULL)
	  args->next_token = reverse_tokenlist(args->next_token);

		/* Save dummy arg list in symbol table */
    	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;

	t=datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

		/* check, check, check ... */
	if(storage_class_of(symt->type) == class_VAR)
	   symt->type = type_byte(class_STMT_FUNCTION,t);

	symt->external = TRUE;
}/*def_stmt_function*/




void
#if HAVE_STDC
do_ASSIGN(Token *id)		/* Process ASSIGN statement */
#else /* K&R style */
do_ASSIGN(id)		/* Process ASSIGN statement */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	   if(symt->active_do_var) {
	      syntax_error(id->line_num,id->col_num,
			   "Cannot assign label to active DO index");
	   }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}/*do_ASSIGN*/




void
#if HAVE_STDC
do_assigned_GOTO(Token *id)		/* Process assigned_goto */
#else /* K&R style */
do_assigned_GOTO(id)		/* Process assigned_goto */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->used_flag) { /* record first line where used */
	    equiv->line_used = id->line_num;
	    equiv->file_used = inctable_index;
	}
	if(! equiv->set_flag)
	   equiv->used_before_set = TRUE;
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*do_assigned_GOTO*/





void
#if HAVE_STDC
do_ENTRY(Token *id, Token *args, int hashno)	/* Processes ENTRY statement */
#else /* K&R style */
do_ENTRY(id,args,hashno)	/* Processes ENTRY statement */
	Token *id,*args;
	int hashno;
#endif /* HAVE_STDC */
{
	int datatype;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
	    case type_COMMON_BLOCK:
	        syntax_error(id->line_num,NO_COL_NUM,
			"You cannot have an entry statement here");
		break;
	    case type_SUBROUTINE:	/* Subroutine entry */
		def_function(type_SUBROUTINE,size_DEFAULT,(char *)NULL,
			     id,args);
		break;
	    default:		/* Function entry */
		def_function(type_UNDECL,size_DEFAULT,(char *)NULL,
			     id,args);
		break;
	}
}/*do_ENTRY*/




	/* This routine checks whether a RETURN statement is valid at
	   the present location, and if it is, looks for possible
	   failure to assign return value of function.  Returns 1
	   if RETURN is valid here, 0 if not.
	*/
int
#if HAVE_STDC
do_RETURN(int hashno, Token *keyword)
	           	/* current module hash number */
	               	/* tok_RETURN, or tok_END if implied RETURN */
#else /* K&R style */
do_RETURN(hashno,keyword)
	int hashno;	/* current module hash number */
	Token *keyword;	/* tok_RETURN, or tok_END if implied RETURN */
#endif /* HAVE_STDC */
{
	int i,datatype, valid=1;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
		if(keyword->tclass == tok_RETURN) {
		    syntax_error(keyword->line_num,keyword->col_num,
		    	"You cannot have a RETURN statement here!");
		    valid = 0;
		}
		break;
	    case type_SUBROUTINE:	/* Subroutine return: OK */
		break;
	    default:		/* Function return: check whether entry
				   points have been assigned values. */
		for(i=0; i<loc_symtab_top; i++) {
		    if(storage_class_of(loc_symtab[i].type) == class_VAR
			&& loc_symtab[i].entry_point
			&& ! loc_symtab[i].set_flag ) {
		      if(misc_warn) {
			    warning(keyword->line_num,keyword->col_num,
					loc_symtab[i].name);
			    msg_tail("not set when RETURN encountered");
		      }
		    }
		}
		break;
	}
	return valid;
}/*do_RETURN*/

void
#if HAVE_STDC
equivalence(Token *id1, Token *id2)
#else /* K&R style */
equivalence(id1,id2)
     Token *id1, *id2;
#endif /* HAVE_STDC */
{
	int h1=id1->value.integer, h2=id2->value.integer;
	Lsymtab *symt1,*symt2,*temp;

		/* install the variables in symtab if not seen before */
	if( (symt1=hashtab[h1].loc_symtab) == NULL) {
	   symt1 = install_local(h1,type_UNDECL,class_VAR);
	   symt1->line_declared = id1->line_num;
	   symt1->file_declared = inctable_index;
	}
	if( (symt2=hashtab[h2].loc_symtab) == NULL) {
	   symt2 = install_local(h2,type_UNDECL,class_VAR);
	   symt2->line_declared = id2->line_num;
	   symt2->file_declared = inctable_index;
	}
			/* Check for legality.  Ought to do complementary
			   checks elsewhere.
			 */
	if(symt1 == symt2
	   || symt1->parameter || symt2->parameter
	   || symt1->entry_point || symt2->entry_point
	   || symt1->argument || symt2->argument
	   || symt1->external || symt2->external) {

		syntax_error(id1->line_num,id1->col_num,
			     "illegal to equivalence these");
	}
		/* now swap equiv_links so their equiv lists are united */
	else {
	    temp = symt1->equiv_link;
	    symt1->equiv_link = symt2->equiv_link;
	    symt2->equiv_link = temp;
	}

		/* If either guy is in common, both are in common */
	if(symt1->common_var || symt2->common_var) {
	    Lsymtab *equiv=symt1;
	    do {
		equiv->common_var = TRUE;
		equiv = equiv->equiv_link;
	    } while(equiv != symt1);
	}
}

int
#if HAVE_STDC
get_size(const Lsymtab *symt, int type)			/* ARGSUSED1 */
#else /* K&R style */
get_size(symt,type)			/* ARGSUSED1 */
#endif /* HAVE_STDC */
		    /* Returns size of symbol if explicitly declared
		       or declared using IMPLICIT type*size statement.
		       Otherwise returns size_DEFAULT. */
#if HAVE_STDC
              			/* Evaluated datatype: not used at present */
#else /* K&R style */
     Lsymtab *symt;
     int type;			/* Evaluated datatype: not used at present */
#endif /* HAVE_STDC */
{
  int datasize=symt->size;
  int datatype = datatype_of(symt->type);
  if(datatype != type_UNDECL) /* Declared? */
    return datasize;		/* if declared, use it */
  else {
    int first_char=(int)symt->name[0];

    if(first_char == '$')  first_char = 'Z'+1;
    if(first_char == '_')  first_char = 'Z'+2;

    return implicit_size[first_char - 'A'];
  }
}

char *
#if HAVE_STDC
get_size_text(const Lsymtab *symt, int type)		/* ARGSUSED1 */
              			/* Evaluated datatype: not used at present */
#else /* K&R style */
get_size_text(symt,type)		/* ARGSUSED1 */
     Lsymtab *symt;
     int type;			/* Evaluated datatype: not used at present */
#endif /* HAVE_STDC */
{
  int datatype = datatype_of(symt->type);
  if(datatype != type_UNDECL) {
				/* Declared: use text in symtab entry */
    if(symt->array_var)
      return symt->src.textvec[array_dims(symt->info.array_dim)];
    else
      return symt->src.text;
  }
  else {
				/* Undeclared: use implicit value */
    int first_char=(int)symt->name[0];

    if(first_char == '$')  first_char = 'Z'+1;
    if(first_char == '_')  first_char = 'Z'+2;

    return implicit_len_text[first_char - 'A'];
  }
}

int
#if HAVE_STDC
get_type(const Lsymtab *symt)	/* Returns data type of symbol, using implicit if necessary */
#else /* K&R style */
get_type(symt)	/* Returns data type of symbol, using implicit if necessary */
	Lsymtab *symt;
#endif /* HAVE_STDC */
{
	int datatype = datatype_of(symt->type);

	if(datatype != type_UNDECL)	/* Declared? */
	   return datatype;		/*   Yes: use it */
	else if(storage_class_of(symt->type) == class_SUBPROGRAM
	     && !symt->invoked_as_func )
				/* Function never invoked: assume subr */
	   return type_SUBROUTINE;
	else if (symt->invoked_as_func && symt->intrinsic)
	{
	    IntrinsInfo *defn;

	    defn = find_intrinsic(symt->name);
	    if (defn != (IntrinsInfo *)NULL)
		return defn->result_type;
	}

	/* Fell through, so type must be determined by first letter of name */

	{
	  int first_char=(int)symt->name[0];
			/* kluge: treat any nonalpha chars other than _
			   as if they are $.
			 */
	  if( !isalpha(first_char) && first_char != '_' )
	      first_char = 'Z'+1;
	  if(first_char == '_')  first_char = 'Z'+2;

	   return implicit_type[first_char - 'A'];
	}
}/*get_type*/


	/* hash_lookup finds identifier in hashtable and returns its
	   index.  If not found, a new hashtable entry is made for it,
	   and the identifier string s is copied to local stringspace.
	*/
unsigned
#if HAVE_STDC
hash_lookup(char *s)
#else /* K&R style */
hash_lookup(s)
	char *s;
#endif /* HAVE_STDC */
{
        unsigned h;
	unsigned long hnum;

	hnum = hash(s);

	while(h = hnum%HASHSZ, hashtab[h].name != NULL
	          && strcmp(hashtab[h].name,s) != 0) {
			  hnum = rehash(hnum);	/* Resolve clashes */
	}

	if(hashtab[h].name == NULL) {
		    hashtab[h].name = new_local_string(s);
		    hashtab[h].loc_symtab = NULL;
		    hashtab[h].glob_symtab = NULL;
		    hashtab[h].com_loc_symtab = NULL;
		    hashtab[h].com_glob_symtab = NULL;
        }
	return h;
}/*hash_lookup*/

void
init_tables(VOID)			/* Allocates table space */
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
	if( ((loc_symtab=(Lsymtab*)calloc(LOCSYMTABSZ,sizeof(Lsymtab)))
		== (Lsymtab*)NULL) ||
	    ((glob_symtab=(Gsymtab*)calloc(GLOBSYMTABSZ,sizeof(Gsymtab)))
		== (Gsymtab*)NULL) ||
	    ((hashtab=(HashTable*)calloc(HASHSZ,sizeof(HashTable)))
		== (HashTable*)NULL)
	  ) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for tables");
	}
#endif
}




Gsymtab*
#if HAVE_STDC
install_global(int h, int datatype, int storage_class)	/* Install a global symbol */
	      			/* hash index */
#else /* K&R style */
install_global(h,datatype,storage_class)	/* Install a global symbol */
	int h;			/* hash index */
	int datatype,storage_class;
#endif /* HAVE_STDC */
{
	Gsymtab *gsymt = &glob_symtab[glob_symtab_top];

	if(glob_symtab_top == GLOBSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in global symbol table\n\
Recompile me with larger GLOBSYMTABSZ value\n"
#else
"out of space in global symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
			/* Store symtab pointer in hash table */
	    if(storage_class == class_COMMON_BLOCK)
		hashtab[h].com_glob_symtab = gsymt;
	    else
		hashtab[h].glob_symtab = gsymt;

	    clear_symtab_entry(gsymt);

	 		/* Duplicate copy of string into global stringspace */
	    gsymt->name = new_global_string(hashtab[h].name);

			/* Set symtab info fields */
	    gsymt->type = type_byte(storage_class,datatype);
	    gsymt->size = type_size[datatype];
	    if(storage_class == class_COMMON_BLOCK)
		gsymt->info.comlist = NULL;
	    else
		gsymt->info.arglist = NULL;

	    gsymt->link.child_list = NULL;

	    ++glob_symtab_top;
	}
	return (gsymt);
}/*install_global*/


PRIVATE Lsymtab*
#if HAVE_STDC
install_local(int h, int datatype, int storage_class)	/* Install a local symbol */
	      			/* hash index */
#else /* K&R style */
install_local(h,datatype,storage_class)	/* Install a local symbol */
	int h;			/* hash index */
	int datatype,storage_class;
#endif /* HAVE_STDC */
{
	Lsymtab *symt = &loc_symtab[loc_symtab_top];
	if(loc_symtab_top == LOCSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in local symbol table\n\
Recompile me with larger LOCSYMTABSZ value\n"
#else
"out of space in local symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
	    if(storage_class == class_COMMON_BLOCK)
		hashtab[h].com_loc_symtab = symt;
	    else
		hashtab[h].loc_symtab = symt;

	    clear_symtab_entry(symt);
	    symt->name = hashtab[h].name;
	    symt->info.array_dim = 0;

		      /* Set symtab info fields */
	    symt->type = type_byte(storage_class,datatype);
	    symt->size = type_size[datatype];
	    symt->src.text = NULL;
	    symt->equiv_link = symt;	/* equivalenced only to self */
	    symt->common_block = (Gsymtab*)NULL;
	    symt->common_index = 0;
	    if(incdepth > 0)
	      symt->defined_in_include = TRUE;
	    symt->line_declared = symt->line_set = symt->line_used = NO_LINE_NUM;
				/* initialize indices in incfile table */
	    symt->file_declared = symt->file_set = symt->file_used = -1;
	    ++loc_symtab_top;
	}
	return symt;
}/*install_local*/


		/* Get value specified by an integer-expression token.
		   This will be either an identifier, which should be a
		   parameter whose value is in the symbol table, or else
		   an expression token as propagated by exprtype.c
		   routines, with value stored in the token.
		*/
int
#if HAVE_STDC
int_expr_value(Token *t)
#else /* K&R style */
int_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(!is_true(EVALUATED_EXPR,t->TOK_flags)) {/* something bogus */
				/* warn if error message not already given */
    if(is_true(PARAMETER_EXPR,t->TOK_flags))
      if(misc_warn)
	warning(t->line_num,t->col_num,
	      "Constant not evaluated: value of 0 assumed");
  }
  else {
	if( is_true(ID_EXPR,t->TOK_flags) ) {
		/* Identifier: better be a parameter */
	    int h=t->value.integer;
	    Lsymtab *symt = hashtab[h].loc_symtab;
	    if(symt == NULL || !(symt->parameter) ) {
		syntax_error(t->line_num,t->col_num,
			"symbolic constant required");
	    }
	    else {
		return symt->info.param->value.integer;
	    }
	}
		/* Otherwise, it is a const or expr, use token.value.integer */
	else {
	    return t->value.integer;
	}
  }
				/* Unsuccessful: return value of 0 */
  return 0;
}/*int_expr_value*/

DBLVAL
#if HAVE_STDC
float_expr_value(Token *t)
#else /* K&R style */
float_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(is_true(LIT_CONST,t->TOK_flags))
    return t->value.dbl;
  else
    return (DBLVAL)0;		/* float values are not propagated */
}

char *
#if HAVE_STDC
char_expr_value(Token *t)
#else /* K&R style */
char_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(is_true(LIT_CONST,t->TOK_flags))
    return t->value.string;
  else
    return NULL;		/* char values are not propagated */
}



	/* note_filename():  This routine is called by main prog to give
	   symbol table routines access to current input file name, to be
	   stored in function arg list headers and common list headers, for
	   the use in diagnostic messages. Since filenames are from argv,
	   they are permanent, so pointer is copied, not the string.
	*/
void
#if HAVE_STDC
note_filename(char *s)
#else /* K&R style */
note_filename(s)
	char *s;
#endif /* HAVE_STDC */
{
	current_filename = s;
	top_filename = s;
}/* note_filename */

		/* Routine to output expression tree via msg_tail.  For use
		   in error/warning routines.
		 */
void
msg_expr_tree(const Token *t)
{
    char textbuf[25];
    int ncopied = cp_tree_src_text(textbuf,
				   t->left_token == NULL?t:t->left_token,
				   sizeof(textbuf)-1);
    msg_tail(textbuf);
    if( ncopied == sizeof(textbuf)-1 )
	msg_tail("..");
}

#ifdef DEBUG_EXPRTREES		/* Routines to print out expr tree src text */
void
print_src_text(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_tok_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}

void
print_expr_tree(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_tree_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}

void
print_expr_list(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_list_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}
#endif




void
#if HAVE_STDC
ref_array(Token *id, Token *subscrs)   /* Array reference: install in symtab */
#else /* K&R style */
ref_array(id,subscrs)   /* Array reference: install in symtab */
	Token *id, *subscrs;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

				/* Restore subscripts to original order */
	subscrs->next_token = reverse_tokenlist(subscrs->next_token);

	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
		       "undeclared variable has dim info:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	else{    /* check that subscrs match dimension info */


	  if(arg_count(subscrs->next_token)!=array_dims(symt->info.array_dim)){
	      syntax_error(subscrs->line_num,subscrs->col_num,
			"array");
	      msg_tail(symt->name);
	      msg_tail("referenced with wrong no. of subscripts");
	  }
	}

}/* ref_array */

void
#if HAVE_STDC
ref_namelist(Token *id, int stmt_class)
#else /* K&R style */
ref_namelist(id,stmt_class)
     Token *id;
     int stmt_class;
#endif /* HAVE_STDC */
{
	Token *t;
	TokenListHeader *toklist;
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
			"undeclared identifier is a namelist:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(h,type_NAMELIST,class_NAMELIST);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	   symt->info.toklist = NULL;
	}

			/* Go thru token list of namelist variables,
			   setting flags appropriately. We can't use
			   use_lvalue or use_variable here, since the
			   line number of token in list is that of the
			   namelist declaration, so we use our own code here.
			*/
	toklist = symt->info.toklist;
	if (toklist != NULL){
	    t = toklist->tokenlist;
	    while(t != NULL){
		/* set flags for all equivalenced vars */

	      int th=t->value.integer;
	      Lsymtab *tsymt,*equiv;
	      if((tsymt=hashtab[th].loc_symtab) == NULL) { /* can't happen */
		  tsymt = install_local(th,type_UNDECL,class_VAR);
		  tsymt->line_declared = id->line_num;
		  tsymt->file_declared = inctable_index;
	      }
	      equiv=tsymt;
	      if(stmt_class == tok_READ) /* code like use_lvalue */
		do{
		  if(! equiv->set_flag) { /* record first line where set */
		      equiv->line_set = id->line_num;
		      equiv->file_set = inctable_index;
		  }
		  equiv->set_flag = TRUE;
		  equiv->assigned_flag = TRUE;
		  equiv = equiv->equiv_link;
		} while(equiv != tsymt);
	      else		/* tok_WRITE: code like use_variable */
		do{
		  if(! equiv->used_flag) { /* record first line where used */
		    equiv->line_used = id->line_num;
		    equiv->file_used = inctable_index;
		  }
		  if(! equiv->set_flag) {
		    equiv->used_before_set = TRUE;
		  }
		  equiv->used_flag = TRUE;
		  equiv = equiv->equiv_link;
		} while(equiv != tsymt);
	      t = t->next_token;
	    }
	}
}

void
#if HAVE_STDC
ref_identifier(Token *id)	/* Identifier reference: install in symtab */
#else /* K&R style */
ref_identifier(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt = hashtab[h].loc_symtab;
	if( symt == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}

}/*ref_identifier*/

void
#if HAVE_STDC
ref_variable(Token *id)	/* Variable reference: install in symtab */
#else /* K&R style */
ref_variable(id)	/* Variable reference: install in symtab */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt = hashtab[h].loc_symtab;
	if( symt == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	if(symt->line_declared == NO_LINE_NUM) {
	    symt->line_declared = id->line_num; /* implicit declaration */
	    symt->file_declared = inctable_index;
	}
}/*ref_variable*/


void
#if HAVE_STDC
save_com_block(Token *id)	/* Process SAVEing of a common block */
	          	/* N.B. Legality checking deferred to END */
#else /* K&R style */
save_com_block(id)	/* Process SAVEing of a common block */
	Token *id;	/* N.B. Legality checking deferred to END */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

			/* N.B. SAVE does not create a global table entry */
	if( (symt = hashtab[h].com_loc_symtab) == NULL){
	   symt = install_local(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
				/* record location in case never declared */
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

	if(symt->saved) {
	  syntax_error(id->line_num,id->col_num,
		       "redundant SAVE declaration");
	}
	else
	  symt->saved = TRUE;
}

void
#if HAVE_STDC
save_variable(Token *id)	/* Process SAVEing of a variable */
	          	/* N.B. Legality checking deferred to END */
#else /* K&R style */
save_variable(id)	/* Process SAVEing of a variable */
	Token *id;	/* N.B. Legality checking deferred to END */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

	if(symt->saved) {
	  syntax_error(id->line_num,id->col_num,
		       "redundant SAVE declaration");
	}
	else {		/* set flags for all equivalenced vars */
	  Lsymtab *equiv=symt;
	  do{
	    equiv->saved = TRUE;
	    equiv = equiv->equiv_link;
	  } while(equiv != symt);
	}
}

	/* Following routine sets the implicit typing of characters in
	   range c1 to c2 to the given type. */
void
#if HAVE_STDC
set_implicit_type(int type, long int size, char *len_text, int c1, int c2)
	         		/* Data type of IMPLICIT declaration */
                  		/* Type size or size_DEFAULT if not given */
	               		/* Source text of length spec */
	       			/* First character of range */
	       			/* Last character of range */
#else /* K&R style */
set_implicit_type(type,size,len_text,c1,c2)
	int type;		/* Data type of IMPLICIT declaration */
        long size;		/* Type size or size_DEFAULT if not given */
	char *len_text;		/* Source text of length spec */
	int c1;			/* First character of range */
	int c2;			/* Last character of range */
#endif /* HAVE_STDC */
{
	int c;

	if(c1 == '$')  c1 = 'Z'+1;
	if(c2 == '$')  c2 = 'Z'+1;

	if(c1 == '_')  c1 = 'Z'+2;
	if(c2 == '_')  c2 = 'Z'+2;

	if(c2 < c1) {
		yyerror("IMPLICIT range must be in alphabetical order");
	}
	else {
		/* Fill in the lookup table for the given range of chars */
	  for(c=c1; c<=c2; c++) {
		implicit_type[c-'A'] = type;
		implicit_size[c-'A'] = size;
		implicit_len_text[c-'A'] = len_text;
	  }
	}
}/*set_implicit_type*/


		/* Finish processing statement function.
		   Clears all used-before-set flags of ordinary
		   variables. Reason: statement functions are processed
		   like assignment to an array element, setting ubs flags.
		   At this point, no valid setting of ubs flags should
		   be possible, so clearing them will elim false messages.*/
void
#if HAVE_STDC
stmt_function_stmt(Token *id)			/* ARGSUSED0 */
               			/* Not used at present */
#else /* K&R style */
stmt_function_stmt(id)			/* ARGSUSED0 */
     Token *id;			/* Not used at present */
#endif /* HAVE_STDC */
{
    int i;
    for(i=0; i<loc_symtab_top; i++) {
	if(storage_class_of(loc_symtab[i].type) == class_VAR &&
	   ! loc_symtab[i].parameter )
	  loc_symtab[i].used_before_set = FALSE;
    }
}/*stmt_function_stmt(id)*/

char *
#if HAVE_STDC
token_name(Token *t)
#else /* K&R style */
token_name(t)
	Token *t;
#endif /* HAVE_STDC */
{
	return hashtab[t->value.integer].name;
}/*token_name*/


void
undef_do_variable( int h )	/* Make DO index variable inactive */
{
    Lsymtab *symt=hashtab[h].loc_symtab;
    if( symt != NULL )	/* Just in case: it should always be defined */
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->active_do_var = FALSE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}


void
#if HAVE_STDC
use_actual_arg(Token *id)	/* like use_lvalue except does not set assigned_flag */
#else /* K&R style */
use_actual_arg(id)	/* like use_lvalue except does not set assigned_flag */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}
	else {
			/* If an external other than an intrinsic, set up
			   tokenlist for "call".  If intrinsic, check
			   legality of this usage.) */
	  if(storage_class_of(symt->type) == class_SUBPROGRAM) {
	    if(symt->intrinsic) {
	      IntrinsInfo *defn = symt->info.intrins_info;
	      if( !(symt->declared_intrinsic) ) {
		if(misc_warn) {
		  warning(id->line_num,id->col_num,
				defn->name);
		  msg_tail("not declared INTRINSIC");
		}
	      }
	      if( (defn->intrins_flags&I_NOTARG) ) {
		syntax_error(id->line_num,id->col_num,
				defn->name);
		msg_tail("intrinsic function cannot be a subprogram argument");
	      }
	    }
	    else {		/* External subprogram as actual arg */
	      TokenListHeader *TH_ptr;
	      TH_ptr= make_TL_head(id);

	      TH_ptr->actual_arg = TRUE;
	      TH_ptr->next = symt->info.toklist;
	      symt->info.toklist = TH_ptr;
	    }
	  }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_actual_arg*/


PRIVATE void
#if HAVE_STDC
use_function_arg(Token *id)	/* Like use_variable but invokes use_actual_arg
			   if id is an external (subprogram) passed as
			   arg of a function. This routine is used when
			   pure_functions flag is set. */
#else /* K&R style */
use_function_arg(id)	/* Like use_variable but first invokes use_actual_arg
			   only if id is an external (subprogram) passed as
			   arg of a function. This routine is used when
			   pure_functions flag is set. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

	if(storage_class_of(symt->type) == class_SUBPROGRAM)
	  use_actual_arg(id);

	use_variable(id);

}/*use_function_arg*/

void
#if HAVE_STDC
use_implied_do_index(Token *id)
#else /* K&R style */
use_implied_do_index(id)
	Token *id;
#endif /* HAVE_STDC */
{
		/* Like use_lvalue and use_variable but clears ubs flag.
	           This is because we cannot handle used-before-set
		   properly in this case, and the odds are that ubs
		   was set in the preceding I/O list. */
	int h=id->value.integer;
	Lsymtab *symt;

	use_lvalue(id);
	use_variable(id);
	symt=hashtab[h].loc_symtab;

	symt->used_before_set = FALSE;
}/*use_implied_do_index*/


PRIVATE void
#if HAVE_STDC
use_len_arg(Token *id)		/* Set the use-flag of arg to intrinsic LEN. */
#else /* K&R style */
use_len_arg(id)		/* Set the use-flag of arg to intrinsic LEN. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

    {		/* set flags for all equivalenced vars.  Do not set
		   the used-before-set flag since LEN argument does
		   not need to be defined. */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->used_flag) { /* record first line where used */
	    equiv->line_used = id->line_num;
	    equiv->file_used = inctable_index;
	}
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_len_arg*/

void
#if HAVE_STDC
use_lvalue(Token *id)	/* handles scalar lvalue */
#else /* K&R style */
use_lvalue(id)	/* handles scalar lvalue */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;
	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	    symt->line_declared = id->line_num;
	    symt->file_declared = inctable_index;
	}
	else {
	  /*   check match to previous invocations and update  */
	}

			/* F77 standard section 11.10.5 prohibits modifying
			   DO variable except thru loop mechanism.
			 */
	if(symt->active_do_var) {
	  if(usage_do_var_modified) {
	      syntax_error(id->line_num,id->col_num,
		      "active DO index is modified");
	  }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) { /* record first line where set */
	    equiv->line_set = id->line_num;
	    equiv->file_set = inctable_index;
	}
	equiv->set_flag = TRUE;
	equiv->assigned_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_lvalue*/



void                    /* Process data_constant_value & data_repeat_factor */
#if HAVE_STDC
use_parameter(Token *id)
#else /* K&R style */
use_parameter(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}
	if(! symt->parameter) {
		syntax_error(id->line_num,id->col_num,
			"must be a parameter");
/***		symt->parameter = TRUE;**/  /*oops: must define info etc.*/
	}

	if(! symt->set_flag) {
	   symt->used_before_set = TRUE;
	}

	if(! symt->used_flag) { /* record first line where used */
	    symt->line_used = id->line_num;
	    symt->file_used = inctable_index;
	}

	symt->used_flag = TRUE;

}/*use_parameter*/


void
#if HAVE_STDC
use_variable(Token *id)		/* Set the use-flag of variable. */
#else /* K&R style */
use_variable(id)		/* Set the use-flag of variable. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	   symt->line_declared = id->line_num;
	   symt->file_declared = inctable_index;
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->used_flag) { /* record first line where used */
	    equiv->line_used = id->line_num;
	    equiv->file_used = inctable_index;
	}
	if(! equiv->set_flag) {
	   equiv->used_before_set = TRUE;
	}
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_variable*/


	/* Routine to provide a string with type followed by one of: "",
	   "*n" where n is the declared size of an item, "(l)" where
	   l is the declared array length of an item, or "*n(l)".
	   Note: cannot be used twice in same statement, since
	   it uses a static buffer for the result.
	*/
char *
#if HAVE_STDC
typespec(int t, int has_size, long size, int has_len, long len)
#else /* K&R style */
typespec(t,has_size, size, has_len, len)
    int t;			/* data type code */
    int  has_size,		/* whether it has *size spec */
	 has_len;		/* whether it has (len) spec */
    long size,			/* value of size */
	 len;			/* value of len */
#endif /* HAVE_STDC */
{
			/* Size of buffer allows 3 digits for each byte,
			   which is slightly more than necessary.
			 */
    static char buf[MAX_TYPESPEC];
    strncpy(buf,type_name[t],4); buf[4] = '\0';
    if(has_size) {
	(void) sprintf(buf+strlen(buf),"*%ld",size);
    }
    if(has_len) {
	(void) sprintf(buf+strlen(buf),"(%ld)",len);
    }
    
    return buf;
}


/*  End of symtab.c */

/*

 II. Hash

*/

/*    hash.c:
 	performs a hash function

This was formerly a separate file.

*/

extern int sixclash;	/* flag to check clashes in 1st 6 chars of name */

unsigned long
#if HAVE_STDC
hash(const char *s)
#else /* K&R style */
hash(s)
    char *s;
#endif /* HAVE_STDC */
{
    unsigned long sum = 0, wd;
    unsigned j;

    if(sixclash) {		/* special hashing for six-char limit */
      unsigned i = 0;
      while (i < 6 && s[i] != '\0') {
         wd = 0;
         for(j=1; j <= sizeof(long) && i < 6 && s[i] != '\0'; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (sizeof(long) - j) * 8;}

	sum ^= wd;}
    }
    else {			/* the usual case */
      while( *s != '\0' ) {
         wd = 0;
         for(j=1; j <= sizeof(long) && *s != '\0'; j++) {
            wd += (unsigned long)(*s++ & 0xff) << (sizeof(long) - j) * 8;}

	sum ^= wd;}
    }
    return sum;
}



/*    rehash
        performs a rehash for resolving clashes.
*/

#ifdef COUNT_REHASHES
unsigned long rehash_count=0;
#endif

unsigned long
#if HAVE_STDC
rehash(unsigned long hnum)
#else /* K&R style */
rehash(hnum)
    unsigned long hnum;
#endif /* HAVE_STDC */
{
#ifdef COUNT_REHASHES
    rehash_count++;
#endif
    return hnum+1;
}


/*  End of hash */





#ifdef DEBUG_SIZES
void print_sizeofs()			/* For development: print sizeof for
				   various data structures */
{
#ifdef __STDC__
#define PrintObjSize(OBJ) (void)fprintf(list_fd,#OBJ " size = %d\n",sizeof(OBJ))
#else			/* K&R form */
#define PrintObjSize(OBJ) (void)fprintf(list_fd,"OBJ size = %d\n",sizeof(OBJ))
#endif
  PrintObjSize(char *);
  PrintObjSize(Token);
  PrintObjSize(Lsymtab);
  PrintObjSize(Gsymtab);
  PrintObjSize(HashTable);
  PrintObjSize(ArgListHeader);
  PrintObjSize(ArgListElement);
  PrintObjSize(ComListHeader);
  PrintObjSize(ComListElement);
  PrintObjSize(TokenListHeader);
  PrintObjSize(InfoUnion);
  PrintObjSize(IntrinsInfo);
  PrintObjSize(ParamInfo);
  PrintObjSize(ChildList);
}
#endif
