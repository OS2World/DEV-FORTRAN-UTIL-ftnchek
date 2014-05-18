/* $Id: prlists.c,v 1.10 2003/03/30 18:35:14 moniot Exp $

  Definition of process_lists() and associated routines that transfer
  argument and common-block lists from local to global symbol table

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
#include "ftnchek.h"
#include "symtab.h"
#include "symutils.h"

/*   Private routines */

PROTO(PRIVATE ComListHeader * make_com_array,( Token *t ));
PROTO(PRIVATE ArgListHeader * make_arg_array,( Token *t ));
PROTO(PRIVATE void make_arg_names,( Token *tlist, ArgListHeader *alhead,
			  ArgListHeader *prev_alhead ));
PROTO(PRIVATE void make_com_names,( Token *tlist, ComListHeader *clhead,
			  ComListHeader *prev_clhead ));
PROTO(PRIVATE ArgListHeader * make_arrayless_alist,( void ));
PROTO(PRIVATE ArgListHeader * make_dummy_arg_array ,( Token *t ));
#ifndef T_ALLOC
PROTO(PRIVATE ArgListElement * new_arglistelement,( unsigned count ));
PROTO(PRIVATE ArgListHeader * new_arglistheader,( void ));
PROTO(PRIVATE ComListElement * new_comlistelement,( unsigned count ));
PROTO(PRIVATE ComListHeader * new_comlistheader,( void ));
#endif


void
#if HAVE_STDC
process_lists(int curmodhash)  /* Places pointer to linked list of arrays in
			      global symbol table */
#else /* K&R style */
process_lists(curmodhash)  /* Places pointer to linked list of arrays in
			      global symbol table */
#endif /* HAVE_STDC */

#if HAVE_STDC
	                   /* current_module_hash from fortran.y */
#else /* K&R style */
	int curmodhash;    /* current_module_hash from fortran.y */
#endif /* HAVE_STDC */
{
	int i, h;
	unsigned long hnum;
	Gsymtab *curr_gsymt;

	Gsymtab *gsymt;
	TokenListHeader *head_ptr;

	if( (curr_gsymt=
	     (curmodhash == -1) ? NULL:hashtab[curmodhash].glob_symtab)
	   == NULL) {
	  oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		  "module not in global symtab:");
	  oops_tail(hashtab[curmodhash].name);
	}
	else {
	  if(curr_gsymt->internal_entry) {/* protect ourself */
	    if(misc_warn) {
	      warning(NO_LINE_NUM,NO_COL_NUM,
		    "entry point redefined as module");
	      msg_tail(curr_gsymt->name);
	      msg_tail(": previous definition overridden");
	    }
	    curr_gsymt->link.child_list = NULL;
	  }
	  curr_gsymt->internal_entry = FALSE;
	}

	for (i=0; i<loc_symtab_top; i++){
				/* Skip things which are not true externals */
	    if(loc_symtab[i].argument || loc_symtab[i].intrinsic ||
		   loc_symtab[i].array_var)
		      continue;

	    head_ptr = loc_symtab[i].info.toklist;

	    hnum=hash(loc_symtab[i].name);
	    while(h=hnum%HASHSZ,hashtab[h].name != NULL
		 && strcmp(hashtab[h].name,loc_symtab[i].name)!=0){
		      hnum = rehash(hnum);      /* Resolve clashes */
	    }

	    switch (storage_class_of(loc_symtab[i].type)){
		    case class_COMMON_BLOCK:
			if(head_ptr != NULL) {
if((gsymt=hashtab[h].com_glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		 "common block not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
			Token *tok_ptr;
                        ComListHeader *c;

				/* First we link up possibly multiple
				   declarations of the same common block
				   in this module into one big list */
		    	while (tok_ptr = head_ptr->tokenlist,
			       (head_ptr = head_ptr->next) != NULL){
			    while(tok_ptr->next_token != NULL){
			        tok_ptr = tok_ptr->next_token;
			    }
			    tok_ptr->next_token = head_ptr->tokenlist;
			}
				/* Original token list is in reverse order.
				   Reverse it so order is correct. */
			head_ptr = loc_symtab[i].info.toklist;
			head_ptr->tokenlist =
			  reverse_tokenlist(head_ptr->tokenlist);

				/* Keep a copy for use by makedecls */
			loc_symtab[i].src.toklist = head_ptr;

				/* Now make it into array for global table */
		        c=make_com_array(head_ptr->tokenlist);
			c->module = curr_gsymt;
			c->filename = head_ptr->filename;
			c->topfile = top_filename;
			c->line_num = head_ptr->line_num;
			c->top_line_num = head_ptr->top_line_num;
			c->saved = global_save || loc_symtab[i].saved;

				/* add names to com list */
			make_com_names(head_ptr->tokenlist,
				       c,gsymt->info.comlist);

                        c->next = gsymt->info.comlist;
			gsymt->info.comlist = c;

		/* Replace token list by comlist for check_mixed_common */
			loc_symtab[i].info.comlist = c;
}
			}/* end if(head_ptr != NULL) */

		        break;	/* end case class_COMMON_BLOCK */


			/* Are we inside a function or subroutine? */
		    case class_VAR:
		       if(loc_symtab[i].entry_point) {
if((gsymt=hashtab[h].glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
    "subprog not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
                          ArgListHeader *a;
			  int implied_type;

				/* Make each token list into an array of
				   args for global table */
			  while (head_ptr != NULL){
			     a=make_dummy_arg_array(head_ptr->tokenlist);
			     make_arg_names(head_ptr->tokenlist,
					   a,gsymt->info.arglist);
			     implied_type = get_type(&(loc_symtab[i]));
			     a->type = type_byte(
			         class_SUBPROGRAM,implied_type);
			     a->size = get_size(&(loc_symtab[i]),implied_type);
			     a->module = curr_gsymt;
			     a->filename = head_ptr->filename;
			     a->topfile = top_filename;
			     a->line_num = head_ptr->line_num;
			     a->top_line_num = head_ptr->top_line_num;

			     a->next = gsymt->info.arglist;
			     gsymt->info.arglist = a;
			/* store arglist in local symtab for project file */
			     loc_symtab[i].info.arglist = a;
			     head_ptr = head_ptr->next;
		          }/* end while (head_ptr != NULL) */
			  /* used_flag=1 does not imply call of the routine
			     itself unless it is recursive.  (Recursive procs
			     are not implemented yet.)  So do not copy it
			     from local to global symbol table.
			   */
			  if(loc_symtab[i].set_flag)
			         gsymt->set_flag =
				   gsymt->set_this_file = TRUE;
			  if(loc_symtab[i].library_module)
				 gsymt->library_module = TRUE;
			  gsymt->defined = TRUE;
			  if(gsymt != curr_gsymt) {
			    gsymt->internal_entry = TRUE;
			    gsymt->link.module = curr_gsymt;
			  }
}
			}/* end if(loc_symtab[i].entry_point) */
		    	break; /* end case class_VAR */

                    case class_SUBPROGRAM:
if((gsymt=hashtab[h].glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
    "subprog not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
                        ArgListHeader *a;
			int implied_type;
			while (head_ptr != NULL){
			  if(head_ptr->external_decl || head_ptr->actual_arg)
			    a=make_arrayless_alist();
			  else {
			    a=make_arg_array(head_ptr->tokenlist);
#ifdef DEBUG_ARG_ALIAS
			    if(debug_latest) {
			      int j;
			      (void)fprintf(list_fd,"\n%s arg-same-as: ",
				      loc_symtab[i].name);
			      for(j=0; j<a->numargs; j++) {
				(void)fprintf(list_fd," %d",a->arg_array[j].same_as);
			      }
			      (void)fprintf(list_fd,"\n%s args in common: ",
				      loc_symtab[i].name);
			      for(j=0; j<a->numargs; j++) {
				if(a->arg_array[j].common_block) {
				  (void)fprintf(list_fd,"\n%d = %s[%ld]",
						j+1,
						a->arg_array[j].common_block->name,
						a->arg_array[j].common_index);
				}
			      }
			    }
#endif
			    make_arg_names(head_ptr->tokenlist,
					   a,gsymt->info.arglist);
			  }
			  implied_type = get_type(&(loc_symtab[i]));
			  a->type = type_byte(
			         class_SUBPROGRAM,implied_type);
			  a->size = get_size(&(loc_symtab[i]),implied_type);
			  a->module = curr_gsymt;
			  a->filename = head_ptr->filename;
			  a->topfile = top_filename;
			  a->line_num = head_ptr->line_num;
			  a->top_line_num = head_ptr->top_line_num;
			  a->external_decl = head_ptr->external_decl;
			  a->actual_arg = head_ptr->actual_arg;

			  a->next = gsymt->info.arglist;
			  gsymt->info.arglist = a;
		/* put arglist into local symtab for project file use */
			  loc_symtab[i].info.arglist = a;
			  head_ptr = head_ptr->next;
		        }
			if(loc_symtab[i].used_flag)
			        gsymt->used_flag =
				  gsymt->used_this_file = TRUE;
			if(loc_symtab[i].invoked_as_func)
			        gsymt->invoked_as_func =
				  gsymt->invoked_as_func_this_file = TRUE;
}
				/* Add this guy to linked list of children,
				   unless never actually used. */
			if(loc_symtab[i].used_flag) {
			  ChildList *node=
			    (ChildList *)calloc(1,sizeof(ChildList));
			  node->child = gsymt;
			  node->next = curr_gsymt->link.child_list;
			  curr_gsymt->link.child_list = node;
			}
			if(loc_symtab[i].declared_external)
				 gsymt->declared_external =
				   gsymt->declared_external_this_file =  TRUE;

			break;/* end case class_SUBPROGRAM*/

                    case class_NAMELIST:
			if(head_ptr != NULL) {
			  Token *tok_ptr;

				/* Link up possibly multiple
				   declarations of the same namelist
				   in this module into one big list */
			  while (tok_ptr = head_ptr->tokenlist,
			       (head_ptr = head_ptr->next) != NULL){
			    while(tok_ptr->next_token != NULL){
			        tok_ptr = tok_ptr->next_token;
			    }
			    tok_ptr->next_token = head_ptr->tokenlist;
			  }
				/* Original token lists are in reverse order.
				   Reverse it so order is correct. */
			head_ptr = loc_symtab[i].info.toklist;
			head_ptr->tokenlist =
			  reverse_tokenlist(head_ptr->tokenlist);
			}
				/* Keep a copy for use by makedecls */
			loc_symtab[i].src.toklist = head_ptr;


			break;/* end case class_NAMELIST*/
	    }/* end switch */

        }/* end for (i=0; i<loc_symtab_top; i++) */

}/* process_lists */


	/* Following routine converts a list of tokens into a list of type-
	   flag pairs. */

PRIVATE ArgListHeader *
#if HAVE_STDC
make_arg_array(Token *t)
	         		/* List of tokens */
#else /* K&R style */
make_arg_array(t)
	Token *t;		/* List of tokens */
#endif /* HAVE_STDC */
{
	unsigned i;
	unsigned count;
	Token *s;
	ArgListElement *arglist;
	ArgListHeader *alhead;

	count = arg_count(t);
	if(((alhead=new_arglistheader())
		 		 == (ArgListHeader *) NULL) ||
	  (count != 0 &&
          ((arglist=new_arglistelement(count))
				 == (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for argument list");
	}
	s = t;
	for(i=0; i<count; i++){  /* Here we fill array. */

	    arglist[i].type = s->TOK_type; /* use evaluated type, not symt */
	    arglist[i].size = s->size;
	    arglist[i].same_as = i; /* initialize to same-as-self */
			/* Keep track of array and external declarations */
	    if( is_true(ID_EXPR,s->TOK_flags) ){
		int h = s->value.integer;
		Lsymtab *symt = hashtab[h].loc_symtab;
		if( (arglist[i].array_var = symt->array_var) == 0 ) {
				/* change scalars to 0 dims, size 1 */
		  arglist[i].info.array_dim = array_dim_info(0,1);
		}
		else {
		  arglist[i].info.array_dim = symt->info.array_dim;
		}
		arglist[i].declared_external = symt->declared_external;

				/* If arg is in common, record where,
				   for checking violation of 15.9.3.6.
				*/
		if( symt->common_var ) {
		  Lsymtab *eq = symt;
		  do {	/* follow equivalence chain to find ptr to block */
		    if( eq->common_block != NULL ) {
		      break;
		    }
		    eq = eq->equiv_link;
		  } while(eq != symt);
		  arglist[i].common_block = eq->common_block; /*assert !NULL */
		  arglist[i].common_index = eq->common_index;
		}
				/* Look for other arg that is same as this,
				   also for checking violation of 15.9.3.6.
				*/
		{
		  Token *p=t;
		  unsigned j;
		  for(j=0; j<i; j++) {
		    if( is_true(ID_EXPR,p->TOK_flags) ){
		      int is_same = FALSE;
		      int ph = p->value.integer;
		      Lsymtab *psymt = hashtab[ph].loc_symtab;
		      Lsymtab *eq = psymt;
		      do {	/* follow equivalence chain */
			if( eq == symt ) {
			  is_same = TRUE;
			  break;
			}
			eq = eq->equiv_link;
		      } while(eq != psymt);
		      if( is_same ) {
				/* Swap indices so each is_same as other
				   This does right thing if >2 are same. */
			int tmp = arglist[i].same_as;
			arglist[i].same_as = arglist[j].same_as;
			arglist[j].same_as = tmp;
			break;	/* quit j loop */
		      }
		    }
		    p = p->next_token;
		  }
		}
	    }
	    else {
		arglist[i].info.array_dim = array_dim_info(0,1);
		arglist[i].array_var = FALSE;
		arglist[i].declared_external = FALSE;
	    }

	    arglist[i].array_element =
		arglist[i].array_var && !is_true(ARRAY_ID_EXPR,s->TOK_flags);

	    if( is_true(LVALUE_EXPR,s->TOK_flags) ){
		arglist[i].is_lvalue = TRUE;
			/* is_true(f,x) yields 0 or non-0: convert to 0 or 1 */
		arglist[i].set_flag =
			is_true(SET_FLAG,s->TOK_flags)? TRUE: FALSE;
		arglist[i].assigned_flag =
			is_true(ASSIGNED_FLAG,s->TOK_flags)? TRUE: FALSE;
		arglist[i].used_before_set =
			is_true(USED_BEFORE_SET,s->TOK_flags)? TRUE: FALSE;
		arglist[i].active_do_var =
			is_true(DO_VARIABLE,s->TOK_flags)? TRUE: FALSE;
	    }
	    else {	/* it is an expression or constant, not an lvalue */
		arglist[i].is_lvalue = FALSE;
		arglist[i].set_flag = TRUE;
		arglist[i].assigned_flag = FALSE;
		arglist[i].used_before_set = FALSE;
		arglist[i].active_do_var = FALSE;
	    }

	    s = s->next_token;
	}
	alhead->numargs = (short)count;
	alhead->is_defn = FALSE;
	alhead->is_call = TRUE;
	alhead->external_decl = FALSE;
	alhead->actual_arg = FALSE;

        if (count == 0)
		alhead->arg_array = NULL;
	else
		alhead->arg_array = arglist;
	return(alhead);
}/* make_arg_array */

PRIVATE void
#if HAVE_STDC
make_arg_names(Token *tlist, ArgListHeader *alhead, ArgListHeader *prev_alhead)
#else /* K&R style */
make_arg_names(tlist, alhead, prev_alhead)
     Token *tlist;
     ArgListHeader *alhead, *prev_alhead;
#endif /* HAVE_STDC */
{
	int h, i, n, prev_n;
	Token *s;
#ifdef KEEP_ARG_NAMES
	char *name;
	char expr_text[MAXEXPRTEXT+2]; /* big enough for 1 extra */
#else
	static char expr[]="expr", 	/* text strings to use */
	            var[]="var";
#endif
	ArgListElement *arglist, *prev_arglist;

	n = alhead->numargs;
	if(n > 0) {
	  arglist = alhead->arg_array;
	  if(prev_alhead != NULL) {
	    prev_n = prev_alhead->numargs;
	    prev_arglist = prev_alhead->arg_array;
	  }
	  for(i=0, s=tlist; i<n; i++, s=s->next_token) {
				/* Use symtab name for id's but note that
				   array elements come thru with ID_EXPR
				   true but want to use expr tree for them.*/
	    if(is_true(ID_EXPR,s->TOK_flags)
		    && !is_true(ARRAY_ELEMENT_EXPR,s->TOK_flags)) {
#ifdef KEEP_ARG_NAMES
	      h = s->value.integer;
	      name = hashtab[h].loc_symtab->name;
#else
	      name = var;
#endif
	    }
	    else {				/* expression */
#ifdef KEEP_ARG_NAMES
	      int ncopied;
	      ncopied = cp_tree_src_text(expr_text,
			(s->left_token == NULL?
				s:			/* Primary */
				s->left_token),	/* Expr tree */
			MAXEXPRTEXT+1);
	      if(ncopied > MAXEXPRTEXT)	/* Longer than the limit: */
					/* replace tail by dots   */
		(void)strcpy(expr_text+MAXEXPRTEXT-2,"..");
	      name = expr_text;
#else
	      arglist[i].name = expr;
#endif
	    }
#ifdef KEEP_ARG_NAMES
				/* Try to avoid allocating space again */
	    if(prev_alhead != NULL && i < prev_n
		 && strcmp(name,prev_arglist[i].name) == 0) {
	      name = prev_arglist[i].name;
	    }
	    else if(is_true(ID_EXPR,s->TOK_flags)
		    && !is_true(ARRAY_ELEMENT_EXPR,s->TOK_flags)) {
	      if(hashtab[h].glob_symtab != NULL) {
		name = hashtab[h].glob_symtab->name;
	      }
	      else if(hashtab[h].com_glob_symtab != NULL) {
		name = hashtab[h].com_glob_symtab->name;
	      }
	      else			/* No luck: put it into global space */
		name = new_global_string(name);
	    }
	    else
	      name = new_global_string(name);
#endif
	    arglist[i].name = name;
	  }
	}
}

	/* Following routine converts a list of common block tokens
	    into a list of dimen_info-type pairs. */

PRIVATE ComListHeader *
#if HAVE_STDC
make_com_array(Token *t)
	         		/* List of tokens */
#else /* K&R style */
make_com_array(t)
	Token *t;		/* List of tokens */
#endif /* HAVE_STDC */
{
	Token *s;
	Lsymtab *symt;
	int h;
	unsigned i, count;
	ComListHeader *clhead;
	ComListElement *comlist;

	count = arg_count(t);
	if(((clhead=new_comlistheader())
		 == (ComListHeader *) NULL) ||
	  (count != 0 &&
 	  ((comlist=new_comlistelement(count))
		 == (ComListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for common list");
	}
	s = t;
	for(i=0; i<count; i++){
	   h = s->value.integer;
	   symt = hashtab[h].loc_symtab;
	   comlist[i].name = NULL; /* names are added later by make_com_list */
	   if( (comlist[i].dimen_info = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
	     comlist[i].dimen_info = array_dim_info(0,1);
       	   comlist[i].type = get_type(symt);
	   comlist[i].size = get_size(symt,(int)comlist[i].type);
	   comlist[i].used = symt->used_flag;
	   comlist[i].set = symt->set_flag;
	   comlist[i].used_before_set = symt->used_before_set;
	   comlist[i].assigned = symt->assigned_flag;
	   if (comlist[i].used)
		clhead->any_used = TRUE;
	   if (comlist[i].set)
		clhead->any_set = TRUE;
	   s = s->next_token;
	}
	clhead->numargs = (short)count;
	if (count == 0)
		clhead->com_list_array = NULL;
	else
		clhead->com_list_array = comlist;
	return(clhead);
} /* make_com_array */

PRIVATE void
#if HAVE_STDC
make_com_names(Token *tlist, ComListHeader *clhead, ComListHeader *prev_clhead)
#else /* K&R style */
make_com_names(tlist, clhead, prev_clhead)
     Token *tlist;
     ComListHeader *clhead, *prev_clhead;
#endif /* HAVE_STDC */
{
	int h, i, n, prev_n;
	Token *s;
	ComListElement *comlist, *prev_comlist;
	char *name;
	comlist = clhead->com_list_array;

	n = clhead->numargs;
	if(prev_clhead != NULL) {
	  prev_n = prev_clhead->numargs;
	  prev_comlist = prev_clhead->com_list_array;
	}

	for(i=0, s=tlist; i<n; i++, s=s->next_token) {
	  h = s->value.integer;
	  name = hashtab[h].loc_symtab->name;

		/* Try to avoid allocating new global space for name:
		   Check if the variable matches a global name
		   (unlikely) or name of corresponding variable in
		   previous declaration of same block used the same
		   name (likely), and if so, re-use the global string.
		   Otherwise allocate new space in global table.  */

	  if(prev_clhead != NULL && i < prev_n
	     && strcmp(name,prev_comlist[i].name) == 0) {
	    name = prev_comlist[i].name;
	  }
	  else if(hashtab[h].glob_symtab != NULL) {
	    name = hashtab[h].glob_symtab->name;
	  }
	  else if(hashtab[h].com_glob_symtab != NULL) {
	    name = hashtab[h].com_glob_symtab->name;
	  }
	  else			/* No luck: put it into global space */
	    name = new_global_string(name);

	  comlist[i].name = name;
	}
}

PRIVATE ArgListHeader *
#if HAVE_STDC
make_dummy_arg_array (Token *t)
	         		/* List of tokens */
#else /* K&R style */
make_dummy_arg_array (t)
	Token *t;		/* List of tokens */
#endif /* HAVE_STDC */
{
	unsigned i;
	unsigned count;
	Token *s;
	ArgListElement *arglist;
	ArgListHeader *alhead;

	count = arg_count(t);
	if(((alhead=new_arglistheader())
			 == (ArgListHeader *) NULL) ||
	  (count != 0 &&
          ((arglist=new_arglistelement(count))
			== (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for dummy argument list");
	}
	s = t;
	for(i=0; i<count; i++){
	    if( is_true(ID_EXPR,s->TOK_flags) ){
	        int implied_type;
		int h = s->value.integer;
		Lsymtab *symt = hashtab[h].loc_symtab;
		if( (arglist[i].info.array_dim = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
		  arglist[i].info.array_dim = array_dim_info(0,1);
		implied_type = get_type(symt);
		arglist[i].type = type_byte(storage_class_of(symt->type),
						implied_type);
		arglist[i].size = get_size(symt,implied_type);
		arglist[i].same_as = i; /* always is same-as-self */
		arglist[i].is_lvalue = TRUE;
		arglist[i].set_flag = symt->set_flag;
		arglist[i].assigned_flag = symt->assigned_flag;
		arglist[i].used_before_set = symt->used_before_set;
		arglist[i].array_var = symt->array_var;
		arglist[i].array_element = FALSE;
		arglist[i].declared_external = symt->declared_external;
	    }
	    else {	/* It is a label */
		arglist[i].info.array_dim = 0;
		arglist[i].type = s->TOK_type;
		arglist[i].size = 0;
		arglist[i].is_lvalue = FALSE;
		arglist[i].set_flag = FALSE;	/* Don't currently do labels */
		arglist[i].assigned_flag = FALSE;
		arglist[i].used_before_set = FALSE;
		arglist[i].array_var = FALSE;
		arglist[i].array_element = FALSE;
		arglist[i].declared_external = FALSE;
	    }
	    s = s->next_token;
	}
	alhead->numargs = (short)count;
	alhead->is_defn = TRUE;
	alhead->is_call = FALSE;
	alhead->external_decl = FALSE;
	alhead->actual_arg = FALSE;

        if (count == 0)
		alhead->arg_array = NULL;
	else
		alhead->arg_array = arglist;
	return(alhead);
}/* make_dummy_arg_array */


	/* This routine makes an empty argument list: used for
	   EXTERNAL declarations of subprograms. */
PRIVATE ArgListHeader *
make_arrayless_alist(VOID)
{
	ArgListHeader *alhead;

	if(((alhead=new_arglistheader())
		 		 == (ArgListHeader *) NULL) ) {
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for external decl");
	}

	alhead->numargs = 0;
	alhead->is_defn = FALSE;
	alhead->is_call = FALSE;
	alhead->arg_array = NULL;

	return(alhead);
}/* make_arrayless_arglist */

#ifndef T_ALLOC

	/* This routine allocates permanent space for argument list
	   elements in chunks for efficiency.  It returns a pointer to
	   space for count consecutive elements. */

PRIVATE ArgListElement *
#if HAVE_STDC
new_arglistelement(unsigned int count)
#else /* K&R style */
new_arglistelement(count)
     unsigned count;
#endif /* HAVE_STDC */
{
  static unsigned long arglistspace_bot=0;
  static ArgListElement *arglist_space=NULL;

  arglist_element_used += count;	/* For -resources */

  if(arglistspace_bot < count) {
    unsigned long numalloc = (count > ARGLISTELTSZ? count: ARGLISTELTSZ);
    arglist_space=(ArgListElement *)calloc(numalloc,sizeof(ArgListElement));
    if(arglist_space == (ArgListElement *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for argument list");
      return (ArgListElement *)NULL; /*NOTREACHED*/
    }
    arglistspace_bot = numalloc;
  }
				/* Slots are allocated from top down */
  arglistspace_bot -= count;
  return arglist_space + arglistspace_bot;
}

	/* This routine allocates permanent space for argument list
	   headers in chunks for efficiency.  Returns a pointer to
	   space for one header. */

PRIVATE ArgListHeader *
new_arglistheader(VOID)
{
  static unsigned long arglistheadspace_bot=0;
  static ArgListHeader *arglisthead_space;

  arglist_head_used++;

  if(arglistheadspace_bot < 1) {
    arglisthead_space=
      (ArgListHeader *)calloc(ARGLISTHEADSZ,sizeof(ArgListHeader));
    if(arglisthead_space == (ArgListHeader *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for argument list header");
      return (ArgListHeader *)NULL; /*NOTREACHED*/
    }
    arglistheadspace_bot = ARGLISTHEADSZ;
  }
				/* Slots are allocated from top down */
  return arglisthead_space + (--arglistheadspace_bot);
}

	/* Returns pointer to space for count consecutive common list
	   elements. */

PRIVATE ComListElement *
#if HAVE_STDC
new_comlistelement(unsigned int count)
#else /* K&R style */
new_comlistelement(count)
     unsigned count;
#endif /* HAVE_STDC */
{
  static unsigned long comlistspace_bot=0;
  static ComListElement *comlist_space=NULL;

  comlist_element_used += count;	/* For -resources */

  if(comlistspace_bot < count) {
    unsigned long numalloc = (count > COMLISTELTSZ? count: COMLISTELTSZ);
    comlist_space=(ComListElement *)calloc(numalloc,sizeof(ComListElement));
    if(comlist_space == (ComListElement *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for common block list");
      return (ComListElement *)NULL; /*NOTREACHED*/
    }
    comlistspace_bot = numalloc;
  }
				/* Slots are allocated from top down */
  comlistspace_bot -= count;
  return comlist_space + comlistspace_bot;
}

	/* Returns pointer to space for one common block header */

PRIVATE ComListHeader *
new_comlistheader(VOID)
{
  static unsigned long comlistheadspace_bot=0;
  static ComListHeader *comlisthead_space;

  comlist_head_used++;

  if(comlistheadspace_bot < 1) {
    comlisthead_space=
      (ComListHeader *)calloc(COMLISTHEADSZ,sizeof(ComListHeader));
    if(comlisthead_space == (ComListHeader *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for common block header");
      return (ComListHeader *)NULL; /*NOTREACHED*/
    }
    comlistheadspace_bot = COMLISTHEADSZ;
  }
				/* Slots are allocated from top down */
  return comlisthead_space + (--comlistheadspace_bot);
}


#endif /*T_ALLOC*/
