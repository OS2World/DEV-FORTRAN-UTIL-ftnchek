/* $Id: symspace.c,v 1.6 2001/11/03 00:55:37 moniot Rel $

  Routines to allocate various symbol table-related items.

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
#include "symspace.h"
#include "symutils.h"

PROTO(PRIVATE TokenListHeader * new_tokhead,( void ));


PRIVATE StrSpace *
curr_loc_strspace;		/* Ptr to current local string space struct */

PRIVATE StrSpace *
curr_srctextspace;		/* Ptr to current token string space struct */

PRIVATE ParamInfoSpace *
curr_paraminfospace;		/* Ptr to current param info space struct */

PRIVATE TokHeadSpace *
curr_tokheadspace;		/* Ptr to current TokHeadSpace struct */

PRIVATE TokenSpace *
curr_tokspace;			/* Ptr to current TokenSpace struct */

PRIVATE PtrSpace *
curr_ptrspace;			/* Ptr to current PtrSpace struct */

void
init_globals(VOID)                	/* Clears the global symbol table */
{
  glob_symtab_top = 0;	/* Neither of these stmts is really needed. */
  glob_strings_used = 0;

				/* local strings are now permanently stored */
  curr_loc_strspace = &lstrspace;
  loc_str_top = 0;
  extra_locstrspace = 0;
}/*init_globals*/



void
init_symtab(VOID)                     /* Clears the local symbol table */
{
	int i,h;

		/* Define factor equal to ratio of time to clear hashtable
		   entry by looking up in symbol table to time to clear it
		   directly.  This factor is used to choose the method
		   of clearing out the hashtab.
		 */
#ifndef HINITFACTOR
#define HINITFACTOR 20
#endif
		      /* Clear the hash table of local symbol refs */
	if( loc_symtab_top < HASHSZ/HINITFACTOR ) {
			/* few local symbols: look them up in symtab */
	  for(i=0; i<loc_symtab_top; i++) {
	      h=hash_lookup(loc_symtab[i].name);
	      hashtab[h].loc_symtab = NULL;
	      hashtab[h].com_loc_symtab = NULL;
	  }
	}
	else {
			/* many local symbols: skip lookup, sweep hashtable */
	  for(h=0;h<HASHSZ;h++) {
	    hashtab[h].loc_symtab = NULL;
	    hashtab[h].com_loc_symtab = NULL;
	  }
	}

	loc_symtab_top = 0;	/* Clear local symtab */


	curr_srctextspace = &srctextspace;
	srctextspace_top = 0;	/* Reset storage area for token text */
	extra_srctextspace = 0;

	curr_tokspace = &tokspace;
	token_space_top = 0;	/* Reset storage for tokens in lists & trees */
	extra_tokspace = 0;

	curr_paraminfospace = &paraminfospace;
 	param_info_space_top = 0;/* Reset storage for parameter info structs */
	extra_paraminfospace = 0;

	curr_tokheadspace = &tokheadspace;
 	token_head_space_top = 0;/* Reset storage for tokenlist headers */
	extra_tokheadspace = 0;

	curr_ptrspace = &ptrspace;
	ptrspace_top = 0;	/* Reset storage for array dim textvecs */
	extra_ptrspace = 0;

	parameter_count = 0;

		      /* Restores implicit typing to default values.
		         Note: 27 is '$', 28 is '_' which are default REAL */
	{
		int c;
		for( c=0; c<=('Z'-'A'+2); c++ ) {
	    	    implicit_type[c] = type_REAL;
		    implicit_size[c] = size_DEFAULT;
		    implicit_len_text[c] = NULL;
		}
		for( c='I'-'A'; c <= 'N'-'A'; c++ )
		    implicit_type[c] = type_INTEGER;
	}

	init_labtable();		/* Clear out label table */

}/*init_symtab*/

TokenListHeader *	/* Initializes a tokenlist header */
#if HAVE_STDC
make_TL_head(Token *t)
#else /* K&R style */
make_TL_head(t)
     Token *t;
#endif /* HAVE_STDC */
{
	TokenListHeader *TH_ptr;
	TH_ptr = new_tokhead();
	TH_ptr->line_num = t->line_num;
	TH_ptr->top_line_num = (current_filename == top_filename?
				t->line_num: top_file_line_num);
  	TH_ptr->filename = current_filename;
				/* Clear all the flags */
	TH_ptr->external_decl = FALSE;
	TH_ptr->actual_arg = FALSE;
	TH_ptr->tokenlist = NULL;
	TH_ptr->next = NULL;

  return TH_ptr;
}

PRIVATE TokenListHeader *
new_tokhead(VOID)
{
  if(token_head_space_top == TOKHEADSPACESZ) {
    if(curr_tokheadspace->next == (TokHeadSpace *)NULL) {
      TokHeadSpace *new_tokheadspace;
      if( (new_tokheadspace = (TokHeadSpace *)malloc(sizeof(TokHeadSpace)))
	 == (TokHeadSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for token list header");
	return (TokenListHeader *)NULL;	/*NOTREACHED*/
      }
      else {
	new_tokheadspace->next =  (TokHeadSpace *)NULL;
	curr_tokheadspace->next = new_tokheadspace;
      }
    }
    curr_tokheadspace = curr_tokheadspace->next;
    extra_tokheadspace += TOKHEADSPACESZ;
    token_head_space_top = 0;
  }
  return curr_tokheadspace->tokheadspace + token_head_space_top++;
}

		/* this routine allocates room in global stringspace
		   (top down) for string s, and copies it there. */
char *
#if HAVE_STDC
new_global_string(char *s)
#else /* K&R style */
new_global_string(s)
	char *s;
#endif /* HAVE_STDC */
{
  static unsigned long glob_str_bot = 0;
  static char *glob_strspace;

  unsigned count = strlen(s) + 1;/* no. of chars needed including final nul */

  glob_strings_used += count;	/* keep track for -resource */

  if(glob_str_bot < count) {
    unsigned long numalloc = (count > STRSPACESZ? count: STRSPACESZ);
    glob_strspace = (char *)calloc(numalloc,sizeof(char));
    if(glob_strspace == (char *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for global strings");
      return (char *)NULL; /*NOTREACHED*/
    }
    glob_str_bot = numalloc;
  }

  glob_str_bot -= count;    /*pre-decrement*/
  return strcpy(glob_strspace+glob_str_bot,s);
}/*new_global_string*/

		/* Allocate space for string s in local string space
		   (bottom up), and copy it there. */
char *
#if HAVE_STDC
new_local_string(char *s)
#else /* K&R style */
new_local_string(s)
	char *s;
#endif /* HAVE_STDC */
{
  int count = strlen(s) + 1;	/* No. of chars needed including final nul */
  int orig_top = loc_str_top;
  loc_str_top += count;
  if(loc_str_top > STRSPACESZ) {
    StrSpace *new_loc_strspace;
    new_loc_strspace = (StrSpace *)malloc(sizeof(StrSpace));
    if(new_loc_strspace == (StrSpace *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for local strings");
      return (char *)NULL; /*NOTREACHED*/
    }
    else {
      new_loc_strspace->next = (StrSpace *)NULL;
      curr_loc_strspace->next = new_loc_strspace;
    }
    curr_loc_strspace = curr_loc_strspace->next;
    extra_locstrspace += orig_top; /* Remember amount used so far */
    orig_top = 0;
    loc_str_top = count;
  }
  return strcpy(curr_loc_strspace->strspace+orig_top,s);
}/* new_local_string */

ParamInfo *
new_param_info(VOID)		/* Allocates space for parameter info field */
{
  if(param_info_space_top == PARAMINFOSPACESZ) {
    if(curr_paraminfospace->next == (ParamInfoSpace *)NULL) {
      ParamInfoSpace *new_paraminfospace;
      if( (new_paraminfospace = (ParamInfoSpace *)malloc(sizeof(ParamInfoSpace)))
	 == (ParamInfoSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for parameter info");
	return (ParamInfo *)NULL;	/*NOTREACHED*/
      }
      else {
	new_paraminfospace->next =  (ParamInfoSpace *)NULL;
	curr_paraminfospace->next = new_paraminfospace;
      }
    }
    curr_paraminfospace = curr_paraminfospace->next;
    extra_paraminfospace += PARAMINFOSPACESZ;
    param_info_space_top = 0;
  }
  return curr_paraminfospace->paraminfospace + param_info_space_top++;

}



void
#if HAVE_STDC
free_textvec(char **p)		/*ARGSUSED0*/
#else /* K&R style */
free_textvec(p)		/*ARGSUSED0*/
     char **p;
#endif /* HAVE_STDC */
{
	/* No action necessary since all the space is freed in
	   a lump at end of processing module */
}

char **
#if HAVE_STDC
new_textvec(int n)		/* Allocates space for array of n char ptrs */
#else /* K&R style */
new_textvec(n)		/* Allocates space for array of n char ptrs */
     int n;
#endif /* HAVE_STDC */
{
  int orig_top = ptrspace_top;
  ptrspace_top += n;

  if( ptrspace_top > PTRSPACESZ) {
    if(curr_ptrspace->next == (PtrSpace *)NULL) {
      PtrSpace *new_ptrspace;
      if( (new_ptrspace = (PtrSpace *)malloc(sizeof(PtrSpace)))
	 == (PtrSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for pointers to text");
	return (char **)NULL; /*NOTREACHED*/
      }
      else {
	new_ptrspace->next = (PtrSpace *)NULL;
	curr_ptrspace->next = new_ptrspace;
      }
    }
    curr_ptrspace = curr_ptrspace->next;
    extra_ptrspace += orig_top;
    orig_top = 0;
    ptrspace_top = n;
  }
  return curr_ptrspace->ptrspace + orig_top;
}

				/* Routine to allocate space for
				   a string containing source text
				   of a token. */

char *
#if HAVE_STDC
new_src_text_alloc(int size)
              			/* length counting nul */
#else /* K&R style */
new_src_text_alloc(size)
     int size;			/* length counting nul */
#endif /* HAVE_STDC */
{
  int orig_top = srctextspace_top;
  srctextspace_top += size;

  if(srctextspace_top > STRSPACESZ) {
    StrSpace *new_srctextspace;
    new_srctextspace = (StrSpace *)malloc(sizeof(StrSpace));
    if(new_srctextspace == (StrSpace *)NULL) {
      oops_message(OOPS_FATAL,line_num,col_num,
		   "Cannot alloc space for token text");
      return (char *)NULL; /*NOTREACHED*/
    }
    else {
      new_srctextspace->next = (StrSpace *)NULL;
      curr_srctextspace->next = new_srctextspace;
    }
    curr_srctextspace = curr_srctextspace->next;
    extra_srctextspace += orig_top; /* Remember amount used so far */
    orig_top = 0;
    srctextspace_top = size;
  }

  return curr_srctextspace->strspace + orig_top;
}

				/* Tokens that are 1 char long have their
				   src_text stored in this array, indexed
				   by their codes.  Avoids duplication of
				   strings, wasting space.
				 */
PRIVATE char onechar_text[2*(MAX_CHAR_CODE+1)];

				/* Routine to get space for string
				   containing source text of token
				   and copy it to there.
				 */

char *
#if HAVE_STDC
new_src_text(const char *s, int len)
             			/* string (final nul not needed) */
             			/* length not counting nul */
#else /* K&R style */
new_src_text(s,len)
     char *s;			/* string (final nul not needed) */
     int len;			/* length not counting nul */
#endif /* HAVE_STDC */
{
  int i;
  char *new_s;
				/* If it is a single char, it goes
				   into the special array.  Otherwise
				   allocate space for it. */
  if(len <= 1)
    new_s = &onechar_text[s[0]*2];
  else
    new_s = new_src_text_alloc(len+1);

  for(i=0; i<len; i++)		/* copy string to new space */
    new_s[i] = s[i];
  new_s[i] = '\0';

  return new_s;
}

		/* Copy expr token src text into local stringspace. */

#define MAXTREETEXT (20*72+1)	/* Enough space for any f77 expression. */
PRIVATE char tree_text_space[MAXTREETEXT];

char *
#if HAVE_STDC
new_tree_text(Token *t)
#else /* K&R style */
new_tree_text(t)
     Token *t;
#endif /* HAVE_STDC */
{
  (void) cp_tree_src_text(tree_text_space, t, MAXTREETEXT-1);
  return new_local_string(tree_text_space);
}



Token *
new_token(VOID)			/* Returns pointer to space for a token */
{
  if(token_space_top == TOKENSPACESZ) {
	/* When token space is used up, go to the next.  If none, then
	   allocate a new one.  The memory is never deallocated, since
	   it will likely be needed again later.  So token space structs
	   are linked into a list. */
    if(curr_tokspace->next == (TokenSpace *)NULL) {
      TokenSpace *new_tokspace;
      if( (new_tokspace = (TokenSpace *)malloc(sizeof(TokenSpace)))
	 == (TokenSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for tokens");
	return (Token *)NULL; /*NOTREACHED*/
      }
      else {
	new_tokspace->next =  (TokenSpace *)NULL;
	curr_tokspace->next = new_tokspace;
      }
    }
    curr_tokspace = curr_tokspace->next;
    extra_tokspace += TOKENSPACESZ; /* Keep track of how much for -resource */
    token_space_top = 0;
  }
  return curr_tokspace->tokenspace + token_space_top++;
}

