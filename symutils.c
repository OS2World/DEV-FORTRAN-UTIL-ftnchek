/* $Id: symutils.c,v 1.3 2001/10/07 22:57:23 moniot Rel $

   Routines used by symbol-table processing routines.

 */

#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "symutils.h"

unsigned
#if HAVE_STDC
arg_count(const Token *t)            /* Counts the number of arguments in a token list */
#else /* K&R style */
arg_count(t)            /* Counts the number of arguments in a token list */
	Token *t;
#endif /* HAVE_STDC */
{
	unsigned count;
	count = 0;
	while(t != NULL){
		count++;
		t = t->next_token;
	}
	return(count);
}

		/* Routines to copy src text strings from an
		   expression tree into a char array.  Given max
		   no. of chars (excl. nul) to transfer.  Result is
		   always nul-terminated.  Total no. of non-nul chars
		   stored is returned. */

int
#if HAVE_STDC
cp_tok_src_text(char *s, const Token *t, int max)	/* Copies src text from a token */
             			/* The destination string */
              			/* Expression tree */
             			/* Max no. of chars to transfer (excl. nul)*/
#else /* K&R style */
cp_tok_src_text(s,t,max)	/* Copies src text from a token */
     char *s;			/* The destination string */
     Token *t;			/* Expression tree */
     int max;			/* Max no. of chars to transfer (excl. nul)*/
#endif /* HAVE_STDC */
{
  int i,j;

#ifndef LEX_RAWSTRINGS
  if( ! is_true(LIT_CONST,t->TOK_flags)
     || t->TOK_type != type_byte(class_VAR,type_STRING))
#endif
  {
    j=0;
#if 0 /* this needs to be done only for actual, not dummy arg */
    if(t->TOK_type == type_byte(class_LABEL,type_LABEL))
      s[j++] = '*';		/* for subroutine arg = *label  */
#endif
    for(i=0; j<max && t->src_text[i] != '\0'; i++) {
      s[j++] = t->src_text[i];
    }
  }

#ifndef LEX_RAWSTRINGS
  else {                        /* Strings must be undigested */
    int  quote_char;
    quote_char = t->src_text[0];
    for(i=j=0; j<max && t->src_text[i] != '\0'; i++) {
      s[j++] = t->src_text[i];
      if(i>0 && t->src_text[i] == quote_char) /* Double a quoted quote */
	if(j < max)
	  s[j++] = quote_char;
    }
    if(j < max)
      s[j++] = quote_char; /* Add the final quote */
  }
#endif
  s[j] = '\0';			/* Terminate with nul character */
  return j;			/* Return total xferred */
}

int
#if HAVE_STDC
cp_tree_src_text(char *s, const Token *t, int max)	/* Copies src text from expr tree */
             			/* The destination string */
              			/* Expression tree */
             			/* Max number of chars to transfer (exc. nul)*/
#else /* K&R style */
cp_tree_src_text(s,t,max)	/* Copies src text from expr tree */
     char *s;			/* The destination string */
     Token *t;			/* Expression tree */
     int max;			/* Max number of chars to transfer (exc. nul)*/
#endif /* HAVE_STDC */
{
  int ncopied=0;
  if(t != NULL) {
    if(t->left_token == NULL) {	/* Primary */
      ncopied += cp_tok_src_text(s+ncopied,t,max-ncopied);
    }
    else {			/* Expr tree */
      if(t->next_token != (Token *)NULL) {

				/* binary subtree */
        ncopied += cp_tree_src_text(s+ncopied,t->left_token,max-ncopied);

		/* root node */
	ncopied += cp_tok_src_text(s+ncopied,t,max-ncopied);

        if(t->tclass == '(') {     /* Array, substring, or function ref */
	  ncopied += cp_list_src_text(s+ncopied,t->next_token,max-ncopied);
	  if(max-ncopied > 0) {
	    s[ncopied++] = ')'; /* Add left parenthesis */
	    s[ncopied] = '\0';
	  }
	}
	else {
	  ncopied += cp_tree_src_text(s+ncopied,t->next_token,max-ncopied);
	}
      }
      else {
				/* parent node */
        ncopied = cp_tok_src_text(s+ncopied,t,max-ncopied);

				/* unary subtree */
        ncopied += cp_tree_src_text(s+ncopied,t->left_token,max-ncopied);

        if(t->tclass == '(') {     /* Parenthesized subexpression */
	  if(max-ncopied > 0) {
	    s[ncopied++] = ')'; /* Add left parenthesis */
	    s[ncopied] = '\0';
	  }
	}
      }
    }
  }
  return ncopied;
}

int
#if HAVE_STDC
cp_list_src_text(char *s, const Token *t, int max)	/* Copies text from a tokenlist */
             			/* The destination string */
              			/* Expression tree */
             			/* Max number of chars to transfer (exc. nul)*/
#else /* K&R style */
cp_list_src_text(s,t,max)	/* Copies text from a tokenlist */
     char *s;			/* The destination string */
     Token *t;			/* Expression tree */
     int max;			/* Max number of chars to transfer (exc. nul)*/
#endif /* HAVE_STDC */
{
  int ncopied=0;
  while( t != NULL) {
    if(t->left_token == NULL) {	/* Primary */
      ncopied += cp_tok_src_text(s+ncopied,t,max-ncopied);
    }
    else {
				/* Print tree at this point in list */
      ncopied += cp_tree_src_text(s+ncopied,t->left_token,max-ncopied);
    }
    t = t->next_token;
    if(t != NULL) {		/* If next one coming, print the comma */
      if(max-ncopied > 0) {    /* Parenthesized subexpression */
	s[ncopied++] = ',';
	s[ncopied] = '\0';
      }
    }
  }
  return ncopied;
}

		/* this guy reverses a tokenlist and returns a pointer
		   to the new head. */
Token *
#if HAVE_STDC
reverse_tokenlist(Token *t)
#else /* K&R style */
reverse_tokenlist(t)
	Token *t;
#endif /* HAVE_STDC */
{
	Token *curr,*next,*temp;

	if(t == NULL)
	    return t;

	curr = t;
	next = curr->next_token;
	while(next != NULL) {
		temp = next->next_token;
		next->next_token = curr;
		curr = next;
		next = temp;
	}
	t->next_token = NULL;		/* former head is now tail */
	return curr;			/* curr now points to new head */
}
