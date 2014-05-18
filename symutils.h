/*  $Id: symutils.h,v 1.2 2001/10/07 22:57:35 moniot Rel $

  Prototypes of routines defined in symutils.c

*/


PROTO(unsigned arg_count,( const Token *t ));
PROTO(int cp_tree_src_text,( char *s, const Token *t, int max ));
PROTO(int cp_tok_src_text,( char *s, const Token *t, int max ));
PROTO(int cp_list_src_text,( char *s, const Token *t, int max ));
PROTO(Token * reverse_tokenlist,( Token *t ));
