/* $Id: symspace.h,v 1.3 2001/09/24 01:42:16 moniot Rel $


   Prototypes of routines & shared variables defined in symspace.c

*/

PROTO(TokenListHeader * make_TL_head,( Token *t ));
PROTO(char * new_local_string,( char *s ));
PROTO(ParamInfo * new_param_info,( void ));

SYM_SHARED long
  parameter_count;	/* Count of parameters for keeping them in order */

