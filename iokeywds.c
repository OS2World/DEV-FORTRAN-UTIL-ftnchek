/* 	$Id: iokeywds.c,v 1.5 2003/03/20 22:15:21 moniot Exp $

   Routines to handle I/O keywords as in OPEN statement, and related matters.

*/

#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "tokdefs.h"
#include "iokeywds.h"
#include "utils.h"	/* we use strcasecmp */

		/* variables shared with fortran.y */
extern int io_internal_file, io_list_directed;
extern int current_io_unit_id;
extern int current_io_unit_no;
extern IO_ACCESS_TYPE current_io_access;
extern IO_FORM_TYPE current_io_form;	

PROTO(PRIVATE int find_io_keyword,( char *s ));

	/* use_io_keyword handles keyword=value fields in i/o control lists */


void
#if HAVE_STDC
use_io_keyword(Token *keyword, Token *value, int stmt_class)
#else /* K&R style */
use_io_keyword(keyword,value,stmt_class)
     Token *keyword,*value;
     int stmt_class;
#endif /* HAVE_STDC */
{
    unsigned i;
    int k, stmt_flag=0, type_flag, setit,useit;
    int hkey=keyword->value.integer;

		/* Convert statement_class (a token class) into
		   a bit flag compatible with io_keywords table. */
    for(i=0; i<NUM_IO_STMTS; i++) {
	if(local_class[i].stmt_class == stmt_class) {
	    stmt_flag = local_class[i].stmt_flag;
	    break;
	}
    }
    if(stmt_flag == 0) {
      oops_message(OOPS_NONFATAL,keyword->line_num,keyword->col_num,
	"not an i/o statement class:");
      (void)fprintf(stderr,"%d",stmt_class);
      return;
    }
		/* Convert value datatype into
		   a bit flag compatible with io_keywords table.
		   Note that '*' is handled by using type_UNDECL */
    if(value->tclass == '*')
	type_flag = STAR;
    else
	type_flag = (1<<datatype_of(value->TOK_type));

				/* Look up keyword in table*/
    k = find_io_keyword(hashtab[hkey].name);

		/* Not found or nonstandard: issue warning.  A warning is
		   always given (except under -nocheck) when a keyword
		   is not recognized, since this can mean something is not
		   processed correctly.
		 */
    if( io_keywords[k].nonstandard
	|| io_keywords[k].nonf90
#ifdef ALLOW_VMS_IO /* special VMS case: OPEN(...,NAME=str,...) */
       || (io_keywords[k].vms_name && stmt_flag==OP)
#endif /*ALLOW_VMS_IO*/
	   ) {
		/* If nonstandard and -f77 or -f90 flag given, issue warning */
	if( (io_keywords[k].nonstandard && f77_io_keywords)
	    || (io_keywords[k].nonf90 && f90_io_keywords) ) {
	  nonstandard(keyword->line_num,keyword->col_num,
		      (io_keywords[k].nonf90 && f90_io_keywords),0);
	}


	if(io_keywords[k].name == NULL) {
	    if( f77_io_keywords || f90_io_keywords ) {
	      /* abbrev warning if nonstd message given */
		msg_tail(": unrecognized keyword");
	    }
	    else {
	      if(misc_warn) {
		warning(keyword->line_num,keyword->col_num,
			"Unrecognized keyword");
	      }
	    }
	    if(f77_io_keywords || f90_io_keywords || misc_warn) {
	      msg_tail(hashtab[hkey].name);
	      msg_tail("--  Ftnchek may process incorrectly");
	    }
	}
    }

	/* If label expected, switch integer const to label */
    if( (LAB & io_keywords[k].allowed_types)
       &&  (type_flag == INT && is_true(LIT_CONST,value->TOK_flags))) {
	type_flag = LAB;
    }

	/*  Now check it out */


		/* Check if keyword is allowed with statement */

    if(!(stmt_flag & io_keywords[k].allowed_stmts)) {
	syntax_error(keyword->line_num,keyword->col_num,
		     "keyword illegal in this context");
	return;
    }

		/* Check if the type is OK */

    if( !(type_flag & io_keywords[k].allowed_types) ) {
	syntax_error(value->line_num,value->col_num,
		     "control specifier is incorrect type");
	return;
    }


	/* Now handle usage */

				/* internal file?: WRITE(UNIT=str,...) */
    if(stmt_flag == WR && type_flag == CHR
	    && io_keywords[k].allowed_types == UID) {
	setit = TRUE;
	useit = FALSE;
    }
				/* INQUIRE: set it if inquire_set flag true */
    else if(stmt_flag == INQ && io_keywords[k].inquire_set) {
	setit = TRUE;
	useit = FALSE;
    }
     /* otherwise use use/set flags in table */
    else {
	useit = io_keywords[k].implies_use;
	setit = io_keywords[k].implies_set;
    }

				/* Keep note if format is '*' */
    if(value->tclass == '*' && io_keywords[k].allowed_types == FID ) {
      io_list_directed = TRUE;
    }

			/* Handle NML=namelist */
    if(type_flag == NML){
      ref_namelist(value,stmt_class);
      current_io_form = IO_FORM_FORMATTED; /* namelist I/O is formatted */
    }

			/* Handle UNIT==unit */
    if(io_keywords[k].allowed_types == UID) {
      if( type_flag == STAR ) {
	current_io_unit_id = IO_UNIT_DEFAULT; /* special code to signify default unit */
      }
      /* Be careful only to treat integer variables or literals as unit numbers.
	 Leave internal files as undefined units so they won't create records
	 in the I/O usage table.
       */
      else if( type_flag == INT ) {
	if(is_true(ID_EXPR,value->TOK_flags)){
	   current_io_unit_id = value->value.integer;	/* get hash code of identifier */
	}
	else if( is_true(LIT_CONST,value->TOK_flags) &&
		 value->TOK_type == type_byte(class_VAR,type_INTEGER)) {
	   current_io_unit_no = value->value.integer; /* get literal int value */
	}
      }
			/* Catch internal unit that is a const or expr
			   (F90 std 9.2.2.1) */
      else if( type_flag == CHR && !is_true(ID_EXPR,value->TOK_flags) ) {
	syntax_error(value->line_num,value->col_num,
		     "internal file must be a variable");
      }
    }

			/* Handle FMT=format */
    if(io_keywords[k].allowed_types == FID) {
      current_io_form = IO_FORM_FORMATTED;
      if( type_flag == LAB) {
	ref_label(value,LAB_IO);
      }
    }

			/* Handle END=label and ERR=label */
    if(io_keywords[k].allowed_types == LAB && type_flag == LAB) {
      ref_label(value,LAB_GOTO);
      if( style_goto ) {	/* warn if GOTO considered harmful */
	warning(keyword->line_num,keyword->col_num,
		"Obsolescent feature: branch to labeled statement");
      }
    }

			/*  Handle ACCESS=mode and FORM=mode */
    if( stmt_flag != INQ && (k == IOKW_ACCESS || k == IOKW_FORM) ) {
      if( type_flag == CHR ) { /* protect ourself: error was warned about already */
	char *modespec=(char *)NULL; /* place to record the literal string of value */
	if( is_true(LIT_CONST,value->TOK_flags) ) {
	  modespec = value->value.string;
	}
	else {	/* if specifier value is not literal, let's hope it's a parameter */
	  int h=value->value.integer;
	  Lsymtab *symt;
	  if((symt=hashtab[h].loc_symtab) != NULL) {
	    if(symt->parameter)
	      modespec = symt->info.param->value.string;/* whew! found it! */
	  } /* if not, current value will have to stay default */
	}
	if( modespec != (char *)NULL ) { /* if modespec was found, record it */

				/* Record which type of ACCESS specified */
	  if(k == IOKW_ACCESS) {
	    if( strcasecmp(modespec,"SEQUENTIAL") == 0 )
	      current_io_access = IO_ACCESS_SEQUENTIAL;
	    else if( strcasecmp(modespec,"DIRECT") == 0 )
	      current_io_access = IO_ACCESS_DIRECT;
	    else {
	      syntax_error(value->line_num,value->col_num,
			   "ACCESS specifier must be \"SEQUENTIAL\" or \"DIRECT\"");
	    }
	  }
				/*  Record which type of FORM specified */
	  else { /* k == IOKW_FORM */
	    if( strcasecmp(modespec,"FORMATTED") == 0 )
	      current_io_form = IO_FORM_FORMATTED;
	    else if( strcasecmp(modespec,"UNFORMATTED") == 0 )
	      current_io_form = IO_FORM_UNFORMATTED;
	    else {
	      syntax_error(value->line_num,value->col_num,
			   "FORM specifier must be \"FORMATTED\" or \"UNFORMATTED\"");
	    }
	  }
	}
      }
    }

				/* handle REC=rec in READ or WRITE */
    if( k == IOKW_REC ) {
      current_io_access = IO_ACCESS_DIRECT;
    }

			/* Update usage status if a variable. */
    if( is_true(ID_EXPR,value->TOK_flags)) {
	if(useit) {
	    use_variable(value);
	}
	if(setit) {
	    use_lvalue(value);
	}
		/* Character variable as unit id = internal file */
	if(type_flag == CHR && io_keywords[k].allowed_types == UID) {
	  io_internal_file = TRUE;
	}
    }
    else if(setit) {		/* if value is set, must be an lvalue */
	    syntax_error(value->line_num,value->col_num,
			 "variable required");
	    return;
    }
}

void
record_io_usage( Token *stmt )
{

     /* First, see if we have something to record.  (We don't record I/O to internal
	files, for instance.)  Either id or number should be defined.
      */
  if( current_io_unit_id != IO_UNIT_UNKNOWN || current_io_unit_no != IO_UNIT_UNKNOWN ) {

     /* Allocate space for table if that was not done as yet */
    if(io_unit_info == (IO_Unit_Info*)NULL) {
      max_io_unit_usages = IO_UNIT_INFO_SIZE;
      io_unit_info = (IO_Unit_Info*)malloc(max_io_unit_usages*sizeof(IO_Unit_Info));
    }
    /* Reallocate space if table has filled up */
    if(num_io_unit_usages >= max_io_unit_usages) {
      max_io_unit_usages *= 2;	/* double the table size */
      io_unit_info = (IO_Unit_Info*)realloc(io_unit_info,
					    max_io_unit_usages*sizeof(IO_Unit_Info));
    }

			/* Now insert the new entry.  If the unit_id is a parameter,
			   look it up so the numerical value will be known too.
			 */
    if( current_io_unit_id >= 0 ) {
      Lsymtab *symt;
      if((symt=hashtab[current_io_unit_id].loc_symtab) != NULL) { /* ultra bogus if not in symtab */
	if( symt->type == type_byte(class_VAR,type_INTEGER) &&
	    symt->parameter)	{  /* Can we do business? */
	     current_io_unit_no = symt->info.param->value.integer;
	}
      }
    }

    io_unit_info[num_io_unit_usages].line_num = stmt->line_num;
    io_unit_info[num_io_unit_usages].unit_no = current_io_unit_no;
    io_unit_info[num_io_unit_usages].unit_id = current_io_unit_id;
    io_unit_info[num_io_unit_usages].io_access = current_io_access;
    io_unit_info[num_io_unit_usages].io_form = current_io_form;
    io_unit_info[num_io_unit_usages].io_operation = curr_stmt_class;
    ++num_io_unit_usages;
  }
}

		/* Handle VMS OPEN keywords that have no =value */
void
#if HAVE_STDC
use_special_open_keywd(Token *id)
#else /* K&R style */
use_special_open_keywd(id)
     Token *id;
#endif /* HAVE_STDC */
{
#ifdef ALLOW_VMS_IO
  unsigned i;
  char *id_name= hashtab[id->value.integer].name;

  for(i=0; i<NUM_SPECIAL_OPEN_KEYWDS; i++) {
    if(strcmp(id_name,special_open_keywds[i]) == 0) {
				/* found: report nonstandard if requested */
      if(f77_io_keywords || f90_io_keywords)
	nonstandard(id->line_num,id->col_num,f90_io_keywords,0);
      return;
    }
  }
#endif/*ALLOW_VMS_IO*/
				/* not found or not VMS: report error */
  syntax_error(id->line_num,id->col_num,
	       "Illegal control-list item");
}

	/* find_io_keyword looks up an i/o keyword in io_keywords
	   table and returns its index.  Uses simple linear search
	   since not worth hash overhead.  If not found, returns
	   index of last element of list, which is special. */
PRIVATE int
#if HAVE_STDC
find_io_keyword(char *s)
             			/* given name */
#else /* K&R style */
find_io_keyword(s)
     char *s;			/* given name */
#endif /* HAVE_STDC */
{
    int i;
    for(i=0; io_keywords[i].name != NULL; i++) {
	if(strcmp(io_keywords[i].name, s) == 0) {
	    break;
	}
    }
    return i;
}

