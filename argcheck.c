/* $Id: argcheck.c,v 1.10 2002/12/15 19:43:15 moniot Rel $

	Routines to check subprogram type and argument agreement

*/

/*


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

	Shared functions defined:

		check_arglists()  Scans global symbol table for subprograms
				  and finds subprogram defn if it exists.
*/
#include <stdio.h>
#include <string.h>

#include "ftnchek.h"
#include "symtab.h"
#include "pgsymtab.h"

				/* Local functions defined */

PROTO(PRIVATE void arg_array_cmp,( char *name, ArgListHeader *args1,
			   ArgListHeader *args2 ));


     		/* Compares subprogram calls with definition */
PRIVATE void
#if HAVE_STDC
arg_array_cmp(char *name, ArgListHeader *args1, ArgListHeader *args2)
#else /* K&R style */
arg_array_cmp(name,args1,args2)
	char *name;
	ArgListHeader *args1, *args2;
#endif /* HAVE_STDC */
{
	int i;
	int  n,
	     n1 = args1->numargs,
	     n2 = args2->numargs;
	ArgListElement *a1 = args1->arg_array,
		       *a2 = args2->arg_array;

	n = (n1 > n2) ? n2: n1;		/* n = min(n1,n2) */

	if (argcheck_argnumber && n1 != n2){
	  cmp_error_count = 0;
	  (void) argcmp_error_head(name,args1,"varying number of arguments:");

	  sub_error_report(args1,args1->is_defn? "Defined":"Invoked");
	  msg_tail("with");
	  msg_tail(ulongtostr((unsigned long)n1));
	  msg_tail(n1==1?"argument":"arguments");

	  sub_error_report(args2,args2->is_defn? "Defined":"Invoked");
	  msg_tail("with");
	  msg_tail(ulongtostr((unsigned long)n2));
	  msg_tail(n2==1?"argument":"arguments");
	}

	if(argcheck_argtype)
	{	/* Look for type mismatches */
	    cmp_error_count = 0;
	    for (i=0; i<n; i++) {
	      int c1 = storage_class_of(a1[i].type),
	          c2 = storage_class_of(a2[i].type),
		  t1 = datatype_of(a1[i].type),
	          t2 = datatype_of(a2[i].type);
	      long
		  s1 = a1[i].size,
		  s2 = a2[i].size;
	      int
		  defsize1 = (s1==size_DEFAULT),
		  defsize2 = (s2==size_DEFAULT);
				/* cmptype is type to use for mismatch test.
				   Basically cmptype=type but DP matches
				   REAL, DCPX matches CPLX, and hollerith
				   matches any numeric or logical type
				   but not  character.  The single/double
				   match will be deferred to size check. */
	      int cmptype1, cmptype2;

		/* If -portability, do not translate default sizes so
		   they will never match explicit sizes. */
	      if( port_mixed_size || local_wordsize==0 ) {
		cmptype1 = (t1==type_HOLLERITH && t2!=type_STRING)?
				t2:t1;
		cmptype2 = (t2==type_HOLLERITH && t1!=type_STRING)?
				t1:t2;
	      }
	      else {
		cmptype1 = (t1==type_HOLLERITH && t2!=type_STRING)?
				t2:type_category[t1];
		cmptype2 = (t2==type_HOLLERITH && t1!=type_STRING)?
				t1:type_category[t2];
		if(defsize1)
		  s1 = type_size[t1];
		if(defsize2)
		  s2 = type_size[t2];
	      }


	      if(s1 < 0 || s2 < 0) { /* char size_ADJUSTABLE or UNKNOWN */
		s1 = s2 = size_DEFAULT;	/* suppress warnings on size */
		defsize1 = defsize2 = TRUE;
	      }

			 /* Require exact match between storage classes and
			    compatible data type.  If that is OK, then for
			    non-char args require exact size match.  For char
			    and hollerith defer size check to other section.
			  */
	    if( (c1 != c2) || (cmptype1 != cmptype2) || ( (s1 != s2) &&
			is_num_log_type(t1) && is_num_log_type(t2) ) ) {
		if(argcmp_error_head(name,args1,"argument data type mismatch"))
		  break;

		arg_error_report(args1,args1->is_defn? "Dummy arg": "Actual arg",i,
				 "is type");
		if( t1 != type_LABEL ) /*label arg: only print storage class*/
		  msg_tail(typespec(t1,!defsize1,(long)s1,FALSE,0));
		msg_tail(class_name[storage_class_of(a1[i].type)]);
				     
		arg_error_report(args2,args2->is_defn? "Dummy arg": "Actual arg",i,
				 "is type");
		if( t2 != type_LABEL ) /*label arg: only print storage class*/
		  msg_tail(typespec(t2,!defsize2,(long)s2,FALSE,0L));
		msg_tail(class_name[storage_class_of(a2[i].type)]);
				 
		if(args1->is_defn
			&& storage_class_of(a1[i].type) == class_SUBPROGRAM
			&& storage_class_of(a2[i].type) != class_SUBPROGRAM
			&& datatype_of(a1[i].type) != type_SUBROUTINE
			&& ! a1[i].declared_external )
		  (void)fprintf(list_fd,
		     "\n    (possibly it is an array which was not declared)");
	      }
				/* If no class/type/elementsize clash,
				   and if comparing dummy vs. actual,
				   check character and hollerith sizes */
	      else if(args1->is_defn) {
				/* Character: check size but skip *(*)
				   and dummy array vs. actual array element.
				 */
		if(t1 == type_STRING && s1 > 0 && s2 > 0 &&
		  !(a1[i].array_var && a2[i].array_element)) {
		    long
		      dims1,dims2,size1,size2;

		    if(a1[i].array_var) {
		      dims1 = array_dims(a1[i].info.array_dim);
		      size1 = array_size(a1[i].info.array_dim);
		    }
		    else {
		      dims1 = 0;
		      size1 = 1;
		    }
		    if(a2[i].array_var && !a2[i].array_element) {
		      dims2 = array_dims(a2[i].info.array_dim);
		      size2 = array_size(a2[i].info.array_dim);
		    }
		    else {
		      dims2 = 0;
		      size2 = 1;
		    }

				/* standard requires dummy <= actual size.
			         */
		  if( (s1*size1 > s2*size2 &&
		      (dims1==0 || size1>1) && (dims2==0 || size2>1)) ) {

		    if(argcmp_error_head(name,args1,"argument mismatch"))
				break;

		    arg_error_report(args1,"Dummy arg",i,"is type");
		    msg_tail(typespec(t1,TRUE,(long)s1,dims1>0,size1));

		    arg_error_report(args2,"Actual arg",i,"is type");
		    msg_tail(typespec(t2,TRUE,(long)s2,dims2>0,size2));

		  }/*end if char size mismatch*/
		}/*end if type==char*/

		else if(t2 == type_HOLLERITH) {
			/* Allow hollerith to match any noncharacter type of
			   at least equal aggregate size.  */
		    long dims1,size1;
		    if(a1[i].array_var) {
		      dims1 = array_dims(a1[i].info.array_dim);
		      size1 = array_size(a1[i].info.array_dim);
		    }
		    else {
		      dims1 = 0;
		      size1 = 1;
		    }
		    if(s2 > s1*size1 && (dims1==0 || size1>1)) {
		      if(argcmp_error_head(name,args1,"argument mismatch"))
				break;

		      arg_error_report(args1,"Dummy arg",i,"is type");
		      msg_tail(typespec(t1,!defsize1,(long)s1,dims1>0,size1));

		      arg_error_report(args2,"Actual arg",i,"is type");
		      msg_tail(typespec(t2,TRUE,(long)s2,FALSE,0L));

		    }/*end if holl size mismatch*/
		}/*end if type==holl*/
	      }
	    }/*end for i*/
	}/* end look for type && size mismatches */


		 /* Check arrayness of args only if defn exists */
	if(argcheck_arrayness && args1->is_defn ) {
	    cmp_error_count = 0;
	    for (i=0; i<n; i++) {
			/* Skip if class or datatype mismatch.  This
			   also skips holleriths which were checked above.
			   Do not process externals.
			 */
	      if(datatype_of(a2[i].type) != type_HOLLERITH &&
		 storage_class_of(a1[i].type) == class_VAR &&
		 storage_class_of(a2[i].type) == class_VAR) {

		if( a1[i].array_var ) {	/* I. Dummy arg is array */
		    if( a2[i].array_var ) {
			if( a2[i].array_element ) {
					/*   A. Actual arg is array elt */
					/*	Warn on arraycheck_dims. */
			    if(arraycheck_dims) {

			      if(argcmp_error_head(
				      name,args1,"argument arrayness mismatch"))
				break;

			      arg_error_report(args1,"Dummy arg",i,"is whole array");
			      arg_error_report(args2,"Actual arg",i,"is array element");
			    }
			}/* end case I.A. */

			else {
					/*   B. Actual arg is whole array */
					/*	Warn if dims or sizes differ */
			  unsigned long
			    diminfo1,diminfo2;
			  long dims1,dims2,size1,size2,
			    cmpsize1,cmpsize2;
			  diminfo1 = a1[i].info.array_dim;
			  diminfo2 = a2[i].info.array_dim;
			  dims1 = array_dims(diminfo1);
			  dims2 = array_dims(diminfo2);
			  cmpsize1 = size1 = array_size(diminfo1);
			  cmpsize2 = size2 = array_size(diminfo2);
				/* For char arrays relevant size is no. of
				   elements times element size. But use
				   no. of elements if *(*) involved. */
			  if(datatype_of(a1[i].type) == type_STRING
			     && a1[i].size > 0 && a2[i].size > 0) {
			    cmpsize1 *= a1[i].size;
			    cmpsize2 *= a2[i].size;
			  }

			/* size = 0 or 1 means variable-dim: OK to differ */
			  if( (arraycheck_size &&
				  (size1>1 && size2>1 && cmpsize1 != cmpsize2))
			     || (arraycheck_dims &&
				  (dims1 != dims2)) ) {
			        char sizebuf[2+2*MAX_ULONGTOSTR];
				if(argcmp_error_head(
					name,args1,"argument arrayness mismatch"))
				      break;

				arg_error_report(args1,"Dummy arg",i,"has");
				msg_tail(ulongtostr((unsigned long)dims1));
				msg_tail(dims1==1?"dim":"dims");
				msg_tail("size");
				strcpy(sizebuf,ulongtostr((unsigned long)size1));
				if(datatype_of(a1[i].type) == type_STRING
				   && a1[i].size > 0) {
				    strcat(sizebuf,"*");
				    strcat(sizebuf,ulongtostr((unsigned long)(a1[i].size)));
				}
				msg_tail(sizebuf);

				arg_error_report(args2,"Actual arg",i,"has");
				msg_tail(ulongtostr((unsigned long)dims2));
				msg_tail(dims2==1?"dim":"dims");
				msg_tail("size");
				strcpy(sizebuf,ulongtostr((unsigned long)size2));
				if(datatype_of(a2[i].type) == type_STRING
				   && a2[i].size > 0) {
				    strcat(sizebuf,"*");
				    strcat(sizebuf,ulongtostr((unsigned long)(a2[i].size)));
				}
				msg_tail(sizebuf);
			  }/* end if size mismatch */
			}/* end case I.B. */
		    }
		    else {
					/*   C. Actual arg is scalar */
					/*	Warn in all cases */

		      	if(argcmp_error_head(
				name,args1,"argument arrayness mismatch"))
			  break;

			arg_error_report(args1,"Dummy arg",i,"is array");
			arg_error_report(args2,"Actual arg",i,"is scalar");
		    }/* end case I.C. */
		} /* end dummy is array, case I. */

		else {			/* II. Dummy arg is scalar */
		    if( a2[i].array_var ) {
			if( a2[i].array_element ) {
					/*   A. Actual arg is array elt */
					/*	OK */
			}
			else {
					/*   B. Actual arg is whole array */
					/*	Warn in all cases */

			  if(argcmp_error_head(
				   name,args1,"argument arrayness mismatch"))
			    break;

			  arg_error_report(args1,"Dummy arg",i,"is scalar");
			  arg_error_report(args2,"Actual arg",i,"is whole array");

			}/* end case II.B. */
		    }
		    else {
					/*   C. Actual arg is scalar */
					/*	OK */
		    }

		} /* end dummy is scalar, case II */

	      } /* end if class_VAR */
	    }/* end for (i=0; i<n; i++) */
	}/* if( args1->is_defn ) */


		 /* Check usage of args only if defn exists.  Arg array
		    1 is dummy args, array 2 is actual args.  */
	if( (usage_arg_modified || usage_arg_alias_modified ||
	     usage_array_alias_modified || usage_var_uninitialized ||
	     usage_arg_common_modified || usage_array_common_modified ||
	     usage_do_var_modified)
					&& args1->is_defn ) {
	    cmp_error_count = 0;
	    for (i=0; i<n; i++) {
	      if(storage_class_of(a1[i].type) == class_VAR &&
		 storage_class_of(a2[i].type) == class_VAR ) {
		int nonlvalue_out = (a1[i].assigned_flag && !a2[i].is_lvalue);
		int nonset_in = (a1[i].used_before_set && !a2[i].set_flag);
		int alias_modified = (a1[i].set_flag && (a2[i].same_as != i));
		int arg_alias_modified = (alias_modified && !a2[i].array_var);
		int array_alias_modified = (alias_modified && a2[i].array_var);
		int do_var_modified = (a1[i].set_flag && a2[i].active_do_var);
		int common_modified_as_arg, common_modified_as_com, /*maybe*/
		    common_assigned_as_arg, common_assigned_as_com; /*for sure*/
		int arg_common_modified, /*nonarray arg*/
		    array_common_modified; /*array arg*/
		char *common_alias_name = NULL;
				/* See if arg aliased to common variable, and
				   if so, is either one modified. */
		common_modified_as_arg = common_modified_as_com = FALSE;
		common_assigned_as_arg = common_assigned_as_com = FALSE;
		if( a2[i].common_block != 0 ) {
			/* Find out if block is defined in called routine */
		  ComListHeader *clist = a2[i].common_block->info.comlist;
		  while( clist != NULL ) {
		    if( clist->module == args1->module ) {
		      break;	/* found it */
		    }
		    clist = clist->next;
		  }
		  if( clist != NULL ) {	/* block is defined in called module */
		      if( comcheck_by_name ) { /* Exact common: find the var */
				/* It is not yet an error unless the block
				   is also long enough to include the variable
				   and the variable is modified in either
				   place.
				*/
  			if( a2[i].common_index <= clist->numargs ) {
			/* Don't forget that index goes from 1 to numargs.*/
			  int j = a2[i].common_index - 1;
			  common_alias_name = clist->com_list_array[j].name;
			  common_modified_as_arg = a1[i].set_flag;
			  common_assigned_as_arg = a1[i].assigned_flag;
			  common_modified_as_com = clist->com_list_array[j].set;
			  common_assigned_as_com = clist->com_list_array[j].assigned;
			}
		      }
		      else {	/* Inexact common: just see if block or
				   variable is modified.  Don't set
				   assigned_as_com to always say "may be".
				*/
			common_modified_as_arg = a1[i].set_flag;
			common_assigned_as_arg = a1[i].assigned_flag;
			common_modified_as_com = clist->any_set;
			common_assigned_as_com = FALSE;
		      }
		  } /* clist != NULL */
		} /* a2[i].common_block != 0 */
		arg_common_modified =
		  ((common_modified_as_arg || common_modified_as_com) && !a2[i].array_var);
		array_common_modified =
		  ((common_modified_as_arg || common_modified_as_com) && a2[i].array_var);

#ifdef DEBUG_PGSYMTAB
if(debug_latest) {
(void)fprintf(list_fd,
"\nUsage check: %s[%d] dummy asgnd %d ubs %d  actual lvalue %d set %d do %d",
args1->module->name,
i+1,
a1[i].assigned_flag,
a1[i].used_before_set,
a2[i].is_lvalue,
a2[i].set_flag,
a2[i].active_do_var);
}
#endif

		if( (usage_arg_modified && nonlvalue_out) ||
		    (usage_var_uninitialized && nonset_in)||
		    (usage_arg_alias_modified && arg_alias_modified)||
		    (usage_array_alias_modified && array_alias_modified)||
		    (usage_arg_common_modified && arg_common_modified)||
		    (usage_array_common_modified && array_common_modified)||
		    (usage_do_var_modified && do_var_modified) ) {

		  if(argcmp_error_head(name,args1,"argument usage mismatch"))
		     break;

			/* Usage case 1: Modifying arg that is constant or
			   expression.
			*/
		  if(usage_arg_modified && nonlvalue_out) {
		    arg_error_report(args1,"Dummy arg",i,"is modified");
		    arg_error_report(args2,"Actual arg",i,"is const or expr");
		  }

			/* Usage case 2: Using arg that is not set.
			*/
		  if(usage_var_uninitialized && nonset_in) {

		    arg_error_report(args1,"Dummy arg",i,"is used before set");
		    arg_error_report(args2,"Actual arg",i,"is not set");
		  }

			/* Usage case 3: Modifying arg that is the same as
			   another arg.
			*/
		  if((usage_arg_alias_modified && arg_alias_modified)||
		    (usage_array_alias_modified && array_alias_modified)) {
		    arg_error_report(args1,"Dummy arg",i,
			     a1[i].assigned_flag?
				     "is modified":
				     "may be modified");

		    arg_error_report(args2,"Actual arg",i,
				     a2[i].array_var?
					"may be same as arg":
					"same as arg");
		    msg_tail(ulongtostr((unsigned long)(long)(a2[i].same_as+1)));
		    msg_tail(":");
		    msg_tail(a2[a2[i].same_as].name);

		  }

			/* Usage case 4: Modifying arg that is the same as
			   a variable in common.
			*/
		  if((usage_arg_common_modified && arg_common_modified)||
		     (usage_array_common_modified && array_common_modified)) {
		    char locspec[10+3*sizeof(a2[i].common_index)];
		    if( comcheck_by_name ) {
		      (void)sprintf(locspec,"%ld:",a2[i].common_index);
		    }
		    else {
		      (void)sprintf(locspec,"somewhere");
		    }
		    arg_error_report(args1,"Dummy arg",i,"is aliased to common var");
		    msg_tail(locspec);
		    msg_tail(comcheck_by_name?common_alias_name: "");
		    msg_tail("in block");
		    msg_tail(a2[i].common_block->name);
		    msg_tail(common_modified_as_com?
				   (common_assigned_as_com?
				     "which is modified":
				     "which may be modified"):
				   "");

                    if( common_modified_as_arg ) {
		      arg_error_report(args1,"Dummy arg",i,
				       common_assigned_as_arg?
					"is modified":
					"may be modified");
                    }

		    arg_error_report(args2,"Actual arg",i,"is in common block");
		    msg_tail(a2[i].common_block->name);
		  }

			/* Usage case 5: Modifying arg that is an active
			   DO index variable.
			 */
		  if( usage_do_var_modified && do_var_modified ) {
		    arg_error_report(args1,"Dummy arg",i,
			     a1[i].assigned_flag?
				     "is modified":
				     "may be modified");

		    arg_error_report(args2,"Actual arg",i,
				     "is active DO index");
		  }
		}
	      }
	    }
	}/*end if( (usage_arg...) && args->is_defn) */

}/* arg_array_cmp */



void
check_arglists(VOID)	/* Scans global symbol table for subprograms */
{                       /* and finds subprogram defn if it exists */
	int i;
	ArgListHeader *defn_list, *alist;

	for (i=0; i<glob_symtab_top; i++){

				/* Skip common blocks */
	    if(storage_class_of(glob_symtab[i].type) != class_SUBPROGRAM)
		continue;

				/* Skip unvisited library modules */
	    if(glob_symtab[i].library_module && !glob_symtab[i].visited)
		continue;


	    if((alist=glob_symtab[i].info.arglist) == NULL){
	      oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		      "global symbol has no argument lists:");
	      oops_tail(glob_symtab[i].name);
	    }
	    else{	/* alist != NULL */
		int num_defns= 0;
		ArgListHeader *list_item;

			/* use 1st invocation instead of defn if no defn */
		defn_list = alist;

				/* Find a definition in the linked list of
				   usages.  Count how many defns found. */
		list_item = alist;
		while(list_item != NULL){
		    if(list_item->is_defn){
					/* report multiple defns */
			if(usage_ext_multiply_defined && num_defns > 0) {
			    if(num_defns == 1) {
				cmp_error_count = 0;
				(void)argcmp_error_head(glob_symtab[i].name,
					       defn_list,
					       "multiply defined");
				sub_error_report(defn_list,"Defined");
			    }
			    sub_error_report(list_item,"Defined");
			}
			++num_defns;
			defn_list = list_item;	/* Use last defn found */
		    }
		    else { /* ! list_item->is_defn */
				/* Here treat use as actual arg like call */
			if(list_item->is_call || list_item->actual_arg){
				 /* Use last call by a visited or nonlibrary
				    module as defn if no defn found */
			  if(!defn_list->is_defn
			     && !irrelevant(list_item) )
			    defn_list = list_item;
		        }
		    }

		    list_item = list_item->next;
		}
		if(num_defns == 0){
				/* If no defn found, and all calls are
				   from unvisited library modules, skip. */
		  if(irrelevant(defn_list))
		    continue;

				/* If no definitions found, report error */
		   if( (usage_ext_undefined && glob_symtab[i].used_flag)
		    || (usage_ext_declared_only && !glob_symtab[i].used_flag) ) {
		     cmp_error_count = 0;
		     (void)argcmp_error_head(glob_symtab[i].name,
				defn_list,
				glob_symtab[i].used_flag?
					     "never defined":
					     "never defined nor invoked");
		     sub_error_report(defn_list,
				(defn_list->external_decl)?"Declared":"Invoked");

			/* Warn if it seems it may just be an array they
			   forgot to declare */
		      if(defn_list->numargs != 0
			 && datatype_of(defn_list->type) != type_SUBROUTINE
			 && ! glob_symtab[i].declared_external) {
			if(novice_help)
			  (void)fprintf(list_fd,
	    "\n    (possibly it is an array which was not declared)");
		      }
		   }
		}
				/* If definition is found but module is
				   not in call tree, report it unless -lib */
		else{	/* num_defns != 0 */
		    if(!glob_symtab[i].visited
		       && datatype_of(glob_symtab[i].type) != type_BLOCK_DATA
		       && !glob_symtab[i].library_module
		       && usage_ext_unused ) {
			cmp_error_count = 0;
			(void)argcmp_error_head(glob_symtab[i].name,
				   defn_list,
				   "never invoked");
			sub_error_report(defn_list,"Defined");
		    }
		}

			/* Now check defns/invocations for consistency.  If
			   no defn, 1st invocation will serve. Here treat
			   use as actual arg like call.  Ignore calls & defns
			   in unvisited library modules. */
		if( argcheck_functype &&
		   (defn_list->is_defn || !defn_list->external_decl)) {
		  cmp_error_count = 0;
		  while(alist != NULL){
			if(alist != defn_list && !alist->external_decl
			   && !irrelevant(alist)) {
			  int c1 = storage_class_of(defn_list->type),
			      c2 = storage_class_of(alist->type),
			      t1 = datatype_of(defn_list->type),
			      t2 = datatype_of(alist->type),
			      s1 = defn_list->size,
			      s2 = alist->size,
			      defsize1 = (s1 == size_DEFAULT),
			      defsize2 = (s2 == size_DEFAULT),
			      cmptype1= type_category[t1],
			      cmptype2= type_category[t2];
		/* If -portability, do not translate default sizes so
		   they will never match explicit sizes. */
			  if(!(port_mixed_size || local_wordsize==0)) {
			    if(defsize1)
			      s1 = type_size[t1];
			    if(defsize2)
			      s2 = type_size[t2];
			  }

			  if(s1 < 0 || s2 < 0){ /*size_ADJUSTABLE or UNKNOWN*/
			    s1 = s2 = size_DEFAULT;/* suppress size warnings */
			    defsize1 = defsize2 = TRUE;
			  }
				/* Check class, type, and size */
			  if( (c1 != c2) || (cmptype1 != cmptype2) ||
			     ( (s1 != s2) &&
				/*exclude char size-only mismatch betw calls */
			      (t1 != type_STRING ||
			        defn_list->is_defn || alist->is_defn )) ){

			    	if(argcmp_error_head(glob_symtab[i].name,
					      defn_list,
					      "invoked inconsistently"))
				    break;
				if(cmp_error_count == 1) {
				  sub_error_report(defn_list,
						   defn_list->is_defn?
						     "Defined":
						     "Invoked");
				  msg_tail("as type");
				  msg_tail(typespec(t1,!defsize1,(long)s1,
					   FALSE, 0L));
				}

				sub_error_report(alist,
					       alist->is_defn?
						 "Defined":
						 "Invoked");
				msg_tail("as type");
				msg_tail(typespec(t2,!defsize2,(long)s2,
					 FALSE, 0L));
			  }
			}
			alist = alist->next;

		  }/* end while(alist != NULL) */
	        }/* end if(defn) */

		alist = glob_symtab[i].info.arglist;
		while(alist != NULL){
		  /* Here we require true call, not use as actual arg.
		     Also, do not compare multiple defns against each
		     other. */
		    if(alist != defn_list &&
		       (defn_list->is_defn || defn_list->is_call) &&
		       (alist->is_call && !irrelevant(alist)) ){
			    arg_array_cmp(glob_symtab[i].name,defn_list,alist);
			}
			alist = alist->next;

		}/* end while(alist != NULL) */
	    }/* end else <alist != NULL> */
	}/* end for (i=0; i<glob_symtab_top; i++) */
}
