/* $Id: prlocsym.c,v 1.22 2003/03/17 17:47:56 moniot Exp $

	Prints local symbol table, calling local checking routines along
	the way.

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

		print_loc_symbols(curmodhash) Prints local symtab info.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "plsymtab.h"
#include "loccheck.h"

PROTO(PRIVATE void print_io_unit_usages,(VOID));

PROTO(PRIVATE void sort_io_unit_usages,(VOID));
PROTO(PRIVATE int cmp_io_units, ( IO_Unit_Info *u1, IO_Unit_Info *u2 ));

void
print_loc_symbols(VOID)
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    static Lsymtab **sym_list=(Lsymtab **)NULL;
#else
    Lsymtab *sym_list[LOCSYMTABSZ]; /* temp. list of symtab entries to print */
#endif
    int	mod_type,		/* datatype of this module */
	this_is_a_function;	/* flag for treating funcs specially */
    Lsymtab *module;	 	/* entry of current module in symtab */
    char *mod_name;		/* module name */
    int
	imps=0,			/* count of implicitly declared identifiers */
	numentries;		/* count of entry points of module */

    if (dcl_fd == (FILE*)NULL)
	dcl_fd = stdout;

#ifdef DYNAMIC_TABLES
    if(sym_list == (Lsymtab **)NULL) { /* Initialize if not done before */
      if( (sym_list=(Lsymtab **)calloc(LOCSYMTABSZ,sizeof(Lsymtab *)))
	 == (Lsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for local symbol list");
      }
    }
#endif

				/* Keep track of statement counts
				   for -resource  */
    tot_exec_stmt_count += exec_stmt_count;
    if(exec_stmt_count > max_exec_stmt_count)
	max_exec_stmt_count = exec_stmt_count;

			/* Keep track of symbol table and string usage */
    if(loc_symtab_top > max_loc_symtab) {
	max_loc_symtab = loc_symtab_top;
    }
    if(loc_str_top + extra_locstrspace > max_loc_strings) {
	max_loc_strings = loc_str_top + extra_locstrspace;
    }
    if(srctextspace_top + extra_srctextspace > max_srctextspace) {
      max_srctextspace = srctextspace_top + extra_srctextspace;
    }
    if(token_head_space_top + extra_tokheadspace > max_tokenlists) {
      max_tokenlists=token_head_space_top + extra_tokheadspace;
    }
    if(param_info_space_top + extra_paraminfospace > max_paraminfo) {
      max_paraminfo=param_info_space_top + extra_paraminfospace;
    }
    if(token_space_top + extra_tokspace > max_token_space) {
	max_token_space = token_space_top + extra_tokspace;
    }
    if(ptrspace_top + extra_ptrspace > max_ptrspace) {
      max_ptrspace = ptrspace_top + extra_ptrspace;
    }

    update_label_resources();	/* Do the same in label handler */

			/* Global symbols only increase in number */
    max_glob_symtab = glob_symtab_top;


		/* Set up name & type, and see what kind of module it is */

	      module = hashtab[current_module_hash].loc_symtab;

	      mod_name = module->name;
	      mod_type = get_type(module);

	      if(  mod_type != type_PROGRAM
		&& mod_type != type_SUBROUTINE
		&& mod_type != type_COMMON_BLOCK
		&& mod_type != type_BLOCK_DATA )
			this_is_a_function = TRUE;
	      else
			this_is_a_function = FALSE;

				/* Print name & type of the module */
    if(do_symtab) {
      int i;
      for(i=0,numentries=0;i<loc_symtab_top;i++) {
	if(loc_symtab[i].entry_point)
	  sym_list[numentries++] = &loc_symtab[i];
      }

	   if(numentries > 1) {
	      sort_lsymbols(sym_list,numentries);
	   }


	  (void)fprintf(list_fd,"\n\nModule %s:",mod_name);
	  if( this_is_a_function ) (void)fprintf(list_fd," func:");
	  (void)fprintf(list_fd," %4s",type_name[mod_type]);
			/* Print a * next to non-declared function name */
	  if(datatype_of(module->type) == type_UNDECL ) {
			(void)fprintf(list_fd,"*");
			imps++;
	  }
	  (void)fprintf(list_fd,"\n");


				/* Print Entry Points (skip if only one,
				   since it is same as module name) */
      if(do_symtab && numentries > 1) {
	      (void)fprintf(list_fd,"\nEntry Points\n");
	      (void) print_lsyms_briefly(sym_list,numentries,FALSE);
      }

			/* End of printing module name and entry points */
    }/*if(do_symtab)*/



				/* Print the externals */

    if(do_symtab) {
	int i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
	    if(storage_class_of(loc_symtab[i].type) == class_SUBPROGRAM) {
		  sym_list[n++] = &loc_symtab[i];
	    }
	}
	if(n != 0) {
	      sort_lsymbols(sym_list,n);

	      if (do_symtab)
	      {
		  (void)fprintf(list_fd,"\nExternal subprograms referenced:\n");
		  imps += print_lsyms_briefly(sym_list,n,TRUE);
	      }
	}

      }/*if(do_symtab)*/


				/* Print list of statement functions */
    if(do_symtab || usage_ext_unused) {
	   int i,n;

	   for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(storage_class_of(loc_symtab[i].type) == class_STMT_FUNCTION){
		  sym_list[n++] = &loc_symtab[i];
	       }
	   }
	   if(n != 0) {
	      sort_lsymbols(sym_list,n);
	      if(do_symtab) {
		(void)fprintf(list_fd,"\nStatement functions defined:\n");
		imps += print_lsyms_briefly(sym_list,n,TRUE);
	      }
				/* Note: unused stmt functions are "set"
				   when parsed in assignment stmt, so pattern
				   to look for is !used, set, !ubs
				 */
	      if(usage_ext_unused) {
		check_flags(sym_list,n,0,1,0,
		 "Statement functions defined but never referenced:",mod_name);
	      }
	    }
    }/*if(do_symtab)*/


				/* Print the common blocks */
    if(do_symtab || port_common_alignment || f77_mixed_common) {
	   int i,numblocks;

	   for(i=0,numblocks=0;i<loc_symtab_top;i++) {
	      if(storage_class_of(loc_symtab[i].type) == class_COMMON_BLOCK) {
		  sym_list[numblocks++] = &loc_symtab[i];
	      }
	   }

	   if(numblocks != 0) {
	      sort_lsymbols(sym_list,numblocks);
	      if(do_symtab) {
		  (void)fprintf(list_fd,"\nCommon blocks referenced:\n");
		  (void) print_lsyms_briefly(sym_list,numblocks,FALSE);
	      }
	      if(port_common_alignment || f77_mixed_common) {
		    check_mixed_common(sym_list,numblocks);
	      }
	   }
     }/*if(do_symtab||port_common_alignment||f77_mixed_common)*/

				/* Print the namelists */
    if(do_symtab) {
	   int i,numlists;

	   for(i=0,numlists=0;i<loc_symtab_top;i++) {
	      if(storage_class_of(loc_symtab[i].type) == class_NAMELIST) {
		  sym_list[numlists++] = &loc_symtab[i];
	      }
	   }

	   if(numlists != 0) {
	      sort_lsymbols(sym_list,numlists);
	      if(do_symtab) {
		  (void)fprintf(list_fd,"\nNamelists defined:\n");
		  (void) print_lsyms_briefly(sym_list,numlists,FALSE);
	      }
	    }

    }/* End printing the namelists */

				/* Process the variables */

    if(do_symtab || pure_functions
       || (usage_var_set_unused || usage_var_uninitialized
       || usage_var_unused || usage_arg_unused)) {
	int i,n;

	for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(storage_class_of(loc_symtab[i].type) == class_VAR
	       && (!loc_symtab[i].entry_point || this_is_a_function)) {
		  sym_list[n++] = &loc_symtab[i];
	       }
	}
	if(n != 0) {

	   sort_lsymbols(sym_list,n);

	   if(this_is_a_function && pure_functions) {
	       check_nonpure(sym_list,n,mod_name);
	   }

			/* Print the variables */

	   if(do_symtab) {
	      (void)fprintf(list_fd,"\nVariables:\n ");
	      imps += print_variables(sym_list,n);
	   }
	}
			/* Explain the asterisk on implicitly defined
			   identifiers.  Note that this message will
			   be given also if functions implicitly defined */
	if(do_symtab && imps != 0) {
	     (void)fprintf(list_fd,"\n* Variable not declared.");
	     (void)fprintf(list_fd," Type has been implicitly defined.");
	     ++warning_count;
	}

	if(usage_var_unused || usage_arg_unused
	   || usage_var_set_unused || usage_var_uninitialized) {
	  if(do_symtab || do_list)
	    (void)fprintf(list_fd,"\n");
	  if(usage_var_unused || usage_arg_unused) {
	    check_flags(sym_list,n,0,0,0,
		      "Variables declared but never referenced:",mod_name);
	  }
	  if(usage_var_set_unused) {
	    check_flags(sym_list,n,0,1,0,
		      "Variables set but never used:",mod_name);
	  }
	  if(usage_var_uninitialized) {
	    check_flags(sym_list,n,1,0,1,
		      "Variables used before set",mod_name);
	    check_flags(sym_list,n,1,1,1,
		      "Variables may be used before set:",mod_name);
	  }

	}/*end if(usage_...)*/

	if(do_symtab || do_list)
	  (void)fprintf(list_fd,"\n");

    }/* end if(do_symtab || pure_functions || usage_...) */

			/* List all undeclared vars & functions */
    if(decls_required || implicit_none) {
	int i,n;

	for(i=0,n=0;i<loc_symtab_top;i++) {
	    if(datatype_of(loc_symtab[i].type) == type_UNDECL
		&& ! loc_symtab[i].intrinsic /* omit intrinsics */
				/* omit subroutines called */
		&& (!loc_symtab[i].external || loc_symtab[i].invoked_as_func)
	       ) {
		sym_list[n++] = &loc_symtab[i];
	    }
	}
	if(n != 0) {
	    sort_lsymbols(sym_list,n);
	    local_warn_head(mod_name,
			   top_filename,
			   NO_LINE_NUM, sym_list[0], FALSE,
				"Identifiers of undeclared type");
	    (void) print_lsyms(sym_list,n,FALSE);
	}
    }/*if(decls_required || implicit_none)*/

			/* Under -f77/f90, list any nonstandard intrinsics used */
    if(f77_intrinsics || f90_intrinsics) {
      int i,n;
      for(i=0,n=0;i<loc_symtab_top;i++) {
	if(storage_class_of(loc_symtab[i].type) == class_SUBPROGRAM
	   && loc_symtab[i].intrinsic &&
	   (loc_symtab[i].info.intrins_info->intrins_flags & (f77_intrinsics?I_NONF77:I_NONF90))) {
	  sym_list[n++] = &loc_symtab[i];
	}
      }

      if(n != 0) {
	sort_lsymbols(sym_list,n);
	local_warn_head(mod_name,
			choose_filename(sym_list[0],file_used),
			sym_list[0]->line_used, 
			(Lsymtab *)NULL, FALSE,
			f77_intrinsics? "Non Fortran 77" : "Non Fortran 90");
	msg_tail("intrinsic functions referenced:");
	(void) print_lsyms(sym_list,n,FALSE);
      }
    }/*if(f77_intrinsics || f90_intrinsics)*/


		/* issue -f77 warning for identifiers
		   longer than 6 characters
		*/
    if(f77_long_names) {
	int i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(strlen(loc_symtab[i].name) > (unsigned)6)
		  sym_list[n++] = &loc_symtab[i];
	}

	if(n != 0) {

	   sort_lsymbols(sym_list,n);

	   local_warn_head(mod_name,
			  top_filename,
			  NO_LINE_NUM, sym_list[0], FALSE,
			       "Names longer than 6 chars (nonstandard):");
	   (void) print_lsyms(sym_list,n,FALSE);
	}
    }

	/* If -f77 flag given, list names with underscore or dollarsign */

    if(f77_underscores || f77_dollarsigns || f90_dollarsigns) {
	int n;
	n = find_nonalnum_names(sym_list);

	if(n != 0) {

	   sort_lsymbols(sym_list,n);

	   local_warn_head(mod_name,
			  top_filename,
			  NO_LINE_NUM, sym_list[0], FALSE,
			       "Names containing nonstandard characters:");
	   (void) print_lsyms(sym_list,n,FALSE);
	}
    }/*if(f77_underscores || f77_dollarsigns || f90_dollarsigns)*/

			/* Print out clashes in first six chars of name */
    if(sixclash) {
	 int n;
	 n = find_sixclashes(sym_list);
	 if(n != 0) {
	    sort_lsymbols(sym_list,n);
				/* Use the right line number */
	    local_warn_head(mod_name,
			   top_filename,
			   NO_LINE_NUM, sym_list[0], FALSE,
		      "Identifiers which are not unique in first six chars:");
	    (void) print_lsyms(sym_list,n,FALSE);
	 }/* end if(n != 0) */
    }/* end if(sixclash) */


		/* If portability flag was given, check equivalence
		   groups for mixed type. */
    if(port_mixed_equiv || port_mixed_size || local_wordsize==0) {
	int i,j,n;
	int port_imps=0;
	Lsymtab *equiv;

		/* scan thru table for equivalenced variables */
	for(i=0;i<loc_symtab_top;i++) {
	    if(storage_class_of(loc_symtab[i].type) == class_VAR
	       && loc_symtab[i].equiv_link != (equiv= &loc_symtab[i]) ){
		n=0;
		do {
		    if(equiv < &loc_symtab[i]) { /* skip groups done before */
			n=0;
			break;
		    }
		    sym_list[n++] = equiv;
		    equiv = equiv->equiv_link;
		} while(equiv != &loc_symtab[i]); /* complete the circle */
				/* Check for mixed types */
		if(n != 0) {
		    int mixed_type = FALSE, mixed_size = FALSE,
			mixed_default_size = FALSE;
		    int t1,t2,s1,s2,defsize1,defsize2;

		    t1 = get_type(sym_list[0]);
		    s1 = get_size(sym_list[0],t1);
		    defsize1 = (s1 == size_DEFAULT);
		    if(s1 == size_DEFAULT) s1 = type_size[t1];
		    for(j=1; j<n; j++) {
		      t2 = get_type(sym_list[j]);
		      s2 = get_size(sym_list[j],t2);
		      defsize2 = (s2 == size_DEFAULT);
		      if(s2 == size_DEFAULT) s2 = type_size[t2];
		      if( t1 == t2 ) {
			if( t1 != type_STRING ){
				/* Same non-char types: size must match */
			  if( s1 != s2 ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
		      }
		      else {/* Different types */
				/* It is nonportable to equivalence:
					 Real*8 to Double or
					 Complex*16 to DComplex */
			if(type_category[t1] == type_category[t2]) {
			  if( s1 != s2 ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
				/* It is standard and portable to equivalence:
					 Real to Complex or
					 Double to DComplex */
			else if(equiv_type[t1] == equiv_type[t2]) {
			  if( ((type_category[t1] == type_COMPLEX)?
				s1 != 2*s2: s2 != 2*s1) ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
			else {
			  mixed_type = TRUE;
			  break;
			}
		      }/*end else different types*/

		      t1 = t2;
		      s1 = s2;
		      defsize1 = defsize2;
		    }/*end for j*/

		    if( (mixed_type && port_mixed_equiv) ||
		       ((mixed_size || mixed_default_size) &&
			(port_mixed_size || local_wordsize==0)) )  {
			sort_lsymbols(sym_list,n);
			local_warn_head(mod_name,
					top_filename,
					NO_LINE_NUM, sym_list[0], 
					FALSE, "Mixed");
			if(mixed_type)
			    msg_tail("types");
			else if(mixed_size)
			    msg_tail("sizes");
			else
			    msg_tail("default and explicit size items");
			msg_tail("equivalenced (not portable):");
			port_imps += print_lsyms(sym_list
					   ,n,TRUE);
		    }
		}
	    }
	}
	if(brief && port_imps != 0) {
	     (void)fprintf(list_fd,"\n* Variable not declared.");
	     (void)fprintf(list_fd," Type has been implicitly defined.\n");
	}

    }/*if(port_mixed_size/type)*/

					/*  print table of I/O usages */
    if( do_symtab && num_io_unit_usages > 0 ) {
      print_io_unit_usages();
    }

					/* print table of stmt labels */

    if(misc_warn || usage_label_undefined || usage_label_unused
       || do_symtab || print_lab_refs) {
	sort_labtable();
	if(do_symtab) {
	    print_labels();
	}
	if(print_lab_refs) {
	    print_label_refs();
	}
	if(misc_warn || usage_label_undefined || usage_label_unused) {
	    check_labels(mod_name);
	}
    }

    make_declarations(sym_list,mod_name);
    
   /* Recreate a FULL list of local symbols */
   {
   int i, n;
   for(i=0,n=0;i<loc_symtab_top;i++)
      {
      sym_list[n++] = &loc_symtab[i];
      }
   }
   
   /* Call make_html to create an individual html doc for this FORTRAN module */
   make_html(sym_list,mod_name, module);
   
}/* print_loc_symbols */

/* Routine to print list of I/O unit usages.  It first sorts the list
 * according to unit number/id and operation.  Following the
 * identification of the I/O unit and operation, it lists all the line
 * numbers where that occurs.
 */

PRIVATE void
print_io_unit_usages(VOID)
{
      int i;
			/* Arrays of names of access and formatting modes.
			   Entries for default are left blank.
			 */
      static char* IO_access[]={"","DIR","SEQ"};
      static char* IO_form[] ={"","UNF","FMTD"};

			/* variables for remembering what was printed */
      int old_id, old_no, old_op, old_acc, old_form;
      int cols;

      sort_io_unit_usages();	/* put list into sorted order */

				/* print the heading of table */
      fprintf(list_fd,"\nI/O Operations:\n");
      fprintf(list_fd,"\n     Unit ID Unit No. Access Form Operation   Line");
      for(i=0; i<num_io_unit_usages; i++) {
	const char *unit_name;
	int id = io_unit_info[i].unit_id;
	int no = io_unit_info[i].unit_no;
	int op = io_unit_info[i].io_operation;
			/*  print info if it has changed */
	if( i == 0 || id != old_id || no != old_no || op != old_op ||
	    io_unit_info[i].io_access != old_acc ||
	    io_unit_info[i].io_form != old_form ) {
				/* print unit number or blank if unknown */
				/* print unit name or blank if unknown */
	   if( id < 0) {
			/* handle cases of unknown and default */
	      unit_name = (id == IO_UNIT_DEFAULT)? "*": "";
	   }
	   else {
			/* handle cases where it is an identifier, unit_id=hashnum */
	      unit_name = hashtab[id].name;
	   }
	   fprintf(list_fd,"\n%12s%1s",unit_name,
				/* if id is a parameter, print name=value */
		   (id >= 0 && no >= 0)? "=": " ");
	   if( no < 0 )
	      fprintf(list_fd,"%7s","");
	   else
	      fprintf(list_fd,"%-7d",no);
	   fprintf(list_fd,"   %4s  %4s %-9s",
		   IO_access[io_unit_info[i].io_access],
		   IO_form[io_unit_info[i].io_form],
		   keytok_name(op));
	   old_id = id;
	   old_no = no;
	   old_op = op;
	   old_acc = io_unit_info[i].io_access;
	   old_form = io_unit_info[i].io_form;
	   cols = 43;		/* amount of stuff printed so far */
	}
				/* print line number(s) after info */
	if(cols+7 > wrap_column) {
	   fprintf(list_fd,"\n%43s","");
	   cols = 43;
	}
	fprintf(list_fd,"%6d ",
		io_unit_info[i].line_num);
	cols = cols+7;
      }
}

/* Routine to sort the io_unit_usages array.  It uses bubble sort
 * since the array is normally not very long and should be nearly
 * sorted already in most cases.
 */

PRIVATE void
sort_io_unit_usages(VOID)
{
   int i, j;

   for(i = 0; i < num_io_unit_usages-1; i++) {
      int numswaps=0;
      for(j = num_io_unit_usages-2; j >= i; j--) {
	 if( cmp_io_units(&io_unit_info[j],&io_unit_info[j+1]) > 0 ) {
	    IO_Unit_Info hold = io_unit_info[j+1];
	    io_unit_info[j+1] = io_unit_info[j];
	    io_unit_info[j] = hold;
            numswaps++;
	 }
      }
      if(numswaps == 0)
	 break;
   }
}

/* Function to determine ordering of two I/O unit info entries.  The
 * primary sort key is the unit number or unit id.  If unit number is
 * known, it takes precedence, and sort is in numeric order.
 * Otherwise sort is alphabetic by unit id, with the default unit=*
 * treated as an id that comes before all alphabetic names.  If unit
 * number or id compares equal, then sort is by operation, access and
 * form.  (It should be rare for access and form to differ if
 * operation is the same, but the possibility cannot be ruled out.)
 * Line number is not used as a sort key since list arrives in
 * line-number order and bubble sort preserves initial order.
 */

PRIVATE int
cmp_io_units( IO_Unit_Info *u1, IO_Unit_Info *u2 )
{
   int no1 = u1->unit_no;
   int no2 = u2->unit_no;
   if( no1 == no2 ) { /* same unit no or both unknown */
			/* if unit no unknown, compare unit id name  */
      if( no1 == IO_UNIT_UNKNOWN ) {
	 int id1 = u1->unit_id;
	 int id2 = u2->unit_id;
	 if( id1 == id2 ) {	/* same name: sort by operation, form, access */
	    if( u1->io_operation != u2->io_operation )
	       return u1->io_operation - u2->io_operation;
	    else if( u1->io_access != u2->io_access )
	       return u1->io_access - u2->io_access;
	    else
	       return u1->io_form - u2->io_form;
	 }
	 else {			/* different names: sort by name */
	    if( id1 == IO_UNIT_DEFAULT ) {
	       return -1;	/* '*' precedes all names */
	    }
	    else if( id2 == IO_UNIT_DEFAULT ) {
	       return 1;
	    }
	    else if( id1 >= 0 && id2 >= 0 ) { /* they have names: compare */
	       return strcmp(hashtab[id1].name,hashtab[id2].name);
	    }
	    else {
	       return id1 - id2; /* shouldn't happen */
	    }
	 }
      }
      else {			/* if same unit no, sort by operation */
	    if( u1->io_operation != u2->io_operation )
	       return u1->io_operation - u2->io_operation;
	    else if( u1->io_access != u2->io_access )
	       return u1->io_access - u2->io_access;
	    else
	       return u1->io_form - u2->io_form;
      }
   }
   else {			/* diff unit nos */
      if( no1 == IO_UNIT_UNKNOWN ) {
	 return 1;	/* unit ids follow unit nos */
      }
      else if( no2 == IO_UNIT_UNKNOWN ){
	 return -1;
      }
      else {
	 return no2 < no1;
      }
   }
}
