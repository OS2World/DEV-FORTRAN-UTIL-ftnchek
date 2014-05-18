/* $Id: comcheck.c,v 1.8 2001/08/26 16:24:12 moniot Rel $

	Routines to check common block agreement

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

		check_com_usage() Checks usage status of common blocks & vars

*/

#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "pgsymtab.h"

				/* Local routines defined. */

PROTO(PRIVATE void check_nameclash,(void));
PROTO(PRIVATE void com_block_usage,( char *name, ComListHeader *cl1 ));
PROTO(PRIVATE void com_cmp_lax,( char *name, ComListHeader *c1,
			    ComListHeader *c2 ));
PROTO(PRIVATE void com_cmp_strict,( char *name, ComListHeader *c1,
			    ComListHeader *c2 ));
PROTO(PRIVATE void com_element_usage,( char *name, ComListHeader *r_cl,
			    ComListElement *r_list, int r_num ));
PROTO(PRIVATE void print_marked_com_elts,(ComListElement *r_list, int r_num));

#ifdef DEBUG_COM_USAGE
PROTO(PRIVATE void print_comvar_usage,( ComListHeader *comlist ));
#endif

#define pluralize(n) ((n)==1? "":"s")	/* singular/plural suffix for n */

void
check_comlists(VOID)        /* Scans global symbol table for common blocks */
{
	int i;
	int model_n;
	ComListHeader *first_list, *model, *clist;

				/* Check for name clashes with subprograms */
	if(f77_common_subprog_name) {
	  check_nameclash();
	}

	if(COMCHECK_OFF)
		return;

	for (i=0; i<glob_symtab_top; i++){

	    if (storage_class_of(glob_symtab[i].type) != class_COMMON_BLOCK)
		continue;

	    if((first_list=glob_symtab[i].info.comlist) == NULL){
		(void)fprintf(list_fd,"\nCommon block %s never defined",
			glob_symtab[i].name);
	    }
	    else {
		      /* Find instance with most variables to use as model */
		model=first_list;
		model_n = first_list->numargs;
		clist = model;
		while( (clist=clist->next) != NULL ){
		    if(clist->numargs >= model_n /* if tie, use earlier */
			/* also if model is from an unvisited library
			   module, take another */
		       || irrelevant(model) ) {
			model = clist;
			model_n = clist->numargs;
		    }
		}

		if( irrelevant(model) )
		  continue;	/* skip if irrelevant */

			/* Check consistent SAVEing of block:
			   If SAVEd in one module, must be SAVEd in all.
			   Main prog is an exception: SAVE ignored there. */
	      {
		ComListHeader *saved_list, *unsaved_list;
		saved_list = unsaved_list = (ComListHeader *)NULL;
		clist = first_list;
		while( clist != NULL ){

		    if(!irrelevant(clist) && clist->module->type !=
		       type_byte(class_SUBPROGRAM,type_PROGRAM) ) {

		      if(clist->saved)
			saved_list = clist;
		      else
			unsaved_list = clist;
		    }
		    clist = clist->next;
		}
		if(saved_list != (ComListHeader *)NULL &&
		   unsaved_list != (ComListHeader *)NULL) {
			  cmp_error_count = 0;
			  (void)comcmp_error_head(glob_symtab[i].name,
					       saved_list,
					       "not SAVED consistently");
			  com_error_report(saved_list,"is SAVED");
			  com_error_report(unsaved_list,"is not SAVED");
		}
	      }


				/* Now check agreement of common lists */
		clist = first_list;
		while( clist != NULL ){
		    if(clist != model && !irrelevant(clist)) {

			if(comcheck_by_name)
			  com_cmp_strict(glob_symtab[i].name,model,clist);
			else
			  com_cmp_lax(glob_symtab[i].name,model,clist);
		    }
		    clist = clist->next;
		}
	    }
	}
} /* check_comlists */


		/* Common-list check for comcheck_type or comcheck_length
		   (formerly strictness levels 1 & 2) */
PRIVATE void
#if HAVE_STDC
com_cmp_lax(char *name, ComListHeader *c1, ComListHeader *c2)
#else /* K&R style */
com_cmp_lax(name,c1,c2)
     char *name;
     ComListHeader *c1,*c2;
#endif /* HAVE_STDC */
{
    int i1,i2,			/* count of common variables in each block */
	done1,done2,		/* true when end of block reached */
	type1,type2;		/* type of variable presently in scan */
    unsigned long
	len1,len2,		/* length of variable remaining */
        size1,size2,		/* unit size of variable */
	word1,word2,		/* number of "words" scanned */
	words1,words2,		/* number of "words" in block */
        defsize1,defsize2,	/* default size used? */
	jump;			/* number of words to skip next in scan */
    int byte_oriented=FALSE,	/* character vs numeric block */
        type_clash;		/* flag for catching clashes */
    int n1=c1->numargs,n2=c2->numargs; /* variable count for each block */

    ComListElement *a1=c1->com_list_array, *a2=c2->com_list_array;

				/* Count words in each list */
    words1=words2=0;
    for(i1=0; i1<n1; i1++) {
      size1 = a1[i1].size;
      if(size1 == size_DEFAULT)
	size1 = type_size[a1[i1].type];
      else
	byte_oriented = TRUE;
      words1 += array_size(a1[i1].dimen_info)*size1;
    }
    for(i2=0; i2<n2; i2++) {
      size2 = a2[i2].size;
      if(size2 == size_DEFAULT)
	size2 = type_size[a2[i2].type];
      else
	byte_oriented = TRUE;
      words2 += array_size(a2[i2].dimen_info)*size2;
    }
	/* If not byte oriented, then sizes are all multiples of
	   BpW and can be reported as words according to F77 std. */
    if(!byte_oriented) {
      words1 /= BpW;
      words2 /= BpW;
    }
    if(comcheck_length && words1 != words2) {
      char msg[11+3*sizeof(words1)];
      cmp_error_count = 0;
      (void)comcmp_error_head(name,c1,"varying length:");
      (void)sprintf(msg,"Has %ld %s%s",
		words1,
		byte_oriented? "byte":"word",
		pluralize(words1));
      com_error_report(c1,msg);
      (void)sprintf(msg,"Has %ld %s%s",
		words2,
		byte_oriented? "byte":"word",
		pluralize(words2));
      com_error_report(c2,msg);
    }

				/* Now check type matches */
  if(comcheck_type) {
    done1=done2=FALSE;
    i1=i2=0;
    len1=len2=0;
    word1=word2=1;
    cmp_error_count=0;
    for(;;) {
	if(len1 == 0) {		/* move to next variable in list 1 */
	    if(i1 == n1) {
		done1 = TRUE;
	    }
	    else {
		type1 = a1[i1].type;
		size1 = a1[i1].size;
		defsize1 = (size1 == size_DEFAULT);
		if(defsize1)
		  size1 = type_size[type1];
		if(!byte_oriented)
		  size1 /= BpW;	/* convert bytes to words */
		len1 = array_size(a1[i1].dimen_info)*size1;
		++i1;
	    }
	}
	if(len2 == 0) {		/* move to next variable in list 2 */
	    if(i2 == n2) {
		done2 = TRUE;
	    }
	    else {
		type2 = a2[i2].type;
		size2 = a2[i2].size;
		defsize2 = (size2 == size_DEFAULT);
		if(defsize2)
		  size2 = type_size[type2];
		if(!byte_oriented)
		  size2 /= BpW;
		len2 = array_size(a2[i2].dimen_info)*size2;
		++i2;
	    }
	}

	if(done1 || done2){	/* either list exhausted? */
	    break;		/* then stop checking */
	}

		/* Look for type clash.  Allow explicitly sized real to
		   match double of equal size.
		   Allow real to match complex whose parts are of equal size.
		   Within same type category, size diff counts as clash
		   except with char.
		   Also issue warning under -portability or -nowordsize
		   if an explicit size is matched to an implicit size. */
	type_clash = FALSE;
	if( (type_category[type1] == type_category[type2]) ) {
	  if( type1 != type_STRING &&
	      (size1 != size2
	       || ((port_mixed_size || local_wordsize==0) &&
		   defsize1 != defsize2))) {
	    type_clash = TRUE;
	  }
	}
	else /* different type categories */ {
				/* Equiv_type matches complex to real */
	  if(equiv_type[type1] != equiv_type[type2]) {
	    type_clash = TRUE;
	  }
	  else {
	    if( type_category[type1] == type_COMPLEX ) {
	      type_clash = (size1 != 2*size2);
	    }
	    else {
				/* 2nd block has complex */
	      type_clash = (size2 != 2*size1);
	    }
	  			/* Give warning anyway if default size
				   is matched to explicit. */
	    if( (port_mixed_size || local_wordsize==0)
	       && defsize1 != defsize2 )
	      type_clash = TRUE;
	  }
	}

	if(type_clash) {
	    char msg[15+MAX_TYPESPEC+3*sizeof(word1)];
	    if(comcmp_error_head(name,c1,"data type mismatch"))
		break;
	    (void)sprintf(msg,"%s %ld is type %s",
		     byte_oriented?"Byte":"Word",
		     word1,
		     typespec(type1,!defsize1,(long)size1,FALSE,0L));
	    com_error_report(c1,msg);
	    (void)sprintf(msg,"%s %ld is type %s",
		     byte_oriented?"Byte":"Word",
		     word2,
		     typespec(type2,!defsize2,(long)size2,FALSE,0L));
	    com_error_report(c2,msg);
	}

			/* Advance along list by largest possible
			   step that does not cross a variable boundary.
			   If matching complex to real, only advance
			   the real part.
			 */
	jump = len1 < len2? len1: len2;	/* min(len1,len2) */
	len1 -= jump;
	len2 -= jump;
	word1 += jump;
	word2 += jump;
    }/* end for(;;) */
  }/* end if(comcheck_type) */
}

	/* Common-list check name-by-name (formerly strictness level 3) */
PRIVATE void
#if HAVE_STDC
com_cmp_strict(char *name, ComListHeader *c1, ComListHeader *c2)
#else /* K&R style */
com_cmp_strict(name,c1,c2)
	char *name;
	ComListHeader *c1, *c2;
#endif /* HAVE_STDC */
{
	int i;
	short n,
	      n1 = c1->numargs,
	      n2 = c2->numargs;
	ComListElement *a1 = c1->com_list_array,
		       *a2 = c2->com_list_array;

      if(comcheck_length) {
	n = (n1 > n2) ? n2: n1;
	if(n1 != n2){
	  char msg[15+3*sizeof(n1)];
	  cmp_error_count = 0;
	  (void)comcmp_error_head(name,c1,"varying length:");
	  (void)sprintf(msg,"Has %d variable%s",
		  n1,pluralize(n1));
	  com_error_report(c1,msg);
	  (void)sprintf(msg,"Has %d variable%s",
		  n2,pluralize(n2));
	  com_error_report(c2,msg);
        }
      }
#ifdef DEBUG_PGSYMTAB
if(debug_latest){
(void)fprintf(list_fd,"block %s",name);
(void)fprintf(list_fd,"\n\t1=in module %s line %u file %s (%s)",
		    c1->module->name,
		    c1->line_num,
		    c1->topfile
	            c1->filename);
(void)fprintf(list_fd,"\n\t2=in module %s line %u file %s (%s)",
		    c2->module->name,
		    c2->line_num,
		    c2->topfile,
	            c2->filename);
}
#endif
      if(comcheck_type) {
	cmp_error_count = 0;
	for (i=0; i<n; i++) {
	  int t1 = datatype_of(a1[i].type),
	      t2 = datatype_of(a2[i].type),
	      s1 = a1[i].size,
	      s2 = a2[i].size,
	      defsize1 = (s1==size_DEFAULT),
	      defsize2 = (s2==size_DEFAULT);
		/* If -portability, do not translate default sizes so
		   they will never match explicit sizes. */
	 if(!(port_mixed_size || local_wordsize==0)) {
	   if(defsize1)
	     s1 = type_size[t1];
	   if(defsize2)
	     s2 = type_size[t2];
	 }

	    if( t1 != t2 || s1 != s2 ) {
		if(comcmp_error_head(name,c1,"data type mismatch"))
		    break;
		comvar_error_report(c1,i,"is type");
		msg_tail(typespec(t1,!defsize1,(long)s1,FALSE,0L));
		comvar_error_report(c2,i,"is type");
		msg_tail(typespec(t2,!defsize2,(long)s2,FALSE,0L));

	    }/*end if(type or size mismatch)*/
	}/*end for(i=0; i<n; i++)*/
      }/* end if(comcheck_type) */

      if(comcheck_dims) {
	cmp_error_count = 0;
	for (i=0; i<n; i++){
		unsigned long d1, d2, s1, s2;

		if((d1=array_dims(a1[i].dimen_info)) !=
			(d2=array_dims(a2[i].dimen_info))){

		    if(comcmp_error_head(name,c1,"array dimen/size mismatch"))
			break;
		    comvar_error_report(c1,i,"has");
		    msg_tail(ulongtostr((unsigned long)d1));
		    msg_tail(d1 == 1? "dimension": "dimensions");
		    comvar_error_report(c2,i,"has");
		    msg_tail(ulongtostr((unsigned long)d2));
		    msg_tail(d2 == 1? "dimension": "dimensions");

		}/*end if(num dims mismatch)*/

		if((s1=array_size(a1[i].dimen_info)) !=
			(s2=array_size(a2[i].dimen_info))){


		    if(comcmp_error_head(name,c1,"array dimen/size mismatch"))
			break;
		    comvar_error_report(c1,i,"has size");
		    msg_tail(ulongtostr((unsigned long)s1));

		    comvar_error_report(c2,i,"has size");
		    msg_tail(ulongtostr((unsigned long)s2));

		}/*end if(array size mismatch)*/
	}/*end for(i=0; i<n; i++)*/
      }/*end if(comcheck_dims)*/
}/*com_cmp_strict*/


/**  Common block and common variable usage checks.  Implemented
 **  by John Quinn, Jan-May 1993.  Some modifications made by RKM.
 **/


void
check_com_usage(VOID)
{
    int  i;

				/* Print common block cross-reference list */
    if(print_com_xrefs) {
	com_xref_list();
    }

				/* Print out usage info */
    if(usage_com_any) {
	for(i=0;i<glob_symtab_top;i++){ /* loop thru global table */
	   if (storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK){

	       com_block_usage(glob_symtab[i].name,
				 glob_symtab[i].info.comlist );
	   }
	}
    }
#ifdef DYNAMIC_TABLES
    (void) cfree(gsymlist);
#endif
}

		/* Routine to check for common block having same name
		   as subprogram, which is nonstandard.  */
PRIVATE void
check_nameclash(VOID)
{
  int i;
  ArgListHeader *alist;
  for(i=0;i<HASHSZ;i++) {
    if(hashtab[i].glob_symtab != NULL &&
       hashtab[i].com_glob_symtab != NULL) {
      ComListHeader *clh=hashtab[i].com_glob_symtab->info.comlist;
      cmp_error_count = 0;
      (void)comcmp_error_head(hashtab[i].name,clh,
		       "has same name as a subprogram (nonstandard)");
      com_error_report(clh,"Declared as common block");
      for(alist=hashtab[i].glob_symtab->info.arglist;alist!=NULL;
	  alist=alist->next) {
	if(alist->is_defn) {
	  break;
	}
      }

		/* if not declared: use first reference */
      if(alist==NULL) {
	  sub_error_report( hashtab[i].glob_symtab->info.arglist,
			   "Referenced as subprogram");
      }
      else {
	  sub_error_report(alist,
			   "Declared as subprogram");
      }
    }
  }
}



#ifdef DEBUG_COM_USAGE

PRIVATE void print_comvar_usage(comlist)

	ComListHeader *comlist;
{
        int i, count;
  	ComListElement *c;

  	count = comlist->numargs;
  	c = comlist->com_list_array;

/* prints out caller module and any_used, any_set flags in CLhead */

	(void)fprintf(list_fd, "\nModule %s  any_used %u any_set %u\n",
                comlist->module->name, comlist->any_used, comlist->any_set);

        if((comlist->any_used || comlist-> any_set||1) ){
           for (i=0; i<count; i++){

/* prints out all four flags for each element in array */

              (void)fprintf(list_fd,
		"\n Element %d (%s) used %u set %u used bf set %u asgnd %u\n"
		      , i+1
		      , c[i].name
		      , c[i].used
		      , c[i].set
		      , c[i].used_before_set
		      , c[i].assigned);
	   } /* end of for */

        } /* end of if */
}
#endif

	/* Check used, set status of common block.  First it looks for
	   whether the block is totally unused, and if so prints a warning
	   and returns.  Otherwise, if block is unused by some modules,
	   it says which ones.  Meanwhile, it finds the declaration with
	   the most elements to use as reference.  If common strictness
	   is 3 (variable by variable) then it OR's the usage flags of
	   each block variable among different declarations, saving the
	   result in reference list.  Passes off to com_element_usage
	   to print usage of individual common variables.
	   */

PRIVATE void
#if HAVE_STDC
com_block_usage(char *name, ComListHeader *cl1)
#else /* K&R style */
com_block_usage(name,cl1)
     char *name;
     ComListHeader *cl1;
#endif /* HAVE_STDC */
{
     ComListHeader *ref_cl,	/* reference decl: has most elements */
     	*cur_cl;		/* running cursor thru decls  */
     int j,n,ref_n;
     int block_any_used, block_any_set;
     int block_unused_somewhere;
     ComListElement *ref_list, *c;

	cmp_error_count = 0;

        block_any_used = block_any_set = FALSE;
	block_unused_somewhere = FALSE;
	ref_n = cl1->numargs;
        ref_cl= cl1;
	cur_cl = cl1;
	while (cur_cl!=NULL){  /* traverses CLheads */
	  if(! irrelevant(cur_cl) ) {

            if (cur_cl->any_used){  /* stores TRUE if any are TRUE */
		block_any_used = TRUE;
            }
	    if (cur_cl->any_set){   /* stores TRUE if any are TRUE */
		block_any_set = TRUE;
	    }
	    if( ! (cur_cl->any_used || cur_cl->any_set) &&
		! cur_cl->module->defined_in_include ) {
	      block_unused_somewhere = TRUE;
	    }
   /* if any_set and any_used false after this loop block never used */

	    if (cur_cl->numargs > ref_n){ /* find largest array */
		ref_cl = cur_cl;
		ref_n = cur_cl->numargs;
            } /* end of if */
	  }/* end if not irrelevant */
	  cur_cl = cur_cl->next;
	}

        if(irrelevant(ref_cl))	/* Block not declared by modules in calltree */
	  return;

     if(! (block_any_used || block_any_set) ) {	/* Totally unused */
       if(usage_com_block_unused) {
	   cmp_error_count = 0;
	   (void)comcmp_error_head(name,ref_cl,"unused anywhere");
       }
     }
     else {
				/* If block used somewhere but not everywhere,
				   report it. */
        if(block_unused_somewhere && usage_com_block_unused) {
	  cmp_error_count = 0;
	  (void)comcmp_error_head(name,ref_cl,
			       "unused in the following modules:");

	  cur_cl = cl1;
	  while (cur_cl!=NULL){  /* traverses CLheads */
	    if(! irrelevant(cur_cl) ) {
	      if( ! (cur_cl->any_used || cur_cl->any_set) &&
		  ! cur_cl->module->defined_in_include ) {
		  com_error_report(cur_cl,"Unused");
	      }
	    }
	    cur_cl = cur_cl->next;
	  }
	}/* end if block_unused_somewhere */

	if(! comcheck_by_name) {
				/* If not variablewise checking, just
				   give general warnings. */
	  if (!block_any_set){
	    if(usage_com_var_uninitialized) {
		cmp_error_count = 0;
		(void)comcmp_error_head(name,ref_cl,
			 "No elements are set, but some are used.");
	    }
	  }
	  if (!block_any_used){
	    if(usage_com_var_set_unused) {
		cmp_error_count = 0;
		(void)comcmp_error_head(name,ref_cl,
			 "No elements are used, but some are set.");
	    }
	  }
        }
	else {	/* strictness == 3 */
				/* Now go thru the details for each element */

				/* First, malloc up a temporary list and
				   copy ref_cl and its list there so the
				   original is not clobbered (used later in
				   arg usage checks for common aliasing)
				*/
	  ComListHeader *new_ref_cl;
	  ComListElement *new_ref_list;
	  if( (new_ref_cl=(ComListHeader *)calloc(1,sizeof(ComListHeader)))
	      == (ComListHeader *)NULL ||
	      (new_ref_list=(ComListElement *)calloc(ref_cl->numargs,sizeof(ComListElement)))
	      == (ComListElement *)NULL ) {
	    oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
			 "Cannot alloc space for common block ref list");
	  }
	  *new_ref_cl = *ref_cl; /* Copy the header over to temporary */

	  ref_list = ref_cl->com_list_array;
	  for(j=0; j<ref_cl->numargs; j++) { /* Copy the array as well */
	    new_ref_list[j] = ref_list[j];
	  }
	  ref_cl = new_ref_cl;	/* Now make the temporary the one we use */
	  ref_list = new_ref_list;

	  ref_cl->any_used = block_any_used;
	  ref_cl->any_set = block_any_set;

/* traversing elements in arrays and storing OR'd values in largest array*/

	  cur_cl = cl1;
	  while (cur_cl!=NULL){
	    if(! irrelevant(cur_cl) ) {
	      c = cur_cl->com_list_array;
	      n = cur_cl->numargs;
	      for (j=0; j<n; j++){
		if (c[j].used) {
		  ref_list[j].used = TRUE;
		}
		if (c[j].set){
		  ref_list[j].set = TRUE;
		}
		if (c[j].used_before_set){
		  ref_list[j].used_before_set = TRUE;
		}
		if (c[j].assigned){
		  ref_list[j].assigned = TRUE;
		}
	      }
	    }
	    cur_cl = cur_cl->next;
	  }
	  com_element_usage(name, ref_cl, ref_list, ref_n);

				/* Free up the temporary ref list */
	  free(new_ref_cl); free(new_ref_list);
	}
     }
}

		/* Routine to print a list of common-block elements whose
		   marked flag has been set.
		 */
PRIVATE void
#if HAVE_STDC
print_marked_com_elts(ComListElement *r_list, int r_num)
#else /* K&R style */
print_marked_com_elts(r_list, r_num)
    ComListElement *r_list;	/* list of elements, some marked */
    int r_num;			/* number of elements in whole list */
#endif /* HAVE_STDC */
{
    int i;
    COLNO_t col;
    for (i=0,col=78; i<r_num; i++){
	if (r_list[i].marked){
	    if( (col += 1+(int)strlen(r_list[i].name)) > 78 ) {
		(void)fprintf(list_fd,"\n   ");
		col = 4+(int)strlen(r_list[i].name);
	    }
	    (void)fprintf(list_fd, " %s",
			  r_list[i].name);
	}
    }
}

PRIVATE void
#if HAVE_STDC
com_element_usage(char *name, ComListHeader *r_cl, ComListElement *r_list, int r_num)
#else /* K&R style */
com_element_usage(name,  r_cl, r_list, r_num)

	char *name;
	ComListHeader *r_cl;
        ComListElement  *r_list;
	int r_num;

#endif /* HAVE_STDC */
{
	int i, warnings;

 	if (r_cl->any_used || r_cl->any_set){  /* if false block not used */

	    if(usage_com_var_uninitialized) {
	      warnings = 0;
	      for (i=0; i<r_num; i++){ /* Count used-not-set cases */
		if (r_list[i].used && !r_list[i].set){
		  warnings++;
		  r_list[i].marked = TRUE;
		}
		else {
		  r_list[i].marked = FALSE;
		}
	      }
	      if(warnings > 0) {
		cmp_error_count = 0;
		(void)comcmp_error_head(name,r_cl,
			 "Elements used but never set:");
		if(warnings == r_num) {
		  (void)fprintf(list_fd," all");
		}
		else {
		  print_marked_com_elts(r_list, r_num);
	        }
	      }
	    }

	    if(usage_com_var_set_unused) {
	      warnings = 0;
	      for (i=0; i<r_num; i++){ /* Count set-not-used cases */
		if (r_list[i].set && !r_list[i].used){
		  warnings++;
		  r_list[i].marked = TRUE;
		}
		else {
		  r_list[i].marked = FALSE;
		}
	      }
	      if(warnings > 0) {
		  cmp_error_count = 0;
		  (void)comcmp_error_head(name,r_cl,
			 "Elements set but never used:");
		if(warnings == r_num) {
		  (void)fprintf(list_fd," all");
		}
		else {
		  print_marked_com_elts(r_list, r_num);
	        }
	      }
	    }

	    if(usage_com_var_unused) {
	      warnings = 0;
	      for (i=0; i<r_num; i++){ /* Count not-used, not-set cases */
		if(!r_list[i].set && !r_list[i].used &&
		   !r_list[i].used_before_set){
		  warnings++;
		  r_list[i].marked = TRUE;
		}
		else {
		  r_list[i].marked = FALSE;
		}
	      }
	      if(warnings > 0) {
		  cmp_error_count = 0;
		  (void)comcmp_error_head(name,r_cl,
			 "Elements never used, never set:");
		if(warnings == r_num) {	/* can't happen but keeps code alike */
		  (void)fprintf(list_fd," all");
		}
		else {
		  print_marked_com_elts(r_list, r_num);
	        }
	      }
	    }
	}
	else{	/* This cannot be reached if called only when block is used */
	  if(usage_com_block_unused) {
		  cmp_error_count = 0;
		  (void)comcmp_error_head(name,r_cl,
			 "not used.");
	  }
	}            /* any_used and any_set are both false */



}

