/* $Id: plsymtab.h,v 1.12 2001/10/07 22:58:30 moniot Rel $

	Declarations of things used by local symbol table printing and
	make-declarations routines.

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


PROTO(void local_err_head,( const char *mod_name, const char *filename, LINENO_t lineno, 
			    const Lsymtab *symt, int force_lineno, const char *msg ));

PROTO(void local_warn_head,( const char *mod_name, const char *filename, LINENO_t lineno, 
			     const Lsymtab *symt, int force_lineno, const char *msg ));

PROTO(void local_detail,(int inc_index, LINENO_t lineno,
			 const char *tag, const char *msg));

PROTO(void choose_tag,(int tag_type, const Lsymtab *symt, char **tag, LINENO_t *lineno));

PROTO(int print_lsyms,( Lsymtab **sym_list,
					  int n, int do_types ));
PROTO(int print_lsyms_briefly,( Lsymtab **sym_list,
					  int n, int do_types ));

PROTO(int print_variables,( Lsymtab **sym_list, int n ));

	
PROTO(void sort_lsymbols,( Lsymtab **sp, int n ));
PROTO(void sort_parameters,( Lsymtab **sp, int n ));

		/* This macro returns file name of where symbol was declared,
		   used, or set, allowing for include file location.  The
		   parameter X is one of file_declared, file_used, file_set.
		 */
#define choose_filename(SYMT,X) ((SYMT)->X >= 0? incfile_list[(SYMT)->X].fname: \
				 top_filename)

				/* Tag types for choose_tag */
#define TAG_DEFN 0
#define TAG_SET  1
#define TAG_USED 2

				/* Maximum tag length in choose_tag */
#define MAX_TAG_LEN 16		/* strlen("first occurrence") */
