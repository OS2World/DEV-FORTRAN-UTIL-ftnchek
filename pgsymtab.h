/* $Id: pgsymtab.h,v 1.5 2001/10/07 22:59:35 moniot Rel $

	Declarations used by routines printing global symtab information

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

#ifdef PGSYMTAB
#define PG_SHARED
#else
#define PG_SHARED extern
#endif

PROTO(int comcmp_error_head,(const char *name, ComListHeader *clh, const char *msg));
PROTO(void com_error_report,(ComListHeader *clh, const char *msg));
void com_xref_list(VOID);
PROTO(void comvar_error_report,(ComListHeader *clh,int i,const char *msg));
PROTO(void sub_error_report,(ArgListHeader *alh, const char *msg));

PROTO(int argcmp_error_head,(const char *name, ArgListHeader *alh, const char *msg));
PROTO(void arg_error_report, (ArgListHeader *alh,const char *argtype,int i,const char *msg));

PROTO(void sort_gsymbols ,( Gsymtab *glist[], int n ));

PG_SHARED int cmp_error_count;

		/* Macro for testing whether an arglist or comlist header is
		   irrelevant for purposes of error checking: i.e. it comes
		   from an unvisited library module. */
#define irrelevant(list) ((list)->module->library_module &&\
				!(list)->module->visited_somewhere)

