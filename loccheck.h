/* $Id: loccheck.h,v 1.4 2001/11/03 00:55:37 moniot Rel $

	Prototypes of functions in loccheck.c

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

PROTO(int find_nonalnum_names, (Lsymtab **sym_list));
PROTO(int find_sixclashes,( Lsymtab *list[] ));
PROTO(void check_mixed_common,( Lsymtab *sym_list[], int n ));
PROTO(void check_flags,( Lsymtab *list[], int n, unsigned used,
			 unsigned set, unsigned ubs, const char *msg, const char
			 *mod_name ));

PROTO(void check_nonpure, (Lsymtab* sym_list[], int n, char *mod_name));
