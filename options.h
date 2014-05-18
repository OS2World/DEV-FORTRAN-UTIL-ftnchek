/* $Id: options.h,v 1.5 2003/03/09 07:45:38 landrito Exp $

	Declarations of things shared between ftnchek.c and options.c

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


PROTO(void get_env_options,( void ));

PROTO(void get_rc_options,( void ));

PROTO(void list_options,( FILE *fd ));

PROTO(void set_option,( char *s, const char *where ));

PROTO(void turn_off_checks,( void ));

		/* The following variables are not for options, but the
		   OPT macro does what is needed, namely give them a home
		   and proper initialization.
		 */
OPT(int,actioncount,0);	/* Incremented when file read or -help printed */
OPT(int,must_open_outfile,FALSE); /* Flag set to TRUE when out=name given */
