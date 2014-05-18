/* $Id: advance.h,v 1.2 2001/08/26 16:24:08 moniot Rel $

   Declarations shared between advance.c and include.c

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

#ifdef ADVANCE
#define ADVANCE_SHARED
#else
#define ADVANCE_SHARED extern
#endif

ADVANCE_SHARED
int
	next_index,		/* Index in line of next_char */
	prev_comment_line,	/* True if previous line was comment */
	curr_comment_line,	/* True if current line is comment */
	noncomment_line_count,	/* Number of noncomment lines read so far */
	line_is_printed,	/* True if line has been flushed (printed) */
	prev_line_is_printed,	/* True if line has been flushed (printed) */
	sticky_EOF;		/* Signal to delay EOF a bit for sake
				   of error messages in include files. */
ADVANCE_SHARED
LINENO_t
	prev_line_num;		/* line number of previous input line */

ADVANCE_SHARED
LINENO_t prev_stmt_line_num;	/* line number of previous noncomment */


ADVANCE_SHARED
char
	lineA[MAXLINE+1],lineB[MAXLINE+1],  /* Buffers holding input lines */
	*prev_line,*line;		    /* Pointers to input buffers */
