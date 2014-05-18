/* $Id: forlex.h,v 1.5 2001/11/03 00:55:37 moniot Rel $
		Macros and shared info for lexical analysis routines
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

#ifdef FORLEX
#define LEX_SHARED
#else
#define LEX_SHARED extern
#endif

#define EOL     '\n'    /* Character for end of line, not of statement */

extern YYSTYPE yylval;	  /* Lexical value for Yacc */

LEX_SHARED
int free_form;			/* TRUE if source format is free-form */

	/* Since EOS is special, need special macros for it */
#define makeupper(C) (((C) != EOS && islower((int)(C)))? toupper((int)(C)):(C))
#define iswhitespace(C) ( (C) != EOS && isspace((int)(C)) )
#define isadigit(C)     ( (C) != EOS && isdigit((int)(C)) )
#define isaletter(C)    ( (C) != EOS && isalpha((int)(C)) )
#define ishex(C) ((C) != EOS && (isdigit((int)(C)) ||\
			(toupper((int)(C))>='A' && toupper((int)(C))<='F') ))

	/* Define isidletter to allow nonalpha chars in idletter_list
	   Nonstandardness is handled later. */
#define isidletter(C)    ( (C) != EOS && ( isalpha((int)(C)) || \
		((C) != '\0' && strchr(idletter_list,(C)) != (char *)NULL) ))

		/* lead-in to a string: standard is ' but allow " too*/
#ifdef ALLOW_QUOTEMARKS
#define isaquote(C) ((C) == '\'' || (C) == '"')
#else
#define isaquote(C) ((C) == '\'')
#endif

#define BCD(C) ((C)-'0')	/* Binary value of digit */
#define HEX(C) (isdigit(C)?BCD(C):(makeupper(C)-'A'+10)) /* Hex value */

				/* Blank-insensitive advance in fixed form
				   mode, plain advance in free form mode.
				 */
#define bi_advance()	do {advance();} while(iswhitespace(curr_char))
LEX_SHARED int
	inside_string,		/* TRUE when reading a string or hollerith */
	inside_hollerith,	/* TRUE when reading a hollerith */
	quote_char,		/* string delimiter: ' or "  */
	WHILE_expected,		/* DO seen and WHILE is coming up */
	need_special_lparen,	/* flag for left paren after type name */
	contin_count,		/* Number of continuation lines of stmt */
	prev_char,		/* shared between forlex.c and advance.c */
	curr_char,		/* Current input character */
	next_char;		/* Lookahead character */

LEX_SHARED COLNO_t
	max_stmt_col;		/* line length limit */

#ifdef ALLOW_UNIX_CPP
LEX_SHARED char
	*next_filename;
LEX_SHARED int
	cpp_handled, cpp_start_of_file;
LEX_SHARED LINENO_t
	next_top_file_line_num;

		/* This struct holds info from #include lines that
		   have been turned into #line lines by the
		   preprocessor.  (ftnchek does not handle #include lines
		   itself. It infers them from the #line directives
		   that result.)  The info kept here is not needed to keep
		   track of current line and filename, since that is already
		   done by the #line mechanism.  But it is used to track
		   the original include depth so that top_file_line_num can be
		   maintained correctly.  Also helps avoid storing the
		   same filenames repeatedly in new global space.  */
typedef struct {
  char *filename;		/* Name of file included at this depth */
} CppIncludeStack;

LEX_SHARED
CppIncludeStack cpp_include_stack[MAX_INCLUDE_DEPTH];

LEX_SHARED
int cpp_inc_depth;

#endif


LEX_SHARED
 char src_text_buf[MAX_SRC_TEXT];
LEX_SHARED
 int src_text_len;

#ifdef DEBUG_INCLUDE
LEX_SHARED
int debug_include=FALSE;
#endif

		/* Routines shared by lexical analysis */

				/* defined in advance.c */
PROTO( void advance,( void ));
PROTO( int looking_at_cplx,( void ));
PROTO( int looking_at_keywd,( int token_class ));
PROTO( int looking_at_relop,( void ));
PROTO( int looking_at_implicit_list,( void ));

				/* defined in forlex.c */
PROTO( void get_binary_const,( Token *token, int c, int space_seen ));
PROTO( void get_string,( Token *token ));
PROTO( void space_violation, ( LINENO_t lineno, COLNO_t colno, const char *s ));

				/* defined in keywords.c */
PROTO( void get_identifier,( Token *token ));
