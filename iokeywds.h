/* $Id: iokeywds.h,v 1.5 2002/08/23 21:01:00 moniot Rel $
      Contains definitions of I/O control-list specifier keywords
      and their properties.  If ALLOW_VMS_IO is defined, supports
      many VMS-specific keywords needed to deal with VMS files.
      You may add other locally supported specifiers as appropriate
      (order is not important).


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
  Acknowledgement: Jean-Pierre Rouits contributed the F90 and IBM/MVS
  keywords.
*/

	/* Define bit flag for each I/O command */
#define RD  0x1			/* read */
#define WR  0x2			/* write */
#define OP  0x4			/* open */
#define CL  0x8			/* close */
#define INQ 0x10		/* inquire */
#define BSP 0x20		/* backspace */
#define ENF 0x40		/* endfile */
#define REW 0x80		/* rewind */
#define ANYSTMT (RD|WR|OP|CL|INQ|BSP|ENF|REW)

	/* Define bit flag for each type of specifier value.  All types
	   (see symtab.h) are < 16 so these values will fit in a short.*/
#define LAB (1<<type_LABEL)	/* label */
#define CHR (1<<type_STRING)	/* character */
#define INT (1<<type_INTEGER)	/* integer */
#define LOG (1<<type_LOGICAL)	/* logical */
#define STAR (1<<type_UNDECL)	/* does duty for '*' */
#define FID (LAB|CHR|STAR)	/* format ID */
#define UID (INT|CHR|STAR)	/* unit ID */
#define NML (1<<type_NAMELIST)	/* namelist name */
#define ANYTYPE (unsigned short)(~0) /* used for unknown keywords */

struct {
    char *name;			/* Name of keyword */
    unsigned short allowed_stmts, /* Where keyword can occur */
    		   allowed_types; /* datatypes allowed for value */
    unsigned		/* Flags for handling the specifier value */
      implies_use: 1,		/* Value is used */
      implies_set: 1,		/* Var is set (except INQUIRE) */
      inquire_set: 1,		/* Var is set by INQUIRE */
      nonstandard: 1,		/* not Fortran 77 standard keyword */
      nonf90:	   1,		/* not Fortran 90 either */
      vms_name:	   1;		/* Indicates special case: VMS-style NAME*/
} io_keywords[]={
			/* List has commonest ones first for fast lookup */
/*Name	       Stmts   Types   UseSetInqStdF90Spcl */
{"END",		RD,	LAB,	1, 0, 0, 0, 0, 0},
{"ERR",		ANYSTMT,LAB,	1, 0, 0, 0, 0, 0},
{"FILE",	OP|INQ,	CHR,	1, 0, 0, 0, 0, 0},
{"UNIT",	ANYSTMT,UID,	1, 0, 0, 0, 0, 0},
{"STATUS",	OP|CL,	CHR,	1, 0, 0, 0, 0, 0},
			/* The rest are alphabetical. */
{"ACCESS",	OP|INQ,	CHR,	1, 0, 1, 0, 0, 0},
{"BLANK",	OP|INQ,	CHR,	1, 0, 1, 0, 0, 0},
{"DIRECT",	INQ,	CHR,	0, 0, 1, 0, 0, 0},
{"EXIST",	INQ,	LOG,	0, 0, 1, 0, 0, 0},
{"FMT",		RD|WR,	FID,	1, 0, 0, 0, 0, 0},
{"FORM",	OP|INQ,	CHR,	1, 0, 1, 0, 0, 0},
{"FORMATTED",	INQ,	CHR,	0, 0, 1, 0, 0, 0},
{"IOSTAT",	ANYSTMT,INT,	0, 1, 1, 0, 0, 0},
{"NAMED",	INQ,	LOG,	0, 0, 1, 0, 0, 0},
{"NEXTREC",	INQ,	INT,	0, 0, 1, 0, 0, 0},
{"NML",		RD|WR,	NML,	1, 0, 0, 1, 0, 0},
{"NUMBER",	INQ,	INT,	0, 0, 1, 0, 0, 0},
{"OPENED",	INQ,	LOG,	0, 0, 1, 0, 0, 0},
{"REC",		RD|WR,	INT,	1, 0, 0, 0, 0, 0},
{"RECL",	OP|INQ,	INT,	1, 0, 1, 0, 0, 0},
{"SEQUENTIAL",	INQ,	CHR,	0, 0, 1, 0, 0, 0},
{"UNFORMATTED",	INQ,	CHR,	0, 0, 1, 0, 0, 0},

				/* Fortran 90 I/O keywords */
{"ACTION",    OP|INQ,    CHR,    1, 0, 1, 1, 0, 0},
{"ADVANCE",   RD|WR,     CHR,    1, 0, 0, 1, 0, 0},
{"DELIM",     OP|INQ,    CHR,    1, 0, 1, 1, 0, 0},
{"EOR",       RD,        LAB,	 1, 0, 0, 1, 0, 0},
{"PAD",       OP|INQ,    CHR,    1, 0, 1, 1, 0, 0},
{"POSITION",  OP|INQ,    CHR,    1, 0, 1, 1, 0, 0},
{"READ",      INQ,       CHR,    0, 0, 1, 1, 0, 0},
{"READWRITE", INQ,       CHR,    0, 0, 1, 1, 0, 0},
{"SIZE",      RD,        INT,    0, 1, 0, 1, 0, 0},
{"WRITE",     INQ,       CHR,    0, 0, 1, 1, 0, 0},

				/* Various vendor-specific keywords */

			/* NAME is a special case for VMS */
#ifndef ALLOW_VMS_IO
{"NAME",	INQ,	CHR,	0, 0, 1, 0, 0, 0}, /* normal definition */
#else
{"NAME",	OP|INQ,	CHR,	1, 0, 1, 0, 0, 1}, /* VMS definition */
#endif /*ALLOW_VMS_IO*/
			/* Other simple VMS-isms go here. */
#ifdef ALLOW_VMS_IO
{"BLOCKSIZE",	OP,	INT,	1, 0, 0, 1, 1, 0},
{"BUFFERCOUNT",	OP,	INT,	1, 0, 0, 1, 1, 0},
{"CARRIAGECONTROL",OP|INQ,CHR,	1, 0, 1, 1, 1, 0},
{"DEFAULTFILE",	OP,	CHR,	1, 0, 0, 1, 1, 0},
{"DISP",	OP|CL,	CHR,	1, 0, 0, 1, 1, 0},
{"DISPOSE",	OP|CL,	CHR,	1, 0, 0, 1, 1, 0},
{"EXTENDSIZE",	OP,	INT,	1, 0, 0, 1, 1, 0},
{"INITIALSIZE",	OP,	INT,	1, 0, 0, 1, 1, 0},
{"MAXREC",	OP,	INT,	1, 0, 0, 1, 1, 0},
{"ORGANIZATION",OP|INQ,	CHR,	1, 0, 1, 1, 1, 0},
{"RECORDSIZE",	OP,	INT,	1, 0, 0, 1, 1, 0},
{"RECORDTYPE",	OP|INQ,	CHR,	1, 0, 1, 1, 1, 0},
{"TYPE",	OP,	CHR,	1, 0, 0, 1, 1, 0},
#endif /*ALLOW_VMS_IO*/

				/* IBM/MVS-isms go here.*/
#ifdef ALLOW_IBM_IO
{"NUM",       RD|WR,     INT,    0, 1, 0, 1, 1, 0},
#endif /*ALLOW_IBM_IO*/

			/* Last entry (for not-founds) has defns that should
			   do the right thing most of the time. */
{NULL,	     ANYSTMT,ANYTYPE,	1, 0, 1, 1, 1, 0},
};

	/* Lookup table which maps statement classes into
	   the corresponding bit fields of io_keywords table.
	   Order: commonest first for faster lookup. */
struct {
	short stmt_class, stmt_flag;
} local_class[]= {
{tok_READ,	 RD},
{tok_WRITE,	 WR},
{tok_OPEN,	 OP},
{tok_CLOSE,	 CL},
{tok_BACKSPACE,	 BSP},
{tok_ENDFILE,	 ENF},
{tok_REWIND,	 REW},
{tok_INQUIRE,	 INQ},
};
#define NUM_IO_STMTS (sizeof(local_class)/sizeof(local_class[0]))


	/* The following table contains special keywords for the VMS
	   form of OPEN statement.  These keywords occur alone, i.e.
	   without the =value normally required for I/O control list
	   keywords. */
#ifdef ALLOW_VMS_IO
char *special_open_keywds[]={
"NOSPANBLOCKS",
"READONLY",
"SHARED",
};
#define NUM_SPECIAL_OPEN_KEYWDS (sizeof(special_open_keywds) \
			       / sizeof(special_open_keywds[0]))

#endif /*ALLOW_VMS_IO*/

	/* Here we define an enum type with names of the form IOKW_keyword where
	   keyword is one of the names in io_keywords.  This allows us to do a test
	   like if( index == IOKW_ACCESS ) to see if index is location of "ACCESS".
	   The include file is generated automatically by make from the io_keywords table.
	*/
typedef enum {
#include "iokeywds_enum.h"
} IOKEYWD_ENUM_TYPE;
