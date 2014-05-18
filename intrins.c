/* $Id: intrins.c,v 1.3 2001/11/03 00:55:37 moniot Rel $

	Handles datatyping of intrinsic functions.
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
#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#include "symtab.h"
#include "intrins.h"

	/* Define positional flags to allow specifying more
	   than one allowed type of argument for generics.
	 */

#define I   (1 << type_INTEGER)
#define R   (1 << type_REAL)
#define D   (1 << type_DP)
#define C   (1 << type_COMPLEX)
#define Z   (1 << type_DCOMPLEX)
#define L   (1 << type_LOGICAL)
#define STR (1 << type_STRING)

	/* Table below contains information from Table 5, pp. 15-22
	   to 15-25 of the standard.  Note: num_args == -1 means 1 or 2 args,
	   num_args == -2 means 2 or more args.  Value of arg_type is the OR
	   of all allowable types (I, R, etc. as defined above).  Value of
	   result_type is type returned by function (type_INTEGER, etc.).
	   If result_type is type_GENERIC, function type is same as arg type.

	   If you add your own intrinsics to this list, the order is not
	   important and the table size adjusts automatically.
	*/

PRIVATE IntrinsInfo intrinsic[]={


	/* Table contains: name, num_args, arg_type, result_type, flags.
	   Special num_args values are defined in symtab.h.

	   Flags: I_F77 if it is in Table 5 p. 15-24, I_NONF77 otherwise
		  I_NONF90 if it is not in Chap 13 of F90 standard
		  I_NONSTD = I_NONF77|I_NONF90 for convenience
		  I_MIXED_ARGS if arguments are not all of same type.
		  I_NONPURE if arg need not have defined value (LEN).
		  I_C_TO_R indicates complex -> real in generic cases
		      (ABS,IMAG,REAL).
		  I_SP_R indicates specific REAL result (REAL)
	          I_NOTARG if it is a generic with no specific meaning,
		      or if it is a type conversion, lexical relationship,
		      or min or max (cf. p. 15-3, sec. 15.3.2)
		  I_EXTRA indicates common nonstd function
		  I_VMS indicates VMS-specific function
		  I_UNIX indicates UNIX-specific function
	 */

#define I_NONSTD (I_NONF77|I_NONF90)

{"INT", 	1,	I|R|D|C|Z,type_INTEGER,	I_F77|I_NOTARG},
{"IFIX",	1,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"IDINT",	1,	D,	type_INTEGER,	I_F77|I_NOTARG},
{"REAL",	1,	I|R|D|C|Z,type_GENERIC, I_F77|I_NOTARG|I_C_TO_R|I_SP_R},
{"FLOAT",	1,	I,	type_REAL,	I_F77|I_NOTARG},
{"SNGL",	1,	D,	type_REAL,	I_F77|I_NOTARG},
{"DBLE",	1,	I|R|D|C|Z,type_DP,	I_F77|I_NOTARG},
{"CMPLX",	I_1or2,	I|R|D|C|Z,type_COMPLEX,	I_F77|I_NOTARG},
{"ICHAR",	1,	STR,	type_INTEGER,	I_F77|I_NOTARG|I_ICHAR},
{"CHAR",	1,	I,	type_STRING,	I_F77|I_NOTARG|I_CHAR},
{"AINT",	1,	R|D,	type_GENERIC,	I_F77},
{"DINT",	1,	D,	type_DP,	I_F77},
{"ANINT",	1,	R|D,	type_GENERIC,	I_F77},
{"DNINT",	1,	D,	type_DP,	I_F77},
{"NINT",	1,	R|D,	type_INTEGER,	I_F77},
{"IDNINT",	1,	D,	type_INTEGER,	I_F77},
{"ABS", 	1,	I|R|D|C|Z,type_GENERIC,	I_F77|I_C_TO_R|I_ABS},
{"IABS",	1,	I,	type_INTEGER,	I_F77|I_ABS},
{"DABS",	1,	D,	type_DP,	I_F77},
{"CABS",	1,	C,	type_REAL,	I_F77},
{"MOD", 	2,	I|R|D,	type_GENERIC,	I_F77|I_MOD},
{"AMOD",	2,	R,	type_REAL,	I_F77},
{"DMOD",	2,	D,	type_DP,	I_F77},
{"SIGN",	2,	I|R|D,	type_GENERIC,	I_F77|I_SIGN},
{"ISIGN",	2,	I,	type_INTEGER,	I_F77|I_SIGN},
{"DSIGN",	2,	D,	type_DP,	I_F77},
{"DIM",		2,	I|R|D,	type_GENERIC,	I_F77|I_DIM},
{"IDIM",	2,	I,	type_INTEGER,	I_F77|I_DIM},
{"DDIM",	2,	D,	type_DP,	I_F77},
{"DPROD",	2,	R,	type_DP,	I_F77},
{"MAX",		I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG|I_MAX},
{"MAX0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG|I_MAX},
{"AMAX1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG},
{"DMAX1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG},
{"AMAX0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG},
{"MAX1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"MIN", 	I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG|I_MIN},
{"MIN0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG|I_MIN},
{"AMIN1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG},
{"DMIN1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG},
{"AMIN0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG},
{"MIN1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"LEN", 	1,	STR,	type_INTEGER,	I_F77|I_LEN},
{"INDEX",	2,	STR,	type_INTEGER,	I_F77|I_INDEX},
{"AIMAG",	1,	C,	type_REAL,	I_F77},
{"CONJG",	1,	C,	type_COMPLEX,	I_F77},
{"SQRT",	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DSQRT",	1,	D,	type_DP,	I_F77},
{"CSQRT",	1,	C,	type_COMPLEX,	I_F77},
{"EXP",		1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DEXP",	1,	D,	type_DP,	I_F77},
{"CEXP",	1,	C,	type_COMPLEX,	I_F77},
{"LOG", 	1,	R|D|C|Z,type_GENERIC,	I_F77|I_NOTARG},
{"ALOG",	1,	R,	type_REAL,	I_F77},
{"DLOG",	1,	D,	type_DP,	I_F77},
{"CLOG",	1,	C,	type_COMPLEX,	I_F77},
{"LOG10",	1,	R|D,	type_GENERIC,	I_F77|I_NOTARG},
{"ALOG10",	1,	R,	type_REAL,	I_F77},
{"DLOG10",	1,	D,	type_DP,	I_F77},
{"SIN", 	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DSIN",	1,	D,	type_DP,	I_F77},
{"CSIN",	1,	C,	type_COMPLEX,	I_F77},
{"COS", 	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DCOS",	1,	D,	type_DP,	I_F77},
{"CCOS",	1,	C,	type_COMPLEX,	I_F77},
{"TAN", 	1,	R|D,	type_GENERIC,	I_F77},
{"DTAN",	1,	D,	type_DP,	I_F77},
{"ASIN",	1,	R|D,	type_GENERIC,	I_F77},
{"DASIN",	1,	D,	type_DP,	I_F77},
{"ACOS",	1,	R|D,	type_GENERIC,	I_F77},
{"DACOS",	1,	D,	type_DP,	I_F77},
{"ATAN",	1,	R|D,	type_GENERIC,	I_F77},
{"DATAN",	1,	D,	type_DP,	I_F77},
{"ATAN2",	2,	R|D,	type_GENERIC,	I_F77},
{"DATAN2",	2,	D,	type_DP,	I_F77},
{"SINH",	1,	R|D,	type_GENERIC,	I_F77},
{"DSINH",	1,	D,	type_DP,	I_F77},
{"COSH",	1,	R|D,	type_GENERIC,	I_F77},
{"DCOSH",	1,	D,	type_DP,	I_F77},
{"TANH",	1,	R|D,	type_GENERIC,	I_F77},
{"DTANH",	1,	D,	type_DP,	I_F77},
{"LGE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LGT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LLE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LLT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},

		/* DOUBLE COMPLEX intrinsics are included regardless
		   of -intrinsics option, since they are essential
		   to support of this datatype.
		 */
{"DCMPLX",	I_1or2,	I|R|D|C|Z,type_DCOMPLEX,I_NONSTD|I_NOTARG},
{"DCONJG",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"DIMAG",	1,	Z,	type_DP,	I_NONSTD},
{"IMAG",	1,	C|Z,	type_GENERIC,	I_NONSTD|I_NOTARG|I_C_TO_R},
{"DREAL",	1,	Z,	type_DP,	I_NONSTD},
{"CDABS",	1,	Z,	type_DP,	I_NONSTD},
{"ZABS",	1,	Z,	type_DP,	I_NONSTD},
{"CDSQRT",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"ZSQRT",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"CDEXP",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"ZEXP",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"CDLOG",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"ZLOG",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"CDSIN",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"ZSIN",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"CDCOS",	1,	Z,	type_DCOMPLEX,	I_NONSTD},
{"ZCOS",	1,	Z,	type_DCOMPLEX,	I_NONSTD},

		/* DFLOAT has been available in almost all Fortran
                   implementations for decades, but curiously, was
                   omitted from the Fortran 66 and Fortran 77
                   standards.  A separate intrinsic is essential,
                   because DBLE(FLOAT()) will lose bits for integer
                   arguments larger than the REAL fraction size.  If
                   we don't include it here, declaration file output
                   will incorrectly type it as REAL instead of DOUBLE
                   PRECISION. -- NHFB */

{"DFLOAT",	1,	I,	type_DP,	I_NONSTD},

		/* Quad precision intrinsics are included regardless
		   of -intrinsics option, since they are essential
		   to support of this datatype.  (Actually most of
		   them are better handled by generics.)
		 */
{"IQINT",	1,	R,	type_INTEGER,	I_NONSTD|I_NOTARG|I_QARG},
{"SNGLQ",	1,	R,	type_REAL,	I_NONSTD|I_NOTARG|I_QARG},
{"QREAL",	1,	C,	type_QUAD,	I_NONSTD|I_NOTARG|I_QARG|I_QUAD},
{"DBLEQ",	1,	R,	type_DP,	I_NONSTD|I_NOTARG|I_QARG},
{"QFLOAT",	1,	I,	type_QUAD,	I_NONSTD|I_NOTARG|I_QUAD},
{"QEXTD",	1,	D,	type_QUAD,	I_NONSTD|I_NOTARG|I_QUAD},
{"QEXT",	1,	I|R|D,	type_QUAD,	I_NONSTD|I_NOTARG|I_QUAD},
{"QCMPLX",	I_1or2,	I|R|D|C|Z,type_CQUAD,	I_NONSTD|I_NOTARG|I_QUAD},
{"QINT",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QNINT",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"IQNINT",	1,	R,	type_INTEGER,	I_NONSTD|I_QARG},
{"QABS",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"CQABS",	1,	C,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QMOD",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QSIGN",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QDIM",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QPROD",	2,	D,	type_QUAD,	I_NONSTD|I_QUAD},
{"QMAX1",	I_2up,	R,	type_QUAD,	I_NONSTD|I_NOTARG|I_QARG|I_QUAD},
{"QMIN1",	I_2up,	R,	type_QUAD,	I_NONSTD|I_NOTARG|I_QARG|I_QUAD},
{"QIMAG",	1,	C,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QCONJG",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QSQRT",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"CQSQRT",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QEXP",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"CQEXP",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QLOG",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"CQLOG",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QLOG10",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QSIN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"CQSIN",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QCOS",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"CQCOS",	1,	C,	type_CQUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QTAN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QARSIN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QARCOS",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QATAN",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QATAN2",	2,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QSINH",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QCOSH",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},
{"QTANH",	1,	R,	type_QUAD,	I_NONSTD|I_QARG|I_QUAD},

#ifdef EXTRA_INTRINSICS

	/* Nonstandard but widely used intrinsics.  These follow both
	   VMS and AIX defns, so they are probably de facto standard.
	   Not included: specifics covered by a generic.
	   N.B. Argument checking is not tight for these: some
	   take arrays, 0 or 1 arguments, etc. that are not
	   handled by check_intrins_args().  Remarks are placed by
	   these cases.
	 */


		/* Bit test & Shift operations: these follow Mil. Std. 1753 */
{"BTEST",	2,	I,	type_LOGICAL,	I_NONF77|I_EXTRA},
{"IAND",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IOR",		2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IBSET",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IBCLR",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IBITS",	3,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IEOR",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"ISHFT",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"ISHFTC",	3,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"MVBITS",	5,	I,	type_SUBROUTINE,I_NONF77|I_EXTRA},
{"NOT",		1,	I,	type_INTEGER,	I_NONF77|I_EXTRA},

		/* Address-of function */
{"LOC",		1,I|R|D|C|Z|L|STR,type_INTEGER,	I_NONSTD|I_EXTRA},

		/* Utility routines */
{"EXIT",       I_0or1,	I,	type_SUBROUTINE,I_NONSTD|I_EXTRA},
#endif

		/* Unix only.  These are a selected subset of the F77
		   library routines listed in the USENIX manual section 3F.
		 */
#ifdef UNIX_INTRINSICS
{"ABORT",	1,	STR,	type_SUBROUTINE,I_NONSTD|I_UNIX},
{"AND",		2,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
		/* I, then STR not enforced in GETARG. */
{"GETARG",	2,	I|STR,	type_SUBROUTINE,I_MIXED_ARGS|I_NONSTD|I_UNIX},
{"GETENV",	2,	STR,	type_SUBROUTINE,I_NONSTD|I_UNIX},
{"GMTIME",	2,	I,	type_SUBROUTINE,I_NONSTD|I_UNIX},/*2nd arg array(9)*/
#ifdef IARGC_NO_ARG
{"IARGC",	0,	0,	type_INTEGER,	I_NONSTD|I_UNIX},
#else
#ifdef IARGC_ONE_ARG
{"IARGC",	1,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
#else  /* default is to allow 0 or 1 */
{"IARGC",	I_0or1,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
#endif
#endif
{"LSHIFT",	2,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
{"LTIME",	2,	I,	type_SUBROUTINE,I_NONSTD|I_UNIX},/*2nd arg array(9)*/
{"OR",		2,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
#ifdef RAND_NO_ARG	/*RAND() form*/
{"IRAND",	0,	0,	type_INTEGER,	I_NONSTD|I_UNIX},
{"RAND",	0,	0,	type_REAL,	I_NONSTD|I_UNIX},
#else
#ifdef RAND_ONE_ARG	/*RAND(ISEED) form*/
{"IRAND",	1,	I,	type_INTEGER,	I_NONSTD|I_UNIX|I_NONPURE},
{"RAND",	1,	I,	type_REAL,	I_NONSTD|I_UNIX|I_NONPURE},
#else				/* Allow either form */
{"IRAND",	I_0or1,	I,	type_INTEGER,	I_NONSTD|I_UNIX|I_NONPURE},
{"RAND",	I_0or1,	I,	type_REAL,	I_NONSTD|I_UNIX|I_NONPURE},
#endif
#endif
{"RSHIFT",	2,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
{"SRAND",	1,	I|R,	type_SUBROUTINE,I_NONSTD|I_UNIX},/*AIX has this*/
{"SYSTEM",	1,	STR,	type_INTEGER,	I_NONSTD|I_UNIX},
{"TIME",	I_0or1,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
{"XOR",		2,	I,	type_INTEGER,	I_NONSTD|I_UNIX},
#endif

#ifdef VMS_INTRINSICS		/* VMS only */
{"DATE",	1,	STR,	type_SUBROUTINE,I_NONSTD|I_VMS},
{"ERRSNS",	5,	I,	type_SUBROUTINE,I_NONSTD|I_VMS},
{"IDATE",	3,	I,	type_SUBROUTINE,I_NONSTD|I_VMS},
{"RAN",		1,	I,	type_REAL,	I_NONSTD|I_VMS|I_NONPURE},
{"SECNDS",	1,	R,	type_REAL,	I_NONSTD|I_VMS},
{"SIZEOF",	1,	I|R|D|C|Z|L|STR,type_INTEGER,	I_NONSTD|I_VMS},
{"TIME",	1,	STR,	type_SUBROUTINE,I_NONSTD|I_VMS},
#endif

#undef I
#undef R
#undef D
#undef C
#undef Z
#undef L
#undef STR

};

#define NUM_INTRINSICS (sizeof(intrinsic)/sizeof(intrinsic[0]))


			/* Definitions of routines start here */

PROTO( PRIVATE void set_intrinsic_numargs, ( const char *name, int choice ));
PROTO(PRIVATE unsigned long kwd_hash,( const char *s ));


#define EMPTY 255

PRIVATE unsigned char intrins_hashtab[INTRINS_HASHSZ];

/*    init_intrins_hashtab:
                 Initializes the intrinsic hash table by clearing it to EMPTY
                 and then hashes all the intrinsic names into the table.
*/

unsigned long
init_intrins_hashtab(VOID)
{
    unsigned i,h;
    unsigned long hnum;
    unsigned long numclashes=0;

    for(h=0;h<INTRINS_HASHSZ;h++) {
           intrins_hashtab[h] = EMPTY;
    }
    for(i=0; i < NUM_INTRINSICS; i++) {
	   hnum = kwd_hash(intrinsic[i].name);
	   while(h=hnum%INTRINS_HASHSZ, intrins_hashtab[h] != EMPTY) {
		hnum = rehash(hnum);
		numclashes++;
	   }
	   intrins_hashtab[h] = i;
    }
    return numclashes;
}


		/* Function called by do_preps to alter intrinsic table for
		   user-selected options respecting RAND and IARGC.
		 */

#ifndef STANDARD_INTRINSICS
void
set_intrinsic_options(VOID)
{
  int numargs;

				/* numargs = 0 if only no_arg is set, and
				   1 if only one_arg is set.  numargs =
				   I_0or1 if both are set.  If neither
				   is set (-intrinsic=none),
				   return to compile-time default. */


				/* Form a two-bit number */
  switch( (intrinsic_rand_one_argument<<1) | intrinsic_rand_no_argument ) {
    case 1:
      numargs = 0;		/* no_argument */
      break;
    case 2:
      numargs = 1;		/* one_argument */
      break;
    case 3:
      numargs = I_0or1;		/* both cases */
      break;
    default:
      numargs= (DEF_INTRINSIC_RAND & 1?
		    (DEF_INTRINSIC_RAND & 2? I_0or1: 0)
		    :(DEF_INTRINSIC_RAND & 2? 1:I_0or1));
      break;
  }

  set_intrinsic_numargs("RAND",numargs);
  set_intrinsic_numargs("IRAND",numargs);

  switch( (intrinsic_iargc_one_argument<<1) | intrinsic_iargc_no_argument ) {
    case 1:
      numargs = 0;		/* no_argument */
      break;
    case 2:
      numargs = 1;		/* one_argument */
      break;
    case 3:
      numargs = I_0or1;		/* both cases */
      break;
    default:
      numargs = (DEF_INTRINSIC_IARGC & 1?
		    (DEF_INTRINSIC_IARGC & 2? I_0or1: 0)
		    :(DEF_INTRINSIC_IARGC & 2? 1:I_0or1));
      break;
  }

  set_intrinsic_numargs("IARGC",numargs);
}

#endif /* not STANDARD_INTRINSICS */



PRIVATE
#if HAVE_STDC
void set_intrinsic_numargs(const char *name, int choice)
#else /* K&R style */
void set_intrinsic_numargs(name,choice)
     char *name;		/* Name of function to fix up */
     int choice;		/* 0 = none, 1 = one, I_0or1 = either */
#endif /* HAVE_STDC */
{
  IntrinsInfo *defn;

  defn = find_intrinsic(name);
  if(defn != (IntrinsInfo *)NULL) {
    defn->num_args = choice;
  }
}


	/* Function to look up an intrinsic function name in table.
	   If found, returns ptr to table entry, otherwise NULL.
	*/
IntrinsInfo *
#if HAVE_STDC
find_intrinsic(const char *s)
	        			/* given name */
#else /* K&R style */
find_intrinsic(s)
	char *s;			/* given name */
#endif /* HAVE_STDC */
{
	unsigned i, h;
	unsigned long hnum;

	hnum = kwd_hash(s);
	for(;;) {
	  h=hnum%INTRINS_HASHSZ;
	  if( (i=intrins_hashtab[h]) == EMPTY )
	    break;		/* Not found */

				/* Something found: see if a match */
	  if( strcmp(s,intrinsic[i].name) == 0
#ifndef STANDARD_INTRINSICS
	      &&
	      ((intrinsic[i].intrins_flags&(I_EXTRA|I_VMS|I_UNIX))==0 ||
	       ((intrinsic[i].intrins_flags&I_EXTRA) && intrinsic_set_extra) ||
	       ((intrinsic[i].intrins_flags&I_UNIX) && intrinsic_set_unix) ||
	       ((intrinsic[i].intrins_flags&I_VMS) && intrinsic_set_vms)
	      ) 
#endif
	      ) {

	    return &intrinsic[i];
	  }
	  else {		/* No match: try next */
	    hnum = rehash(hnum);
	  }
	}
				/* Not an intrinsic function */
	return (IntrinsInfo *)NULL;
}

	/* kwd_hash: Same as hash() but always uses full length of keyword.
	   To keep the keyword table clash-free on any machine,
	   packs only 4 bytes per word even if long is bigger */

PRIVATE unsigned long
#if HAVE_STDC
kwd_hash(const char *s)
#else /* K&R style */
kwd_hash(s)
    char *s;
#endif /* HAVE_STDC */
{
    unsigned long sum = 0, wd;
    int i = 0,j;

    int n = strlen(s);

    while (i < n) {
         wd = 0;
         for(j=1; j <= 4 && i < n; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (4 - j) * 8;}

	sum ^= wd;}
    return sum;
}

