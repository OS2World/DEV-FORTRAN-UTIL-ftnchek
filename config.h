		/* These sizeofs are used in determining what kind of
		   int is needed to have enough bits to hold a flag
		   field.  It is OK for the sizes defined here to be
		   smaller than the true sizes: at worst it will just
		   use more space than necessary.  The generic version
		   uses values of 2, 4, 4 which should work fine on all
		   recent-vintage 32-bit machines.  */

#define SIZEOF_SHORT	2
#define SIZEOF_INT	4
#define SIZEOF_LONG	4

		/* If unistd.h exists, use it to declare unlink in ftnchek.c */
#define HAVE_UNISTD_H 1

		/* If stdlib.h exists, use it to declare malloc and friends */
#define HAVE_STDLIB_H	1

		/* Need regex.h for makehtml to work. */
#define HAVE_REGEX_H 1

		/* Either memset or bzero is needed in forlex.c: memset
		   is preferred.
		 */
#define HAVE_MEMSET	1
#define HAVE_BZERO	0

		/* Some compilers don't have strcasecmp but have stricmp
		   or strcmpi instead.  Set up suitable defines here.
		*/
#define HAVE_STRCASECMP 0
#define HAVE_STRICMP 1
#define HAVE_STRCMPI		0
