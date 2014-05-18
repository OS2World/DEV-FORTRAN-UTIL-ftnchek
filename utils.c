#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "config.h"
#include "utils.h"

#ifdef USE_OUR_CASECMP

/* Code contributed by Nelson Beebe */
/**********************************************************************/
/****************************** strcasecmp ****************************/
/**********************************************************************/


/***********************************************************************
 Compare strings (ignoring case), and return:
	s1>s2:	>0
	s1==s2:  0
	s1<s2:	<0
***********************************************************************/

/* toupper() is supposed to work for all letters, but PCC-20 does it
incorrectly if the argument is not already lowercase; this definition
fixes that. */

#define TOUPPER(c) (islower((int)(c)) ? toupper((int)(c)) : (c))

int
#if HAVE_STDC
our_strcasecmp(
register const char *s1,
register const char *s2
)
#else /* NOT HAVE_STDC */
our_strcasecmp(s1, s2)
register const char *s1;
register const char *s2;
#endif /* HAVE_STDC */
{
    while ((*s1) && (TOUPPER(*s1) == TOUPPER(*s2)))
    {
	s1++;
	s2++;
    }
    return((int)(TOUPPER(*s1) - TOUPPER(*s2)));
}

#ifdef TOUPPER
#undef TOUPPER
#endif /* TOUPPER */


/**********************************************************************/
/****************************** strncasecmp ***************************/
/**********************************************************************/


/***********************************************************************
Compare strings ignoring case, stopping after n characters, or at
end-of-string, whichever comes first.
***********************************************************************/

int
#if HAVE_STDC
our_strncasecmp(
const char	*s1,
const char	*s2,
size_t		n
)
#else /* NOT HAVE_STDC */
our_strncasecmp(s1,s2,n)
const char	*s1;
const char	*s2;
size_t		n;
#endif /* HAVE_STDC */
{
    int	   c1;
    int	   c2;
    int	   result;

    for (; (n > 0) && *s1 && *s2; ++s1, ++s2, --n)
    {
	c1 = 0xff & (islower((int)(*s1)) ? (int)*s1 : tolower((int)(*s1)));
	c2 = 0xff & (islower((int)(*s2)) ? (int)*s2 : tolower((int)(*s2)));
	if (c1 < c2)
	    return (-1);
	else if (c1 > c2)
	    return (1);
    }
    if (n <= 0)		   /* first n characters match */
	result = 0;
    else if (*s1 == '\0')
	result = ((*s2 == '\0') ? 0 : -1);
    else /* (*s2 == '\0') */
	result = 1;

    return (result);
}

#endif /* end USE_OUR_CASECMP */
