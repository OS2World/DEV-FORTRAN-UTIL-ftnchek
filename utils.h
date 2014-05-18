#ifndef UTILS_H
#define UTILS_H

#if ! HAVE_STRCASECMP
#  if HAVE_STRICMP
#    define strcasecmp(A,B) stricmp(A,B)
#    define strncasecmp(A,B,C) strnicmp(A,B,C)
#  elif HAVE_STRCMPI
#    define strcasecmp(A,B) strcmpi(A,B)
#    define strncasecmp(A,B,C) strncmpi(A,B,C)
#  else /* neither STRICMP nor STRCMPI: roll our own */
#    define USE_OUR_CASECMP
#    define strcasecmp(A,B) our_strcasecmp(A,B)
#    define strncasecmp(A,B,C) our_strncasecmp(A,B,C)
#  endif
#endif

#ifdef USE_OUR_CASECMP
int our_strcasecmp(register const char *s1, register const char *s2);
int our_strncasecmp(const char *s1, const char *s2, size_t n);
#endif

#endif /* UTILS_H */
