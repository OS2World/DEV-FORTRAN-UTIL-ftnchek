/* $Id: include.c,v 1.4 2001/11/03 00:55:37 moniot Rel $

   Routines to handle finding and opening F90-standard include files.

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
#include <ctype.h>
#include <string.h>

#include "ftnchek.h"
#include "symtab.h"
#include "forlex.h"
#include "advance.h"

#ifdef ALLOW_INCLUDE
/* Definition of structure for saving the input stream parameters while
   processing an include file.
*/

typedef struct {
  FILE     *input_fd;
  char	   *fname;
  char     line[MAXLINE];  /* MAXLINE is defined in ftnchek.h */
  int      curr_char;
  int      next_char;
  int	   next_index;
  COLNO_t  col_num;
  COLNO_t  next_col_num;
  int	   line_is_printed;
  int	   do_list;
  LINENO_t line_num;
  LINENO_t next_line_num;
  short    inctable_index;
} IncludeFileStack;

PRIVATE IncludeFileStack include_stack[MAX_INCLUDE_DEPTH];

#endif /*ALLOW_INCLUDE*/


#ifdef ALLOW_INCLUDE
PROTO(PRIVATE FILE* find_include,( char **fname, const char *mode ));
PROTO(PRIVATE FILE * fopen_with_path,( const char *inc_path, char **fname,
				       const char *mode ));
#endif

PROTO(PRIVATE int push_include_file,( char *fname, FILE *fd, LINENO_t
			      include_line_num ));





#ifdef ALLOW_INCLUDE		/* defns of include-file handlers */

PRIVATE int
#if HAVE_STDC
push_include_file(char *fname, FILE *fd, LINENO_t include_line_num)
#else /* K&R style */
push_include_file(fname,fd,include_line_num)
	char *fname;
	FILE *fd;
	LINENO_t include_line_num;
#endif /* HAVE_STDC */
{
	 if (incdepth == MAX_INCLUDE_DEPTH) {
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
			"include files nested too deep");
	   return FALSE;
	 }

#ifdef DEBUG_INCLUDE
if(debug_include){
(void)fprintf(list_fd,"\npush_include_file: curr_char=%c (%d)",curr_char,curr_char);
}
#endif

	 if(incdepth == 0) /* Save line num of outermost include */
	   top_file_line_num = include_line_num;

	 include_stack[incdepth].input_fd = input_fd;
	 input_fd = fd;

	 include_stack[incdepth].fname = current_filename;
	 current_filename = fname;

	 (void)strcpy(include_stack[incdepth].line,line);
	 include_stack[incdepth].curr_char = curr_char;
	 include_stack[incdepth].next_char = next_char;
	 include_stack[incdepth].next_index = next_index;
	 include_stack[incdepth].col_num = col_num;
	 include_stack[incdepth].next_col_num = next_col_num;
	 include_stack[incdepth].line_is_printed = line_is_printed;
	 include_stack[incdepth].line_num = line_num;
	 include_stack[incdepth].next_line_num = next_line_num;
	 include_stack[incdepth].do_list = do_list;
	 include_stack[incdepth].inctable_index = inctable_index;

	 incdepth++;

	 init_stream();

	 return TRUE;
}

int
pop_include_file(VOID)
{
#ifdef DEBUG_INCLUDE
if(debug_include){
(void)fprintf(list_fd,"\npop_include_file: line %u = %s depth %d",line_num,line,
incdepth);
}
#endif

	 if (incdepth == 0) {	/* Stack empty: no include file to pop. */
	   return FALSE;
	 }
	 incdepth--;


	 if(do_list) {
	   (void)flush_line_out(next_line_num);
	   (void)fprintf(list_fd,"\nResuming file %s:",
		   include_stack[incdepth].fname);
	 }

	 (void)fclose(input_fd);
	 input_fd = include_stack[incdepth].input_fd;

	 current_filename = include_stack[incdepth].fname;

	 (void)strcpy(line,include_stack[incdepth].line);
	 curr_char = include_stack[incdepth].curr_char;
	 next_char = include_stack[incdepth].next_char;
	 next_index = include_stack[incdepth].next_index;
	 col_num = include_stack[incdepth].col_num;
	 next_col_num = include_stack[incdepth].next_col_num;
	 line_is_printed = include_stack[incdepth].line_is_printed;
	 line_num = include_stack[incdepth].line_num;
	 next_line_num = include_stack[incdepth].next_line_num;
	 do_list = include_stack[incdepth].do_list;
	 inctable_index = include_stack[incdepth].inctable_index;

	 curr_comment_line = FALSE;
	 prev_line_is_printed = TRUE;
	 initial_flag = TRUE;
	 sticky_EOF = TRUE;

	 return TRUE;
}


PRIVATE
int incfile_list_space=16;	/* no. of entries allocated for incfile_list */

void
#if HAVE_STDC
open_include_file(char *fname, LINENO_t include_line_num)
#else /* K&R style */
open_include_file(fname,include_line_num)
     char *fname;
     LINENO_t include_line_num;
#endif /* HAVE_STDC */
{
  FILE *fd;
  int list_option=FALSE;	/* /[NO]LIST qualifier: default=NOLIST */
  short inc_index;

				/* for VMS: default extension is .for */
  if(source_vms_include) {
    if(has_extension(fname,"/nolist")) {
      list_option = FALSE;
      fname[strlen(fname)-strlen("/nolist")] = '\0'; /* trim off qualifier */
    }
    else if(has_extension(fname,"/list")) {
      list_option = TRUE;
      fname[strlen(fname)-strlen("/list")] = '\0'; /* trim off qualifier */
    }
  }

				/* Look for inc file name in the list. */

  for(inc_index=0; inc_index<num_incfiles; inc_index++) {
      if(strcmp(fname,incfile_list[inc_index].fname) == 0) {
	  break;
      }
  }

  if(inc_index == num_incfiles) { /* not found */

      if( incfile_list == (IncFile*)NULL ) { /* first time */
	  incfile_list = (IncFile*)malloc(incfile_list_space*
					  sizeof(IncFile));
      }
      else if( num_incfiles == incfile_list_space ) {	/* list full */
	  incfile_list_space *= 2; /* increase the space */
	  incfile_list = (IncFile*)realloc(incfile_list,
					   incfile_list_space*sizeof(IncFile));
      }
      if( incfile_list == (IncFile*)NULL ) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "cannot allocate space for incfile list");
      }
	
		/* Need to put the name in permanent space */
      incfile_list[inc_index].fname = new_global_string(fname);
      ++num_incfiles;
  }

  fname = incfile_list[inc_index].fname; /* use the permanent version */
			/* Record line where included in topmost file */
  incfile_list[inc_index].line =
      (incdepth == 0? include_line_num: top_file_line_num);


  if ((fd = find_include(&fname,"r")) == NULL) {
		/* If not found, try it with extension (vms mode only) */
    if(source_vms_include && ! has_extension(fname,DEF_INC_EXTENSION)) {
      char *fname_ext = add_ext(fname, DEF_INC_EXTENSION);
      fname_ext = new_global_string(fname_ext);
      if( (fd = find_include(&fname_ext,"r")) != NULL)
	fname = fname_ext;	/* adopt the new name if successful */
    }
    if( fd == NULL ) {
      (void)fprintf(stderr,"\nerror opening include file %s\n",fname);
      return;
    }
  }

			/* Print the INCLUDE line if do_list */
  if(do_list)
    (void)flush_line_out(prev_line_num);

			/* Report inclusion of file */
  if(!quiet || do_list)
    (void)fprintf(list_fd,"\nIncluding file %s:",fname);

		/* Save the current input stream and then open
		   the include file as input stream. */
  if( push_include_file(fname,fd,include_line_num) ) {
    if(source_vms_include) {
	/* put /[NO]LIST option into effect */
      if(do_list != list_option)
	(void)fprintf(list_fd," (listing %s)", list_option? "on":"off");
      do_list = list_option;
    }
    inctable_index = inc_index;
  }
  else
    (void)fclose(fd);
}

PRIVATE FILE*
#if HAVE_STDC
find_include(char **fname, const char *mode)	/* looks for file locally or in include dir */
                  		/* If found, fname is returned with full path*/
#else /* K&R style */
find_include(fname,mode)	/* looks for file locally or in include dir */
     char **fname,		/* If found, fname is returned with full path*/
     *mode;
#endif /* HAVE_STDC */
{
  FILE *fp;
  char *env_include_var;
  IncludePathNode *p;
  char *path_end=(char *)NULL;
  int fname_path_absolute=FALSE;

			/* Look first for bare filename.  If it is an
			   absolute path, then it must be found as it is.
			   But if it is not absolute and the current source
			   filename is qualified with a path, evaluate
			   the include filename relative to that path.
			   Note: if OS is not included in the stanzas below,
			   the default behavior is to assume the include
			   file name is not an absolute path, and the
			   current file name is not qualified by a path.
			*/
  /* Still to do:
       ==> Handle MacOS.
   */
#ifdef UNIX
  fname_path_absolute = ((*fname)[0] == '/');
#endif
			/* VMS test is kinda simplistic: it just looks for
			   a colon, as in 'SOME_LOGNAME:INCFILE.H', or else
			   a left bracket not followed by a '-' or '.' which
			   are the two ways I know of to do relative paths.
			   I would appreciate hearing from somebody who knows
			   a way to do this more surely.
			*/
#ifdef VMS
  {
    char *lbracket;
    fname_path_absolute = ( strchr(*fname,':') != NULL
      || ((lbracket=strchr(*fname,'[')) != NULL &&
	  (lbracket[1] != '-' && lbracket[1] != '.') ) );
  }
#endif

			/* MSDOS test looks for forms like A:path or
			   \path or /path (the last since some
			   development environments support / as path
			   separator.)
			*/
#ifdef MSDOS
  fname_path_absolute = ((isalpha((*fname)[0]) && (*fname)[1] == ':')
    || (*fname)[0] == '\\' || (*fname)[0] == '/');
#endif


  if(fname_path_absolute) {	/* include filename is an absolute path */
    return fopen(*fname,mode);
  }


			/* Now look for a path qualifying source file name */
#ifdef UNIX
  path_end = strrchr(current_filename,'/');
#endif

#ifdef VMS
  path_end = strrchr(current_filename,']');
#endif

#ifdef MSDOS			/* look for either \ or / at end. */
  path_end = strrchr(current_filename,'\\');
  if(path_end == (char *)NULL)
    path_end = strrchr(current_filename,'/');
#endif

  if( path_end == (char *)NULL ) {
    if( (fp=fopen(*fname,mode)) != (FILE *)NULL) /* Not qualified by a path */
      return fp;
  }
  else				/* Input file name is qualified by a path */
  {
    char *local_path;
#ifdef VMS
    ++path_end;		/* need to retain the ']' */
#endif
				/* Get a copy of the local path */
    if( (local_path=(char *)malloc(path_end-current_filename+1))
	 == (char *)NULL ) {
      (void)fflush(list_fd);
      (void)fprintf(stderr,"\nCannot allocate memory for include file path");
      return (FILE *)NULL;
    }
    strncpy(local_path,current_filename,path_end-current_filename);
    local_path[path_end-current_filename] = '\0';
    fp = fopen_with_path(local_path,fname,mode);
    (void)free(local_path);
    if( fp != (FILE *)NULL ) {
      return fp;
    }
  }
      
		      /* If not found, look in directories given
			 by include_path_list from -include options */

  for(p=include_path_list; p!=NULL; p=p->link) {
    if( (fp=fopen_with_path(p->include_path,fname,mode)) != (FILE *)NULL)
      return fp;
  }

		      /* If not found, look in directory given by
			 env variable ENV_INCLUDE_VAR (e.g. set by
			 % setenv INCLUDE ~/myinclude ) */

  if( (env_include_var=getenv(ENV_INCLUDE_VAR)) != NULL) {
    if( (fp=fopen_with_path(env_include_var,fname,mode)) != (FILE *)NULL)
      return fp;
  }

			/* Still not found: look in systemwide
			   default directory */

#ifdef DEFAULT_INCLUDE_DIR
  if( (fp=fopen_with_path(DEFAULT_INCLUDE_DIR,fname,mode)) != NULL)
    return fp;
#endif/* DEFAULT_INCLUDE_DIR */

				/* Not found anywhere: fail */
  return (FILE *)NULL;
}/*find_include*/

		/* Routine to open file with name given by include_path
		   followed by fname.  If successful, fname is replaced
		   by pointer to full name.  */
PRIVATE FILE *
#if HAVE_STDC
fopen_with_path(const char *inc_path, char **fname, const char *mode)
#else /* K&R style */
fopen_with_path(inc_path,fname,mode)
     char *inc_path, **fname, *mode;
#endif /* HAVE_STDC */
{
    FILE *fp;
    char *tmpname;		/* holds name with path prepended */
    if( (tmpname = (char *)malloc(strlen(inc_path)+strlen(*fname)+2))
	== (char *)NULL ) {
      (void)fflush(list_fd);
      (void)fprintf(stderr,"\nCannot allocate memory for include file path");
      return (FILE *)NULL;
    }

    (void)strcpy(tmpname,inc_path);
				/* Add "/" or "\" if not provided */
#ifdef UNIX
    if(tmpname[strlen(tmpname)-1] != '/')
      (void)strcat(tmpname,"/");
#endif
#ifdef MSDOS
    if(tmpname[strlen(tmpname)-1] != '\\')
      (void)strcat(tmpname,"\\");
#endif
    (void)strcat(tmpname,*fname);

    if( (fp=fopen(tmpname,mode)) != (FILE *)NULL) {
			/* Found: save new name in permanent space */
	*fname = new_global_string(tmpname);
    }

    free(tmpname);
    return fp;
}/*fopen_with_path*/

#else /* no ALLOW_INCLUDE */
				/* disabled forms of include handlers */
PRIVATE int
push_include_file(fname,fd,include_line_num)
	char *fname;
	FILE *fd;
	LINENO_t include_line_num;
{return FALSE;}

PRIVATE int
pop_include_file()
{return FALSE;}

void
open_include_file(fname,include_line_num)
     char *fname;
     LINENO_t include_line_num;
{}

#endif /*ALLOW_INCLUDE*/
