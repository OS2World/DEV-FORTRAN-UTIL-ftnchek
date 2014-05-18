/* configure_os2.cmd -- Christian Bartels, 1996

   A REXX script to create an OS/2 specific makefile for FTNCHEK using
   the file 'Makefile.in' as input.

   This script is based on ideas of:
     Jan Ftacnik, 1993
     Stefan A. Deutscher, 1996 (sad@utk.edu)

   The building of the makefile is a five phase process:
   1) Parsing the command line arguments
   2) Checking of the system configuration
   3) Building of the actual substitutes for some variables
   4) Processing of Makefile.in
   5) Processing of config-generic.h to write config.h

   Extended by Stefan A. Deutscher to also generate stuff in test/
   directory automagically.  10/1996 until 05/2003

   6) Processing of test/Makefile.in
   7) Processing of test/Compare.sh.in

   Updated by R. Moniot for changes in 2.11.  (Not tested.) 10/99
   Updated by Stefan A. Deutscher for changes in 3.0.0, tested. 11/2000
   Updated by Stefan A. Deutscher for changes in 3.1.0, tested. 06/2001
   Updated by Stefan A. Deutscher for changes in 3.2.2, tested. 05/2003
*/

/* load library REXXUTIL */
  call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
  call SysLoadFuncs


/* get the command line arguments */
  parse arg argv
  argc = words(argv)

/* set some constants */
  InputFile   = 'Makefile.in'
  TmpFile     = SysTempFileName('Makefile.???')
  OutputFile  = 'Makefile'

  ConfigGenericFile = 'config-generic.h'
  ConfigOS2File     = 'config-os2.h'

  TestDir         = 'test'
  TestInputFile   = 'Makefile.in'
  TestOutputFile  = 'Makefile'

  CompInputFile   = 'Compare.sh.in'
  CompOutputFile  = 'Compare.sh'

  ObjectStyle = 'aout'
  Linking     = 'dynamic'
  PrefixDir   = 'c:\usr\local'

  UseRegex = 'no'
  RegexDef = '0'


/*******************************************/
/*     parse command line arguments        */
/*******************************************/

  i = 1
  do while (i <= argc)
     argum = word(argv,i)
     i = i + 1

/*   check whether program options start correctly with '-'  */
     c = substr(argum,1,1)
     if (c \= '-') then
        call Usage

/*   select the appropriate action for commandline options */
     c = substr(argum,2,1)
     select
        when (c = 'f') then do
           ObjectStyle = ToLower( word(argv,i) )
           if (ObjectStyle \= 'omf') & (ObjectStyle \= 'aout') then do
              say " Error: Only supported object formats are omf and aout."
              say
              exit 1
           end
           i = i + 1
        end
        when (c = 'h') then do
           call Usage
        end
        when (c = 'l') then do
           Linking = ToLower( word(argv,i) )
           if (Linking \= 'static') & (Linking \= 'dynamic') then do
              say " Error: Only static or dynamic linking is supported."
              say
              exit 1
           end
           i = i + 1
        end
        when (c = 'o') then do
           OutputFile = word(argv,i)
           i = i + 1
        end
        when (c = 'p') then do
           PrefixDir = word(argv,i)
           i = i + 1
        end
        when (c = 'r') then do
           UseRegex = 'yes'
           i = i + 1
	end
        otherwise do
           say ' Error: Argument -'c' is unknown.'
           say
           call Usage
        end
     end
  end


/*******************************************/
/*     check the system configuration      */
/*******************************************/

  say ' Checking system configuration ....'

/* Check if sed is installed */
  SedProg     = IsProgramInPath( 'sed.exe' )
  if (SedProg = '') then do
     say " Error: In order to run this procedure, 'sed.exe' has",
         "to be installed."
     say
     exit 1
  end

/* Check if make is installed */
  MakeProg     = IsProgramInPath( 'make.exe' )
  if (MakeProg = '') then do
     say " Error: In order to build this code, 'make.exe' has",
         "to be installed."
     say " Your best option is to get and install a port of",
         " GNU make."
     say
     exit 1
  end

/* Man  is not too critical: used only in install */
  ManProg     = IsProgramInPath( 'man.exe' )

/* Grep is not too critical: used only in make precheck */
  GrepProg     = IsProgramInPath( 'grep.exe' )

/* Check if pwd is installed: */
  PwdProg     = IsProgramInPath( 'pwd.exe' )

/* Zip is not too critical: used only to make distribution zipfile for
   MS-DOS/Windows/OS2 */
  ZipProg     = IsProgramInPath( 'zip.exe' )

/* Check if printenv is installed */
  PrintenvProg   = IsProgramInPath( 'printenv.exe' )

/* Check if emx+gcc is installed */
  CCompiler   = IsProgramInPath( 'gcc.exe' )
  if (CCompiler = '') then do
     say ' Error: To build FTNCHEK you will have to install the',
         'EMX program'
     say '        development package including the GNU C compiler.'
  end

/* Check which port of a new awk is installed */
  AwkProg     = SearchProgramFromList( "gawk.exe mawk.exe" )

/* No port of strip to OS/2 allows stripping of debug information or
   local symbols from an executable (neither EMX-bound nor LX). Therefore
   under OS/2 the stripping is done while producing the object files and
   during the link stage. Fill in a dummy for strip  */
  StripProg   = 'echo Requested: strip'

/* Check which yacc is installed */
  YaccProg    = SearchProgramFromList( "bison.exe yacc.exe")
  if ( YaccProg = 'yacc.exe' ) then do
     say ' Warning: If the installed yacc is not bison, then if fortran.c',
         ' is remade make check will report (minor) differences.'
     end
  else do
     if ( YaccProg = 'bison.exe' ) then
        YaccProg = YaccProg '-y'
  end

/* Check if tar.exe is installed */
  TarProg     = IsProgramInPath( 'tar.exe' )

/* To my knowledge no port of col to OS/2 exists, but check anyway */
  ColProg     = IsProgramInPath( 'col.exe' )

/* Check if the groff port to OS/2 is fully installed */
  NroffProg   = IsProgramInPath( 'groff.exe' )
  SoelimProg  = IsProgramInPath( 'soelim.exe' )
  EqnProg     = IsProgramInPath( 'eqn.exe' )
  TblProg     = IsProgramInPath( 'tbl.exe' )

/* Check if a unix sh-like shell is installed */
CmdShell    = SearchProgramFromList( "sh.exe ksh.exe bash.exe cmd.exe" )

/* Check which basic file utilities are installed */
/*  --- chmod */
  ChmodCmd    = IsProgramInPath( 'chmod.exe' )
  if (ChmodCmd = '') then
     ChmodCmd    = 'echo Requested: chmod'
/*  --- cmp */
  CmpCmd      = SearchProgramFromList( "diff.exe cmp.exe comp.com" )
  if (CmpCmd \= 'diff.exe') then do
     say ' Warning: 'CmpCmd' claims that files are different even if only'
     say '          the OS/2 EOL sequence CRLF is used instead of LF.'
  end
/*  --- cp */
  CpCmd       = IsProgramInPath( 'cp.exe' )
  if (CpCmd = '') & (CmdShell = 'cmd.exe') then
     CpCmd = 'copy'     /* use built-in command of cmd.exe */
/*  --- diff */
  DiffCmd     = SearchProgramFromList( "diff.exe cmp.exe comp.com" )
  if (DiffCmd \= 'diff.exe') then do
     say ' Warning: 'DiffCmd' claims that files are different even if only'
     say '          the OS/2 EOL sequence CRLF is used instead of LF.'
  end
/*  --- mkdir */
  MkdirCmd    = IsProgramInPath( 'mkdir.exe' )
  if (MkdirCmd = '') & (CmdShell = 'cmd.exe') then
     MkdirCmd = 'mkdir' /* use built-in command of cmd.exe */
/*  --- mv */
  MvCmd       = IsProgramInPath( 'mv.exe' )
  if (MvCmd = '') & (CmdShell = 'cmd.exe') then do
     MvCmd = 'move'     /* use built-in command of cmd.exe */
     say ' Warning: The OS/2 command MOVE cannot move files across drives.'
  end
/*  --- rm */
  RmCmd       = IsProgramInPath( 'rm.exe' )
  if (RmCmd = '') & (CmdShell = 'cmd.exe') then do
     RmCmd = 'del'      /* use built-in command of cmd.exe */
     say ' Warning: The OS/2 command DEL cannot remove read-only files.'
  end
  if (RmCmd = 'rm.exe') then RmCmd = RmCmd '-f'
/*  --- rmdir */
  RmdirCmd    = IsProgramInPath( 'rmdir.exe' )
  if (RmdirCmd = '') & (CmdShell = 'cmd.exe') then
     RmdirCmd = 'rmdir' /* use built-in command of cmd.exe */


/*  --- sh */
  ShPath = Translate(SysSearchPath('PATH', 'sh.exe'), '/', '\')
  if (ShPath = '') then do
     say ' Warning: sh.exe was not found. make check will not work.'
     say '          Setting path to UNIX default: /bin/sh'
     ShPath = '/bin/sh'
  end

/* Check if omf type libraries are installed */
  if ObjectStyle = 'omf' then do
     CLibraryPath = value( 'library_path',, 'OS2ENVIRONMENT' )
     CLibraryPath = translate( CLibraryPath, ' \', ';/' )
     emx2lib = ''
     gcclib  = ''
     do i = 1 to words( CLibraryPath )
        CurSearchDir = word( CLibraryPath, i )
        if ( right( CurSearchDir, 1) \= '\' ) then
           CurSearchDir = CurSearchDir'\'
        test = stream( CurSearchDir'emx2.lib', 'C', 'QUERY EXIST' )
        if ( test \= '' ) then
           emx2lib = test
        test = stream( CurSearchDir'gcc.lib', 'C', 'QUERY EXIST' )
        if ( test \= '' ) then
           gcclib = test
     end
     if (emx2lib = '') | (gcclib = '') then do
        say ' Warning: The libraries necessary for linking with object',
            'files in'
        say '          OMF format do not exist. Object format reset to AOUT.'
        ObjectStyle = 'aout'
     end
  end


/****************************************************/
/*     build the substitutes for some strings       */
/****************************************************/

/* Cflags */
  Cflags  = '-DUNIX -O2 -m486'

/* PrefixDir */
  if CmdShell = 'cmd.exe' then do
     PrefixDir = translate( PrefixDir, '\', '/')
     if ( right( PrefixDir, 1 ) = '\' ) then
        PrefixDir = left( PrefixDir, length(PrefixDir)-1 )
     j = LastPos('\', PrefixDir)
     do while (j \= 0)
        j = j - 1
        PrefixDir = insert('\\\', PrefixDir , j)
        j = LastPos('\', SubStr( PrefixDir, 1, j ))
     end
     end
  else do
     PrefixDir = translate( PrefixDir, '/', '\')
     if ( right( PrefixDir, 1 ) = '/' ) then
        PrefixDir = left( PrefixDir, length(PrefixDir)- 1 )
     j = LastPos('/', PrefixDir)
     do while (j \= 0)
        j = j - 1
        PrefixDir = insert('\', PrefixDir , j)
        j = LastPos('/', SubStr( PrefixDir, 1, j ))
     end
  end

/* Ldflags */
  if ObjectStyle = 'omf' then do
     if Linking = 'dynamic' then
        Ldflags = '-s -Zomf -Zcrtdll -Zstack 8192 -Zlinker \/pm:vio'
     else
        Ldflags = '-s -Zomf -Zsys -Zstack 8192 -Zlinker \/pm:vio'
     end
  else do
     if Linking = 'dynamic' then do
        Ldflags = '-s'
        if UseRegex = 'yes' then do
          Ldflags = '-s'
          Ldlibs = '-lregex'
          RegexDef = '1'
	end
        else
          Ldlibs = ''
	end
     else
        say ' Error: Static linking is not possible for ',
            'aout object file format.'
  end

/* ObjectsListName, ObjectsToDelete */
  if ObjectStyle = 'omf' then do
     ObjectsListName = '$(OBJS:.o=.obj)'
     ObjectsToDelete = '*.o *.obj'
     end
  else do
     ObjectsListName = '$(OBJS)'
     ObjectsToDelete = '*.o'
  end


  say ' Generating Makefiles : '

/****************************************/
/*     process Makefile.in              */
/****************************************/

  say '  Processing Makefile.in ...'

  '@echo off'
  'sed -e "s/\/usr\/local/'PrefixDir'/" ',
      '-e "s/@EXE@/.exe/" ',
      '-e "s/@CMD@/.cmd/" ',
      '-e "s/@CC@/'CCompiler'/" ',
      '-e "s/@MANtoPS@//" ',
      '-e "s/@AWK@/'AwkProg'/" ',
      '-e "s/@NROFF@/'NroffProg'/" ',
      '-e "s/@SED@/'SedProg'/" ',
      '-e "s/@STRIP@/'StripProg'/" ',
      '-e "s/@YACC@/'YaccProg'/" ',
      '-e "s/@COL@/'ColProg'/" ',
      '-e "s/@CHMOD@/'ChmodCmd'/" ',
      '-e "s/@CMP@/'CmpCmd'/" ',
      '-e "s/@CP@/'CpCmd'/" ',
      '-e "s/@DIFF@/'DiffCmd'/" ',
      '-e "s/@EQN@/'EqnProg'/" ',
      '-e "s/@GREP@/'GrepProg'/" ',
      '-e "s/@PWD_PROG@/'PwdProg'/" ',
      '-e "s/@ZIP@/'ZipProg'/" ',
      '-e "s/@PRINTENV@/'PrintenvProg'/" ',
      '-e "s/@MKDIR@/'MkdirCmd'/" ',
      '-e "s/@MV@/'MvCmd'/" ',
      '-e "s/@RM@/'RmCmd'/" ',
      '-e "s/@RMDIR@/'RmdirCmd'/" ',
      '-e "s/@SH@/'CmdShell'/" ',
      '-e "s/@SOELIM@/'SoelimProg'/" ',
      '-e "s/@TBL@/'TblProg'/" ',
      '-e "s/@TAR@/'TarProg'/" ',
      '-e "s/@CFLAGS@/'Cflags'/" ',
      '-e "s/@LDFLAGS@/'Ldflags'/" ',
      '-e "s/@LDLIBS@/'Ldlibs'/" ',
      '-e "s/@INSTALL_MAN@//" ',
      '-e "s/@FTNPP@//" ',
      '-e "s/@CPPFLAGS@//" ',
      '<'InputFile ' >'TmpFile

  'sed -e "s/$(OBJS)/'ObjectsListName'/" ',
      '-e "s/(RM) \*\.o/(RM) 'ObjectsToDelete'/" ',
      '<'TmpFile ' >'OutputFile

  if ObjectStyle = 'omf' then do
     call lineout OutputFile, ""
     call lineout OutputFile, "# Target to convert object files from",
                              "a.out format to OMF format"
     call lineout OutputFile, "%.obj: %.o"
     call lineout OutputFile,"	emxomf -s $?"
     call lineout OutputFile
  end

  if CmdShell = 'cmd.exe' then do
     'copy 'OutputFile TmpFile '>nul:'
     'sed -e "s/X)\/bin/X)\\bin/" ',
         '-e "s/X)\/man/X)\\man/" ',
         '-e "s/X)\/lib\/ftnchek/X)\\\\lib\\\\ftnchek/" ',
         '-e "s/DIR)\//DIR)\\/" ',
         '-e "s/cat1\//cat1\\/" ',
         '-e "s/man1\//man1\\/" ',
         '-e "s/= \.\//= \.\\/" ',
         '-e "s/^fortran.c:/fortran.c.unx:/" ',
         ' <'TmpFile ' > ' OutputFile

     call lineout OutputFile, ''
     call lineout OutputFile, '# Build dcl2inc.cmd using the local',
                              'values for NAWK and LIBDIR.'
     call lineout OutputFile, 'dcl2inc.cmd: dcl2inc.in' OutputFile
     call lineout OutputFile, '	sed -e "s%#!/bin/sh%@echo off%"  \'
     call lineout OutputFile, '	    -e "s%#%rem %"               \'
     call lineout OutputFile, '	     dcl2inc.in > dcl2inc.cmd'
     call lineout OutputFile, '	echo $(NAWK) -f $(LIBDIR)\\dcl2inc.awk',
                              '%1 >> dcl2inc.cmd'
     call lineout OutputFile, ''
     call lineout OutputFile, '# N.B. tokdefs.h is copy of y.tab.h used',
                              'to avoid remaking stuff when'
     call lineout OutputFile, '# grammar changes but not tokens.'
     call lineout OutputFile, '# The following copies y.tab.h to',
                              'tokdefs.h if changed, then aborts make,'
     call lineout OutputFile, '# since dependencies may have changed.'
     call lineout OutputFile, 'fortran.c: fortran.y'
     call lineout OutputFile, '	$(YACC) $(YFLAGS) fortran.y'
     call lineout OutputFile, '	$(MV) y.tab.c fortran.c'
     call lineout OutputFile, '	$(CMP) y.tab.h tokdefs.h ||  \'
     call lineout OutputFile, '	(echo tokdefs.h changed -- repeat make  &  \'
     call lineout OutputFile, '		$(CP) y.tab.h tokdefs.h)'
     call lineout OutputFile
  end
  'del ' TmpFile
  say '  Makefile written.'

/****************************************/
/*     process config-generic.h         */
/****************************************/

/*
 emx09d on OS/2 has unistd.h, stdlib.h.
 regex.h. is available from GNU libregex-0_12 port, which must be
 installed. To do so, one needs to fetch both
 http://ftp-os2.nmsu.edu/pub/os2/dev/unix/libregex-0_12-bin.zip
 http://ftp-os2.nmsu.edu/pub/os2/dev/unix/libregex-0_12.zip
 as bin contains the actual library but the header file is in the
 source archive
 emx09d also has both memset and bzero, and stricmp instead of
	 strcasecmp
*/

  say '  Processing ' ConfigGenericFile ' ...'

  '@echo off'
  'sed -e "s/#define HAVE_UNISTD_H.*0$/#define HAVE_UNISTD_H 1/" ',
      '-e "s/#define HAVE_STRCASECMP.*1/#define HAVE_STRCASECMP 0/" ',
      '-e "s/#define HAVE_STRICMP.*0/#define HAVE_STRICMP 1/" ',
      '-e "s/#define HAVE_REGEX_H.*[01]/#define HAVE_REGEX_H 'RegexDef'/" ',
      ConfigGenericFile ' > ' ConfigOS2File
  say '  ' ConfigOS2File ' written.'

/** weiter hier ***/

/****************************************/
/*     process files in test directory: */
/****************************************/

  'cd 'TestDir

/****************************************/
/*     process test/Makefile.in         */
/****************************************/

  say '  Processing test/Makefile.in ...'

  '@echo off'
  'sed -e "s/\/usr\/local/'PrefixDir'/" ',
      '-e "s/@EXE@/.exe/" ',
      '-e "s/@CMD@/.cmd/" ',
      '-e "s/@CC@/'CCompiler'/" ',
      '-e "s/@MAKE@/'MakeProg'/" ',
      '-e "s/@AWK@/'AwkProg'/" ',
      '-e "s/@NROFF@/'NroffProg'/" ',
      '-e "s/@SED@/'SedProg'/" ',
      '-e "s/@STRIP@/'StripProg'/" ',
      '-e "s/@YACC@/'YaccProg'/" ',
      '-e "s/@COL@/'ColProg'/" ',
      '-e "s/@CHMOD@/'ChmodCmd'/" ',
      '-e "s/@CMP@/'CmpCmd'/" ',
      '-e "s/@CP@/'CpCmd'/" ',
      '-e "s/@DIFF@/'DiffCmd'/" ',
      '-e "s/@EQN@/'EqnProg'/" ',
      '-e "s/@GREP@/'GrepProg'/" ',
      '-e "s/@PWD_PROG@/'PwdProg'/" ',
      '-e "s/@ZIP@/'ZipProg'/" ',
      '-e "s/@PRINTENV@/'PrintenvProg'/" ',
      '-e "s/@MKDIR@/'MkdirCmd'/" ',
      '-e "s/@MV@/'MvCmd'/" ',
      '-e "s/@RM@/'RmCmd'/" ',
      '-e "s/@RMDIR@/'RmdirCmd'/" ',
      '-e "s/@SH@/'CmdShell'/" ',
      '-e "s/@SOELIM@/'SoelimProg'/" ',
      '-e "s/@TBL@/'TblProg'/" ',
      '-e "s/@TAR@/'TarProg'/" ',
      '-e "s/@CFLAGS@/'Cflags'/" ',
      '-e "s/@LDFLAGS@/'Ldflags'/" ',
      '-e "s/@CPPFLAGS@//" ',
      '<'TestInputFile ' >'TestOutputFile

  say '  test/Makefile written.'

/****************************************/
/*     process test/Compare.sh.in          */
/****************************************/

  say '  Processing test/Compare.sh.in ...'

  '@echo off'
  'sed -e "s/\/usr\/local/'PrefixDir'/" ',
      '-e "s/@EXE@/.exe/" ',
      '-e "s/@CMD@/.cmd/" ',
      '-e "s/@CC@/'CCompiler'/" ',
      '-e "s/@MANtoPS@//" ',
      '-e "s/@AWK@/'AwkProg'/" ',
      '-e "s/@NROFF@/'NroffProg'/" ',
      '-e "s/@SED@/'SedProg'/" ',
      '-e "s/@STRIP@/'StripProg'/" ',
      '-e "s/@YACC@/'YaccProg'/" ',
      '-e "s/@COL@/'ColProg'/" ',
      '-e "s/@CHMOD@/'ChmodCmd'/" ',
      '-e "s/@CMP@/'CmpCmd'/" ',
      '-e "s/@CP@/'CpCmd'/" ',
      '-e "s/@DIFF@/'DiffCmd'/" ',
      '-e "s/@EQN@/'EqnProg'/" ',
      '-e "s/@GREP@/'GrepProg'/" ',
      '-e "s/@PWD_PROG@/'PwdProg'/" ',
      '-e "s/@ZIP@/'ZipProg'/" ',
      '-e "s/@MKDIR@/'MkdirCmd'/" ',
      '-e "s/@MV@/'MvCmd'/" ',
      '-e "s/@RM@/'RmCmd'/" ',
      '-e "s/@RMDIR@/'RmdirCmd'/" ',
      '-e "s/@SH@/'CmdShell'/" ',
      '-e "s/@SOELIM@/'SoelimProg'/" ',
      '-e "s/@TBL@/'TblProg'/" ',
      '-e "s/@TAR@/'TarProg'/" ',
      '-e "s/@CFLAGS@/'Cflags'/" ',
      '-e "s/@LDFLAGS@/'Ldflags'/" ',
      '-e "s;/bin/sh;'ShPath';" ',
      '<'CompInputFile ' >'CompOutputFile

  say '  test/Compare.sh written. '

/* -------------------------------------------------------------- */
/* Note: This can be done by REXX:                                */
/* -------------------------------------------------------------- */

  if (ShPath = '/bin/sh') then do
    say '  Note: Edit first line by hand to point to sh.exe!'
  end

  'cd ..'

  say ' Done. Now copy ' ConfigOS2File ' onto config.h and run make'
  say ' '
  say '   copy ' ConfigOS2File ' config.h'  
  say '   make        to make ftnchek.exe and the manual page ftnchek.1'
  say '   make check  to run the tests (that takes time)'
  say
  say ' In case you have the GNU regex library (*.h, *.a) installed, you'
  say ' may wish to enable also the -makehtml option of ftnchek. To do so,'
  say ' rerun configure_os2.cmd -r before the above four steps.'
  say ' To save about 80kB, you can compress the executable with LxLite,'
  say ' if you have it installed:'
  say '               lxlite ftnchek.exe'

  exit 0

/* PROCEDURE Usage */
  Usage: PROCEDURE
     say ' Usage: configure_os2 [options]'
     say
     say '    Options:'
     say '       -f <string>  format of object files: omf, aout'
     say '       -l <string>  linking: static, dynamic'
     say '       -o <string>  name of newly created Makefile'
     say '       -p <string>  prefix directory'
     say '       -r           build using GNU regex library'
     say '       -h           show this help'
     exit 1

/* PROCEDURE ToLower */
  ToLower: PROCEDURE
     parse arg OldString

     NewString = translate(OldString, xrange('a','z'), xrange('A','Z'))
     return NewString

/* PROCEDURE IsProgramInPath */
  IsProgramInPath: PROCEDURE
     parse arg ProgramName

     Answer = FileSpec('name', SysSearchPath('PATH', ProgramName))
     return Answer

/* PROCEDURE SearchProgramFromList */
  SearchProgramFromList: PROCEDURE
     parse arg ProgramList

     NumberInList = words( ProgramList )

     FirstFound = ''
     i = 1
     do while (i <= NumberInList) & (FirstFound = '')
        Program = word( ProgramList, i )
        i = i + 1

        FirstFound = IsProgramInPath( Program )
     end
     return FirstFound
