#=======================================================================
# $Id: Makefile.in,v 1.65 2003/03/20 22:07:55 moniot Exp $
#
# UNIX version of Makefile for Fortran program checker ftnchek
#
# Current target list:
#	TAGS
#	all			build ftnchek and its documentation files
#	blurb.txt
#	check			run validation test suite
#	clean			remove unnecessary intermediate files
#	clobber			make clean, and remove ftnchek executable
#	dcl2inc.doc		ASCII form of documentation
#	dcl2inc.ps		PostScript form of documentation
#	dcl2inc			Shell script to convert decls to includes
#	devel			Build development version (extra debugging).
#	distclean		remove everything not in the distribution
#	docs			documentation files
#	fortran.c		translate yacc code to C
#	ftnchek			ftnchek executable program
#	ftnchek.1		UNIX manual page document
#	ftnchek.doc		ASCII form of documentation
#	ftnchek.hlp		VAX/VMS HELP library source
#	htmldocs		hypertext form of documentation
#	ftnchek.ps		PostScript form of documentation
#	install			install ftnchek and dcl2inc and their man pages
#	install-exe		install ftnchek and dcl2inc executables
#	install-man		install only man pages
#	install-man-sgi		install only man pages on SGI systems
#	install-lisp		install only ftnchek.el
#	lint			run lint on source code
#	manpage			Unix man page
#	mostlyclean		same as clean
#	prog			same as ftnchek
#	realclean		same as clobber
#	spotless		make distclean, then remove formatted
#				documentation (do NOT do this if you don't
#				have groff or nroff/troff to recreate the
#				documentation!)
#	maintainer-clean	distclean, then things maintainer can rebuild
#	TAGS			emacs editor tags file
#	tags			ex and vi editor tags file
#	uninstall		undo an ftnchek installation
#	tar			distribution tar file
#
# Object file targets		see list of objects at OBJS
#
#
#
# Copyright (c) 2001 by Robert K. Moniot.
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following
# conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the
# Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
# KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
# Acknowledgement: the above permission notice is what is known
# as the "MIT License."
# 
#
# Acknowledgements and thanks to Nelson H. F. Beebe of the University
# of Utah for improvements to this Makefile.
# [25-Apr-1994]
#=======================================================================

# These definitions should be customized for your local conventions
# if you want to do "make install" or "make uninstall".
#  prefix is the root of the destination for installing things.
#  bindir is directory for executables, mandir for man pages.
#  manext is the extension on man pages, including the dot.
#  libdir is the directory for library files (dcl2inc.awk).  It must
#    agree with the libdir defined in dcl2inc as made by configure.
#  lispdir is where to put ftnchek.el.

prefix		= @prefix@
exec_prefix	= @exec_prefix@
bindir		= @bindir@
datadir		= @datadir@
mandir		= @mandir@
manext		= .1
sgimansubdir	= @sgimansubdir@
libdir		= @libdir@/ftnchek
lispdir		= @lispdir@

# Define suffix for executables, for non-unix systems which have one.
EXE		= .exe
#EXE		= .exe

# Define suffix for scripts, to go on the end of dcl2inc script.
CMD		= .cmd
#CMD		= .cmd
#CMD		= .bat

# The following definitions are set for your system by configure
CC              = gcc.exe
CHMOD		= chmod.exe
CMP		= diff.exe
CP		= cp.exe
DIFF		= diff.exe
EMACS		= @EMACS@
EQN		= eqn.exe
GREP		= grep.exe
MANtoHTML	= @MANtoHTML@
MANtoPS		= 
MKDIR		= mkdir.exe
MV		= mv.exe
NAWK		= gawk.exe
NROFF		= groff.exe
NROFFPLAIN	= @NROFFPLAIN@
PATCH		= @PATCH@
PWD_PROG	= pwd.exe
RM		= rm.exe -f
RMDIR		= rmdir.exe
SED		= sed.exe
SHELL		= sh.exe
SOELIM		= soelim.exe
STRIP           = echo Requested: strip
TAR		= tar.exe
TBL		= tbl.exe
YACC            = bison.exe -y
ZIP		= zip.exe

# The following is only used for targets ftnchek.doc and ftnchek.hlp
# which are not needed for unix platforms.  It removes control chars
# and converts tabs to blanks.  Normally COL=col -bx but if you don't
# have it, can make do with cat.
COL		= 

# Editor tags file support
CTAGS		= ctags
CTAGSFLAGS	= -t
ETAGS		= etags

# The following suffixes are for making various forms of documentation
.SUFFIXES: .cat .catman .doc .man .ps .i


# Pack is needed only for install-man-sgi target
PACK		= @PACK@

FTNCHEK		= ./ftnchek$(EXE)

# OPTIONS is used to define various characteristics.  Most commonly
# needed ones are given below; uncomment whichever you like.
# See ftnchek.h for others, with their defaults and explanations.
# OPTIONS shouldn't include things controlled by "configure".
# To include your own options without touching the Makefile, say
#              make "OPTIONS= <your-list-of-options>"

# If you want -f77=all to be the default, uncomment this.
#OPTIONS        = -DSTRICT_SYNTAX

# OPT is optimization level
# CFLAGS is used to define the operating system and options
# LDFLAGS gives linker options
# YFLAGS specifies yacc options
OPT		= @OPT@
CFLAGS          = -DUNIX -O2 -m486 $(DEBUG_FLAGS) $(DEVEL_CFLAGS)
LDFLAGS         = -s $(DEBUG_FLAGS) $(DEVEL_LDFLAGS)
LDLIBS          = -lregex
YFLAGS          = -d

# fortran.o first because of possible remake if tokdefs.h changes (see below)
OBJS            = fortran.o advance.o argcheck.o calltree.o comcheck.o exprtype.o \
		  forlex.o ftnchek.o include.o intrins.o iokeywds.o keywords.o labels.o \
		  loccheck.o makedcls.o makehtml.o message.o options.o pgsymtab.o \
		  plsymtab.o prlists.o prlocsym.o project.o \
		  symspace.o symtab.o symutils.o utils.o

DOCS		= README INSTALL LICENSE PATCHES FAQ ToDo project.magic \
			average.f average.out correct.f \
			blurb.txt dcl2inc.doc dcl2inc.man dcl2inc.ps \
			ftnchek.doc ftnchek.hlp ftnchek.man ftnchek.ps html \
			macro-doc.txt project-doc.tex \
			$$dirname.lsm
#		 the above expands to ftnchek-x.y.z.lsm in make tar

UNIX_BUILDFILES	= Makefile.in configure configure.in config.guess config.sub \
			config.h.in configure.generic install-sh \
			add_html_refs.sh make_html.sh \
			make_blockmatch.pl make_blockmatch.pl.in

VMS_BUILDFILES	= build.com build-vax.com cc.com link.com check.com

OS2_BUILDFILES	= ftnchek.def configure_os2.cmd

OTHER_BUILDFILES= makefile.bcc32 \
			makefile.generic makefile.mpw \
			ftnchek.dsp ftnchek.dsw

BUILDFILES	= $(UNIX_BUILDFILES) $(VMS_BUILDFILES) $(OS2_BUILDFILES) \
			$(OTHER_BUILDFILES)

SRCS            = advance.c advance.h argcheck.c block_match.h \
		  calltree.c comcheck.c config-generic.h config-win32.h exprtype.c \
		  forlex.c forlex.h fortran.y fortran.c ftnchek.c ftnchek.h \
		  include.c intrins.c intrins.h iokeywds.h iokeywds_enum.h iokeywds.c \
		  keywords.c labels.c \
		  loccheck.c loccheck.h makedcls.c makehtml.c message.c options.c \
		  options.h pgsymtab.c pgsymtab.h plsymtab.c plsymtab.h \
		  prlists.c prlocsym.c project.c symspace.c \
		  symspace.h symtab.c symtab.h symutils.c symutils.h tokdefs.h \
			utils.h utils.c
SCRIPTS		= dcl2inc dcl2inc.in dcl2inc.awk dcl2inc.awk.in \
		  man2ps ftnchek.el
TESTDIR		= test
HTMLDIR		= html
# The following defines all files to go into distribution tar file
DISTFILES	= $(DOCS) $(BUILDFILES) $(SRCS) $(SCRIPTS) $(TESTDIR)
# The following defines all files to go into MS-DOS distribution zip file
ZIPFILES	= $(DISTFILES)
# The following defines all files that go into the auxiliary catman tar file
CATMAN		= dcl2inc.catman ftnchek.catman

# Files in the following list are created by the configure script.
# Generic versions for the distribution are made by the generic-config
# target.
CONFIGURED_FILES = makefile.generic dcl2inc.awk dcl2inc make_blockmatch.pl \
			test/Makefile test/Compare.sh config-generic.h
#=======================================================================
#  Rule to run source thru preprocessor only; useful for tracing down
#  compilation problems.  Blank lines are removed for convenient viewing.
.c.i:
	$(CC) -E -I. $(DEFS) $(CPPFLAGS) $(CFLAGS) $< 2>&1 | \
		grep -v '^[ 	]*$$' > $@

#=======================================================================
all:	prog manpage

# N.B. dcl2inc is also a prog but it is made by configure.  A generic
# copy of it is provided with the distribution also, in case the user
# doesn't want to or can't use configure.
prog:	ftnchek$(EXE)

# The manpage target is a "clean-ed up" version of the Unix man page,
# with conditional text, used by developer to create other forms of
# documentation, removed.
manpage: ftnchek$(manext)

#  Development version includes debugging machinery not normally used.  It
#  evaluates some floating-point expressions including sqrt, so needs math lib.
devel:
	$(MAKE) prog DEVEL_CFLAGS='-DDEVELOPMENT' DEVEL_LDFLAGS='-lm'

check: prog
	cd $(TESTDIR); $(MAKE) NAWK="$(NAWK)" -s precheck
	cd $(TESTDIR); $(MAKE) NAWK="$(NAWK)" -i -s check

lint:
	lint $(LINTFLAGS) $(OBJS:.o=.c) -lm

# Remove intermediate files that are not required after the program is
# built.
clean mostlyclean:
	-$(RM) *.o
	-$(RM) \#*
	-$(RM) a.out
	-$(RM) *~
	-$(RM) core
	-$(RM) y.tab.h
	-$(RM) config.status config.log config.cache
	-$(RM) ftnchek.html.orig ftnchek.html.rej

# Remove almost everything that make can rebuild.  We do not remove
# fortran.c since some places don't have yacc or bison.
clobber realclean:	clean
	-$(RM) ftnchek$(EXE)

# Remove everything that make can rebuild, preparatory to making a
# distribution version.  We intentionally do NOT remove .ps and .doc
# files, because some UNIX systems lack nroff/troff/groff.
distclean:	realclean testclean htmlclean
	-$(RM) *.dcl
	-$(RM) cscope.out
	-$(RM) tags
	-$(RM) TAGS
	-$(RM) ftnchek$(manext)
	-$(RM) dcl2inc.cat ftnchek.cat

testclean:
	cd $(TESTDIR); $(MAKE) distclean

htmlclean:
	cd $(HTMLDIR); $(MAKE) -f ../Makefile clean

maintainer-clean:	distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."
	-$(RM) Makefile
	-$(RM) configure
	-$(RM) fortran.c
	-$(RM) *.catman

ftnchek$(EXE): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o ftnchek$(EXE) $(OBJS) $(LDLIBS)

# N.B. tokdefs.h is copy of y.tab.h used to avoid remaking stuff when
# grammar changes but not tokens.  If the parser is made by bison,
# it is edited to make token names in "parse error" messages more readable.
# The following copies y.tab.h to tokdefs.h if changed, then aborts make,
# since dependencies may have changed.
fortran.c: fortran.y
	$(YACC) $(YFLAGS) fortran.y
	@if test "$(YACC)" = "bison$(EXE) -y" -o "$(YACC)" = "bison -y" ; \
	then \
	  $(SED) -e '/yytname\[] =/,/^};/s/tok_//g' \
	         -e '/yytname\[] =/,/^};/s/_/ /g' \
	         -e '/yytname\[] =/,/^};/s/EOS/end of statement/g' \
		y.tab.c > fortran.c ; \
	  $(RM) y.tab.c ; \
	else \
	  $(MV) y.tab.c fortran.c ; \
	fi
	@if $(CMP) -s y.tab.h tokdefs.h ; then true ; else \
		echo; echo tokdefs.h changed -- repeat make ; \
		$(CP) y.tab.h tokdefs.h; \
		false ; \
	fi

# If tokdefs.h changes, update the lookup table used by block-balancing code.
block_match.h: tokdefs.h
	./make_blockmatch.pl > block_match.h

# This target makes an enum type for doing reverse lookup in the iokeywords table.
# The uniq filter is needed since some keywords may have alternate definitions depending
# on #defines.  These are always adjacent.  The last sed command removes
# comma from last item in enum list, in accordance with c89.
iokeywds_enum.h: iokeywds.h
	sed -n '/^} io_keywords\[\]={/,/^};$$/s/^{"\([^"]*\).*$$/IOKW_\1,/p' iokeywds.h \
		| uniq \
		| sed '$$s/,$$//' > $@

# Documentation targets: If you make changes to the documentation,
# you should edit only ftnchek.man and then re-make these targets.
# If ftnchek.1 fails to make, ftnchek.man is usable directly (after
# applying soelim) except that some troff-like processors may not
# give satisfactory results.

docs:	average.out blurb.txt ftnchek.doc ftnchek.hlp \
	htmldocs ftnchek.ps dcl2inc.doc dcl2inc.ps


# Following sed script trims out the if-else machinery contained in
# ftnchek.man to produce a clean nroff document.  The lines of the
# sed script correspond to the following actions:
#	1. remove text between lines of form ``.if \nh \{'' or .ie \nh \{''
#	   and lines of form ``\}'' (these are the help-related additions)
#	2. remove lines of form ``.if !\nh \{''  (these preface man text)
#	   Also remove ``.if !\nb \{'' which are for blurb.
#	3. remove lines of form ``.el \{''  (start of the else sections of
#	   the .if's of step 1)
#	4. remove lines of form ``\}'' (closures of step 3 lines)
#	5. remove lines of form ``.ie \nh text'' (one-liner help text)
#	6. change lines of form ``.el \nh text'' to ``text'' (these are
#	   one-liner man text)
#	7. change lines of form ``.if !\nh text'' to ``text'' (ditto)
ftnchek$(manext):	ftnchek.man average.f average.out correct.f
	@if test "$(SOELIM)" = "soelim$(EXE)"; \
	then \
	  echo "Making cleaned-up manpage ftnchek$(manext)" ; \
	  $(SOELIM) ftnchek.man | \
	  $(SED) -e '/^\.i[fe] \\nh \\{/,/^\\}$$/d' \
	    -e '/^\.if !\\n[bh] \\{/d' \
	    -e '/^\.el \\{/d' \
	    -e '/^\\}/d' \
	    -e '/^\.i[fe] \\nh /d' \
	    -e 's/^\.el *\(.*\)$$/\1/' \
	    -e 's/^\.if !\\nh *\(.*\)$$/\1/' \
			> ftnchek$(manext) ; \
	else \
	  echo "$(SOELIM) not available: cannot make cleaned-up manpage ftnchek$(manext)." ; \
	  echo "You must use the catman documentation (provided separately)." ; \
	fi

# .doc files are ascii text form of documentation.
.man.doc:
	$(TBL) $< | $(NROFFPLAIN) -man | $(COL) > $@

# ftnchek.ps is PostScript form of documentation.  For ftnchek.ps we use
# cleaner ftnchek.1 rather than ftnchek.man.
ftnchek.ps:	ftnchek$(manext)
	$(TBL) <ftnchek$(manext) | $(EQN) | $(MANtoPS) >ftnchek.ps

# Other man files are clean enough as they are.
.man.ps:
	$(TBL) < $< | $(EQN) | $(MANtoPS) > $@

# $(HTMLDIR)/toc.html represents all the files in the html directory.  They
# are built from ftnchek.html by the script make_html.sh whenever
# ftnchek.html changes.  Note: edits should be done to ftnchek.man if
# they affect content, to ftnchek.html if they affect html form.  Do
# not edit anything in html directory.

htmldocs: $(HTMLDIR)/toc.html

$(HTMLDIR)/toc.html: ftnchek.html
	./make_html.sh ftnchek.html

# ftnchek.html is produced from ftnchek.1 by MANtoHTML followed by a
# simple script to add internal links to option descriptions, then
# lightly edited.  Edits were initially by hand, now are saved in
# ftnchek.html.patch (not distributed) to be re-applied each time.
# Any time new edits are done to ftnchek.html, the patch file should
# be remade.  Also make ftnchek.html.patch after editing ftnchek.man,
# so that the patch offsets don't get too large for patch to succeed.

ftnchek.html:	ftnchek.html.orig
	$(CP) ftnchek.html.orig ftnchek.html
	if test -f ftnchek.html.patch ; \
	then \
	   $(PATCH) < ftnchek.html.patch ; \
	fi

ftnchek.html.orig: ftnchek$(manext)
	$(MANtoHTML) ftnchek$(manext) | ./add_html_refs.sh > ftnchek.html.orig

ftnchek.html.patch: ftnchek.html.orig ftnchek.html
	@echo "It is normal for the next command to report an error"
	-$(DIFF) -c ftnchek.html.orig ftnchek.html > ftnchek.html.patch

# blurb.txt is an ascii file for informational mailings.
blurb.txt:	ftnchek.man
	$(TBL) ftnchek.man | $(NROFFPLAIN) -man -rb1 | $(COL) >blurb.txt

# ftnchek.hlp is a VMS HELP library source document:
# create ftnchek.hlb with $ LIBR/CREATE/HELP FTNCHEK.HLB FTNCHEK.HLP
# The leading and trailing newlines in ftnchek.hlp should be removed.
ftnchek.hlp:	ftnchek.man
	$(SED) -e '1d' ftnchek.man | \
	$(TBL) | $(NROFFPLAIN) -man -rh1 | $(COL) | \
	$(SED) -e 's/^-\([a-zA-Z][a-zA-Z]*\)/\/\1/' \
	    -e 's/\([^a-zA-Z]\)-\([a-zA-Z][a-zA-Z]*\)/\1\/\2/g' \
	>ftnchek.hlp


# Rebuild average.out when version changes.  (We don't put the
# dependency on ftnchek to avoid building ftnchek just for docs.)
average.out: ftnchek.h
	$(FTNCHEK) -list -symtab average > average.out

# Install program and documentation on system.  Obsolete fcl2vcg script
# & doc is removed if present.  Old program is first deleted, in case
# it is a hard link to specific previous version, so that version does
# not get overwritten.
install:	install-exe  install-lisp

install-exe:	ftnchek$(EXE) dcl2inc$(CMD)
	-$(RM) $(bindir)/ftnchek$(EXE)
	-$(MKDIR) -p $(bindir)
	$(CP) ftnchek$(EXE) $(bindir)
	-$(STRIP) $(bindir)/ftnchek$(EXE)
	$(CHMOD) 755 $(bindir)/ftnchek$(EXE)
	-$(MKDIR) -p $(libdir)
	$(CP) dcl2inc.awk $(libdir)/dcl2inc.awk
	$(CHMOD) 644 $(libdir)/dcl2inc.awk
	$(CP) dcl2inc$(CMD) $(bindir)/dcl2inc$(CMD)
	$(CHMOD) 755 $(bindir)/dcl2inc$(CMD)
	-$(RM) $(bindir)/fcl2vcg

# Install man pages, taking care to remove old formatted ones, because
# many man implentations fail to compare time stamps of raw and
# formatted files, and will show out-of-date formatted files.
install-man: ftnchek$(manext)
	-$(MKDIR) -p $(mandir)/man1
	$(CP) dcl2inc.man $(mandir)/man1/dcl2inc$(manext)
	-$(RM) $(mandir)/cat1/dcl2inc$(manext)
	$(CHMOD) 644 $(mandir)/man1/dcl2inc$(manext)
	$(CP) ftnchek$(manext) $(mandir)/man1/ftnchek$(manext)
	-$(RM) $(mandir)/cat1/ftnchek$(manext)
	$(CHMOD) 644 $(mandir)/man1/ftnchek$(manext)
	-$(RM) $(mandir)/man1/fcl2vcg$(manext)
	-$(RM) $(mandir)/cat1/fcl2vcg$(manext)

# IRIX uses pre-formatted, packed man pages and nroff is not bundled with it.
install-man-sgi: catman
	if $(PACK) dcl2inc.cat ; \
	then \
		$(MV) dcl2inc.cat.z $(mandir)$(sgimansubdir)/dcl2inc.z ; \
		$(CHMOD) 644 $(mandir)$(sgimansubdir)/dcl2inc.z ; \
	fi
	if $(PACK) ftnchek.cat ; \
	then \
		$(MV) ftnchek.cat.z $(mandir)$(sgimansubdir)/ftnchek.z ; \
		$(CHMOD) 644 $(mandir)$(sgimansubdir)/ftnchek.z ; \
	fi
	-$(RM) $(mandir)/fcl2vcg.z

# The catman target makes formatted ("cat") versions of the manpages
# for use in install-man-sgi.  These files are not in the standard
# distribution but are available separately (with suffix catman) for
# sites that do not have nroff.  If this Unix does not have nroff,
# then if user has obtained the catman versions, use them.  Otherwise
# inform them and use the flat ascii files as fallback.
catman: dcl2inc.cat ftnchek.cat

.man.cat:
	@if test -x $(TBL) -a  -x $(NROFF) ; then \
	  echo '$(TBL) $< | $(NROFF) -man > $@' ; \
	  $(TBL) $< | $(NROFF) -man > $@ ; \
	else \
	  if test -f $*.catman ; then \
	    echo cp $*.catman $@ ; \
	    cp $*.catman $@ ; \
	  else \
	    echo "--> No catman documentation found -- see INSTALL." ; \
	    echo "Using plain text documentation instead." ; \
	    echo cp $*.doc $@ ; \
	    cp $*.doc $@ ; \
	  fi ; \
	fi

#  The emacs lisp file will be installed only if lispdir exists.  It will
#  be byte-compiled if emacs is present.
install-lisp:
	@if test -d "$(lispdir)" ; \
	then \
	  echo $(CP) ftnchek.el $(lispdir)/ftnchek.el ; \
	  if $(CP) ftnchek.el $(lispdir)/ftnchek.el ; \
	  then \
	     if test -x "$(EMACS)" ; \
	     then \
	       $(EMACS) -batch -f batch-byte-compile $(lispdir)/ftnchek.el ; \
	     else \
	       echo "If desired, use emacs to byte-compile $(lispdir)/ftnchek.el"; \
	     fi \
	  fi \
	else \
	  echo "$(lispdir) does not exist -- ftnchek.el not installed." ; \
	  echo "If you want to install ftnchek.el, create $(lispdir)" ; \
	  echo "or re-run make install with 'lispdir=path-to-site-lisp'" ; \
	fi

# Remove everything that the install target installed.
uninstall:
	-$(RM) $(bindir)/dcl2inc$(CMD)
	-$(RM) $(bindir)/ftnchek$(CMD)
	-$(RM) $(libdir)/dcl2inc.awk
	-$(RM) $(mandir)/cat1/dcl2inc$(manext)
	-$(RM) $(mandir)/cat1/ftnchek$(manext)
	-$(RM) $(mandir)/dcl2inc.z $(mandir)/ftnchek.z # SGI
	-$(RM) $(mandir)/man1/dcl2inc$(manext)
	-$(RM) $(mandir)/man1/ftnchek$(manext)
	-$(RM) $(lispdir)/ftnchek.el
	-$(RMDIR) $(libdir)

# WARNING: do NOT execute this target, unless you have nroff/troff or groff
# to recreate the formatted documentation files.
spotless:	distclean
	$(RM) blurb.txt
	$(RM) dcl2inc.doc ftnchek.doc
	$(RM) dcl2inc.ps ftnchek.ps
	$(RM) ftnchek.hlp

# ex and vi editor tags file
tags:	$(SRCS)
	$(CTAGS) $(CTAGSFLAGS) $(SRCS)

# emacs editor tags file
TAGS:	$(SRCS)
	$(ETAGS) $(SRCS)

#=======================================================================
# Object file dependencies on include files

advance.o:	config.h ftnchek.h symtab.h tokdefs.h forlex.h advance.h

argcheck.o:	config.h ftnchek.h pgsymtab.h symtab.h

calltree.o:	config.h ftnchek.h pgsymtab.h symtab.h

comcheck.o:	config.h ftnchek.h pgsymtab.h symtab.h

exprtype.o:	config.h ftnchek.h symtab.h tokdefs.h

forlex.o:	config.h ftnchek.h symtab.h tokdefs.h forlex.h advance.h

fortran.o:	config.h ftnchek.h symtab.h block_match.h fortran.c

ftnchek.o:	config.h ftnchek.h intrins.h options.h utils.h

include.o:	config.h ftnchek.h symtab.h forlex.h advance.h

intrins.o:	config.h ftnchek.h intrins.h symtab.h

iokeywds.o:	config.h ftnchek.h symtab.h tokdefs.h iokeywds.h iokeywds_enum.h utils.h

keywords.o:	config.h ftnchek.h symtab.h tokdefs.h forlex.h

labels.o:	config.h ftnchek.h plsymtab.h symtab.h

loccheck.o:	config.h ftnchek.h loccheck.h plsymtab.h symtab.h

makedcls.o:	config.h ftnchek.h plsymtab.h symtab.h

makehtml.o:     config.h ftnchek.h plsymtab.h symtab.h

message.o:	config.h ftnchek.h

options.o:	config.h ftnchek.h options.h utils.h

pgsymtab.o:	config.h ftnchek.h pgsymtab.h symtab.h

plsymtab.o:	config.h ftnchek.h plsymtab.h symtab.h

prlists.o:	config.h ftnchek.h symtab.h symutils.h

prlocsym.o:	config.h ftnchek.h loccheck.h plsymtab.h symtab.h

project.o:	config.h ftnchek.h symtab.h

symspace.o:	config.h ftnchek.h symtab.h symspace.h symutils.h

symtab.o:	config.h ftnchek.h iokeywds.h intrins.h symtab.h \
		symspace.h symutils.h tokdefs.h

symutils.o:	config.h ftnchek.h symtab.h symutils.h

utils.o:	config.h utils.h

#=======================================================================
# The configure.generic script creates generic versions of CONFIGURED_FILES
# for users on deficient Unix systems or using GNU software on non-Unix
# platforms.  It tries to put reasonably generic values in for all the
# things that the configure script is supposed to handle.  Pathnames
# are removed since these may vary wildly.  The rule also checks for
# any unsubstituted items indicating configure.generic needs to be
# updated.  The config-generic.h produced here should need few if any
# changes to serve as config.h on most recent 32-bit systems.
# This target has no dependencies since its purpose is
# to replace the developer's configured versions with the generic ones,
# regardless of whether the .in files have changed.  Note that
# makefile.generic.in and config-generic.h.in are treated specially:
# they are copies of Makefile.in and config.h.in respectively, but the
# generic configured versions are not named Makefile and config.h, because
# these are deliberately left out of the distribution.
generic-config:
	$(CP) Makefile.in makefile.generic.in
	$(CP) config.h.in config-generic.h.in
	./configure.generic $(CONFIGURED_FILES)
	$(RM) makefile.generic.in
	$(RM) config-generic.h.in
	chmod +x dcl2inc test/Compare.sh
	@if $(GREP) \
	   '@[^*@]*@' $(CONFIGURED_FILES) ; \
	then \
	    echo '####################################################' ; \
	    echo '## FIX configure.generic TO SUBSTITUTE THE ABOVE. ##' ; \
	    echo '####################################################' ; \
	fi

# Here we provide rules for making the two CONFIGURED_FILES that are
# not generated by "configure".  This is mainly a convenience for the
# new developer since it is not so obvious where they come from.  They
# are not needed for development but only for the distribution.  They
# are normally made by generic-config.
makefile.generic: Makefile.in
	$(CP) Makefile.in makefile.generic.in
	./configure.generic makefile.generic
	$(RM) makefile.generic.in

config-generic.h: config.h.in
	$(CP) config.h.in config-generic.h.in
	./configure.generic config-generic.h
	$(RM) config-generic.h.in

#=======================================================================
#  This target warns if VMS build command files are out of date.  The sed
#  command concatenates continuation lines, marked by "-" as last character.
checkvmsbuild:
	@-for obj in $(OBJS:.o=) ; \
	do \
	  for com in build.com build-vax.com ; \
	  do \
	    if grep -q "^\$$ @CC.* $$obj[	 ]" $$com ; \
	    then true; else \
		echo "Source $$obj missing from CC in $$com" ; \
	    fi ; \
	    if sed -e ':CAT' -e '/-$$/N'  -e 's/-\n//' -e tCAT \
		   -e s/$$/,/ $$com | \
		grep -q "^\$$ @LINK.*[ ,]$$obj," ; \
	    then true; else \
		echo "Object $$obj missing from LINK in $$com" ; \
	    fi ; \
	  done; \
	done

#=======================================================================
# Making the distribution tar file.  Dirname is name of current
# directory.  Makefile is not distributed because it is supposed
# to be created by local configure.  Files to archive are explicitly
# named so that detritus from development will not be accidentally
# included.  However, test files are not listed separately so one
# should make clean in $(TESTDIR)/ first.
#
# This target assumes GNU tar for the --exclude option.  If you don't
# have it, remove test/Makefile manually and use ``make tar TAR_EXCLUDE=''

TAR_EXCLUDE	= --exclude $$dirname/$(TESTDIR)/Makefile

tar:
	dirname=`$(PWD_PROG)|$(SED) 's%.*/%%'` ; cd .. ; \
	  $(TAR) -cf $$dirname.tar $(TAR_EXCLUDE) \
	  `echo " "$(DISTFILES) | $(SED) "s% % $$dirname/%g"`

#  This target is for maintainer: generates the catman tar file for separate
#  distribution.
catman.tar: $(CATMAN)
	dirname=`$(PWD_PROG)|$(SED) 's%.*/%%'` ; \
	$(TAR) -cf ../$$dirname.catman.tar $(CATMAN)

.cat.catman:
	$(CP) $< $@

#  Make a distribution zipfile for MS-DOS/Windows/OS2.  The -l changes \n
#  to \r\n.  But configure_os2.cmd already has \r\n sequences in it,
#  and test/Checkbat.zip is binary.
#  Note that unlike tar, zip file unpacks into current directory.
#  Rule renames ftnchek-3.0.0 to ftnchek-3_0_0 for zip and lsm files.
zip:
	dirname=`$(PWD_PROG)|$(SED) 's%.*/%%'` ; \
	dosname=`echo $$dirname|$(SED) -e 's/\./_/g'` ; \
	$(CP) $$dirname.lsm $$dosname.lsm ; \
	dirname=$$dosname ; \
	$(RM) ../$$dirname.zip ; \
	$(ZIP) -lr ../$$dirname.zip $(ZIPFILES) ; \
	$(ZIP)     ../$$dirname.zip  configure_os2.cmd test/Checkbat.zip

# Targets that should be freshened prior to making tar (so I don't put
# stale ones into the distribution).
pretar:	docs generic-config clean testclean checkvmsbuild
