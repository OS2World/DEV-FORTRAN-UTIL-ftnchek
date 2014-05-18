#  $Id: Makefile.in,v 1.33 2002/11/18 00:35:18 moniot Rel $
#  Makefile for smoke-testing ftnchek.  Say ``make'' to make all checks.
#
#

EXE		= .exe
CMD		= .cmd

# gawk/nawk specially needed for dcl2inc.awk
NAWK		= gawk.exe
# ordinary awk used to print some messages
AWK		= gawk.exe
# This defn uses version of dcl2inc.awk in the parent directory. 
DCL2INC		= $(NAWK) -f ../dcl2inc.awk

# This defn uses version of ftnchek in the parent directory.
FTNCHEK		= ../ftnchek$(EXE)
# Since ftnpp is not included with ftnchek package, it is searched
# for by configure.  If it does not exist, checkftnpp will be skipped.
FTNPP		= @FTNPP@$(EXE)
FTNPPFLAGS	=

# OKAY subdirectory is where the correct output files are stored.
OKAY		= Okay

# VARY subdirectory is where the varying output files are stored.
VARY		= Vary


# COMPARE invokes the comparison script to check new vs archive.
COMPARE		= ./Compare.sh $(OKAY) $(VARY)

# RCFILE is the name of the startup file for checkrc
RCFILE= ftnchek.ini

# DEFAULT_FLAGS is the set of Ftnchek flags used by the default rule.
# Note: The default rule also sets -f77 via the environment mechanism
# in order to check that mechanism.
DEFAULT_FLAGS	= -list -symt -port

MAKE		= make.exe FTNCHEK=$(FTNCHEK) OKAY=$(OKAY) VARY=$(VARY)

CP              = cp.exe
GREP		= grep.exe
MV		= mv.exe
PRINTENV	= printenv.exe
RM		= rm.exe -f
SED		= sed.exe
SHELL		= sh.exe


.SUFFIXES: .chk .f .f90 .prj .fcl .fc1 .fc2 .fc3 .fc4 .fc5 .fc6 .fc7 .fc8 \
 .fc9 .fcA .fcB .fcC .fcD .fcE .fcF .fcx


#=======================================================================
#
#                   Definitions of File Groups
#
#
# LISTOUT programs are simple, and all use the default options or else
# the options given in their .opt file.  These produce listing files
# *.fcl (or *.fcx in -nonovice, i.e. expert mode).  Note: Those with
# .opt files may have associated routines in other files that do not
# appear here.

LISTOUT= average.f backslash.f chestnuts.f comclash.f comcmp.f\
 complex.f comtest.f comusage.f correct.f dims.f do_enddo.f\
 help.f include.f namelist.f noblanks.f strings.f unixincluded.f\
 dectab.f quad.f sequence.f substr.f autoarray.f cycle-exit.f\
 relops.f embedded-space.f arg-alias.f common-alias.f dec-parameter.f\
 iffy.f pure-function.f primes.f labeltest.f case.f allkeywords.f\
 blockerrors.f t208f.f lotsalabels.f initializers.f

FREEFORM=model.f90 blockcheck.f90

# set of files for testing f77/f90/f95 options
F90= iokeywords.f f95deleted.f 

# set of files for testing -arguments setting
ARGS= args01.f args02.f args03.f args04.f args05.f args06.f\
 args07.f args08.f args09.f args10.f args11.f args12.f\
 args13.f args14.f args15.f

# set of files for testing -array setting
ARRAY= arrayclash.f

# Set of files for testing -trun -port -f77 options and -word setting
EXPR= assign.f wordsize.f mixed-equiv.f

# set of files for testing -common setting
COMMON= comcmp.f comusage.f

# set of files for testing -intrinsic options
INTRINSIC= intrinstest.f

# set of files for testing dcl2inc processing
DCL2INCFILES= t208x.f

# set of files for testing -makedcls setting
MAKEDCLS= builtin.f dcltest.f dims.f model.f90 namelist.f params.f\
 t208a.f t208b.f t208c.f t208d.f t208e.f t208f.f t208g.f t208h.f \
 t208i.f t208j.f t208k.f t208l.f t208m.f t208n.f t208o.f t208p.f \
 t208q.f t208r.f t208s.f t208t.f t208u.f t208v.f t208w.f t208x.f

# set of files for testing -mkhtml option (also uses MAKEDCLS)
MKHTML= animated_pendulum_main.f animated_pendulum_cls.f \
 animated_pendulum_entergr.f animated_pendulum_plotball.f \
 animated_pendulum_exitgr.f

# set of files for testing -usage options
USAGE= usage.f

# set of files for testing -vcg switch (use target checkvcg)
VCGLIST= comcmp.f comusage.f

#=======================================================================
#
#                   Default Rules
#
# Rule for creating source code listing file, suffix .fcl.  Stdout and
# stderr are merged and environment mechanism for setting command
# options is tested.

.f.fcl:
	@if [ -f $*.opt ] ; then \
	  FTNCHEK_F77=1; export FTNCHEK_F77; \
	  $(FTNCHEK) `cat $*.opt` > $@ 2>&1 ; \
	else \
	  FTNCHEK_F77=1; export FTNCHEK_F77; \
	  $(FTNCHEK) $(DEFAULT_FLAGS) $< > $@ 2>&1 ; \
	fi

# Rule for expert-mode listings.  Same as above except for -nonovice flag.
.f.fcx:
	@if [ -f $*.opt ] ; then \
	  FTNCHEK_F77=1; export FTNCHEK_F77; \
	  $(FTNCHEK) -nonovice `cat $*.opt` > $@ 2>&1 ; \
	else \
	  FTNCHEK_F77=1; export FTNCHEK_F77; \
	  $(FTNCHEK) -nonovice $(DEFAULT_FLAGS) $< > $@ 2>&1 ; \
	fi

# Rule for creating free-form source code listing file, suffix .fcl.
# Same as for .f except F90 flag is set instead of F77.
.f90.fcl:
	@if [ -f $*.opt ] ; then \
	  FTNCHEK_F90=1; export FTNCHEK_F90; \
	  $(FTNCHEK) `cat $*.opt` > $@ 2>&1 ; \
	else \
	  FTNCHEK_F90=1; export FTNCHEK_F90; \
	  $(FTNCHEK) $(DEFAULT_FLAGS) $< > $@ 2>&1 ; \
	fi


# Rule for creating project file.  The .fcl file is made as a side-effect.
.f.prj:
	if [ -f $*.opt ] ; then \
	   $(FTNCHEK) -project `cat $*.opt` > $*.fcl 2>&1 ; \
	else \
	   $(FTNCHEK) -project $(DEFAULT_FLAGS) $< > $*.fcl 2>&1 ; \
	fi


# Rule for running checks on source code listing.  If OKAY directory does
# not exist, it is created.  If OKAY code listing does not exist, it
# is created and moved into OKAY.  Note: In this case, you must examine
# the results to see if it is really OK!
.f.chk:
	@-$(RM) $*.fcl
	@$(MAKE) FTNCHEK='$(FTNCHEK)' $*.fcl "DEFAULT_FLAGS=$(DEFAULT_FLAGS)"
	@$(COMPARE) $*.fcl

.f90.chk:
	@-$(RM) $*.fcl
	@$(MAKE) FTNCHEK='$(FTNCHEK)' $*.fcl "DEFAULT_FLAGS=$(DEFAULT_FLAGS)"
	@$(COMPARE) $*.fcl



#=======================================================================
#
#		Targets.
#
#

all:	check

check:	announce clearfail checkall checkfail

announce:
	@echo "============================================================"
	@echo "There should be no file differences reported by these tests."
	@echo "Output files matching master output files will be removed,"
	@echo "leaving any erroneous output files in directory $(VARY)"
	@echo "for manual examination.  If you built ftnchek with non-"
	@echo "standard compilation options, there may be some differences."
	@echo "============================================================"

# Remove signal file created by Compare.sh
clearfail:
	@-$(RM) CHECK_FAILED CHECK_FAILED.save

# Look for CHECK_FAILED file -- if found, then Compare.sh found diffs.
checkfail:
	@if [ -f CHECK_FAILED ] ; \
	then \
	  echo "===============================================" ; \
	  echo "===> There were differences.  Check failed. <==" ; \
	  echo "===============================================" ; \
	  $(RM) CHECK_FAILED CHECK_FAILED.save  ; \
	else \
	    echo "No differences found.  Check successful." ; \
	fi

checkall:	checksyntax checksettings checkother

checksyntax:	checklistings checkexpert checkf90 checkexpr
checksettings:	checkargs checkarray checkcommon checkintrinsic checkoutput checkusage
checkother:	checkproject checkrc checkwildcard checkmakedcls \
	checkmkhtml checkvcg checkdcl2inc checkftnpp

#  User should make precheck before running checks.  It looks out for
#  situations that will cause the check to fail: ftnchek not built,
#  user has an rc file in home directory, or FTNCHEK_* environment
#  variables are set.
precheck:
	@-$(RM) $(RCFILE)
	@errorcode=0 ; \
	if [ ! -f "$(FTNCHEK)" ] ; then \
	   echo "ERROR ===> Cannot run checks: no ftnchek found <==="; \
	   errorcode=1 ; \
	fi ; \
	if [ -f "$$HOME/.ftnchekrc" ] ; then \
	   echo "WARNING ===> $$HOME/.ftnchekrc exists <==="; \
	   echo "delete or rename it before running check" ; \
	   errorcode=1 ; \
	fi ; \
	if [ -f "$$HOME/ftnchek.ini" ] ; then \
	   echo "WARNING ===> $$HOME/ftnchek.ini exists <==="; \
	   echo "delete or rename it before running check" ; \
	   errorcode=1 ; \
	fi ; \
	if $(PRINTENV) | $(GREP) '^FTNCHEK_' >/dev/null 2>&1 ; then \
	   echo "WARNING ===> environment variables set:"; \
	   $(PRINTENV) | $(GREP) '^FTNCHEK_' ; \
	   echo "unset them before running check" ; \
	   errorcode=1 ; \
	fi ; \
	if [ "$$errorcode" = 0 ] ; then \
	   true ; \
	else \
	   false ; \
	fi

clean mostlyclean clobber distclean realclean spotless: clearfail
	-$(RM) *.dc[ln0-9a] *.fc[lx0-9A-F] *.htm[l0-0a]
	-$(RM) *.pcl *.prj *.inc *.vcg *.err *.mak
	-$(RM) $(RCFILE)
	-$(RM) *.o
	-$(RM) *~ \#*
	-$(RM) a.out core
	-$(RM) $(VARY)/*

# checklistings checks listing output on programs in LISTOUT and FREEFORM
checklistings: announcelistings $(LISTOUT:.f=.chk) $(FREEFORM:.f90=.chk)
	@-echo "done"

# Some systems do not have echo -n option, so to suppress newline
# in a portable way, we use this `awk'ward kluge.
announcelistings:
	@-echo|$(AWK) '{printf("%s","checking listing output:");}'

# checkexpert is like checklistings but uses -nonovice mode to get
# the expert style of warning messages.
checkexpert:
	@-echo|$(AWK) '{printf("%s","checking expert-mode output:");}'
	@for f in $(LISTOUT:.f=) ; \
	do \
	   $(RM) $$f.fcx ; \
	   $(MAKE) FTNCHEK='$(FTNCHEK)' $$f.fcx "DEFAULT_FLAGS=$(DEFAULT_FLAGS)" ; \
	   $(COMPARE) $$f.fcx ; \
	done
	@-echo "done"


# Next series of targets checks variations of output as options change.
# The output files are named *.fc[1-9A-F] for different cases.  Note that
# in some instances the same source file is also in a set that is used
# to check source listing as usual, with output file named *.fcl

# checkf90 checks the -f77, -f90, and -f95 flag behavior.
checkf90:
	@-echo |$(AWK) '{printf("%s","checking standard-compliance warnings:");}'
	@for f in $(F90:.f=) ; \
	do \
	 $(FTNCHEK) -nof77 -nof90 -nof95  $$f.f > $$f.fc0 ; \
	 $(COMPARE) $$f.fc0 ; \
	 $(FTNCHEK)   -f77 -nof90 -nof95  $$f.f > $$f.fc1 ; \
	 $(COMPARE) $$f.fc1 ; \
	 $(FTNCHEK) -nof77   -f90 -nof95  $$f.f > $$f.fc2 ; \
	 $(COMPARE) $$f.fc2 ; \
	 $(FTNCHEK) -nof77 -nof90   -f95  $$f.f > $$f.fc3 ; \
	 $(COMPARE) $$f.fc3 ; \
	done
	@-echo "done"

# checkexpr checks listing output for various size & type clashes in
# expressions and assignment stmts
checkexpr:
	@-echo |$(AWK) '{printf("%s","checking size and type mismatch:");}'
	@for f in $(EXPR:.f=) ; \
	do \
	 $(FTNCHEK) -notrun -noport -nof77 -lis    $$f.f > $$f.fc1 ; \
	 $(COMPARE) $$f.fc1 ; \
	 $(FTNCHEK) -notrun -noport   -f77         $$f.f > $$f.fc2 ; \
	 $(COMPARE) $$f.fc2 ; \
	 $(FTNCHEK) -notrun   -port -nof77         $$f.f > $$f.fc3 ; \
	 $(COMPARE) $$f.fc3 ; \
	 $(FTNCHEK) -notrun   -port   -f77         $$f.f > $$f.fc4 ; \
	 $(COMPARE) $$f.fc4 ; \
	 $(FTNCHEK)   -trun -noport -nof77         $$f.f > $$f.fc5 ; \
	 $(COMPARE) $$f.fc5 ; \
	 $(FTNCHEK)   -trun -noport   -f77         $$f.f > $$f.fc6 ; \
	 $(COMPARE) $$f.fc6 ; \
	 $(FTNCHEK)   -trun   -port -nof77         $$f.f > $$f.fc7 ; \
	 $(COMPARE) $$f.fc7 ; \
	 $(FTNCHEK)   -trun   -port   -f77         $$f.f > $$f.fc8 ; \
	 $(COMPARE) $$f.fc8 ; \
	 $(FTNCHEK)   -trun   -port -nof77 -word=2 $$f.f > $$f.fcA ; \
	 $(COMPARE) $$f.fcA ; \
	 $(FTNCHEK)   -trun   -port -nof77 -word=4 $$f.f > $$f.fcB ; \
	 $(COMPARE) $$f.fcB ; \
	 $(FTNCHEK)   -trun   -port -nof77 -word=8 $$f.f > $$f.fcC ; \
	 $(COMPARE) $$f.fcC ; \
	 $(FTNCHEK)   -trun   -noport -nof77 -word=2 $$f.f > $$f.fcD ; \
	 $(COMPARE) $$f.fcD ; \
	 $(FTNCHEK)   -trun   -noport -nof77 -word=4 $$f.f > $$f.fcE ; \
	 $(COMPARE) $$f.fcE ; \
	 $(FTNCHEK)   -trun   -noport -nof77 -word=8 $$f.f > $$f.fcF ; \
	 $(COMPARE) $$f.fcF ; \
	done
	@-echo "done"

# checkargs checks the -args=n setting
checkargs:
	@-echo |$(AWK) '{printf("%s","checking -args setting:");}'
	@$(MAKE) FTNCHEK='$(FTNCHEK)' \
		CheckSetting "SETTING=-args" "RANGE=0 1 2 3" \
		"FLIST=$(ARGS)" "FLAGS="
	@-echo "done"

# checkarray checks the -array=n setting
checkarray:
	@-echo |$(AWK) '{printf("%s","checking -array setting:");}'
	@$(MAKE) FTNCHEK='$(FTNCHEK)' \
		CheckSetting "SETTING=-array" "RANGE=0 1 2 3" \
		"FLIST=$(ARRAY)" "FLAGS=-port"
	@-echo "done"

#checkcommon checks the -common=n setting
checkcommon:
	@-echo |$(AWK) '{printf("%s","checking -common setting:");}'
	@$(MAKE) FTNCHEK='$(FTNCHEK)' \
		CheckSetting "SETTING=-common" "RANGE=0 1 2 3" \
		"FLIST=$(COMMON)" "FLAGS=-nof77"
	@-echo "done"

checkdcl2inc:
	@-echo |$(AWK) '{printf("%s","checking dcl2inc processing:");}'
	@for f in $(DCL2INCFILES) ; \
	do \
		b=`basename $$f .f` ; \
		$(RM) $$b.dcl $$b.dcn $$b.err $$b.mak ; \
		$(RM) *.inc ; \
		$(FTNCHEK) -nocheck -makedcls=1 $$f 1>/dev/null 2>/dev/null ; \
		$(DCL2INC) $$b.dcl >$$b.mak 2>$$b.err ; \
		for g in $$b.dcl $$b.dcn $$b.mak $$b.err *.inc ; \
		do \
			$(COMPARE) $$g ; \
		done ; \
	done
	@-echo "done"

# checkftnpp tests the ftnpp filter program if it was built.  The fiddling
# with CHECK_FAILED is to avoid giving the special message if the test
# failure occured elsewhere.
checkftnpp:
	@if [ -x "$(FTNPP)" ] ; \
	then \
	  if [ -f CHECK_FAILED ] ; \
	  then \
	     mv CHECK_FAILED CHECK_FAILED.save ; \
	  fi ; \
	  echo |$(AWK) '{printf("%s","checking ftnpp:");}' ; \
	  $(FTNPP) $(FTNPPFLAGS) -DNEC=IBM -IInclude include.f | $(FTNCHEK) > filter.fcl ; \
	  $(COMPARE) filter.fcl ; \
	  echo "done" ; \
	  if [ -f CHECK_FAILED ] ; \
	  then \
	    echo "" ; \
	    echo "--> Some systems are known to fail the ftnpp test.  Check the ftnchek web" ; \
	    echo "--> site, Latest Information (http://www.dsm.fordham.edu/~ftnchek/Plan)," ; \
	    echo "--> to see if your system is listed, before reporting such error." ; \
	  fi ; \
	  if [ -f CHECK_FAILED.save ] ; \
	  then \
	     mv CHECK_FAILED.save CHECK_FAILED ; \
	  fi ; \
	fi

# checkintrinsic checks the -intrinsic=optionlist setting
checkintrinsic:
	@-echo |$(AWK) '{printf("%s","checking -intrinsic setting:");}'
	@for f in $(INTRINSIC:.f=) ; \
	do \
	   $(FTNCHEK) -intrinsic=none $$f.f > $$f.fc0 ; \
	   $(COMPARE) $$f.fc0 ; \
	   $(FTNCHEK) -intrinsic=none,extra $$f.f > $$f.fc1 ; \
	   $(COMPARE) $$f.fc1 ; \
	   $(FTNCHEK) -intrinsic=none,unix $$f.f > $$f.fc2 ; \
	   $(COMPARE) $$f.fc2 ; \
	   $(FTNCHEK) -intrinsic=none,vms $$f.f > $$f.fc3 ; \
	   $(COMPARE) $$f.fc3 ; \
	   $(FTNCHEK) -intrinsic=extra,unix,no-vms,no-rand-no-arg $$f.f > $$f.fc4 ; \
	   $(COMPARE) $$f.fc4 ; \
	   $(FTNCHEK) -intrinsic=extra,unix,no-vms,no-rand-one-arg $$f.f > $$f.fc5 ; \
	   $(COMPARE) $$f.fc5 ; \
	   $(FTNCHEK) -intrinsic=extra,unix,no-vms,no-iargc-no-arg $$f.f > $$f.fc6 ; \
	   $(COMPARE) $$f.fc6 ; \
	   $(FTNCHEK) -intrinsic=extra,unix,no-vms,no-iargc-one-arg $$f.f > $$f.fc7 ; \
	   $(COMPARE) $$f.fc7 ; \
	   $(FTNCHEK) -intrinsic=all $$f.f > $$f.fc8 ; \
	   $(COMPARE) $$f.fc8 ; \
	done
	@-echo "done"

# checkoutput checks the -output=file string setting
checkoutput:
	@-echo |$(AWK) '{printf("%s","checking -output setting:");}'
	@$(FTNCHEK) -novice -symt -list -port -out=average.out average.f 2> average.fc2
	@-$(COMPARE) average.out
	@-$(COMPARE) average.fc2
	@-echo "done"

# checkusage checks the -usage=options setting
USAGE_UNUSED=com-block-unused,com-var-set-unused,com-var-unused,ext-unused,var-set-unused,var-unused
USAGE_UNINIT=com-var-uninitialized,var-uninitialized
checkusage:
	@-echo |$(AWK) '{printf("%s","checking -usage setting:");}'
	@for f in $(USAGE:.f=) ; \
	do \
	   $(FTNCHEK) -usage=none $$f.f > $$f.fc0 ; \
	   $(COMPARE) $$f.fc0 ; \
	   $(FTNCHEK) -usage=none,$(USAGE_UNINIT) $$f.f > $$f.fc1 ; \
	   $(COMPARE) $$f.fc1 ; \
	   $(FTNCHEK) -usage=none,$(USAGE_UNUSED) $$f.f > $$f.fc2 ; \
	   $(COMPARE) $$f.fc2 ; \
	   $(FTNCHEK) -usage=all $$f.f > $$f.fc3 ; \
	   $(COMPARE) $$f.fc3 ; \
	done
	@-echo "done"

# checkproject checks creation and use of project files
# Note that correct.f is used in LISTOUT also, but with default
# compilation options.  So we make listing file suffix .fc1 here

checkproject:
	@-echo |$(AWK) '{printf("%s","checking project files:");}'
	@$(RM) correct.prj correct.fc1 correct.pcl
	@$(MAKE) FTNCHEK='$(FTNCHEK)' correct.prj
	@$(MAKE) FTNCHEK='$(FTNCHEK)' correct.pcl
	@-for f in correct.fc1 correct.prj correct.pcl ; \
	do \
	    $(COMPARE) $$f ; \
	done
	@-echo "done"


# Need explicit rule since the default rule is used by listing-file check
correct.prj: correct.f
	@$(FTNCHEK) -project correct.f > correct.fc1 2>&1

#  This target creates output listing file from project file input
correct.pcl: correct.f
	@$(FTNCHEK) correct.prj > correct.pcl 2>&1

# Check the reading of startup file
checkrc:
	@-echo |$(AWK) '{printf("%s","checking reading rc file:");}'
	@-echo "f77=all" > $(RCFILE)
	@-echo "symtab" >> $(RCFILE)
	@$(FTNCHEK) -f77=no-long-name,no-name-underscore do_enddo.f > rc.fcl 2>&1
	@$(COMPARE) rc.fcl
	@$(RM) $(RCFILE)
	@-echo "done"

# Check the interpretation of wildcards in warn-option lists
checkwildcard:
	@-echo |$(AWK) '{printf("%s","checking interpreting wildcards:");}'
	@$(FTNCHEK) '-port=mixed*' -port=help > wildcard.fc1 2>&1
	@$(COMPARE) wildcard.fc1
	@$(RM) wildcard.fc1
	@$(FTNCHEK) '-usage=no-*var*' -usage=help > wildcard.fc2 2>&1
	@$(COMPARE) wildcard.fc2
	@$(RM) wildcard.fc2
	@$(FTNCHEK) '-usage=no-var*' -usage=help > wildcard.fc3 2>&1
	@$(COMPARE) wildcard.fc3
	@$(RM) wildcard.fc3
	@$(FTNCHEK) '-f77=*array*,format*' -f77=help > wildcard.fc4 2>&1
	@$(COMPARE) wildcard.fc4
	@$(RM) wildcard.fc4
	@$(FTNCHEK) '-pretty=*ugly*' -pretty=help > wildcard.fc5 2>&1
	@$(COMPARE) wildcard.fc5
	@$(RM) wildcard.fc5
	@-echo "done"


# Check the generation of declarations files.  Listing goes to /dev/null.
# The values in MAKEDCLS_VALS are used as 3rd char in .fc? suffix.  They
# are converted to -makedcls=num values by raising 2 to the power.  Thus
# file.dc0 contains -makedcls=1, ... file.dca contains -makedcls=1024.
# There is also a test to be sure that empty .dcl files are removed
# automatically as they should be.
MAKEDCLS_VALS	= 0 1 2 3 4 5 6 7 8 9 a
checkmakedcls:
	@-echo |$(AWK) '{printf("%s","checking -makedcls setting:");}'
	@-for n in $(MAKEDCLS_VALS) ; \
	  do \
	    dcl=dc$$n ;\
	    val=`echo $$n|$(SED) -e 's/a/10/'|\
		$(AWK) '{x=$$1;p=1;while(x>0){x--;p*=2;}print p;}'` ;\
	    for f in $(MAKEDCLS) ;\
	    do \
		stem=`echo $$f|$(SED) 's/[.][^.]*//'` ;\
		$(FTNCHEK) -nocheck -makedcls=$$val $$f 1>/dev/null 2>/dev/null ; \
		if [ -f $$stem.dcl ] ; \
		then \
		  if [ `wc -c $$stem.dcl | $(AWK) '{printf("%s",$$1);}'` -eq 0 ] ; \
		  then \
		    $(CP) $$stem.dcl $(VARY)/$$stem.$$dcl ; \
		    echo ; echo "Empty declarations file" \
			 $(VARY)/$$stem.$$dcl "should have been deleted" ; \
		  fi ; \
		else \
		   touch $$stem.dcl ; \
		fi ; \
		$(MV) $$stem.dcl $$stem.$$dcl ; \
		$(COMPARE) $$stem.$$dcl ; \
	    done ; \
	  done
	@echo "done"

# The first part of this target tests -mkhtml on a multi-file program,
# generating the CallTree.html file as well as a separate .html file
# for each component of the program.
# The next part of the target tests the various sub-options, and treats
# each input file separately, generating only its .html file.  It is
# similar to checkmakedcls, using the same set of source files, but
# not all powers of two are meaningful for -mkhtml.
MKHTML_VALS	= 0 2 3 4 5 6 9 a
checkmkhtml:
	@-echo |$(AWK) '{printf("%s","checking -mkhtml setting:");}'
	@$(FTNCHEK) -mkhtml -call=tree $(MKHTML) > mkhtml.fcl 2>&1
	@$(COMPARE) mkhtml.fcl
	@$(COMPARE) CallTree.html
	@-for f in $(MKHTML:.f=.html) ; \
	  do \
	    $(COMPARE) $$f ; \
	  done
	@-for n in $(MKHTML_VALS) ; \
	  do \
	    html=htm$$n ;\
	    val=`echo $$n|$(SED) -e 's/a/10/'|\
		$(AWK) '{x=$$1;p=1;while(x>0){x--;p*=2;}print p;}'` ;\
	    for f in $(MAKEDCLS:.f=) ;\
	    do \
		stem=`echo $$f|$(SED) 's/[.][^.]*//'` ;\
		$(FTNCHEK) -nocheck -mkhtml=$$val $$f 1>/dev/null 2>/dev/null ; \
		$(MV) $$stem.html $$stem.$$html ; \
		$(COMPARE) $$stem.$$html ; \
	    done ; \
	  done
	@echo "done"

checkvcg:
	@-echo |$(AWK) '{printf("%s","checking -vcg setting:");}'
	@-for f in $(VCGLIST:.f=) ; \
	do \
	  $(FTNCHEK) -vcg $$f.f 1>/dev/null 2>/dev/null ; \
	  $(COMPARE) $$f.vcg ; \
	done
	@echo "done"

# CheckSetting is a "subroutine" with parameters SETTING=name of setting,
# RANGE=list of setting values, FLIST=list of files to check, and
# FLAGS=set of ftnchek flags to use for each case.  "Call" this routine
# by invoking make with appropriate macro definitions on command line.

CheckSetting:
	@for val in $(RANGE) ; \
	do \
	  for f in $(FLIST:.f=) ; \
	  do \
	    $(FTNCHEK) $(SETTING)=$$val $(FLAGS) $$f.f > $$f.fc$$val ; \
	    $(COMPARE) $$f.fc$$val ; \
	  done ; \
	done
