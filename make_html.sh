#!/bin/sh
#
#       $Id: make_html.sh,v 1.7 2001/05/06 14:16:23 moniot Rel $
#
# Script to break up ftnchek.html into separate sections in html directory.
#
# Author: Robert Moniot
# Date:   31 Jul 1998
# Originally written for use with PolyglotMan (rman)
# Modified for use with vh-man2html 22 Apr 2001
#

	    # This function puts out header needed by all but 1st page.
do_header(){
cat <<EOF
<HTML>
<HEAD>
<TITLE>$*</TITLE>
</HEAD>
<BODY bgcolor=white>
EOF
}

	    # This function puts out ref to table of contents that
	    # follows header for all but 1st and toc page.
do_href_toc(){
cat <<EOF
<A HREF="toc.html">Table of Contents</A><P>
EOF
}
	    # This function puts out ref to previous section
do_href_prev(){
  echo "<P>Previous: <A HREF=\"$1.html\">$2</A><HR><P>"
}


	    # This function puts out footer needed by all but last page.
do_footer(){
cat <<EOF
</BODY></HTML>
EOF
}

	    # This function changes the trailing NAME anchor
	    # of a page into an href to the next page.
	    # The first edit is for links to section pages.  The second edit is
	    # for the link to table of contents.  The third edit is
	    # for links to option pages.
href_next(){
 sed -e '$s,^.*<A NAME="\([^"]*\)">[^<]*</A> <H2>\([^<]*\)</H2>*$,<P><HR><P>Next: <A HREF="\1.html">\2</A>,' \
     -e '$s,^<A NAME="toc">.*$,<P><HR><P>Next: <A HREF="toc.html">Table of contents</A>,' \
     -e '$s,^.*<A NAME="\([^"]*\)".*$,<P><HR><P>Next: <A HREF="\1.html">-\1</A>,'
}

	    # This function adapts internal refs from one-big-file
	    # form into multi-file format.
	    # First edit changes "#lbAB" and "#lbAC" (refs to 1st two sections)
	    # into "index.html#lb[BC]" (where they are combined).
	    # Next changes all other "lbXX" hrefs to into "lbXX.html#lbXX".
	    # Third edit changes "#option" into "option.html"
change_hrefs(){
    sed -e 's,HREF="#\(lbA[BC]\)",HREF="index.html#\1",g' \
	-e 's,HREF="#\(lb[A-Z][A-Z]\)",HREF="\1.html#\1",g' \
        -e 's,HREF="#\([^"]*\)",HREF="\1.html",g'
}


#  Execution begins here.
	    # Filter the input (file arg or stdin) thru a sed script
	    # that concatenates section NAME tags with the H2 titles
	    # on following line, to associate section name with title.
	    # It also changes the name "index" to "toc" to avoid
	    # confusion with index.html, which is the first page, not
	    # the table of contents page.
	    # Put the input in a tmp file so it can be re-read.

filename="/tmp/html_split_$$.html"
trap "rm -f $filename" 0
sed -e '/^<A NAME="lb[A-Z][A-Z]"/N' \
    -e 's/\n/ /' \
    -e 's/HREF="#index">Index</HREF="#toc">Table of Contents</' \
    -e 's/<A NAME="index"/<A NAME="toc"/' \
    -e '/^<A HREF=[^>]*>Return to Main Contents/d' \
     $1 > $filename

		#  Get a list of all ftnchek main options, excluding the
		#  leading '-'.  Sort them for use on Options page.
options=`ftnchek -help | \
	  awk '/^ *-/ {split($0,command);
		 sub(/\[no\]/,"",command[1]);
		 sub(/=.*$/,"",command[1]);
		 sub(/^-/,"",command[1]);
		 print command[1];
		}' | sort`

		# Get a list of all the sections excluding special ones.
		# Sections are named "lbAB" "lbAC" ... by vh-man2html.
		# (As of v1.5.3 it doesn't use lbAA.)
sectlist=`sed -n -e 's,^.*<A NAME="\(lb[A-Z][A-Z]*\)".*$,\1,p' $filename |
	    grep -v 'lbA[BC]$'`

# This little bit, copied from GNU configure, sets ac_n and ac_c such
# that echo $ac_n "stuff $ac_c" yields "stuff " w/o following newline.
if (echo "testing\c"; echo 1,2,3) | grep c >/dev/null; then
  # Stardent Vistra SVR4 grep lacks -e, says ghazi@caip.rutgers.edu.
  if (echo -n testing; echo 1,2,3) | sed s/-n/xn/ | grep xn >/dev/null; then
    ac_n= ac_c='
' ac_t='	'
  else
    ac_n=-n ac_c= ac_t=
  fi
else
  ac_n= ac_c='\c' ac_t=
fi

		# Create html directory if it does not exist.
if [ -d html ]
then
  echo "Re-using directory html."
else
  echo "Creating directory html..."
  if mkdir html
  then
    true
  else
    echo "Failed!"
    exit 1
  fi
fi

echo "Creating section pages..."

		# Produce index.html page.  It is special, since it combines
		# NAME and SYNOPSIS and uses the man2html-generated header.
		# Also make sure the background color is same as the rest.
echo $ac_n "lbAB lbAC $ac_c"
(sed -n -e 's/<BODY>/<BODY bgcolor=white>/' \
        -e '1,/^<A NAME="lbAD"/p'  $filename | \
 href_next | \
 change_hrefs ;
 do_footer) > html/index.html

		# Produce pages for all other sections except last (toc).
		# Section for options gets special treatment since 
		# individual options will be split off into separate files,
		# and links to all options are placed at end of page.

prevsect='index'
prevtext=`sed -n 's,^<A NAME="lbAC">&nbsp;</A> <H2>\([^<]*\)</H2>$,\1,p' $filename`
for sect in $sectlist;
do
  echo $ac_n "$sect $ac_c"
  headtext=`sed -n 's,^<A NAME="'$sect'">&nbsp;</A> <H2>\([^<]*\)</H2>$,\1,p' $filename`

		# save section name of OPTIONS section for use by first option.
  if [ "$headtext" = "OPTIONS" ]
  then
    optsect="$sect"
  fi

  (do_header $headtext ; do_href_toc ;
   do_href_prev "$prevsect" "$prevtext" ;
  if [ "$headtext" = "OPTIONS" ]
  then
    (sed -n -e '/<A NAME="'$sect'"/,/<A NAME="lb/p' $filename | \
     href_next | \
     sed -n -e '1,/<A NAME=/p' -e '$p' | \
     sed -e '/<A NAME="a/d' ;
     echo "<P><HR><P>" ;
     echo $options | \
     sed -e 's@\([^ ][^ ]*\)@\
<B><A HREF="\1.html">-\1,</A></B>@g' \
	 -e 's@,</A></B> *$@.</A></B>@'
    )
  else
    sed -n -e '/<A NAME="'$sect'"/,/<A NAME=/p' $filename | \
    href_next
  fi | \
  change_hrefs ;
  do_footer) > html/$sect.html
  prevsect="$sect"
  prevtext="$headtext"
done


		# Produce table of contents
echo "toc"
(do_header Table of Contents ;
 sed -n -e '/<A NAME="toc">/,$p' $filename | \
 change_hrefs ;
) > html/toc.html

		# Now produce pages for all the options
echo "Creating option pages..."

prevsect="$optsect"
prevtext="OPTIONS"
for opt in $options
do
 echo $ac_n "$opt $ac_c"
(do_header "Option: $opt" ; do_href_toc ;
 do_href_prev "$prevsect" "$prevtext" ;
 echo "<H2>Option: <font color="#FF0080">$opt</font></H2><P>" ;
 sed -n -e '/<A NAME="'$opt'"/,/<A NAME=/p' $filename | \
 href_next | \
 change_hrefs ;
 do_footer) > html/$opt.html
 prevsect="$opt"
 prevtext="-$opt"
done
echo ""

