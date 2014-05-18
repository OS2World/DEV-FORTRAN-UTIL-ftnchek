#!/bin/sh
#
#     $Id: add_html_refs.sh,v 1.7 2002/08/18 18:43:21 moniot Rel $
#
# Script to insert links from all occurrences of each ftnchek option
# to the main description of the option in ftnchek.html.
#
# Author: Robert Moniot
# Date:   19 Jul 1998
# Originally written for use with PolyglotMan (rman)
# Modified for use with vh-man2html 22 Apr 2001
#
#  Get list of all ftnchek main options, excluding the leading '-'.
#  Turn it into a list of sed substitutions to change them into hrefs.
#  The href is "#option" (without the -).  The substitutions look for -opt
#  preceded by non-letter so that, e.g. f77's param-intrinsic won't match
#  -intrinsic.  This will fail if a real option comes at the start of a
#  line, but that shouldn't happen since man-to-html converter puts bolding
#  around them.

ftnchek -help | \
	  awk '/^ *-/ {split($0,opt);
		 sub(/\[no\]/,"",opt[1]);
		 sub(/=.*$/,"",opt[1]);
		 sub(/^-/,"",opt[1]);
		 printf("s,\\([^a-z]\\)-%s,\\1<A HREF=\"#%s\">-%s</A>,g\n",
		    opt[1],opt[1],opt[1]);
		}' \
	  > option_sub.sed

# Now create the cross-references.

# The tr command substitutes quote marks for the <BEL> characters that
# vh-man2html substutes for quote marks.
# The first sed command deletes any Content-type header intended for cgi use,
# and creates and attaches anchors to all the option descriptions in
# OPTIONS section.
# The second sed substitutes hot-links to these anchors at all the places
# where the options occur in the entire text.
# The third sed puts hot-links in for the -[no]options in SYNOPSIS section
# that are not recognized by the second sed.
tr '\007' '"' |
sed -e '/^Content-type:/d' \
    -e '/<H2>OPTIONS<\/H2>/,/<H2>.*<\/H2>/s,^<DT><B>-\([a-z][-a-z0-9]*\)[^<]*</B>,<A NAME="\1"></A>&,' | \
sed -f option_sub.sed  | \
sed -e '/<H2>SYNOPSIS<\/H2>/,/<H2>.*<\/H2>/s,<B>-</B>\[<B>no</B>\]<B>\([a-z][-a-z0-9]*\),<A HREF="#\1">&</A>,g'


