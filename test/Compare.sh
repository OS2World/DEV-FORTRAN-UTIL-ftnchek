#!D:/bin/os2/bin/sh.exe
#
# $Id: Compare.sh.in,v 1.2 2000/07/22 19:53:36 moniot Rel $
#
# Bourne shell script to compare File in directory ./Vary with
# original in directory ./Okay and print a message if they differ.
# If no differences it echoes a dot to show progress.
# If differences, touches file CHECK_FAILED
#
# Usage: Compare.sh Okay Vary File

OKAY=$1
VARY=$2
FILE=$3

	if [ ! -d ${OKAY} ]
	then
	    echo "Creating directory ${OKAY}"
	    mkdir.exe ${OKAY};
	fi
	if [ ! -f ${OKAY}/${FILE} ]
	then
	    echo "${FILE} is new"
	    cp.exe ${FILE} ${OKAY}/${FILE}
	fi
	if diff.exe ${OKAY}/${FILE} ${FILE} > /dev/null
	then
	    rm.exe -f -f ${FILE}
	    echo '.' | gawk.exe '{printf("%s",$1);}'
	else
	    touch CHECK_FAILED
	    echo ""
	    echo "--------------------------------------------------"
	    echo "====> Differences found in ${FILE} test <===="
	    echo "Master: ${OKAY}/${FILE}"
	    echo "Test:   ${VARY}/${FILE}"
	    if [ ! -d ${VARY} ]
	    then
		echo "Creating directory ${VARY}"
		mkdir.exe ${VARY}
	    fi
	    mv.exe ${FILE} ${VARY}/${FILE}
#  To show diffs uncomment next line
#	    diff.exe ${OKAY}/${FILE} ${VARY}/${FILE}
	fi
	exit 0
