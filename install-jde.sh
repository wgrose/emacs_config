#!/bin/bash

NOTICE="
\n
Warning!
Please note, this script now uses CEDET - all in one packages\n
instead of separate packages for additional tools like:\n
EIEIO, SEMANTIC and SPEEDBAR.\n
Don't forget to change load-path settings to include\n
'cedet/package' instead of separate 'package'.\n
You can do it just to replace existings setting with one simple:\n
(load-file \"~/.emacs.d/site-lisp/cedet/common/cedet.el\")
\n
"
# Author: Artur Hefczyc <kobit@users.sourceforge.net>
# http://wttools.sf.net/
# Version: $Revision: 1.30 $ tested for JDEE version 2.3.5.1
VERSION="\n
Author:	Artur Hefczyc <kobit@users.sourceforge.net>\n
Script version: $Revision: 1.30 $Foo for JDEE 2.3.5.1\n
----\n
Contributors:\n
1. Not GNU tar support (MKS Toolkit, Solaris):\n
\t - Joe Berry <joe@topshot.com>\n
\t - Artur Agaronyan <artura7@comcast.net>\n
2. Corrected all 'path' calls which can now contain spaces.\n
\t - Chadwick McHenry <red@mitre.org>\n
3.1. Fixed very annoying CEDET problem preventing it to run on all non-cvs emacs versions\n
3.2. Corrected checking of command execution result code, now it should work correctly for all common bash versions. \n
\t - Tavis Elliott <tavise@nwlink.com>\n
"

#    Copyright (C) 2001 "Artur Hefczyc" <kobit@users.sourceforge.net>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License as published
#    by the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#    GNU Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public License
#    along with this program; if not, write to the Free Software Foundation,
#    Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# What it does:
# Purpose of this script is to allow installation of JDEE
# and all required libraries for local emacs environment.
# All libraries are installed from last available versions.
# It should work well for both: new installation and updating
# previous JDEE package.
# The goal is to provide utility for the most simple use.
# Just set all variables (1 or 2 indeed) one time according to
# your emacs configuration and run it multiple times at each
# JDEE upgrade.
#
# What it requires:
# This script is created to run it in Unix like environment.
# It means it requires some tools to perform all actions:
# - 'bash'  command line shell
# - 'sed'   command line steam editor
# - 'wget'  non-interactive download tool (it is possible also
#           to use different download through http tool, but some
#           modifications might be necessary)
# - 'tar'   and 'gzip' unpacking utility
# - 'emacs' for precompilation sources
# - 'cp', 'rm', 'mkdir', 'pwd', tail - standard unix set
# - 'getopt' command line arguments parser
# In real use it should work well on all unixes and MS Win+CygWin
# system. I tested it on some last RedHat distributions,
# MS Win-XP+CygWin and MS Win-2k+CygWin
#
# How to run it:
# Simply run it from command line shell. It does not require any
# additional command line parameters. However some variables
# might be necessary to change before. Look in section:
# 'Basic system constants'. There is at least 1 main variable which
# are often different for many emacs configurations.
# So set 'SITE_LISP_DIR' to emacs site-lisp location on your system.
# It can be often useful to set different 'DIRTMP' location according
# to your preferences and at last 'EMACS' command for precompilation
# downloaded libraries.
#
# You may also want to update this script with info about new releases.
# Although I try to keep it up to date, sometimes I can be busy and
# may provide new version late. So look in section: 'Location of last
# version all necessary libraries' and change locations to last available
# packages.
#
# If you have any troubles with installing JDEE using this script
# try to run it with "-v" option. It will give you more verbose output
# so it will be easier to help you for me.

# Currently it is well tested for last cygwin+MS WindowsXP,
# Linux RedHat-8.0, Gentoo

VALID_PARAMETERS="You can run script with following parameters:\n
  -v \t\t verbose operations\n
  -n \t\t do not delete temp files after installation\n
  -s \t\t install last stable version instead of last available version\n
  -d dir \t site-lisp directory where packages have to be installed\n
  -t dir \t temporary directory for keeping processing files\n
  -e path \t emacs command, xemacs or full path to emacs program\n
  -m host.name \t set your prefered SF mirror for downloading files\n
  -h \t\t print this help info\n
  -i \t\t print version info\n
"

# Basic system constants

# For CygWin systems use CygWin like paths, for example if
# your site-lisp is in directory: "c:\ntemacs\site-lisp" use this format:
# "/cygdrive/c/ntemacs/site-lisp"
SITE_LISP_DIR="$HOME/.emacs.d/site-lisp"
DIRTMP="jde-tmp"
EMACS="emacs"
MIRROR="heanet.dl.sourceforge.net"
VERBOSE=""
EMACS_OUT="null"
DELETE_TMP=true
JDE_VERSION="beta"
CEDET_FILE="cedet-1.0pre3.tar.gz"

while getopts sihvnd:t:e: opt
  do
  case "$opt" in
      s)
          JDE_VERSION="latest" ;;
      v)
          set -x
          VERBOSE="-v"
          EMACS_OUT=""
          echo -e ${NOTICE}
          ;;
      n)
          DELETE_TMP=false ;;
      d)
          SITE_LISP_DIR="$OPTARG" ;;
      t)
          DIRTMP="$OPTARG" ;;
      e)
          EMACS="$OPTARG" ;;
      m)
          MIRROR="$OPTARG" ;;
      i)
          echo -e $VERSION
          exit 0 ;;
      h|*)
          echo -e $VERSION
          echo -e $VALID_PARAMETERS
	  exit 1 ;;
    esac
done

TEST_EMACS=`"$EMACS" --version 2>&1`
if [ $? -ne 0 ] ; then
    echo "Your path to emacs command: $EMACS is wrong."
    echo "Please set correct path to emacs and run egain."
    echo "To set your emacs command run script with:"
    echo "'-e /path/to/emacs' parameter."
    echo "----"
    echo -e $VALID_PARAMETERS
    exit 127;
fi

# Location of last version all necessary libraries

JDE_LOC="http://downloads.sourceforge.net/project/jdee/jdee/2.4.0.1/jdee-bin-2.4.0.1.zip"
ELIB_LOC="http://downloads.sourceforge.net/project/jdee/jdee/Dependencies/elib-1.0.zip"
## Now CEDET package contains tools EIEIO, SEMANTIC and SPEEDBAR.
CEDET_LOC="http://downloads.sourceforge.net/project/cedet/cedet/cedet-1.0.tar.gz"
BSH_LOC="http://www.beanshell.org/bsh-2.0b4.jar"

# Set to proper values for your system

# Do you use 'wget' or another 'ncftpget' for example tool
# to download files.
if [ "$VERBOSE" = "-v" ] ; then
    DOWNLOAD_CMD="wget -c -t0 -T 15 -v"
else
    DOWNLOAD_CMD="wget -c -t0 -T 15 -nv"
fi

# If you prefer 'zip' format change variables below to proper values
FILE_SUFFIX="tar.gz"
UNPACK_CMD="tar -xzvf"
UNPACK_ZIP_CMD="unzip -o"

# Local variables.

JDE_FILE=jde
ELIB_FILE=elib
CEDET_FILE=cedet
BSH_FILE=bsh.jar

# Remember current directory
CURRDIR=`pwd`

mkdir -p $VERBOSE "$DIRTMP"

# Download last JDE
rm -f "$DIRTMP/$JDE_FILE.$FILE_SUFFIX"
$DOWNLOAD_CMD -O "$DIRTMP/$JDE_FILE.$FILE_SUFFIX" $JDE_LOC
if [ $? -ne 0 ] ; then
    if [ "$JDE_VERSION" = "beta" ] ; then
        # Maybe beta does not exists, so try to get latest
        #JDE_LOC="http://jdee.sunsite.dk/jde-latest.tar.gz"
        $DOWNLOAD_CMD -O "$DIRTMP/$JDE_FILE.$FILE_SUFFIX" $JDE_LOC
        if [ $? -ne 0 ] ; then
            echo "Couldn't download $JDE_FILE file, installation aborted"
            exit 1
        fi
    else
        echo "Couldn't download $JDE_FILE file, installation aborted"
        exit 1
    fi
fi
# Download last ELIB
rm -f "$DIRTMP/$ELIB_FILE.$FILE_SUFFIX"
$DOWNLOAD_CMD -O "$DIRTMP/$ELIB_FILE.$FILE_SUFFIX" $ELIB_LOC
if [ $? -ne 0 ] ; then
    echo "Couldn't download $ELIB_FILE file, installation aborted"
    exit 1
fi
# Download last CEDET
rm -f "$DIRTMP/$CEDET_FILE.$FILE_SUFFIX"
$DOWNLOAD_CMD -O "$DIRTMP/$CEDET_FILE.$FILE_SUFFIX" $CEDET_LOC
if [ $? -ne 0 ] ; then
    echo "Couldn't download $CEDET_FILE file, installation aborted"
    exit 1
fi
# Download last BSH
rm -f "$DIRTMP/$BSH_FILE"
$DOWNLOAD_CMD -O "$DIRTMP/$BSH_FILE" $BSH_LOC
if [ $? -ne 0 ] ; then
    echo "Couldn't download $BSH_FILE file, installation aborted"
    exit 1
fi

cd "$DIRTMP"

# Unpack all files and remember target directories
GNU_TAR=true
TAR_OUTPUT=`tar --version 2>&1`
if [ $? -eq 2 ] ; then
    GNU_TAR=false
else
    echo $TAR_OUTPUT | head -n 1 | grep "GNU tar"
    if [ $? -ne 0 ] ; then
        GNU_TAR=false
    fi
fi
if $GNU_TAR ; then
    JDE_DIR_TMP=`$UNPACK_ZIP_CMD $JDE_FILE.$FILE_SUFFIX | tail -n 1`
    ELIB_DIR_TMP=`$UNPACK_ZIP_CMD $ELIB_FILE.$FILE_SUFFIX | tail -n 1`
    CEDET_DIR_TMP=`$UNPACK_CMD $CEDET_FILE.$FILE_SUFFIX | tail -n 1`

    JDE_DIR=`echo $JDE_DIR_TMP | sed -e "s/.*: //g" -e "s:/.*::g"`
    ELIB_DIR=`echo $ELIB_DIR_TMP | sed -e "s/.*: //g" -e "s:/.*::g"`
    CEDET_DIR=`echo $CEDET_DIR_TMP | sed -e "s%^\([^/]*\)/.*%\1%"`
else
    JDE_DIR_TMP=`$UNPACK_ZIP_CMD $JDE_FILE.$FILE_SUFFIX 2>&1 | tail -n 1`
    ELIB_DIR_TMP=`$UNPACK_ZIP_CMD $ELIB_FILE.$FILE_SUFFIX 2>&1 | tail -n 1`
    CEDET_DIR_TMP=`$UNPACK_CMD $CEDET_FILE.$FILE_SUFFIX 2>&1 | tail -n 1`

    JDE_DIR=`echo $JDE_DIR_TMP | sed -e "s/.*: //g" -e "s:/.*::g"`
    ELIB_DIR=`echo $ELIB_DIR_TMP | sed -e "s/.*: //g" -e "s:/.*::g"`
    CEDET_DIR=`echo $CEDET_DIR_TMP | sed -e "s%^\([^/]*\)/.*%\1%" -e "s/^x *//"`
fi

if [ -z "$ELIB_DIR" ] || [ -z "$JDE_DIR" ] || [ -z "$CEDET_DIR" ] ; then
    echo "Couldn't detect path to unpacked packages,"
    echo "can't continue than, sorry."
    echo "Please contact with script author and"
    echo "attach your system details, at least:"
    echo "operating system, tar version, sed version."
    exit 99
fi

# OS specific support.
cygwin=false
case "`uname`" in
  CYGWIN*) cygwin=true ;;
esac

if $cygwin ; then
    SITE_LISP_DIR=`cygpath -m "$SITE_LISP_DIR"`
fi
if [ ! -d "$SITE_LISP_DIR" ] ; then mkdir $VERBOSE "$SITE_LISP_DIR" ; fi

rm -rf $VERBOSE "$SITE_LISP_DIR/$JDE_FILE/"*
rm -rf $VERBOSE "$SITE_LISP_DIR/$ELIB_FILE/"*
rm -rf $VERBOSE "$SITE_LISP_DIR/$CEDET_FILE/"*

if [ -d "$ELIB_DIR" ] ; then
    mkdir -p $VERBOSE "$SITE_LISP_DIR/$ELIB_FILE"
    cp -rf $VERBOSE "$ELIB_DIR/"* "$SITE_LISP_DIR/$ELIB_FILE"
else
    echo "Can't determine $ELIB_FILE source dir, instalation aborted."
    exit 1
fi
if [ -d "$CEDET_DIR" ] ; then
    mkdir -p $VERBOSE "$SITE_LISP_DIR/$CEDET_FILE"
    cp -rf $VERBOSE "$CEDET_DIR/"* "$SITE_LISP_DIR/$CEDET_FILE"
else
    echo "Can't determine $CEDET_FILE source dir, instalation aborted."
    exit 1
fi
if [ -d $JDE_DIR ] ; then
    mkdir -p $VERBOSE "$SITE_LISP_DIR/$JDE_FILE"
    cp -rf $VERBOSE "$JDE_DIR/"* "$SITE_LISP_DIR/$JDE_FILE"
else
    echo "Can't determine $JDE_FILE source dir, instalation aborted."
    exit 1
fi

# Create compile script, I am skipping provided make files due to
# to problems with using them on different platforms
cd "$SITE_LISP_DIR"
echo "(add-to-list 'load-path \"$SITE_LISP_DIR/$ELIB_FILE/\")" > compile-script
# Load CEDET packages
echo "(load-file \"$SITE_LISP_DIR/$CEDET_FILE/common/cedet.el\")" >> compile-script
echo "(add-to-list 'load-path \"$SITE_LISP_DIR/$JDE_FILE/lisp/\")" >> compile-script

# Compiling - not absolutly necessary, but improves using JDEE
cd "$ELIB_FILE"
"$EMACS" --no-site-file -batch -l elib-compile-all.el -f compile-elib &>emacs_c.log
if [ "$EMACS_OUT" != "null" ] ; then
    cat emacs_c.log
fi
cd "../$CEDET_FILE"
# Compile CEDET package:
# Make sure the Makefile timestamps are new (odd)
find . -name "Makefile" | xargs touch
# Make SURE the *-loaddef.el files are cleaned up.
make clean-autoloads
# Compile CEDET
make SHELL=bash &> emacs_c.log
if [ "$EMACS_OUT" != "null" ] ; then
    cat emacs_c.log
fi
cd "../$JDE_FILE/lisp"
"$EMACS" --no-site-file -batch -l ../../compile-script -f \
    batch-byte-compile *.el &>emacs_c.log
if [ "$EMACS_OUT" != "null" ] ; then
    cat emacs_c.log
fi
cd ../..

# Cleanup
if $DELETE_TMP ; then
    rm -f $VERBOSE compile-script
# Necessary for a case when $DIRTMP is relative to starting current directory
    cd "$CURRDIR"
    cd "$DIRTMP"
# Clearing, but only what I just created
    rm -rf $VERBOSE \
        "$JDE_DIR"      "$JDE_FILE.$FILE_SUFFIX" \
        "$ELIB_DIR"     "$ELIB_FILE.$FILE_SUFFIX" \
        "$CEDET_DIR"    "$CEDET_FILE.$FILE_SUFFIX" \
        "$BSH_FILE"
fi

echo -e ${NOTICE}
