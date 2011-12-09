#!/bin/bash

# guile-gnome
# Copyright (C) 2002 Thomas Vander Stichele
#               2003,2004 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or    
# modify it under the terms of the GNU General Public License as   
# published by the Free Software Foundation; either version 2 of   
# the License, or (at your option) any later version.              
                                                                 
# This program is distributed in the hope that it will be useful,  
# but WITHOUT ANY WARRANTY; without even the implied warranty of   
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
# GNU General Public License for more details.                     
                                                                 
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:

# Free Software Foundation           Voice:  +1-617-542-5942
# 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
# Boston, MA  02111-1307,  USA       gnu@gnu.org


# Run this to generate all the initial makefiles, etc.

DIE=0
package=guile-gobject
srcfile=autogen-pkg.sh

. ./autogen-support.sh
. ./autogen-pkg.sh

CONFIGURE_DEF_OPT='--enable-maintainer-mode'

autogen_options $@

# OK now this is seriously broken. Autoconf gives two different
# versions, depending on whether there's a configure.ac file in the
# current directory. Although we're generating ours, touch it first so
# we get the right version.
touch configure.ac

echo -n "+ check for build tools"
if test ! -z "$NOCHECK"; then echo ": skipped version checks"; else  echo; fi
version_check "autoconf" "$AUTOCONF autoconf autoconf-2.54 autoconf-2.53 autoconf-2.52 autoconf2.50" \
              "ftp://ftp.gnu.org/pub/gnu/autoconf/" 2 52 || DIE=1
version_check "automake" "$AUTOMAKE automake automake-1.8 automake-1.7 automake17" \
              "ftp://ftp.gnu.org/pub/gnu/automake/" 1 7 || DIE=1
version_check "libtoolize" "libtoolize libtoolize14" \
              "ftp://ftp.gnu.org/pub/gnu/libtool/" 1 4 0 || DIE=1
version_check "pkg-config" "" \
              "http://www.freedesktop.org/software/pkgconfig" 0 8 0 || DIE=1

die_check $DIE

if test -z "$ONLY_GENCONFIGURE"; then
  aclocal_check || DIE=1
  autoheader_check || DIE=1
fi

die_check $DIE

# if no arguments specified then this will be printed
if test -z "$*" -a -z "$ONLY_GENCONFIGURE" ; then
  echo "+ checking for autogen.sh options"
  echo "  This autogen script will automatically run ./configure as:"
  echo "  ./configure $CONFIGURE_DEF_OPT"
  echo "  To pass any additional options, please specify them on the $0"
  echo "  command line."
fi

echo "+ creating configure.ac and top-level Makefile.am"
autogen_pkg

toplevel_check $srcfile

if test -z "$ONLY_GENCONFIGURE"; then
  tool_run "$libtoolize" "--copy --force"

  if test -f acinclude.m4; then rm acinclude.m4; fi
  tool_run "$aclocal" "$ACLOCAL_FLAGS"

  tool_run "$autoheader"
fi

tool_run "$autoconf"
if test -n "$ONLY_GENCONFIGURE"; then
  exit 0
fi

debug "automake: $automake"
tool_run "$automake" "-a -c"

test -n "$NOCONFIGURE" && {
  echo "skipping configure stage for package $package, as requested."
  echo "autogen.sh done."
  exit 0
}

echo "+ running configure ... "
test ! -z "$CONFIGURE_DEF_OPT" && echo "  ./configure default flags: $CONFIGURE_DEF_OPT"
test ! -z "$CONFIGURE_EXT_OPT" && echo "  ./configure external flags: $CONFIGURE_EXT_OPT"
echo

./configure $CONFIGURE_DEF_OPT $CONFIGURE_EXT_OPT || {
        echo "  configure failed"
        exit 1
}

echo "Now type 'make' to compile $package."
