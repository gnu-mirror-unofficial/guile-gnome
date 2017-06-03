#!/bin/sh
# Run this to generate all the initial makefiles, etc.

# configure.ac uses the guile.m4 GUILE_FLAGS macro, among others,
# which needs build-aux/config.rpath which is not installed anymore by
# modern version of automake, and without it, this script will raise
# an error. For why it's needed, see the comments wtr in m4/guile.m4.
if [ ! -d "build-aux" ]; then
    mkdir build-aux
fi
touch build-aux/config.rpath

autoreconf -vif

echo "Now run ./configure --prefix=/your/prefix to configure guile-gnome-platform."
