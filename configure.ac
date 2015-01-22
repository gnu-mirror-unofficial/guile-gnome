define(GUILE_GNOME_PLATFORM_CONFIGURE_COPYRIGHT,[[

Copyright (C) 2011, 2012, 2015  Free Software Foundation, Inc.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program.  If not, see
<http://www.gnu.org/licenses/>.

]])

AC_PREREQ(2.52)
AC_INIT(guile-gnome-platform, 2.16.2)
AC_CONFIG_SRCDIR(ChangeLog.pre-2.16)
AC_CONFIG_AUX_DIR([build-aux])
AC_COPYRIGHT(GUILE_GNOME_PLATFORM_CONFIGURE_COPYRIGHT)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE(-Wno-portability -Wno-syntax)

AC_SUBST(VERSION,2.16.2)

# Meaning of the API version
# --------------------------
#
# If 0, guile-gnome is unstable, and the API might change anytime.
# Otherwise, guile-gnome is stable. Future incompatible releases will
# bump this number so as to allow parallel, incompatible versions to
# coexist.
API_VERSION=2
AC_SUBST(API_VERSION)

AC_DISABLE_STATIC

AC_ISC_POSIX
AC_PROG_CC
AC_STDC_HEADERS
AC_PROG_LIBTOOL

AC_SUBST(AG_PKG_CONFIG_PATH, [$pkg_config_path])

if test "x$prefix" = xNONE; then
   AC_MSG_ERROR([No explicit prefix given.

Guile-GNOME requires you to explicitly enter in a prefix when
configuring. This is because the default prefix, /usr/local, is not in
the default guile load path. What you probably want to do is to run

  $0 --prefix=/usr

so that guile can find the guile-gnome modules when they are installed.

If you choose to install to a prefix that is not in guile's load path,
you will need to modify GUILE_LOAD_PATH and LD_LIBRARY_PATH every time
you run guile. (Which is a pain.)])
fi

WARN_CFLAGS=-Wall
AC_ARG_ENABLE([Werror], AC_HELP_STRING([--enable-Werror],[Stop the build on warnings]),
        [WARN_CFLAGS="-Wall -Werror"], [])
AC_SUBST(WARN_CFLAGS)

DISABLE_DEPRECATED=false
AC_ARG_ENABLE([deprecated],
AC_HELP_STRING([--disable-deprecated],[Disable wrapping of functions deprecated upstream]),
        DISABLE_DEPRECATED=true, DISABLE_DEPRECATED=false)

#
# Check for Guile
#
GUILE_EFFECTIVE_VERSION=no
all_versions="2.2 2.0"
for v in $all_versions; do
  if test $GUILE_EFFECTIVE_VERSION = no; then
    AC_MSG_NOTICE([checking for Guile $v.x])
    PKG_CHECK_MODULES(GUILE, guile-$v, [GUILE_EFFECTIVE_VERSION=$v], [true])
  fi
done

AC_SUBST(GUILE_EFFECTIVE_VERSION)

if test $GUILE_EFFECTIVE_VERSION = no; then
  AC_MSG_ERROR([Failed to find Guile.  Is it installed?])
else
  AC_MSG_NOTICE([found Guile $GUILE_EFFECTIVE_VERSION.x])
fi

# Check for g-wrap

PKG_CHECK_MODULES(G_WRAP, g-wrap-2.0-guile >= 1.9.14)
AC_SUBST(G_WRAP_CFLAGS)
AC_SUBST(G_WRAP_LIBS)
AC_SUBST(G_WRAP_MODULE_DIR, `${PKG_CONFIG} --variable=module_directory g-wrap-2.0-guile`)
AC_SUBST(G_WRAP_LIB_DIR, `echo $G_WRAP_MODULE_DIR | sed -e 's|share/guile|lib|'`)

PACKAGES_TO_BUILD=""
PACKAGES_NOT_BUILT=""
record_check()
{
    local package=$1;
    local buildp=$2;
    case $buildp in
        yes|true)
            if test -z "$PACKAGES_TO_BUILD"; then
                PACKAGES_TO_BUILD="$package"
            else
                PACKAGES_TO_BUILD="$PACKAGES_TO_BUILD $package"
            fi
            ;;
        no|false)
            if test -z "$PACKAGES_NOT_BUILT"; then
                PACKAGES_NOT_BUILT="$package"
            else
                PACKAGES_NOT_BUILT="$PACKAGES_NOT_BUILT $package"
            fi
            ;;
        *)
            echo "bad record_check value: \"$buildp\" (for package \"$package\")"
            exit 1
            ;;
    esac
}

# Per-package checks follow
# atk
PKG_CHECK_MODULES(ATK, atk >= 1.12, HAVE_ATK=true, HAVE_ATK=false)
AC_SUBST(ATK_CFLAGS)
AC_SUBST(ATK_LIBS)
AM_CONDITIONAL(HAVE_ATK, $HAVE_ATK)
record_check atk $HAVE_ATK

# cairo

# Cairo-GObject defines some integration between Cairo and the GObject
# type system.  It has only been around since Cairo 1.10, but without
# there's no sane way to pass Cairo values through callbacks, so we
# require it to build the Cairo integration for Guile-GNOME.
PKG_CHECK_MODULES(GUILE_CAIRO, guile-cairo cairo-gobject, HAVE_CAIRO=true, HAVE_CAIRO=false)
AC_SUBST(GUILE_CAIRO_CFLAGS)
AC_SUBST(GUILE_CAIRO_LIBS)
AM_CONDITIONAL(HAVE_CAIRO, $HAVE_CAIRO)
record_check cairo $HAVE_CAIRO

# corba
PKG_CHECK_MODULES(CORBA, libbonobo-2.0, HAVE_CORBA=true, HAVE_CORBA=false)
AC_SUBST(CORBA_CFLAGS)
AC_SUBST(CORBA_LIBS)
AM_CONDITIONAL(HAVE_CORBA, $HAVE_CORBA)
ORBIT_IDL="`pkg-config --variable=orbit_idl ORBit-2.0`"
AC_SUBST(ORBIT_IDL)
BONOBO_IDL_DIR="`pkg-config --variable=idldir libbonobo-2.0`"
AC_SUBST(BONOBO_IDL_DIR)
record_check corba $HAVE_CORBA

# gconf
PKG_CHECK_MODULES(GCONF, gconf-2.0 >= 2.18, HAVE_GCONF=true, HAVE_GCONF=false)
AC_SUBST(GCONF_CFLAGS)
AC_SUBST(GCONF_LIBS)
AM_CONDITIONAL(HAVE_GCONF, $HAVE_GCONF)
record_check gconf $HAVE_GCONF

# glib
PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.10.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)
PKG_CHECK_MODULES(GOBJECT, gobject-2.0)
AC_SUBST(GOBJECT_CFLAGS)
AC_SUBST(GOBJECT_LIBS)
PKG_CHECK_MODULES(GTHREAD, gthread-2.0)
AC_SUBST(GTHREAD_CFLAGS)
AC_SUBST(GTHREAD_LIBS)
GUILE_GLIB_CFLAGS='-I$(top_srcdir)/glib/gnome/gobject -I$(top_builddir)/glib/gnome/gobject'
GUILE_GLIB_LIBS='$(top_builddir)/glib/gnome/gobject/libguile-gnome-gobject-$(API_VERSION).la'
AC_SUBST(GUILE_GLIB_CFLAGS)
AC_SUBST(GUILE_GLIB_LIBS)
AC_CONFIG_COMMANDS([glib/gnome/gobject/guile-gnome-gobject],
[
 rm -f glib/gnome/gobject/guile-gnome-gobject
 ln -s `cd $srcdir && pwd`/glib/gnome/gobject/ glib/gnome/gobject/guile-gnome-gobject
])
record_check glib true

# gnome-vfs
PKG_CHECK_MODULES(GNOME_VFS, gnome-vfs-2.0 >= 2.16.0, HAVE_GNOME_VFS=true, HAVE_GNOME_VFS=false)
AC_SUBST(GNOME_VFS_CFLAGS)
AC_SUBST(GNOME_VFS_LIBS)
AM_CONDITIONAL(HAVE_GNOME_VFS, $HAVE_GNOME_VFS)
if $DISABLE_DEPRECATED; then
  GNOME_VFS_DEPRECATED_CFLAGS=-DGNOME_VFS_DISABLE_DEPRECATED
else
  GNOME_VFS_DEPRECATED_CFLAGS=
fi
AC_SUBST(GNOME_VFS_DEPRECATED_CFLAGS)
record_check gnome-vfs $HAVE_GNOME_VFS

# gtk
PKG_CHECK_MODULES(GDK_PIXBUF, gdk-pixbuf-2.0 >= 2.10.0, HAVE_GDK_PIXBUF=true, HAVE_GDK_PIBXUF=false)
AC_SUBST(GDK_PIXBUF_CFLAGS)
AC_SUBST(GDK_PIXBUF_LIBS)
AM_CONDITIONAL(HAVE_GDK_PIXBUF, $HAVE_GDK_PIXBUF)
if $HAVE_CAIRO; then
  PKG_CHECK_MODULES(GDK, gdk-2.0 >= 2.10.0, HAVE_GDK=true, HAVE_GDK=false)
  AC_SUBST(GDK_CFLAGS)
  AC_SUBST(GDK_LIBS)

  PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.10.0, HAVE_GTK=true, HAVE_GTK=false)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)

  if $DISABLE_DEPRECATED; then
    GTK_DEPRECATED_CFLAGS=-DGTK_DISABLE_DEPRECATED
  else
    GTK_DEPRECATED_CFLAGS=
  fi
  AC_SUBST(GTK_DEPRECATED_CFLAGS)
else
  AC_MSG_WARN(not building gdk/gtk due to missing guile-cairo)
  HAVE_GDK=false
  HAVE_GTK=false
fi
AM_CONDITIONAL(HAVE_GDK, $HAVE_GDK)
AM_CONDITIONAL(HAVE_GTK, $HAVE_GTK)
record_check gtk $HAVE_GTK

# libglade
PKG_CHECK_MODULES(LIBGLADE, libglade-2.0 >= 2.6, HAVE_LIBGLADE=true, HAVE_LIBGLADE=false)
AC_SUBST(LIBGLADE_CFLAGS)
AC_SUBST(LIBGLADE_LIBS)
AM_CONDITIONAL(HAVE_LIBGLADE, $HAVE_LIBGLADE)
record_check libglade $HAVE_LIBGLADE

# libgnome
PKG_CHECK_MODULES(LIBGNOME, libgnome-2.0 >= 2.16, HAVE_LIBGNOME=true, HAVE_LIBGNOME=false)
AC_SUBST(LIBGNOME_CFLAGS)
AC_SUBST(LIBGNOME_LIBS)
AM_CONDITIONAL(HAVE_LIBGNOME, $HAVE_LIBGNOME)
record_check libgnome $HAVE_LIBGNOME

# libgnomecanvas
PKG_CHECK_MODULES(CANVAS, libgnomecanvas-2.0 >= 2.14, HAVE_CANVAS=true, HAVE_CANVAS=false)
AC_SUBST(CANVAS_CFLAGS)
AC_SUBST(CANVAS_LIBS)
AM_CONDITIONAL(HAVE_CANVAS, $HAVE_CANVAS)
record_check libgnomecanvas $HAVE_CANVAS

# libgnomeui
PKG_CHECK_MODULES(LIBGNOMEUI, libgnomeui-2.0 >= 2.16.0, HAVE_LIBGNOMEUI=true, HAVE_LIBGNOMEUI=false)
AC_SUBST(LIBGNOMEUI_CFLAGS)
AC_SUBST(LIBGNOMEUI_LIBS)
AM_CONDITIONAL(HAVE_LIBGNOMEUI, $HAVE_LIBGNOMEUI)
if $DISABLE_DEPRECATED; then
  GNOMEUI_DEPRECATION_FLAGS=-DGNOME_DISABLE_DEPRECATED_SOURCE -DGNOME_DISABLE_DEPRECATED
else
  GNOMEUI_DEPRECATION_FLAGS=
fi
AC_SUBST(GNOMEUI_DEPRECATION_FLAGS)
record_check libgnomeui $HAVE_LIBGNOMEUI

# pango
PKG_CHECK_MODULES(PANGO, pango >= 1.14, HAVE_PANGO=true, HAVE_PANGO=false)
AC_SUBST(PANGO_CFLAGS)
AC_SUBST(PANGO_LIBS)
AM_CONDITIONAL(HAVE_PANGO, $HAVE_PANGO)
PKG_CHECK_MODULES(PANGOCAIRO, pangocairo >= 1.14, HAVE_PANGOCAIRO=true, HAVE_PANGOCAIRO=false)
AC_SUBST(PANGOCAIRO_CFLAGS)
AC_SUBST(PANGOCAIRO_LIBS)
AM_CONDITIONAL(HAVE_PANGOCAIRO, $HAVE_PANGOCAIRO)
record_check pango $HAVE_PANGO

if test -z "$PACKAGES_TO_BUILD"; then
    AC_MSG_ERROR([All wrappers failed their prerequisites.

These following wrappers cannot be built:
  $PACKAGES_NOT_BUILT

Check the README files in the above subdirectories, install the necessary
packages, and try again.
])
fi

if test -z "$PACKAGES_NOT_BUILT"; then
    AC_MSG_NOTICE([
All available wrappers will be built:
  $PACKAGES_TO_BUILD

])
else
    AC_MSG_NOTICE([Some packages will not be built.

These wrappers will be built:
  $PACKAGES_TO_BUILD

These wrappers failed their prerequisites and will NOT be built:
  $PACKAGES_NOT_BUILT

Check the README files in the above subdirectories for more information
on the prerequisites of a package.
])
fi

AC_CONFIG_FILES(dev-environ, [chmod +x ./dev-environ])
AC_CONFIG_FILES(
Makefile
atk/doc/Makefile
atk/tests/Makefile
atk/gnome/gw/Makefile
atk/gnome/Makefile
atk/gnome/overrides/Makefile
atk/Makefile
cairo/gnome/gw/Makefile
cairo/gnome/Makefile
cairo/Makefile
corba/doc/Makefile
corba/tests/Makefile
corba/gnome/gw/Makefile
corba/gnome/Makefile
corba/gnome/corba/Makefile
corba/Makefile
corba/demos/Makefile
corba/demos/corba/Makefile
defs/tools/Makefile
defs/gnome/Makefile
defs/gnome/defs/Makefile
defs/Makefile
gconf/examples/Makefile
gconf/examples/gconf/Makefile
gconf/doc/Makefile
gconf/tests/Makefile
gconf/gnome/gw/Makefile
gconf/gnome/Makefile
gconf/gnome/overrides/Makefile
gconf/Makefile
glib/examples/Makefile
glib/doc/Makefile
glib/doc/glib/Makefile
glib/doc/gobject/Makefile
glib/gnome/gw/Makefile
glib/gnome/gw/support/Makefile
glib/gnome/Makefile
glib/gnome/overrides/Makefile
glib/gnome/gobject/Makefile
glib/test-suite/Makefile
glib/Makefile
glib/bin/Makefile
gnome-vfs/examples/gnome-vfs/Makefile
gnome-vfs/examples/Makefile
gnome-vfs/doc/Makefile
gnome-vfs/tests/Makefile
gnome-vfs/gnome/gw/Makefile
gnome-vfs/gnome/Makefile
gnome-vfs/gnome/overrides/Makefile
gnome-vfs/Makefile
gtk/examples/Makefile
gtk/doc/gtk/Makefile
gtk/doc/gdk/Makefile
gtk/doc/Makefile
gtk/tests/gtk/Makefile
gtk/tests/gdk/Makefile
gtk/tests/Makefile
gtk/gnome/contrib/Makefile
gtk/gnome/gw/Makefile
gtk/gnome/gtk/Makefile
gtk/gnome/Makefile
gtk/gnome/overrides/Makefile
gtk/Makefile
libglade/examples/Makefile
libglade/doc/Makefile
libglade/tests/Makefile
libglade/gnome/gw/Makefile
libglade/gnome/Makefile
libglade/gnome/overrides/Makefile
libglade/Makefile
libgnome/doc/Makefile
libgnome/tests/Makefile
libgnome/gnome/gw/Makefile
libgnome/gnome/Makefile
libgnome/gnome/overrides/Makefile
libgnome/Makefile
libgnomecanvas/examples/Makefile
libgnomecanvas/doc/Makefile
libgnomecanvas/tests/Makefile
libgnomecanvas/gnome/gw/Makefile
libgnomecanvas/gnome/Makefile
libgnomecanvas/gnome/overrides/Makefile
libgnomecanvas/Makefile
libgnomeui/doc/Makefile
libgnomeui/tests/Makefile
libgnomeui/gnome/gw/Makefile
libgnomeui/gnome/Makefile
libgnomeui/gnome/overrides/Makefile
libgnomeui/Makefile
pango/doc/pango/Makefile
pango/doc/pangocairo/Makefile
pango/doc/Makefile
pango/tests/pango/Makefile
pango/tests/pangocairo/Makefile
pango/tests/Makefile
pango/gnome/gw/Makefile
pango/gnome/Makefile
pango/gnome/overrides/Makefile
pango/Makefile
cairo/guile-gnome-cairo.pc
cairo/guile-gnome-cairo-uninstalled.pc
defs/guile-gnome-defs.pc
defs/guile-gnome-defs-uninstalled.pc
glib/guile-gnome-glib.pc
glib/guile-gnome-glib-uninstalled.pc
gtk/guile-gnome-gtk.pc
gtk/guile-gnome-gtk-uninstalled.pc
libgnomeui/guile-gnome-libgnomeui.pc
libgnomeui/guile-gnome-libgnomeui-uninstalled.pc
)
AC_OUTPUT
