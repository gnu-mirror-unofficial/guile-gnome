include $(top_srcdir)/common.mk

if HAVE_GTK
GTKMODULES = gtk-spec.scm gtk.scm \
	gdk-spec.scm gdk.scm
GTKLIB = libgw-guile-gnome-gtk.la \
	libgw-guile-gnome-gdk.la
else
GTKMODULES =
GTKLIB =
endif

# Make sure the user isn't compiling with -O, because it makes gcc 3.3
# and up break
#CFLAGS=-g 

guilegwmodule_DATA = $(GTKMODULES)

EXTRA_DIST = gdk-spec.scm gtk-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

guilegnomelib_LTLIBRARIES = $(GTKLIB)

########################################################################
## gdk
nodist_libgw_guile_gnome_gdk_la_SOURCES = guile-gnome-gw-gdk.c
libgw_guile_gnome_gdk_la_SOURCES = gdk-support.c gdk-support.h \
	gdk-pixbuf-support.c gdk-pixbuf-support.h

libgw_guile_gnome_gdk_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(GUILE_CAIRO_CFLAGS) $(GDK_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) $(GTK_DEPRECATED_CFLAGS)

libgw_guile_gnome_gdk_la_LIBADD = $(GDK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS) $(GUILE_CAIRO_LIBS) $(GDK_PIXBUF_LIBS)

libgw_guile_gnome_gdk_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-gdk.*) gdk.scm

########################################################################
## gtk
nodist_libgw_guile_gnome_gtk_la_SOURCES = guile-gnome-gw-gtk.c
libgw_guile_gnome_gtk_la_SOURCES = \
	guile-gtk-tree-model.c guile-gtk-tree-model.h gtk-support.c gtk-support.h

libgw_guile_gnome_gtk_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(GUILE_CAIRO_CFLAGS) $(GTK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) $(GTK_DEPRECATED_CFLAGS)

libgw_guile_gnome_gtk_la_LIBADD = $(GTK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS) $(GUILE_CAIRO_CFLAGS)

libgw_guile_gnome_gtk_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-gtk.*) gtk.scm

BUILT_SOURCES = \
	guile-gnome-gw-gdk.c \
	guile-gnome-gw-gtk.c
