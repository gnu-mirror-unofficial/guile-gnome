include $(top_srcdir)/common.mk

if HAVE_GDK
GDKMODULES = gdk-spec.scm gdk.scm
GDKLIB = libgw-guile-gnome-gdk.la
else
GDKMODULES =
GDKLIB =
endif

if HAVE_GTK
GTKMODULES = gtk-spec.scm gtk.scm
GTKLIB = libgw-guile-gnome-gtk.la
else
GTKMODULES =
GTKLIB =
endif

guilegwmodule_DATA = $(GDKMODULES) $(GTKMODULES)

EXTRA_DIST = gdk-spec.scm gtk-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = $(GDKLIB) $(GTKLIB)

########################################################################
## gdk
libgw_guile_gnome_gdk_la_SOURCES = guile-gnome-gw-gdk.c \
	gdk-support.c gdk-support.h

libgw_guile_gnome_gdk_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(GDK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_gdk_la_LIBADD = $(GDK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS)  $(GUILE_GLIB_LIBS)

libgw_guile_gnome_gdk_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-gdk.*) gdk.scm

########################################################################
## gtk
libgw_guile_gnome_gtk_la_SOURCES = guile-gnome-gw-gtk.c	\
	guile-gtk-tree-model.c guile-gtk-tree-model.h gtk-support.c gtk-support.h

libgw_guile_gnome_gtk_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(GTK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) # -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_gtk_la_LIBADD = $(GTK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_gtk_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-gtk.*) gtk.scm

BUILT_SOURCES = \
	guile-gnome-gw-gdk.c \
	guile-gnome-gw-gtk.c
