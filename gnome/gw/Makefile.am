include $(top_srcdir)/common.mk

guilegwmodule_DATA = libglade-spec.scm libglade.scm

EXTRA_DIST = libglade-spec.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-libglade.la

########################################################################
## glade
nodist_libgw_guile_gnome_libglade_la_SOURCES = guile-gnome-gw-libglade.c
libgw_guile_gnome_libglade_la_SOURCES = glade-support.c glade-support.h

libgw_guile_gnome_libglade_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(LIBGLADE_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_libglade_la_LIBADD = $(LIBGLADE_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_libglade_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-libglade.*) libglade.scm

BUILT_SOURCES = guile-gnome-gw-libglade.c $(DOT_X_FILES)

