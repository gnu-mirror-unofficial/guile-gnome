include $(top_srcdir)/common.mk

guilegwmodule_DATA = libgnome-spec.scm libgnome.scm

EXTRA_DIST = libgnome-spec.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-libgnome.la

########################################################################
## libgnome
nodist_libgw_guile_gnome_libgnome_la_SOURCES = guile-gnome-gw-libgnome.c
libgw_guile_gnome_libgnome_la_SOURCES = gnome-support.c gnome-support.h

libgw_guile_gnome_libgnome_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(LIBGNOME_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_libgnome_la_LIBADD = $(LIBGNOME_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_libgnome_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-libgnome.*) libgnome.scm

BUILT_SOURCES = guile-gnome-gw-libgnome.c

