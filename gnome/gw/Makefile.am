include $(top_srcdir)/common.mk

guilegwmodule_DATA = glade-spec.scm glade.scm

EXTRA_DIST = glade-spec.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-glade.la

########################################################################
## glade
libgw_guile_gnome_glade_la_SOURCES = guile-gnome-gw-glade.c \
	glade-support.c glade-support.h

libgw_guile_gnome_glade_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(LIBGLADE_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_glade_la_LIBADD = $(LIBGLADE_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_glade_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-glade.*) glade.scm

BUILT_SOURCES = guile-gnome-gw-glade.c $(DOT_X_FILES)

