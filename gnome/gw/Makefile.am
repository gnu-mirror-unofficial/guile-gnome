include $(top_srcdir)/common.mk

guilegwmodule_DATA = gconf-spec.scm gconf.scm

EXTRA_DIST = gconf-spec.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

guilegnomelib_LTLIBRARIES = libgw-guile-gnome-gconf.la

########################################################################
## gconf
nodist_libgw_guile_gnome_gconf_la_SOURCES = guile-gnome-gw-gconf.c
libgw_guile_gnome_gconf_la_SOURCES = gconf-support.c gconf-support.h

libgw_guile_gnome_gconf_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(GCONF_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_gconf_la_LIBADD = $(GCONF_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_gconf_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-gconf.*) gconf.scm

BUILT_SOURCES = guile-gnome-gw-gconf.c $(DOT_X_FILES)

