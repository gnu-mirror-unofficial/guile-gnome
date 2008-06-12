include $(top_srcdir)/common.mk

guilegwmodule_DATA = atk-spec.scm atk.scm

EXTRA_DIST = atk-spec.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

guilegnomelib_LTLIBRARIES = libgw-guile-gnome-atk.la

########################################################################
## atk
nodist_libgw_guile_gnome_atk_la_SOURCES = guile-gnome-gw-atk.c

libgw_guile_gnome_atk_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(ATK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)
libgw_guile_gnome_atk_la_LIBADD = $(ATK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)
libgw_guile_gnome_atk_la_LDFLAGS = -module

CLEANFILES = $(wildcard guile-gnome-gw-atk.*) atk.scm

BUILT_SOURCES = guile-gnome-gw-atk.c
