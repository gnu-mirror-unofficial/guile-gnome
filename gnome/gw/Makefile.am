include $(top_srcdir)/common.mk

guilegwmodule_DATA = pango-spec.scm pango.scm

EXTRA_DIST = pango-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

guilegnomelib_LTLIBRARIES = libgw-guile-gnome-pango.la

########################################################################
## pango
nodist_libgw_guile_gnome_pango_la_SOURCES = guile-gnome-gw-pango.c 

libgw_guile_gnome_pango_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(PANGO_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_pango_la_LIBADD = $(PANGO_LIBS) $(GUILE_GLIB_LIBS) \
	$(GUILE_LIBS) $(G_WRAP_LIBS) 

libgw_guile_gnome_pango_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-pango.*) pango.scm

BUILT_SOURCES = guile-gnome-gw-pango.c

