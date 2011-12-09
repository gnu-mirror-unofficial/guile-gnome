include $(top_srcdir)/common.mk

guilegwmodule_DATA = cairo-spec.scm cairo.scm

EXTRA_DIST = cairo-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# I wanted to avoid having a cairo shlib, because there is no need, but
# fighting g-wrap is too difficult for the moment.

guilegnomelib_LTLIBRARIES = libgw-guile-gnome-cairo.la

########################################################################
## cairo
nodist_libgw_guile_gnome_cairo_la_SOURCES = guile-gnome-gw-cairo.c

libgw_guile_gnome_cairo_la_CFLAGS = $(GUILE_GLIB_CFLAGS) $(GLIB_CFLAGS) \
	$(AM_CFLAGS) $(GUILE_CAIRO_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)
libgw_guile_gnome_cairo_la_LIBADD = $(GUILE_CAIRO_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)
libgw_guile_gnome_cairo_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-cairo.*) cairo.scm

BUILT_SOURCES = guile-gnome-gw-cairo.c
