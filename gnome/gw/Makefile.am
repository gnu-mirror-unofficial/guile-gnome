include $(top_srcdir)/common.mk

guilegwmodule_DATA = libgnomeui-spec.scm libgnomeui.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

guilegnomelib_LTLIBRARIES = libgw-guile-gnome-libgnomeui.la

########################################################################
## libgnomeui
nodist_libgw_guile_gnome_libgnomeui_la_SOURCES = guile-gnome-gw-libgnomeui.c

libgw_guile_gnome_libgnomeui_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(LIBGNOMEUI_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) $(GNOMEUI_DEPRECATION_CFLAGS)

libgw_guile_gnome_libgnomeui_la_LIBADD = $(LIBGNOMEUI_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_libgnomeui_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-*) libgnomeui.scm

BUILT_SOURCES = guile-gnome-gw-libgnomeui.c

EXTRA_DIST = libgnomeui-spec.scm
