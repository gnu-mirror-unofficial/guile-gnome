include $(top_srcdir)/common.mk

guilegwmodule_DATA = gnome-ui-spec.scm gnome-ui.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-gnome-ui.la

########################################################################
## gnome-ui
libgw_guile_gnome_gnome_ui_la_SOURCES = guile-gnome-gw-gnome-ui.c

libgw_guile_gnome_gnome_ui_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(LIBGNOMEUI_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_gnome_ui_la_LIBADD = $(LIBGNOMEUI_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_gnome_ui_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-*) gnome-ui.scm

BUILT_SOURCES = guile-gnome-gw-gnome-ui.c

EXTRA_DIST = gnome-ui-spec.scm
