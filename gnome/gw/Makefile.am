include $(top_srcdir)/common.mk

guilegwmodule_DATA = libgnomecanvas-spec.scm libgnomecanvas.scm

EXTRA_DIST = libgnomecanvas-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-libgnomecanvas.la

########################################################################
## canvas
libgw_guile_gnome_libgnomecanvas_la_SOURCES = guile-gnome-gw-libgnomecanvas.c \
	libgnomecanvas-support.c libgnomecanvas-support.h

libgw_guile_gnome_libgnomecanvas_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(CANVAS_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_libgnomecanvas_la_LIBADD = $(CANVAS_LIBS) $(GUILE_GLIB_LIBS) \
	$(GUILE_LIBS) $(G_WRAP_LINK_ARGS) 

libgw_guile_gnome_libgnomecanvas_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-libgnomecanvas.*) libgnomecanvas.scm

BUILT_SOURCES = guile-gnome-gw-libgnomecanvas.c

