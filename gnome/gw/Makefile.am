include $(top_srcdir)/common.mk

guilegwmodule_DATA = canvas-spec.scm canvas.scm

EXTRA_DIST = canvas-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-canvas.la

########################################################################
## canvas
libgw_guile_gnome_canvas_la_SOURCES = guile-gnome-gw-canvas.c \
	libgnomecanvas-support.c libgnomecanvas-support.h

libgw_guile_gnome_canvas_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(CANVAS_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_canvas_la_LIBADD = $(CANVAS_LIBS) $(GUILE_GLIB_LIBS) \
	$(GUILE_LIBS) $(G_WRAP_LINK_ARGS) 

libgw_guile_gnome_canvas_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-canvas.*) canvas.scm

BUILT_SOURCES = guile-gnome-gw-libgnomecanvas.c

