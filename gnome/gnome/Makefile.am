AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

if HAVE_LIBGNOME
LIBGNOMEMODULES=gw-gnome-spec.scm gw-gnome.scm
LIBGNOMELIB=libguile-gnome-gw-gnome.la
else
LIBGNOMEMODULES=
LIBGNOMELIB=
endif

if HAVE_LIBGNOMECANVAS
LIBGNOMECANVASMODULES=gw-canvas-spec.scm gw-canvas.scm
LIBGNOMECANVASLIB=libguile-gnome-gw-canvas.la
else
LIBGNOMECANVASMODULES=
LIBGNOMECANVASLIB=
endif

if HAVE_LIBGNOMEUI
LIBGNOMEUIMODULES=gw-ui-spec.scm gw-ui.scm
LIBGNOMEUILIB=libguile-gnome-gw-ui.la
else
LIBGNOMEUIMODULES=
LIBGNOMEUILIB=
endif

if HAVE_VTE
VTEMODULES=gw-vte-spec.scm gw-vte.scm
VTELIB=libguile-gnome-gw-vte.la
else
VTEMODULES=
VTELIB=
endif

if HAVE_GTKSOURCEVIEW
GTKSOURCEVIEWMODULES=gw-source-view-spec.scm gw-source-view.scm
GTKSOURCEVIEWLIB=libguile-gnome-gw-source-view.la
else
GTKSOURCEVIEWMODULES=
GTKSOURCEVIEWLIB=
endif

guilegnomemoduledir = $(datadir)/guile/gnome/gnome

guilegnomemodule_DATA = $(VTEMODULES) $(GTKSOURCEVIEWMODULES)

EXTRA_DIST = gw-vte-spec.scm gw-vte.scm

CLEANFILES =

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = $(VTELIB) $(GTKSOURCEVIEWLIB)

########################################################################
## vte
libguile_gnome_gw_vte_la_SOURCES = guile-gnome-gw-vte.c guile-gnome-gw-vte.h

libguile_gnome_gw_vte_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(VTE_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_vte_la_LIBADD = $(VTE_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libguile-gnome-gw-gobject.la

libguile_gnome_gw_vte_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += guile-gnome-gw-vte.c guile-gnome-gw-vte.h \
	guile-gnome-gw-glib.log guile-gnome-gw-atk.log \
	guile-gnome-gw-gdk.log guile-gnome-gw-pango.log \
	guile-gnome-gw-gtk.log \
	guile-gnome-gw-vte.log guile-gnome-gw-vte.html gw-vte.scm

gw-vte.scm guile-gnome-gw-vte.c guile-gnome-gw-vte.h: gw-vte-spec.scm ../defs/vte.defs
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-vte-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-vte\")"
	mv guile-gnome-gw-vte.scm gw-vte.scm

########################################################################
## GtkSourceView
libguile_gnome_gw_source_view_la_SOURCES = \
	guile-gnome-gw-source-view.c guile-gnome-gw-source-view.h \
	source-view-support.c source-view-support.h

libguile_gnome_gw_source_view_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(GTKSOURCEVIEW_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_source_view_la_LIBADD = $(GTKSOURCEVIEW_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libguile-gnome-gw-gobject.la

libguile_gnome_gw_source_view_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += guile-gnome-gw-source-view.c guile-gnome-gw-source-view.h \
	guile-gnome-gw-glib.log guile-gnome-gw-atk.log \
	guile-gnome-gw-gdk.log guile-gnome-gw-pango.log \
	guile-gnome-gw-gtk.log \
	guile-gnome-gw-source-view.log guile-gnome-gw-source-view.html \
	gw-source-view.scm

gw-source-view.scm guile-gnome-gw-source-view.c guile-gnome-gw-source-view.h: \
	gw-source-view-spec.scm ../defs/gtksourceview.defs ../defs/gtksourceview-overrides.defs
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-source-view-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-source-view\")"
	mv guile-gnome-gw-source-view.scm gw-source-view.scm

DISTCLEANFILES = $(CLEANFILES)

BUILT_SOURCES = \
	guile-gnome-gw-gnome.h guile-gnome-gw-gnome.c \
	guile-gnome-gw-canvas.h guile-gnome-gw-canvas.c \
	guile-gnome-gw-ui.h guile-gnome-gw-ui.c \
	guile-gnome-gw-vte.h guile-gnome-gw-vte.c \
	guile-gnome-gw-source-view.h guile-gnome-gw-source-view.c

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done
