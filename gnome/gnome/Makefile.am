AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

GUILE_FLAGS = 

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

EXTRA_DIST = gw-gnome-spec.scm gw-canvas-spec.scm gw-ui-spec.scm \
	 gw-vte-spec.scm gw-source-view-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = $(LIBGNOMELIB) $(LIBGNOMECANVASLIB) $(LIBGNOMEUILIB) \
	$(VTELIB) $(GTKSOURCEVIEWLIB)

########################################################################
## libgnome
libguile_gnome_gw_gnome_la_SOURCES = \
	guile-gnome-gw-gnome.c \
	gnome-support.c gnome-support.h

libguile_gnome_gw_gnome_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(LIBGNOME_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_gnome_la_LIBADD = $(LIBGNOME_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libguile_gnome_gw_gnome_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-gnome.*) gw-gnome.scm

gw-gnome.scm guile-gnome-gw-gnome.c: \
	gw-gnome-spec.scm ../defs/gnome.defs ../defs/gnome-overrides.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gnome gw-gnome-spec)) \
	   (generate-wrapset guile 'gnome-libgnome \"guile-gnome-gw-gnome\")"
	mv guile-gnome-gw-gnome.scm gw-gnome.scm

########################################################################
## libgnomecanvas
libguile_gnome_gw_canvas_la_SOURCES = guile-gnome-gw-canvas.c

libguile_gnome_gw_canvas_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(LIBGNOMECANVAS_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_canvas_la_LIBADD = $(LIBGNOMECANVAS_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libguile_gnome_gw_canvas_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += $(wildcard guile-gnome-gw-canvas.*) gw-canvas.scm

gw-canvas.scm guile-gnome-gw-canvas.c: \
	gw-canvas-spec.scm ../defs/canvas.defs ../defs/canvas-overrides.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gnome gw-canvas-spec)) \
	   (generate-wrapset guile 'gnome-canvas \"guile-gnome-gw-canvas\")"
	mv guile-gnome-gw-canvas.scm gw-canvas.scm

########################################################################
## libgnomeui
libguile_gnome_gw_ui_la_SOURCES = guile-gnome-gw-ui.c

libguile_gnome_gw_ui_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(LIBGNOMEUI_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_ui_la_LIBADD = $(LIBGNOMEUI_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libguile_gnome_gw_ui_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-ui.*) gw-ui.scm

gw-ui.scm guile-gnome-gw-ui.c: \
	gw-ui-spec.scm ../defs/ui.defs ../defs/ui-overrides.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gnome gw-ui-spec)) \
	   (generate-wrapset guile 'gnome-ui \"guile-gnome-gw-ui\")"
	mv guile-gnome-gw-ui.scm gw-ui.scm

########################################################################
## vte
libguile_gnome_gw_vte_la_SOURCES = guile-gnome-gw-vte.c

libguile_gnome_gw_vte_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(VTE_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_vte_la_LIBADD = $(VTE_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libguile_gnome_gw_vte_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-vte.*) gw-vte.scm

gw-vte.scm guile-gnome-gw-vte.c: gw-vte-spec.scm ../defs/vte.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gnome gw-vte-spec)) \
	   (generate-wrapset guile 'gnome-vte \"guile-gnome-gw-vte\")"
	mv guile-gnome-gw-vte.scm gw-vte.scm

########################################################################
## GtkSourceView
libguile_gnome_gw_source_view_la_SOURCES = \
	guile-gnome-gw-source-view.c \
	source-view-support.c source-view-support.h

libguile_gnome_gw_source_view_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(GTKSOURCEVIEW_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_source_view_la_LIBADD = $(GTKSOURCEVIEW_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libguile_gnome_gw_source_view_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-source-view.*) gw-source-view.scm

gw-source-view.scm guile-gnome-gw-source-view.c: \
	gw-source-view-spec.scm ../defs/gtksourceview.defs ../defs/gtksourceview-overrides.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gnome gw-source-view-spec)) \
	   (generate-wrapset guile 'gnome-source-view \"guile-gnome-gw-source-view\")"
	mv guile-gnome-gw-source-view.scm gw-source-view.scm

DISTCLEANFILES = $(CLEANFILES)

BUILT_SOURCES = \
	guile-gnome-gw-gnome.c \
	guile-gnome-gw-canvas.c \
	guile-gnome-gw-ui.c \
	guile-gnome-gw-vte.c \
	guile-gnome-gw-source-view.c

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done
