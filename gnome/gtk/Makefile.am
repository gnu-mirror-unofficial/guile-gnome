AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

if HAVE_ATK
ATKMODULES=gw-atk-spec.scm gw-atk.scm
ATKLIB=libgw-guile-gnome-atk.la
else
ATKMODULES=
ATKLIB=
endif

if HAVE_PANGO
PANGOMODULES=gw-pango-spec.scm gw-pango.scm
PANGOLIB=libgw-guile-gnome-pango.la
else
PANGOMODULES=
PANGOLIB=
endif

if HAVE_GDK
GDKMODULES=gw-gdk-spec.scm gw-gdk.scm gdk-event.scm
GDKLIB=libgw-guile-gnome-gdk.la
else
GDKMODULES=
GDKLIB=
endif

if HAVE_GTK
GTKMODULES=gw-gtk-spec.scm gw-gtk.scm graphical-repl.scm
GTKLIB=libgw-guile-gnome-gtk.la
else
GTKMODULES=
GTKLIB=
endif

if HAVE_GLADE
GLADEMODULES=gw-glade-spec.scm gw-glade.scm
GLADELIB=libgw-guile-gnome-glade.la
else
GLADEMODULES=
GLADELIB=
endif

guilegtkmoduledir = $(datadir)/guile/gnome/gtk

guilegtkmodule_DATA = $(ATKMODULES) $(PANGOMODULES) $(GDKMODULES) \
		      $(GTKMODULES) $(GLADEMODULES)

EXTRA_DIST = $(guilegtkmodule_DATA)

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = $(ATKLIB) $(PANGOLIB) $(GDKLIB) $(GTKLIB) $(GLADELIB)

########################################################################
## atk
libgw_guile_gnome_atk_la_SOURCES = guile-gnome-gw-atk.c

libgw_guile_gnome_atk_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(ATK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_atk_la_LIBADD = $(ATK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libgw_guile_gnome_atk_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += $(wildcard guile-gnome-gw-atk.*) gw-atk.scm

########################################################################
## pango
libgw_guile_gnome_pango_la_SOURCES = guile-gnome-gw-pango.c 

libgw_guile_gnome_pango_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(PANGO_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_pango_la_LIBADD = $(PANGO_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libgw_guile_gnome_pango_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += $(wildcard guile-gnome-gw-pango.*) gw-pango.scm

########################################################################
## gdk
libgw_guile_gnome_gdk_la_SOURCES = guile-gnome-gw-gdk.c \
	gdk-support.c gdk-support.h

libgw_guile_gnome_gdk_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(GDK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_gdk_la_LIBADD = $(GDK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libgw_guile_gnome_gdk_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += $(wildcard guile-gnome-gw-gdk.*) gw-gdk.scm

########################################################################
## gtk
libgw_guile_gnome_gtk_la_SOURCES = guile-gnome-gw-gtk.c	\
	guile-gtk-tree-model.c guile-gtk-tree-model.h gtk-support.c gtk-support.h

libgw_guile_gnome_gtk_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(GTK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) # -DGTK_DISABLE_DEPRECATED

libgw_guile_gnome_gtk_la_LIBADD = $(GTK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libgw_guile_gnome_gtk_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += $(wildcard guile-gnome-gw-gtk.*) gw-gtk.scm

GUILE_FLAGS =

########################################################################
## glade
libgw_guile_gnome_glade_la_SOURCES = guile-gnome-gw-glade.c \
	glade-support.c glade-support.h

libgw_guile_gnome_glade_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(GLADE_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_glade_la_LIBADD = $(GLADE_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libgw_guile_gnome_glade_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += $(wildcard guile-gnome-gw-glade.*) gw-glade.scm

gw-atk.scm guile-gnome-gw-atk.c: gw-atk-spec.scm ../gobject/defs-support.scm
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gtk gw-atk-spec)) \
	   (generate-wrapset guile 'gnome-atk \"guile-gnome-gw-atk\")"
	mv guile-gnome-gw-atk.scm gw-atk.scm

gw-pango.scm guile-gnome-gw-pango.c: gw-pango-spec.scm ../gobject/defs-support.scm
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gtk gw-pango-spec)) \
	   (generate-wrapset guile 'gnome-pango \"guile-gnome-gw-pango\")"
	mv guile-gnome-gw-pango.scm gw-pango.scm

gw-gdk.scm guile-gnome-gw-gdk.c: gw-gdk-spec.scm ../gobject/defs-support.scm
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gtk gw-gdk-spec)) \
	   (generate-wrapset guile 'gnome-gdk \"guile-gnome-gw-gdk\")"
	mv guile-gnome-gw-gdk.scm gw-gdk.scm

gw-gtk.scm guile-gnome-gw-gtk.c: gw-gtk-spec.scm ../gobject/defs-support.scm ../defs/gtk.defs ../defs/gtk-overrides.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (debug-set! stack 100000) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gtk gw-gtk-spec)) \
	   (generate-wrapset guile 'gnome-gtk \"guile-gnome-gw-gtk\")"
	mv guile-gnome-gw-gtk.scm gw-gtk.scm

gw-glade.scm guile-gnome-gw-glade.c: gw-glade-spec.scm \
	../gobject/defs-support.scm ../defs/libglade.defs ../defs/libglade-overrides.defs
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gtk gw-glade-spec)) \
	   (generate-wrapset guile 'gnome-glade \"guile-gnome-gw-glade\")"
	mv guile-gnome-gw-glade.scm gw-glade.scm

guile-gnome-gtk.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-gtk-procedures.txt: guile-gnome-gtk.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

#schemelib_DATA = guile-gnome-gobject-procedures.txt

DISTCLEANFILES = $(CLEANFILES)

BUILT_SOURCES = \
	guile-gnome-gw-atk.c \
	guile-gnome-gw-pango.c \
	guile-gnome-gw-gdk.c \
	guile-gnome-gw-gtk.c \
	guile-gnome-gw-glade.c \
	$(DOT_X_FILES)

# other potentially built files: gnomegobject.html $(DOT_DOC_FILES)

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done
