AM_CFLAGS = $(WARN_CFLAGS) -Werror

if HAVE_ATK
ATKMODULES=gw-atk-spec.scm gw-atk.scm
ATKLIB=libguile-gnome-gw-atk.la
else
ATKMODULES=
ATKLIB=
endif

if HAVE_PANGO
PANGOMODULES=gw-pango-spec.scm gw-pango.scm
PANGOLIB=libguile-gnome-gw-pango.la
else
PANGOMODULES=
PANGOLIB=
endif

if HAVE_GDK
GDKMODULES=gw-gdk-spec.scm gw-gdk.scm
GDKLIB=libguile-gnome-gw-gdk.la
else
GDKMODULES=
GDKLIB=
endif

if HAVE_GTK
GTKMODULES=gw-gtk-spec.scm gw-gtk.scm generics.scm
GTKLIB=libguile-gnome-gw-gtk.la
else
GTKMODULES=
GTKLIB=
endif

guilegtkmoduledir = $(datadir)/guile/gnome/gtk

guilegtkmodule_DATA = $(ATKMODULES) $(PANGOMODULES) $(GDKMODULES) $(GTKMODULES)

EXTRA_DIST = $(guilegtkmodule_DATA)

CLEANFILES =

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = $(ATKLIB) $(PANGOLIB) $(GDKLIB) $(GTKLIB)

########################################################################
## atk
libguile_gnome_gw_atk_la_SOURCES = guile-gnome-gw-atk.c guile-gnome-gw-atk.h

libguile_gnome_gw_atk_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(ATK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libguile_gnome_gw_atk_la_LIBADD = $(ATK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libguile-gnome-gw-gobject.la

libguile_gnome_gw_atk_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += guile-gnome-gw-atk.c guile-gnome-gw-atk.h \
	guile-gnome-gw-glib.log \
	guile-gnome-gw-atk.log guile-gnome-gw-atk.html gw-atk.scm

########################################################################
## pango
libguile_gnome_gw_pango_la_SOURCES = guile-gnome-gw-pango.c guile-gnome-gw-pango.h

libguile_gnome_gw_pango_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(PANGO_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libguile_gnome_gw_pango_la_LIBADD = $(PANGO_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libguile-gnome-gw-gobject.la

libguile_gnome_gw_pango_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += guile-gnome-gw-pango.c guile-gnome-gw-pango.h \
	guile-gnome-gw-pango.log guile-gnome-gw-pango.html gw-pango.scm

########################################################################
## gdk
libguile_gnome_gw_gdk_la_SOURCES = guile-gnome-gw-gdk.c guile-gnome-gw-gdk.h

libguile_gnome_gw_gdk_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(GDK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libguile_gnome_gw_gdk_la_LIBADD = $(GDK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libguile-gnome-gw-gobject.la

libguile_gnome_gw_gdk_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += guile-gnome-gw-gdk.c guile-gnome-gw-gdk.h \
	guile-gnome-gw-gdk.log guile-gnome-gw-gdk.html gw-gdk.scm

########################################################################
## gtk
libguile_gnome_gw_gtk_la_SOURCES = guile-gnome-gw-gtk.c	guile-gnome-gw-gtk.h

libguile_gnome_gw_gtk_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(GTK_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) -DGTK_DISABLE_DEPRECATED

libguile_gnome_gw_gtk_la_LIBADD = $(GTK_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libguile-gnome-gw-gobject.la

libguile_gnome_gw_gtk_la_LDFLAGS = \
	-export-dynamic

CLEANFILES += guile-gnome-gw-gtk.c guile-gnome-gw-gtk.h \
	guile-gnome-gw-gtk.log guile-gnome-gw-gtk.html gw-gtk.scm

gw-atk.scm guile-gnome-gw-atk.c guile-gnome-gw-atk.h: gw-atk-spec.scm ../gobject/defs-support.scm
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-atk-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-atk\")"
	mv guile-gnome-gw-atk.scm gw-atk.scm

gw-pango.scm guile-gnome-gw-pango.c guile-gnome-gw-pango.h: gw-pango-spec.scm ../gobject/defs-support.scm
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-pango-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-pango\")"
	mv guile-gnome-gw-pango.scm gw-pango.scm

gw-gdk.scm guile-gnome-gw-gdk.c guile-gnome-gw-gdk.h: gw-gdk-spec.scm ../gobject/defs-support.scm
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-gdk-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-gdk\")"
	mv guile-gnome-gw-gdk.scm gw-gdk.scm

gw-gtk.scm guile-gnome-gw-gtk.c guile-gnome-gw-gtk.h: gw-gtk-spec.scm ../gobject/defs-support.scm
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-gtk-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-gtk\")"
	mv guile-gnome-gw-gtk.scm gw-gtk.scm

guile-gnome-gtk.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-gtk-procedures.txt: guile-gnome-gtk.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

#schemelib_DATA = guile-gnome-gobject-procedures.txt

DISTCLEANFILES = $(CLEANFILES)

BUILT_SOURCES = \
	guile-gnome-gw-atk.h guile-gnome-gw-atk.c \
	guile-gnome-gw-pango.h guile-gnome-gw-pango.c \
	guile-gnome-gw-gdk.h guile-gnome-gw-gdk.c \
	guile-gnome-gw-gtk.h guile-gnome-gw-gtk.c \
	$(DOT_X_FILES)

# other potentially built files: gnomegobject.html $(DOT_DOC_FILES)

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done
