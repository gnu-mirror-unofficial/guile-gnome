schemelibdir = $(pkgdatadir)/$(VERSION)

AM_CFLAGS = -I. -I$(srcdir) $(WARN_CFLAGS) $(DEBUG_CFLAGS)

# fixme: what is this for?
ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

guilegobjectmoduledir = $(datadir)/guile/gnome/gobject

guilegobjectmodule_DATA = primitives.scm gw-gobject-spec.scm gw-gobject.scm \
	gw-glib-spec.scm gw-glib.scm gw-spec-utils.scm gw-utils.scm \
	defs-support.scm event-repl.scm generics.scm \
	gw-standard.scm gw-standard-spec.scm

# There are two shared libraries:
#
# libguile-gnome-gobject, which contains the core gobject support code
# (both types and functions)
#
# libguile-gnome-gw-gobject, produced by g-wrap, which supports the
# g-wrap type wrappings for gobject-related types. This one is not
# used by the scheme<->gobject code; it's only there to support other
# wrapsets.
#
# Also, there's a g-wrap-produced wrapper for glib types and functions,
# libguile-gnome-gw-glib.la.

lib_LTLIBRARIES = libguile-gnome-gobject.la \
	libguile-gnome-gw-gobject.la \
	libguile-gnome-gw-glib.la \
	libguile-gnome-gw-standard.la

# libguile-gnome-gobject (core library)

pkginclude_HEADERS = guile-gnome-gobject.h	\
	guile-gnome-gobject-primitives.h

libguile_gnome_gobject_la_SOURCES = \
	guile-gnome-gobject.c			\
	guile-gnome-gobject.h			\
	guile-gnome-gobject-primitives.c	\
	guile-gnome-gobject-primitives.h	\
	guile-support.c				\
	guile-support.h

libguile_gnome_gobject_la_CFLAGS = $(AM_CFLAGS) \
				   $(GOBJECT_CFLAGS) $(GUILE_CFLAGS) \
	-DGUILE_GOBJECT_DIR=\""$(schemelibdir)"\"

libguile_gnome_gobject_la_LIBADD = $(GOBJECT_LIBS) $(GUILE_LIBS)

libguile_gnome_gobject_la_LDFLAGS = \
	-export-dynamic

# libguile-gnome-gw-gobject (g-wrap support)

libguile_gnome_gw_gobject_la_SOURCES = \
	guile-gnome-gw-gobject.c	\
	guile-gnome-gw-gobject.h	\
	glib-support.c

noinst_HEADERS=glib-support.h

libguile_gnome_gw_gobject_la_CFLAGS = $(AM_CFLAGS) $(GOBJECT_CFLAGS) \
	$(GUILE_CFLAGS) $(G_WRAP_CFLAGS)

libguile_gnome_gw_gobject_la_LIBADD = $(GOBJECT_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) libguile-gnome-gobject.la

libguile_gnome_gw_gobject_la_LDFLAGS = \
	-export-dynamic

# libguile-gnome-gw-glib (g-wrap support for glib)

libguile_gnome_gw_glib_la_SOURCES = \
	guile-gnome-gw-glib.c	\
	guile-gnome-gw-glib.h

libguile_gnome_gw_glib_la_CFLAGS = $(GLIB_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_glib_la_LIBADD = $(GLIB_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) libguile-gnome-gobject.la

libguile_gnome_gw_glib_la_LDFLAGS = \
	-export-dynamic

# libguile-gnome-gw-standard (g-wrap support for standard types)

libguile_gnome_gw_standard_la_SOURCES = \
	guile-gnome-gw-standard.c	\
	guile-gnome-gw-standard.h

libguile_gnome_gw_standard_la_CFLAGS = $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_standard_la_LIBADD = $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) libguile-gnome-gobject.la

libguile_gnome_gw_standard_la_LDFLAGS = \
	-export-dynamic

DOT_X_FILES = \
	guile-gnome-gobject.x			\
	guile-gnome-gobject-primitives.x

DOT_DOC_FILES = \
	guile-gnome-gobject.doc			\
	guile-gnome-gobject-primitives.doc

SUFFIXES = .x .doc

GUILE_SNARF_CFLAGS = $(DEFS) $(AM_CFLAGS) $(GUILE_CFLAGS) $(GOBJECT_CFLAGS)

# For overriding from the command line (e.g. --debug)
GUILE_FLAGS = 

.c.x:
	guile-snarf $(GUILE_SNARF_CFLAGS) $< > $@ \
	|| { rm $@; false; }
.c.doc:
	-(guile-func-name-check $<)
	(guile-snarf-docs $(GUILE_SNARF_CFLAGS) $< | \
	guile_filter_doc_snarfage --filter-snarfage) > $@ || { rm $@; false; }

gw-gobject.scm guile-gnome-gw-gobject.c guile-gnome-gw-gobject.h: gw-gobject-spec.scm
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-gobject-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-gobject\")"
	mv guile-gnome-gw-gobject.scm gw-gobject.scm

gw-glib.scm guile-gnome-gw-glib.c guile-gnome-gw-glib.h: gw-glib-spec.scm
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-glib-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-glib\")"
	mv guile-gnome-gw-glib.scm gw-glib.scm

gw-standard.scm guile-gnome-gw-standard.c guile-gnome-gw-standard.h: gw-standard-spec.scm
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-standard-spec.scm\") \
	   (gw:generate-wrapset \"guile-gnome-gw-standard\")"
	mv guile-gnome-gw-standard.scm gw-standard.scm

guile-gnome-gobject.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-gobject-procedures.txt: guile-gnome-gobject.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

#schemelib_DATA = guile-gnome-gobject-procedures.txt

BUILT_SOURCES = \
	guile-gnome-gw-standard.h guile-gnome-gw-standard.c \
	guile-gnome-gw-gobject.h guile-gnome-gw-gobject.c \
	guile-gnome-gw-glib.h guile-gnome-gw-glib.c $(DOT_X_FILES)

# other potentially built files: $(DOT_DOC_FILES)

CLEANFILES = $(BUILT_SOURCES) guile-gnome-gobject.texi		\
        guile-gnome-gw-gobject.html guile-gnome-gw-glib.html	\
	guile-gnome-gw-standard.html guile-gnome-gw-standard.log\
	guile-gnome-gw-gobject.log guile-gnome-gw-glib.log	\
	gw-gobject.scm gw-glib.scm gw-standard.scm

DISTCLEANFILES = $(CLEANFILES) 

EXTRA_DIST = primitives.scm gw-gobject-spec.scm \
	gw-glib-spec.scm gw-spec-utils.scm gw-utils.scm \
	defs-support.scm event-repl.scm generics.scm gw-standard-spec.scm

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done
