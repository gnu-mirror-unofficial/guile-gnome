schemelibdir = $(pkgdatadir)/$(VERSION)

AM_CFLAGS = -I. -I$(srcdir) $(WARN_CFLAGS) $(DEBUG_CFLAGS)

# fixme: what is this for?
ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

guilegobjectmoduledir = $(datadir)/guile/gnome/gobject

guilegobjectmodule_DATA = primitives.scm gw-gobject-spec.scm gw-gobject.scm \
	gw-glib-spec.scm gw-glib.scm gw-spec-utils.scm gw-utils.scm \
	defs-support.scm event-repl.scm generics.scm

# There are two shared libraries:
#
# libguile-gnome-gobject, which contains the core gobject support code
# (both types and functions).
#
# This library and its headers can be accessed via pkg-config (the
# .pc.in files).
#
# libguile-gnome-gw-gobject, produced by g-wrap, which supports the
# g-wrap type wrappings for gobject-related types. This one is not
# used by the scheme<->gobject code; it's only there to support other
# wrapsets.
#
# Also, there's a g-wrap-produced wrapper for glib types and functions,
# libgw-guile-gnome-glib.la.

lib_LTLIBRARIES = libguile-gnome-gobject.la \
	libgw-guile-gnome-gobject.la \
	libgw-guile-gnome-glib.la

# libguile-gnome-gobject (core library)

libguile_gnome_gobject_la_SOURCES = \
	guile-gnome-gobject.c			\
	guile-gnome-gobject.h			\
	guile-gnome-gobject-primitives.c	\
	guile-gnome-gobject-primitives.h	\
	guile-support.c				\
	guile-support.h

libguile_gnome_gobject_la_CFLAGS = \
  $(AM_CFLAGS) $(GOBJECT_CFLAGS) $(GUILE_CFLAGS) $(G_WRAP_CFLAGS) \
  -DGUILE_GOBJECT_DIR=\""$(schemelibdir)"\"

libguile_gnome_gobject_la_LIBADD = $(G_WRAP_LIBS) $(GOBJECT_LIBS) $(GUILE_LIBS)

libguile_gnome_gobject_la_LDFLAGS = \
	-export-dynamic

pkginclude_HEADERS = guile-gnome-gobject.h guile-gnome-gobject-primitives.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = guile-gobject.pc

# libgw-guile-gnome-gobject (g-wrap support)

libgw_guile_gnome_gobject_la_SOURCES = guile-gnome-gw-gobject.c

noinst_HEADERS = glib-support.h

libgw_guile_gnome_gobject_la_CFLAGS = $(AM_CFLAGS) $(GOBJECT_CFLAGS) \
	$(GUILE_CFLAGS) $(G_WRAP_CFLAGS)

libgw_guile_gnome_gobject_la_LIBADD = $(G_WRAP_LIBS) $(GOBJECT_LIBS) \
	$(GUILE_LIBS) \
	 libguile-gnome-gobject.la

libgw_guile_gnome_gobject_la_LDFLAGS = \
	-export-dynamic

# libgw-guile-gnome-glib (g-wrap support for glib)

libgw_guile_gnome_glib_la_SOURCES = guile-gnome-gw-glib.c glib-support.c

libgw_guile_gnome_glib_la_CFLAGS = $(GLIB_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_glib_la_LIBADD = $(GLIB_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) libguile-gnome-gobject.la

libgw_guile_gnome_glib_la_LDFLAGS = \
	-export-dynamic

DOT_X_FILES = \
	guile-gnome-gobject.x			\
	guile-gnome-gobject-primitives.x

DOT_DOC_FILES = \
	guile-gnome-gobject.doc			\
	guile-gnome-gobject-primitives.doc

SUFFIXES = .x .doc

GUILE_SNARF_CFLAGS = $(DEFS) $(AM_CFLAGS) $(GUILE_CFLAGS) $(GOBJECT_CFLAGS) \
		     $(G_WRAP_CFLAGS)

# For overriding from the command line (e.g. --debug)
GUILE_FLAGS = 

.c.x:
	guile-snarf $(GUILE_SNARF_CFLAGS) $< > $@ \
	|| { rm $@; false; }
.c.doc:
	-(guile-func-name-check $<)
	(guile-snarf-docs $(GUILE_SNARF_CFLAGS) $< | \
	guile_filter_doc_snarfage --filter-snarfage) > $@ || { rm $@; false; }

gw-gobject.scm guile-gnome-gw-gobject.c: gw-gobject-spec.scm
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gobject gw-gobject-spec)) \
	   (generate-wrapset guile 'gnome-gobject \"guile-gnome-gw-gobject\")"
	mv guile-gnome-gw-gobject.scm gw-gobject.scm

gw-glib.scm guile-gnome-gw-glib.c: gw-glib-spec.scm
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gobject gw-glib-spec)) \
	   (generate-wrapset guile 'gnome-glib \"guile-gnome-gw-glib\")"
	mv guile-gnome-gw-glib.scm gw-glib.scm

guile-gnome-gobject.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-gobject-procedures.txt: guile-gnome-gobject.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

#schemelib_DATA = guile-gnome-gobject-procedures.txt

BUILT_SOURCES = \
	guile-gnome-gw-gobject.c \
	guile-gnome-gw-glib.c $(DOT_X_FILES)

# other potentially built files: $(DOT_DOC_FILES)

CLEANFILES = $(BUILT_SOURCES) guile-gnome-gobject.texi			\
        $(wildcard gnome-*.log) 					\
        $(wildcard guile-gnome-gw-gobject.* guile-gnome-gw-glib.*) 	\
	gw-gobject.scm gw-glib.scm

DISTCLEANFILES = $(CLEANFILES) 

EXTRA_DIST = primitives.scm gw-gobject-spec.scm \
	gw-glib-spec.scm gw-spec-utils.scm gw-utils.scm \
	defs-support.scm event-repl.scm generics.scm \
	guile-gobject.pc.in guile-gobject-uninstalled.pc.in

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done
