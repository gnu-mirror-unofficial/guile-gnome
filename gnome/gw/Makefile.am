include $(top_srcdir)/common.mk

GUILE_SNARF_CFLAGS += $(GOBJECT_CFLAGS)

package = glib

guilegwmodule_DATA = gobject-spec.scm gobject.scm glib-spec.scm glib.scm 

# There are two shared libraries:
#
# libguile-gnome-gobject, which contains the core gobject support code
# (both types and functions).
#
# This library and its headers can be accessed via pkg-config (the
# .pc.in files).
#
# libgw-guile-gnome-gobject, produced by g-wrap, which supports the
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
libguile_gnome_gobject_la_LDFLAGS = # FIXME: version info

pkginclude_HEADERS = guile-gnome-gobject.h guile-gnome-gobject-primitives.h

# libgw-guile-gnome-gobject (g-wrap support)

libgw_guile_gnome_gobject_la_SOURCES = guile-gnome-gw-gobject.c

noinst_HEADERS = glib-support.h

libgw_guile_gnome_gobject_la_CFLAGS = $(AM_CFLAGS) $(GOBJECT_CFLAGS) \
	$(GUILE_CFLAGS) $(G_WRAP_CFLAGS)

libgw_guile_gnome_gobject_la_LIBADD = $(G_WRAP_LIBS) $(GOBJECT_LIBS) \
	$(GUILE_LIBS) \
	 libguile-gnome-gobject.la

libgw_guile_gnome_gobject_la_LDFLAGS = -module

# libgw-guile-gnome-glib (g-wrap support for glib)

libgw_guile_gnome_glib_la_SOURCES = guile-gnome-gw-glib.c glib-support.c

libgw_guile_gnome_glib_la_CFLAGS = $(GLIB_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_glib_la_LIBADD = $(GLIB_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) libguile-gnome-gobject.la

libgw_guile_gnome_glib_la_LDFLAGS = -module

DOT_X_FILES = \
	guile-gnome-gobject.x			\
	guile-gnome-gobject-primitives.x

DOT_DOC_FILES = \
	guile-gnome-gobject.doc			\
	guile-gnome-gobject-primitives.doc

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
	gobject.scm glib.scm

DISTCLEANFILES = $(CLEANFILES) 

EXTRA_DIST = gobject-spec.scm glib-spec.scm 

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done

# fixme: what is this for?
ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

