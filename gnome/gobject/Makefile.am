include $(top_srcdir)/common.mk

lib_LTLIBRARIES = libguile-gnome-gobject.la

# libguile-gnome-gobject (core library)

C_FILES = gobject.c gsignal.c gclosure.c gvalue.c gparameter.c gtype.c
H_FILES = $(C_FILES:.c=.h)

libguile_gnome_gobject_la_SOURCES = \
       $(C_FILES) $(H_FILES) guile-support.c guile-support.h

libguile_gnome_gobject_la_CFLAGS = \
  $(AM_CFLAGS) $(GOBJECT_CFLAGS) $(GUILE_CFLAGS) \
  -DGUILE_GOBJECT_DIR=\""$(schemelibdir)"\"

libguile_gnome_gobject_la_LIBADD = $(GOBJECT_LIBS) $(GUILE_LIBS)
libguile_gnome_gobject_la_LDFLAGS = # FIXME: version info

guile_gnome_includedir = $(includedir)/guile-gnome-2.6
guile_gnome_include_HEADERS = guile-gnome-gobject.h

guile_gnome_subincludedir = $(guile_gnome_includedir)/guile-gnome-gobject
guile_gnome_subinclude_HEADERS = $(H_FILES)

# Scheme files

moduledir = $(guilemoduledir)/gobject

module_DATA = defs-support.scm event-repl.scm generics.scm \
	      gw-spec-utils.scm gw-utils.scm \
              gtype.scm gparameter.scm gobject.scm gsignal.scm \
	      gvalue.scm gclosure.scm \
	      utils.scm

GUILE_SNARF_CFLAGS += $(GOBJECT_CFLAGS)

DOT_X_FILES = $(C_FILES:.c=.x)
DOT_DOC_FILES = $(C_FILES:.c=.doc)

guile-gnome-gobject.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-gobject-procedures.txt: guile-gnome-gobject.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

BUILT_SOURCES = $(DOT_X_FILES)

CLEANFILES = $(DOT_X_FILES)

#schemelib_DATA = guile-gnome-gobject-procedures.txt

EXTRA_DIST = $(module_DATA) private.h
