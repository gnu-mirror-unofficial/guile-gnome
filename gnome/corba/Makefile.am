include $(top_srcdir)/common.mk

lib_LTLIBRARIES = libguile-gnome-corba.la

# libguile-gnome-gobject (core library)

C_FILES = guile-gnome-corba.c \
	  guile-gnome-corba-types.c \
	  guile-gnome-corba-primitives.c \
	  guile-gnome-corba-generic.c
H_FILES = $(C_FILES:.c=.h)

libguile_gnome_corba_la_SOURCES = $(C_FILES) $(H_FILES)

libguile_gnome_corba_la_CFLAGS = $(CORBA_CFLAGS)  \
  $(AM_CFLAGS) $(GUILE_GLIB_CFLAGS) $(GUILE_CFLAGS) -DORBIT2_STUBS_API

libguile_gnome_corba_la_LIBADD = $(GUILE_GLIB_LIBS) $(GUILE_LIBS)
libguile_gnome_corba_la_LDFLAGS = -export-dynamic # FIXME: version info

guile_gnome_includedir = $(includedir)/guile-gnome-corba
guile_gnome_include_HEADERS = $(H_FILES)

# include_HEADERS = guile-gnome-gobject.h

# Scheme files

moduledir = $(guilemoduledir)/corba

module_DATA = primitives.scm types.scm

GUILE_SNARF_CFLAGS += $(GUILE_GLIB_CFLAGS) $(CORBA_CFLAGS)

DOT_X_FILES = $(C_FILES:.c=.x)
DOT_DOC_FILES = $(C_FILES:.c=.doc)

guile-gnome-corba.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-corba-procedures.txt: guile-gnome-corba.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

BUILT_SOURCES = $(DOT_X_FILES)

CLEANFILES = $(DOT_X_FILES)

#schemelib_DATA = guile-gnome-gobject-procedures.txt

EXTRA_DIST = $(module_DATA)
