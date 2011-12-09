include $(top_srcdir)/common.mk

lib_LTLIBRARIES = libguile-gnome-corba-@API_VERSION@.la

# libguile-gnome-gobject (core library)

C_FILES = guile-gnome-corba.c \
	  guile-gnome-corba-types.c \
	  guile-gnome-corba-primitives.c \
	  guile-gnome-corba-generic.c
H_FILES = $(C_FILES:.c=.h)

libguile_gnome_corba_@API_VERSION@_la_SOURCES = $(C_FILES) $(H_FILES)

libguile_gnome_corba_@API_VERSION@_la_CFLAGS = $(CORBA_CFLAGS)  \
  $(AM_CFLAGS) $(GUILE_GLIB_CFLAGS) $(GUILE_CFLAGS) -DORBIT2_STUBS_API

libguile_gnome_corba_@API_VERSION@_la_LIBADD = $(GUILE_GLIB_LIBS) $(GUILE_LIBS)
libguile_gnome_corba_@API_VERSION@_la_LDFLAGS = -export-dynamic # FIXME: version info

guile_gnome_includedir = $(includedir)/guile-gnome-@API_VERSION@
guile_gnome_subincludedir = $(guile_gnome_includedir)/guile-gnome-corba
guile_gnome_subinclude_HEADERS = $(H_FILES)

# Scheme files

moduledir = $(guilemoduledir)/corba

module_DATA = primitives.scm types.scm

GUILE_SNARF_CFLAGS += $(GUILE_GLIB_CFLAGS) $(CORBA_CFLAGS)

DOT_X_FILES = $(C_FILES:.c=.x)
DOT_DOC_FILES = guile-gnome-corba.doc \
	  guile-gnome-corba-types.doc \
	  guile-gnome-corba-primitives.doc

BUILT_SOURCES = $(DOT_X_FILES) $(DOT_DOC_FILES)

CLEANFILES = $(DOT_X_FILES) $(DOT_DOC_FILES)

#guilegnome_DATA = guile-gnome-corba-procedures.txt

EXTRA_DIST = $(module_DATA)
