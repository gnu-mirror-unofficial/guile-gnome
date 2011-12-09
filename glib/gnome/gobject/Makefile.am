include $(top_srcdir)/common.mk

lib_LTLIBRARIES = libguile-gnome-gobject-@API_VERSION@.la

# libguile-gnome-gobject (core library)

C_FILES = gc.c gutil.c gtype.c gparameter.c gvalue.c gclosure.c \
	gsignal.c gobject.c
H_FILES = $(C_FILES:.c=.h)

libguile_gnome_gobject_@API_VERSION@_la_SOURCES = \
       $(C_FILES) $(H_FILES) guile-support.c guile-support.h

libguile_gnome_gobject_@API_VERSION@_la_CFLAGS = \
  $(AM_CFLAGS) $(GOBJECT_CFLAGS) $(GTHREAD_CFLAGS) $(GUILE_CFLAGS) \
  -DGUILE_GOBJECT_DIR=\""$(guilegnomedir)"\"

libguile_gnome_gobject_@API_VERSION@_la_LIBADD = $(GOBJECT_LIBS) $(GTHREAD_LIBS) $(GUILE_LIBS)
libguile_gnome_gobject_@API_VERSION@_la_LDFLAGS = # FIXME: version info

guile_gnome_includedir = $(includedir)/guile-gnome-@API_VERSION@
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
DOT_DOC_FILES = gobject.doc gsignal.doc gclosure.doc gvalue.doc	\
	gtype.doc

guile-gnome-gobject.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-gobject-procedures.txt: guile-gnome-gobject.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

BUILT_SOURCES = $(DOT_X_FILES) $(DOT_DOC_FILES)

CLEANFILES = $(DOT_X_FILES) $(DOT_DOC_FILES)

real_builddir=`cd . && pwd`

all-local: config.scm

config.scm: Makefile config.scm.in
	sed -e "s|@guilegnomegobjectlibpath\@|$(real_builddir)/libguile-gnome-gobject-@API_VERSION@|" \
	    $(srcdir)/config.scm.in > config.scm

install-data-local: Makefile config.scm.in
	$(mkinstalldirs) $(DESTDIR)$(moduledir)
	sed -e "s|@guilegnomegobjectlibpath\@|$(libdir)/libguile-gnome-gobject-@API_VERSION@|" \
	    $(srcdir)/config.scm.in > $(DESTDIR)$(moduledir)/config.scm
	chmod 644 $(DESTDIR)$(moduledir)/config.scm

uninstall-local:
	rm -f $(DESTDIR)$(moduledir)/config.scm

CLEANFILES += config.scm

#guilegnome_DATA = guile-gnome-gobject-procedures.txt

EXTRA_DIST = config.scm.in $(module_DATA) private.h
