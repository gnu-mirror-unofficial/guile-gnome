include $(top_srcdir)/common.mk

noinst_LTLIBRARIES = libgw-guile-test-gobject.la libgw-guile-test-glib.la 

std_cflags = $(GUILE_GLIB_CFLAGS) $(GOBJECT_CFLAGS) $(G_WRAP_CFLAGS)

libgw_guile_test_gobject_la_SOURCES = test-gobject.c
nodist_libgw_guile_test_gobject_la_SOURCES = gw-test-gobject.c
libgw_guile_test_gobject_la_CFLAGS = $(std_cflags)
libgw_guile_test_gobject_la_LDFLAGS = -module -rpath $(libdir) 
libgw_guile_test_gobject_la_LIBADD = \
	$(GUILE_GLIB_LIBS) $(G_WRAP_LIBS) $(GUILE_LIBS)

libgw_guile_test_glib_la_SOURCES = test-glib.c
nodist_libgw_guile_test_glib_la_SOURCES = gw-test-glib.c
libgw_guile_test_glib_la_CFLAGS = $(std_cflags)
libgw_guile_test_glib_la_LDFLAGS = -module -rpath $(libdir) 
libgw_guile_test_glib_la_LIBADD = \
	$(GUILE_GLIB_LIBS) $(G_WRAP_LIBS) $(GUILE_LIBS)

noinst_HEADERS = test-gobject.h test-glib.h

SCM_TESTS = gobject.test gw-gobject.test

EXTRA_DIST = lib.scm exceptions.scm gw-test-gobject-spec.scm \
	     gw-test-glib-spec.scm test-gobject.defs test-glib.defs \
	     guile-test-env guile-gobject-test $(SCM_TESTS)

CLEANFILES = $(wildcard gnome-*.log test-*.log) 		\
             $(wildcard gw-test-gobject.* gw-test-glib.*) 	\
	     guile-gobject.log

wrapset_stem = glib

top_module_name = (gnome $(wrapset_stem))
gw_module_name = (gnome gw $(wrapset_stem))
extra_module_names =
wrapset_modules = ($(top_module_name) $(gw_module_name) $(extra_module_names))
TESTS_ENVIRONMENT=\
	API_FILE=$(srcdir)/gobject.api \
	DOC_SCM=$(srcdir)/../doc/gobject/guile-gnome-gobject.scm \
	WRAPSET_MODULES="$(wrapset_modules)" \
	WRAPSET_API_FILE=$(srcdir)/wrapset.api \
	LTDL_LIBRARY_PATH=.:${LTDL_LIBRARY_PATH} \
	GUILE_LOAD_PATH=".:${srcdir:-.}/..:..:${GUILE_LOAD_PATH}" \
	$(top_builddir)/dev-environ guile --debug -e main -s

script := '(load (getenv "DOC_SCM"))\
(for-each \
(lambda (mod) \
(display " \"") (write (car mod)) (display "\" ")) \
 *modules*)'

gobject.api.update:
	$(top_builddir)/dev-environ guile --debug -e update-api -s $(srcdir)/api.scm \
	  $(shell DOC_SCM=$(srcdir)/../doc/gobject/guile-gnome-gobject.scm \
	     guile -c $(script)) > $(srcdir)/gobject.api

wrapset.api.update:
	WRAPSET_MODULES="$(wrapset_modules)" \
	WRAPSET_API_FILE=$(srcdir)/wrapset.api \
	$(top_builddir)/dev-environ guile --debug -e update-api -s $(srcdir)/wrapset.scm

%.check: %
	$(TESTS_ENVIRONMENT) $(srcdir)/$*

EXTRA_DIST+=gobject.api api.scm wrapset.api wrapset.scm

TESTS = guile-gobject-test \
	api.scm \
	wrapset.scm

gw-test-%.scm gw-test-%.c: gw-test-%-spec.scm
	echo GLP=$(GUILE_LOAD_PATH)
	guile $(GUILE_FLAGS) -c \
	  "(use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gw-test-$*-spec)) \
	   (generate-wrapset 'guile 'test-$* \"gw-test-$*\")"
