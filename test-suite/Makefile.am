noinst_LTLIBRARIES = libgw-test-gobject.la

libgw_test_gobject_la_SOURCES = test-gobject.c
nodist_libgw_test_gobject_la_SOURCES = gw-test-gobject.c
libgw_test_gobject_la_CFLAGS = \
	-I$(top_srcdir)/gnome/gobject $(GOBJECT_CFLAGS) $(G_WRAP_CFLAGS)
libgw_test_gobject_la_LDFLAGS = -export-dynamic -rpath $(libdir) 
libgw_test_gobject_la_LIBADD = \
	$(top_builddir)/gnome/gobject/libguile-gnome-gw-gobject.la \
	$(G_WRAP_LIBS) $(GUILE_LIBS)

noinst_HEADERS = test-gobject.h

SCM_TESTS = gobject.test gw-gobject.test

EXTRA_DIST = lib.scm exceptions.scm gw-test-gobject-spec.scm \
	     test-gobject.defs guile-test-env guile-gobject-test $(SCM_TESTS)

CLEANFILES = guile-gnome-gw-glib.log gw-test-gobject.log gw-test-gobject.h \
	     gw-test-gobject.c gw-test-gobject.html gw-test-gobject.scm \
	     guile-gobject.log

TESTS = guile-gobject-test

gw-test-gobject.c gw-test-gobject.h gw-test-gobject.scm: gw-test-gobject-spec.scm test-gobject.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (primitive-load \"$(srcdir)/gw-test-gobject-spec.scm\") \
	   (gw:generate-wrapset \"gw-test-gobject\")"


