noinst_LTLIBRARIES = libgw-guile-test-gobject.la libgw-guile-test-glib.la 

libgw_guile_test_gobject_la_SOURCES = test-gobject.c
nodist_libgw_guile_test_gobject_la_SOURCES = gw-test-gobject.c
libgw_guile_test_gobject_la_CFLAGS = \
	-I$(top_srcdir)/gnome/gobject $(GOBJECT_CFLAGS) $(G_WRAP_CFLAGS)
libgw_guile_test_gobject_la_LDFLAGS = -module -rpath $(libdir) 
libgw_guile_test_gobject_la_LIBADD = \
	$(top_builddir)/gnome/gobject/libguile-gnome-gobject.la \
	$(G_WRAP_LIBS) $(GUILE_LIBS)

libgw_guile_test_glib_la_SOURCES = test-glib.c
nodist_libgw_guile_test_glib_la_SOURCES = gw-test-glib.c
libgw_guile_test_glib_la_CFLAGS = \
	-I$(top_srcdir)/gnome/gobject $(GOBJECT_CFLAGS) $(G_WRAP_CFLAGS)
libgw_guile_test_glib_la_LDFLAGS = -module -rpath $(libdir) 
libgw_guile_test_glib_la_LIBADD = \
	$(G_WRAP_LIBS) $(GUILE_LIBS)

noinst_HEADERS = test-gobject.h test-glib.h

SCM_TESTS = gobject-primitives.test gobject.test gw-gobject.test

EXTRA_DIST = lib.scm exceptions.scm gw-test-gobject-spec.scm \
	     gw-test-glib-spec.scm test-gobject.defs test-glib.defs \
	     guile-test-env guile-gobject-test $(SCM_TESTS)

CLEANFILES = $(wildcard gnome-*.log test-*.log) 		\
             $(wildcard gw-test-gobject.* gw-test-glib.*) 	\
	     guile-gobject.log

TESTS = guile-gobject-test

gw-test-gobject.c gw-test-gobject.scm: gw-test-gobject-spec.scm test-gobject.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (set! %load-path (cons \"${srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gw-test-gobject-spec)) \
	   (generate-wrapset guile 'test-gobject \"gw-test-gobject\")"


gw-test-glib.c gw-test-glib.scm: gw-test-glib-spec.scm test-glib.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (set! %load-path (cons \"${srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gw-test-glib-spec)) \
	   (generate-wrapset guile 'test-glib \"gw-test-glib\")"


