include $(top_srcdir)/common.mk

guilegwmodule_DATA = template-spec.scm template.scm

EXTRA_DIST = template-spec.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-template.la

########################################################################
## template
nodist_libgw_guile_gnome_template_la_SOURCES = guile-gnome-gw-template.c
libgw_guile_gnome_template_la_SOURCES = template-support.c template-support.h

libgw_guile_gnome_template_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(TEMPLATE_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_template_la_LIBADD = $(TEMPLATE_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_template_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-template.*) template.scm

BUILT_SOURCES = guile-gnome-gw-template.c $(DOT_X_FILES)

