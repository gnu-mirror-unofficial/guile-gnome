include $(top_srcdir)/common.mk

guilegwmodule_DATA = corba-spec.scm corba.scm

EXTRA_DIST = corba-spec.scm

CLEANFILES = $(wildcard gnome-*.log)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libgw-guile-gnome-corba.la

########################################################################
## corba
nodist_libgw_guile_gnome_corba_la_SOURCES = guile-gnome-gw-corba.c

libgw_guile_gnome_corba_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(CORBA_CFLAGS) $(GUILE_CFLAGS) -DORBIT2_STUBS_API \
	$(G_WRAP_CFLAGS) -I$(top_srcdir)/corba/gnome/corba
libgw_guile_gnome_corba_la_LIBADD = $(CORBA_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS) ../corba/libguile-gnome-corba.la
libgw_guile_gnome_corba_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-corba.*) corba.scm

BUILT_SOURCES = guile-gnome-gw-corba.c
