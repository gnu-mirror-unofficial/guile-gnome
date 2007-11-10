include $(top_srcdir)/common.mk

guilegwmodule_DATA = 
EXTRA_DIST = 
CLEANFILES = $(wildcard gnome-*.log)
guilegnomelib_LTLIBRARIES = 
BUILT_SOURCES =

########################################################################
## pango
guilegwmodule_DATA += pango-spec.scm pango.scm

EXTRA_DIST += pango-spec.scm

guilegnomelib_LTLIBRARIES += libgw-guile-gnome-pango.la

nodist_libgw_guile_gnome_pango_la_SOURCES = guile-gnome-gw-pango.c 

libgw_guile_gnome_pango_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(PANGO_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libgw_guile_gnome_pango_la_LIBADD = $(PANGO_LIBS) $(GUILE_GLIB_LIBS) \
	$(GUILE_LIBS) $(G_WRAP_LIBS) 

libgw_guile_gnome_pango_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-pango.*) pango.scm

BUILT_SOURCES += guile-gnome-gw-pango.c

########################################################################
## pangocairo

if HAVE_CAIRO
if HAVE_PANGOCAIRO

guilegwmodule_DATA += pangocairo-spec.scm pangocairo.scm

EXTRA_DIST += pangocairo-spec.scm

guilegnomelib_LTLIBRARIES += libgw-guile-gnome-pangocairo.la

nodist_libgw_guile_gnome_pangocairo_la_SOURCES = guile-gnome-gw-pangocairo.c 

libgw_guile_gnome_pangocairo_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(PANGO_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) $(GUILE_CAIRO_CFLAGS) $(PANGOCAIRO_CFLAGS)

libgw_guile_gnome_pangocairo_la_LIBADD = $(PANGO_LIBS) $(GUILE_GLIB_LIBS) \
	$(GUILE_LIBS) $(G_WRAP_LIBS)  $(GUILE_CAIRO_LIBS) $(PANGOCAIRO_LIBS)

libgw_guile_gnome_pangocairo_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-pangocairo.*) pangocairo.scm

BUILT_SOURCES += guile-gnome-gw-pangocairo.c

endif # HAVE_PANGOCAIRO
endif # HAVE_CAIRO
