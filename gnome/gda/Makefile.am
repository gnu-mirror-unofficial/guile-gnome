AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

GUILE_SNARF_CFLAGS = $(DEFS) $(AM_CFLAGS)  -I$(top_srcdir)/gnome/gobject \
		     $(GUILE_CFLAGS) $(LIBGDA_CFLAGS) $(G_WRAP_CFLAGS) 

# For overriding from the command line (e.g. --debug)
GUILE_FLAGS = 

%.x: %.c
	guile-snarf $(GUILE_SNARF_CFLAGS) $< > $@ \
	|| { rm $@; false; }
%.doc: %.c
	-(guile-func-name-check $<)
	(guile-snarf-docs $(GUILE_SNARF_CFLAGS) $< | \
	guile_filter_doc_snarfage --filter-snarfage) > $@ || { rm $@; false; }

DOT_X_FILES = gda-support.x

DOT_DOC_FILES = gda-support.doc

SUFFIXES = .x .doc

GDAMODULES = gw-gda-spec.scm gw-gda.scm

guilegnomemoduledir = $(datadir)/guile/gnome/gda

guilegnomemodule_DATA = $(GDAMODULES)

EXTRA_DIST = gw-gda-spec.scm gw-gda.scm

CLEANFILES = $(DOT_X_FILES)

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

lib_LTLIBRARIES = libguile-gnome-gw-gda.la

########################################################################
## gda
libguile_gnome_gw_gda_la_SOURCES = \
	gda-support.c gda-support.h \
	guile-gnome-gw-gda.c

libguile_gnome_gw_gda_la_CFLAGS = -I$(top_srcdir)/gnome/gobject \
	$(AM_CFLAGS) $(LIBGDA_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS)

libguile_gnome_gw_gda_la_LIBADD = $(LIBGDA_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LINK_ARGS) ../gobject/libgw-guile-gnome-gobject.la

libguile_gnome_gw_gda_la_LDFLAGS = -module

CLEANFILES += $(wildcard guile-gnome-gw-gda.*) gw-gda.scm

gw-gda.scm guile-gnome-gw-gda.c guile-gnome-gw-gda.h: gw-gda-spec.scm ../defs/gda.defs
	guile $(GUILE_FLAGS) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome gda gw-gda-spec)) \
	   (generate-wrapset guile 'gnome-gda \"guile-gnome-gw-gda\")"
	mv guile-gnome-gw-gda.scm gw-gda.scm

BUILT_SOURCES = guile-gnome-gw-gda.c $(DOT_X_FILES)

