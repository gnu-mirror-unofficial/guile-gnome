AM_CFLAGS = -I. -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/gnome/gobject $(WARN_CFLAGS) -Werror -Wunused

guilecorbamoduledir = $(datadir)/guile/gnome/corba

guilecorbamodule_DATA = primitives.scm types.scm gw-corba.scm gw-corba-spec.scm

EXTRA_DIST = primitives.scm types.scm gw-corba-spec.scm

guile-gnome-gw-corba.c guile-gnome-gw-corba.html: gw-corba-spec.scm
	guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	   (set! %load-path (cons \"${top_srcdir}\" %load-path)) \
	   (use-modules (g-wrap)) \
	   (use-modules (g-wrap guile)) \
	   (use-modules (gnome corba gw-corba-spec)) \
	   (generate-wrapset guile 'gnome-corba \"guile-gnome-gw-corba\")"
	mv guile-gnome-gw-corba.scm gw-corba.scm

DOT_X_FILES = \
	guile-gnome-corba.x			\
	guile-gnome-corba-primitives.x		\
	guile-gnome-corba-types.x

DOT_DOC_FILES = \
	guile-gnome-corba.doc			\
	guile-gnome-corba-primitives.doc	\
	guile-gnome-corba-types.doc

GUILE_SNARF_CFLAGS = $(DEFS) $(AM_CFLAGS) $(GUILE_CFLAGS) $(CORBA_CFLAGS) \
		     $(G_WRAP_CFLAGS) 

SUFFIXES = .x .doc
.c.x:
	guile-snarf $(GUILE_SNARF_CFLAGS) $< > $@ \
	|| { rm $@; false; }
.c.doc:
	-(guile-func-name-check $<)
	(guile-snarf-docs $(GUILE_SNARF_CFLAGS) $< | \
	guile_filter_doc_snarfage --filter-snarfage) > $@ || { rm $@; false; }

guile-gnome-corba.texi: $(DOT_DOC_FILES)
	cat $(DOT_DOC_FILES) | guile-snarf-docs-texi > $@ \
	|| { rm $@; false; }

guile-gnome-corba-procedures.txt: guile-gnome-corba.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

BUILT_SOURCES = \
	guile-gnome-gw-corba.c $(DOT_X_FILES) 
#$(DOT_DOC_FILES)  gnomecorba.html

CLEANFILES = $(BUILT_SOURCES) guile-gnome-gw-corba.log	\
	guile-gnome-gw-corba.html guile-gnome-corba.texi gw-corba.scm

DISTCLEANFILES = $(CLEANFILES)

lib_LTLIBRARIES = libguile-gnome-corba.la libguile-gnome-gw-corba.la

libguile_gnome_corba_la_SOURCES = \
	guile-gnome-corba.h		\
	guile-gnome-corba.c		\
	guile-gnome-corba-primitives.h	\
	guile-gnome-corba-primitives.c	\
	guile-gnome-corba-types.h	\
	guile-gnome-corba-types.c	\
	guile-gnome-corba-generic.h	\
	guile-gnome-corba-generic.c

libguile_gnome_corba_la_CFLAGS = $(AM_CFLAGS) $(CORBA_CFLAGS) -DORBIT2_INTERNAL_API  

libguile_gnome_corba_la_LIBADD = $(CORBA_LIBS) ../gobject/libguile-gnome-gobject.la

libguile_gnome_corba_la_LDFLAGS = -export-dynamic

libguile_gnome_gw_corba_la_SOURCES = guile-gnome-gw-corba.c

libguile_gnome_gw_corba_la_CFLAGS = $(AM_CFLAGS) $(CORBA_CFLAGS) $(G_WRAP_CFLAGS) -DORBIT2_INTERNAL_API  

libguile_gnome_gw_corba_la_LIBADD = $(CORBA_LIBS) $(G_WRAP_LINK_ARGS) libguile-gnome-corba.la

libguile_gnome_gw_corba_la_LDFLAGS = -export-dynamic

dist-hook:
	for file in $(DISTCLEANFILES); do rm -f $(distdir)/$$file; done
