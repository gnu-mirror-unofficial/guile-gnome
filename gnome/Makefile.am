if HAVE_GTK
GTKDIR=gtk
GTKMODULE=gtk.scm
else
GTKDIR=
GTKMODULE=
endif

if HAVE_CORBA
CORBADIR=corba
CORBAMODULE=corba.scm
else
CORBADIR=
CORBAMODULE=
endif

SUBDIRS=gobject $(GTKDIR) $(CORBADIR) defs
DIST_SUBDIRS=gobject gtk corba defs

guilemoduledir = $(datadir)/guile/gnome

guilemodule_DATA = gobject.scm glib.scm $(GTKMODULE) $(CORBAMODULE)

EXTRA_DIST = gobject.scm gtk.scm corba.scm glib.scm
