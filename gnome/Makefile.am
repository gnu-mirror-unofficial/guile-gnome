SUBDIRS = gobject defs
DIST_SUBDIRS = gobject defs gtk gnome corba

if HAVE_GTK
GTKMODULE = gtk.scm
SUBDIRS += gtk
else
GTKMODULE =
endif

if HAVE_LIBGNOME
GNOMEMODULE = gnome.scm
SUBDIRS += gnome
else
GNOMEMODULE =
endif

if HAVE_CORBA
CORBAMODULE = corba.scm
SUBDIRS += corba
else
CORBAMODULE =
endif

guilemoduledir = $(datadir)/guile/gnome

guilemodule_DATA = gobject.scm glib.scm $(GTKMODULE) $(GNOMEMODULE) $(CORBAMODULE)

EXTRA_DIST = gobject.scm glib.scm gtk.scm gnome.scm corba.scm
