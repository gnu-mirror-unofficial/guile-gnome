SUBDIRS = gobject gnome defs

if HAVE_GTK
GTKMODULE = gtk.scm
SUBDIRS += gtk
else
GTKMODULE =
endif

if HAVE_CORBA
CORBAMODULE = corba.scm
SUBDIRS += corba
else
CORBAMODULE =
endif

guilemoduledir = $(datadir)/guile/gnome

guilemodule_DATA = gobject.scm glib.scm $(GTKMODULE) $(CORBAMODULE)

EXTRA_DIST = gobject.scm gtk.scm corba.scm glib.scm
