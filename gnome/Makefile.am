SUBDIRS = gobject defs

guilemodule_DATA = gobject.scm glib.scm

if HAVE_GTK
guilemodule_DATA += gtk.scm
SUBDIRS += gtk
endif

if HAVE_LIBGNOME
GNOMEMODULE = gnome.scm
SUBDIRS += gnome
else
GNOMEMODULE =
endif

if HAVE_CORBA
guilemodule_DATA += corba.scm
SUBDIRS += corba
else
CORBAMODULE =
endif

if HAVE_LIBGDA
guilemodule_DATA += gda.scm
SUBDIRS += gda
endif

guilemoduledir = $(datadir)/guile/gnome

EXTRA_DIST = gobject.scm gtk.scm corba.scm glib.scm gda.scm
