if HAVE_CORBA
SUBDIRS = gnome demos examples
else
SUBDIRS = gnome examples
endif

DIST_SUBDIRS = gnome demos examples

EXTRA_DIST = RELEASE-NOTES-0.2.0.txt RELEASE-NOTES-0.5.0.txt dev-environ \
	h2def.py guile-gobject.pc.in \
	guile-gobject-uninstalled.pc.in

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = guile-gobject.pc

DISTCLEANFILES = stamp-h1
