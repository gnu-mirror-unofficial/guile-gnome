include $(top_srcdir)/common.mk

SUBDIRS = gnome test-suite examples bin

EXTRA_DIST = guile-gnome-glib.pc.in guile-gnome-glib-uninstalled.pc.in \
	AUTHORS NEWS TODO WARTS REFCOUNTING THREADING NEWS.guile-gobject \
	README $(wildcard *.ac)

all-local: $(pcifiles) $(pcufiles)
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(pcifiles)
CLEANFILES = $(pcifiles) $(pcufiles)
