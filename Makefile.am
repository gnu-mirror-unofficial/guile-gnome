include $(top_srcdir)/common.mk

SUBDIRS = gnome tools

EXTRA_DIST = ChangeLog.pre-2.16 NEWS README AUTHORS $(wildcard *.ac) \
	guile-gnome-defs.pc.in guile-gnome-defs-uninstalled.pc.in

all-local: $(pcifiles) $(pcufiles)
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(pcifiles)
CLEANFILES = $(pcifiles) $(pcufiles)
