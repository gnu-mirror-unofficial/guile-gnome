include $(top_srcdir)/common.mk

SUBDIRS = gnome

EXTRA_DIST = ChangeLog NEWS AUTHORS README $(wildcard *.ac)

all-local: $(pcifiles) $(pcufiles)
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(pcifiles)
CLEANFILES = $(pcifiles) $(pcufiles)
