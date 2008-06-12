include $(top_srcdir)/common.mk

SUBDIRS = gnome examples  doc tests

EXTRA_DIST = ChangeLog.pre-2.16 NEWS AUTHORS README $(wildcard *.ac)

if HAVE_GTK
all-local: $(pcifiles) $(pcufiles)
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(pcifiles)
endif

CLEANFILES = $(pcifiles) $(pcufiles)
