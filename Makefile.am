include $(top_srcdir)/common.mk

SUBDIRS = gnome examples 

EXTRA_DIST = ChangeLog NEWS AUTHORS README $(wildcard *.ac)

if HAVE_GTK
all-local: $(pcifiles) $(pcufiles)
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(pcifiles)
endif

CLEANFILES = $(pcifiles) $(pcufiles)
