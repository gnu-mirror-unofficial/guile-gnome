include $(top_srcdir)/common.mk

SUBDIRS = gnome

EXTRA_DIST = ChangeLog NEWS AUTHORS README $(wildcard *.ac)

if HAVE_LIBGNOMEUI
all-local: $(pcifiles) $(pcufiles)
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(pcifiles)
endif

CLEANFILES = $(pcifiles) $(pcufiles)
