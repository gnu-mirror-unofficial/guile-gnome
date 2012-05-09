include $(top_srcdir)/common.mk

SUBDIRS = gnome doc tests

EXTRA_DIST = ChangeLog.pre-2.16 AUTHORS README

if HAVE_LIBGNOMEUI
all-local: $(pcifiles) $(pcufiles)
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(pcifiles)
endif

CLEANFILES = $(pcifiles) $(pcufiles)
