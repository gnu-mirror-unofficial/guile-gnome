include $(top_srcdir)/common.mk

SUBDIRS = overrides

guilegwmodule_DATA = 

if HAVE_LIBGNOME
guilegwmodule_DATA += gnome.scm
SUBDIRS += gw
endif

EXTRA_DIST = gnome.scm
DIST_SUBDIRS = gw overrides
