include $(top_srcdir)/common.mk

SUBDIRS = overrides

guilegwmodule_DATA = 

if HAVE_LIBGNOMEUI
guilegwmodule_DATA += gnome-ui.scm
SUBDIRS += gw
endif

EXTRA_DIST = gnome-ui.scm
DIST_SUBDIRS = gw overrides
