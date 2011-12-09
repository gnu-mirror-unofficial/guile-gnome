include $(top_srcdir)/common.mk

SUBDIRS = overrides

guilemodule_DATA = 

if HAVE_LIBGNOMEUI
guilemodule_DATA += gnome-ui.scm
SUBDIRS += gw
endif

EXTRA_DIST = gnome-ui.scm
DIST_SUBDIRS = gw overrides
