include $(top_srcdir)/common.mk

SUBDIRS = overrides

guilemodule_DATA = 

if HAVE_LIBGNOME
guilemodule_DATA += gnome.scm
SUBDIRS += gw
endif

EXTRA_DIST = gnome.scm
DIST_SUBDIRS = gw overrides
