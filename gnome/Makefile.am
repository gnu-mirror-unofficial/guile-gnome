include $(top_srcdir)/common.mk

SUBDIRS = overrides
guilemodule_DATA =

if HAVE_LIBGLADE
SUBDIRS += gw
guilemodule_DATA += glade.scm
endif

EXTRA_DIST = glade.scm
DIST_SUBDIRS = gw overrides
