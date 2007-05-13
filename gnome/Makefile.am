include $(top_srcdir)/common.mk

SUBDIRS = 

guilemodule_DATA = 

if HAVE_PANGO
guilemodule_DATA += pango.scm
SUBDIRS += gw overrides
endif

EXTRA_DIST = pango.scm
DIST_SUBDIRS = gw overrides
