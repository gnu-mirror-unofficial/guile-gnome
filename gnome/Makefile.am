include $(top_srcdir)/common.mk

SUBDIRS = 

guilemodule_DATA = 

if HAVE_PANGO
guilemodule_DATA += atk.scm
SUBDIRS += gw
endif

EXTRA_DIST = atk.scm
DIST_SUBDIRS = gw overrides
