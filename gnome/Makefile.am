include $(top_srcdir)/common.mk

SUBDIRS = 
guilemodule_DATA =

if HAVE_CAIRO
SUBDIRS += gw
guilemodule_DATA += cairo.scm
endif

EXTRA_DIST = cairo.scm
DIST_SUBDIRS = gw
