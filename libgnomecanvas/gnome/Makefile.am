include $(top_srcdir)/common.mk

SUBDIRS = overrides

guilemodule_DATA = 

if HAVE_CANVAS
guilemodule_DATA += canvas.scm
SUBDIRS += gw
endif

EXTRA_DIST = canvas.scm
DIST_SUBDIRS = gw overrides
