include $(top_srcdir)/common.mk

SUBDIRS = overrides

guilegwmodule_DATA = 

if HAVE_CANVAS
guilegwmodule_DATA += canvas.scm
SUBDIRS += gw
endif

EXTRA_DIST = canvas.scm
DIST_SUBDIRS = gw overrides
