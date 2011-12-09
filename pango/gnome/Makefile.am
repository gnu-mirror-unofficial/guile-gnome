include $(top_srcdir)/common.mk

SUBDIRS = 

guilemodule_DATA = 

if HAVE_PANGO
guilemodule_DATA += pango.scm
SUBDIRS += gw overrides
endif

if HAVE_PANGOCAIRO
guilemodule_DATA += pangocairo.scm
endif

EXTRA_DIST = pango.scm pangocairo.scm
DIST_SUBDIRS = gw overrides
