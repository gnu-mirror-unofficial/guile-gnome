include $(top_srcdir)/common.mk

SUBDIRS = overrides
guilemodule_DATA =

if HAVE_GCONF
SUBDIRS += gw
guilemodule_DATA += gconf.scm
endif

EXTRA_DIST = gconf.scm
DIST_SUBDIRS = gw overrides
