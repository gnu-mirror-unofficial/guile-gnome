include $(top_srcdir)/common.mk

SUBDIRS = overrides
guilemodule_DATA =

if HAVE_TEMPLATE
SUBDIRS += gw
guilemodule_DATA += template.scm
endif

EXTRA_DIST = template.scm
DIST_SUBDIRS = gw overrides
