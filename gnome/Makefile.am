include $(top_srcdir)/common.mk

SUBDIRS = gtk contrib overrides

DIST_SUBDIRS = gtk contrib gw overrides

guilemodule_DATA = 

if HAVE_GTK
guilemodule_DATA += gtk.scm
SUBDIRS += gw
endif

EXTRA_DIST = gtk.scm
