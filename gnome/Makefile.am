include $(top_srcdir)/common.mk

SUBDIRS = gtk

guilemodule_DATA = 

if HAVE_GTK
guilemodule_DATA += gtk.scm
SUBDIRS += gw
endif

EXTRA_DIST = gtk.scm
