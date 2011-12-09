include $(top_srcdir)/common.mk

SUBDIRS = overrides
guilemodule_DATA =

if HAVE_GNOME_VFS
SUBDIRS += gw
guilemodule_DATA += vfs.scm
endif

EXTRA_DIST = vfs.scm
DIST_SUBDIRS = gw overrides
