include $(top_srcdir)/common.mk

SUBDIRS = overrides
guilemodule_DATA =

if HAVE_GNOME_VFS
SUBDIRS += gw
guilemodule_DATA += gnome-vfs.scm
endif

EXTRA_DIST = gnome-vfs.scm
DIST_SUBDIRS = gw overrides
