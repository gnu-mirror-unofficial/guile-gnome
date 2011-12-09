include $(top_srcdir)/common.mk

guilegwmodule_DATA = gnome-vfs-spec.scm gnome-vfs.scm

EXTRA_DIST = gnome-vfs-spec.scm

# There is one shared library for each wrapset. Each is produced by
# g-wrap, but also may contain some hand-coded routines.

guilegnomelib_LTLIBRARIES = libgw-guile-gnome-gnome-vfs.la

########################################################################
## gnome-vfs
nodist_libgw_guile_gnome_gnome_vfs_la_SOURCES = guile-gnome-gw-gnome-vfs.c
libgw_guile_gnome_gnome_vfs_la_SOURCES = \
	gnome-vfs-support.c gnome-vfs-support.h \
	gnome-vfs-port.c gnome-vfs-port.h

libgw_guile_gnome_gnome_vfs_la_CFLAGS = $(GUILE_GLIB_CFLAGS) \
	$(AM_CFLAGS) $(GNOME_VFS_CFLAGS) $(GUILE_CFLAGS) \
	$(G_WRAP_CFLAGS) $(GNOME_VFS_DEPRECATED_CFLAGS)

libgw_guile_gnome_gnome_vfs_la_LIBADD = $(GNOME_VFS_LIBS) $(GUILE_LIBS) \
	$(G_WRAP_LIBS) $(GUILE_GLIB_LIBS)

libgw_guile_gnome_gnome_vfs_la_LDFLAGS = -module

CLEANFILES = $(wildcard gnome-*.log) \
	     $(wildcard guile-gnome-gw-gnome-vfs.*) gnome-vfs.scm

BUILT_SOURCES = guile-gnome-gw-gnome-vfs.c $(DOT_X_FILES)

