include $(top_srcdir)/common.mk

defsdir = $(guilemoduledir)/defs

defs_DATA = $(wildcard $(srcdir)/*.defs)

EXTRA_DIST = $(defs_DATA)

DEFS_HEADER_DIR = /usr/include

glib:
	../../../h2def.py --c-enums --types \
	   $(DEFS_HEADER_DIR)/glib-2.0/glib/*.h > glib-types.defs
	../../../h2def.py --procs --no-methods \
	   --with-header='(include type-ignores) (include "glib-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/glib-2.0/glib/*.h > glib.defs

gdk-pixbuf:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/gtk-2.0/gdk-pixbuf/gdk-pixbuf*.h > gdk-pixbuf-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "gdk-pixbuf-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/gtk-2.0/gdk-pixbuf/gdk-pixbuf*.h > gdk-pixbuf.defs

gtk:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/gtk-2.0/gtk/gtk*.h > gtk-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "gtk-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/gtk-2.0/gtk/gtk*.h > gtk.defs

gnome-vfs:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/gnome-vfs-2.0/libgnomevfs/*.h > gnome-vfs-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "gnome-vfs-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/gnome-vfs-2.0/libgnomevfs/*.h > gnome-vfs.defs

gconf:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/gconf/2/gconf/*.h > gconf-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "gconf-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/gconf/2/gconf/*.h > gconf.defs

gnome-ui:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/libgnomeui-2.0/libgnomeui/*.h > ui-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "ui-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/libgnomeui-2.0/libgnomeui/*.h > ui.defs
