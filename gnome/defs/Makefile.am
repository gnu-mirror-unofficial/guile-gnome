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

atk:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/atk-1.0/atk/*.h > atk-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "common-ignores.defs") (include "atk-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/atk-1.0/atk/*.h > atk.defs

PANGO_INCLS = pango/pango.h \
	   pango/pango-attributes.h \
	   pango/pango-break.h \
	   pango/pango-context.h \
	   pango/pango-coverage.h \
	   pango/pango-enum-types.h \
	   pango/pango-font.h \
	   pango/pango-fontmap.h \
	   pango/pango-glyph.h \
	   pango/pango-item.h \
	   pango/pango-layout.h \
	   pango/pango-renderer.h \
	   pango/pango-script.h \
	   pango/pango-types.h

pango:
	../../../h2def.py --types \
	   $(addprefix $(DEFS_HEADER_DIR)/pango-1.0/,$(PANGO_INCLS)) \
	   > pango-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "common-ignores.defs") (include "pango-types.defs") (include overrides)' \
	   $(addprefix $(DEFS_HEADER_DIR)/pango-1.0/,$(PANGO_INCLS)) \
	   > pango.defs

pangocairo:
	../../../h2def.py --types \
	   $(addprefix $(DEFS_HEADER_DIR)/pango-1.0/,pango/pangocairo.h) \
	   > pangocairo-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "common-ignores.defs") (include "pangocairo-types.defs") (include overrides)' \
	   $(addprefix $(DEFS_HEADER_DIR)/pango-1.0/,pango/pangocairo.h) \
	   > pangocairo.defs

gdk-pixbuf:
	../../../h2def.py --types \
	   $(shell ls $(DEFS_HEADER_DIR)/gtk-2.0/gdk-pixbuf/gdk-pixbuf*.h | grep -v io.h) > gdk-pixbuf-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "gdk-pixbuf-types.defs") (include overrides)' \
	   $(shell ls $(DEFS_HEADER_DIR)/gtk-2.0/gdk-pixbuf/gdk-pixbuf*.h | grep -v io.h) > gdk-pixbuf.defs

gdk:
	../../../h2def.py --types \
	   $(shell ls $(DEFS_HEADER_DIR)/gtk-2.0/gdk/gdk*.h | grep -v alias) > gdk-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "common-ignores.defs") (include "gdk-types.defs") (include overrides)' \
	   $(shell ls $(DEFS_HEADER_DIR)/gtk-2.0/gdk/gdk*.h | grep -v alias) > gdk.defs

gtk:
	../../../h2def.py --types \
	   $(shell ls $(DEFS_HEADER_DIR)/gtk-2.0/gtk/gtk*.h | grep -v filesystem) > gtk-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "gtk-types.defs") (include overrides)' \
	   $(shell ls $(DEFS_HEADER_DIR)/gtk-2.0/gtk/gtk*.h | grep -v filesystem) > gtk.defs

LIBGLADE_INCLS = glade/glade-xml.h

libglade:
	../../../h2def.py --types \
	   $(addprefix $(DEFS_HEADER_DIR)/libglade-2.0/,$(LIBGLADE_INCLS)) \
	   > libglade-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "libglade-types.defs") (include overrides)' \
	   $(addprefix $(DEFS_HEADER_DIR)/libglade-2.0/,$(LIBGLADE_INCLS)) \
	   > libglade.defs

libgnome:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/libgnome-2.0/libgnome/*.h > libgnome-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "libgnome-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/libgnome-2.0/libgnome/*.h > libgnome.defs

libgnomeui:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/libgnomeui-2.0/libgnomeui/*.h > libgnomeui-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "libgnomeui-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/libgnomeui-2.0/libgnomeui/*.h > libgnomeui.defs

libgnomecanvas:
	../../../h2def.py --types \
	   $(DEFS_HEADER_DIR)/libgnomecanvas-2.0/libgnomecanvas/*.h > libgnomecanvas-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "libgnomecanvas-types.defs") (include overrides)' \
	   $(DEFS_HEADER_DIR)/libgnomecanvas-2.0/libgnomecanvas/*.h > libgnomecanvas.defs

gnome-vfs:
	../../../h2def.py --types --type-re='s/^GNOME_VFS_?/GNOME_VFS_TYPE_VFS_/' \
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

