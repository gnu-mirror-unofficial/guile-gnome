guilemoduledir = $(datadir)/guile/gnome/defs

guilemodule_DATA = 				\
        applet.defs				\
	art.defs				\
	atk.defs				\
	atk-types.defs				\
	bonobo.defs				\
	bonobo-types.defs			\
	bonoboui.defs				\
	bonoboui-types.defs			\
	common-ignores.defs			\
	gconf.defs				\
	gconf-types.defs			\
	gdk.defs				\
	gdk-types.defs				\
	gdk-pixbuf.defs				\
	gdk-pixbuf-types.defs			\
	glib.defs				\
	glib-types.defs 			\
	gnome.defs				\
	gnome-vfs.defs				\
	gnome-vfs-types.defs			\
	gtk.defs				\
	gtk-extrafuncs.defs			\
	gtk-types.defs				\
	gtkhtml2.defs				\
	gtksourceview.defs			\
	libgda.defs 				\
	libgda-types.defs			\
	libglade.defs				\
	libgnomecanvas.defs			\
	Makefile.am				\
	nautilus.defs				\
	pango.defs				\
	pango-types.defs			\
	print.defs				\
	printui.defs				\
	ui.defs					\
	ui-types.defs				\
	vte.defs				\
	libwnck.defs				\
	libwnck-types.defs

EXTRA_DIST = $(guilemodule_DATA)

gdk-pixbuf:
	../../../h2def.py --types \
	   /usr/include/gtk-2.0/gdk-pixbuf/gdk-pixbuf*.h > gdk-pixbuf-types.defs
	../../../h2def.py --procs \
	   --with-header='(include "gdk-pixbuf-types.defs") (include overrides)' \
	   /usr/include/gtk-2.0/gdk-pixbuf/gdk-pixbuf*.h > gdk-pixbuf.defs

gtk:
	../../../h2def.py --types \
	   /usr/include/gtk-2.0/gtk/gtk*.h > gtk-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "gtk-types.defs") (include overrides)' \
	   /usr/include/gtk-2.0/gtk/gtk*.h > gtk.defs

gnome-vfs:
	../../../h2def.py --types \
	   /usr/include/gnome-vfs-2.0/libgnomevfs/*.h > gnome-vfs-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "gnome-vfs-types.defs") (include overrides)' \
	   /usr/include/gnome-vfs-2.0/libgnomevfs/*.h > gnome-vfs.defs

gconf:
	../../../h2def.py --types \
	   /usr/include/gconf/2/gconf/*.h > gconf-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "gconf-types.defs") (include overrides)' \
	   /usr/include/gconf/2/gconf/*.h > gconf.defs

gnome-ui:
	../../../h2def.py --types \
	   /usr/include/libgnomeui-2.0/libgnomeui/*.h > ui-types.defs
	../../../h2def.py --procs \
	   --with-header='(include type-ignores) (include "ui-types.defs") (include overrides)' \
	   /usr/include/libgnomeui-2.0/libgnomeui/*.h > ui.defs
