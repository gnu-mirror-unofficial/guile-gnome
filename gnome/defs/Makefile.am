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
	gdk.defs				\
	gdk-types.defs				\
	gdk-pixbuf.defs				\
	gdk-pixbuf-types.defs			\
	glib.defs				\
	glib-types.defs 			\
	gnome.defs				\
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
