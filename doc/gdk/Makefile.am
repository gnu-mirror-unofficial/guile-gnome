################################################################################
## guile-gnome-gdk.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/gtk+/docs/reference/gdk/xml/*.xml \
	~/src/gnome2/gtk+/docs/reference/gdk-pixbuf/xml/*.xml
docbook_xml_ignore_files = \
	%pixbuf/xml/file-saving.xml \
	%pixbuf/xml/gdk-pixbuf-xlib-from-drawables.xml \
	%pixbuf/xml/gdk-pixbuf-xlib-init.xml \
	%pixbuf/xml/gdk-pixbuf-xlib-rendering.xml \
	%pixbuf/xml/gdk-pixbuf-xlib-rgb.xml \
	%pixbuf/xml/initialization_versions.xml \
	%pixbuf/xml/inline.xml \
	%gdk/xml/input.xml \
	%pixbuf/xml/refcounting.xml
docbook_xml_files = $(filter-out $(docbook_xml_ignore_files), $(wildcard $(docbook_xml_wildcard)))

## SIGH, have to munge the parsed entities list
GUILE_FLAGS=-l munge-parsed-ents --debug

wrapset_stem = gdk
GTK_DOC_COVERAGE_MODULES = (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-gdk.texi
guile_gnome_gdk_TEXINFOS=overview.texi $(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
