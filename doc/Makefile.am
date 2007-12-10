################################################################################
## guile-gnome-libgnomeui.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/libgnomeui/doc/reference/xml/*.xml
docbook_xml_ignore_files = \
	%gnome-app-util.xml \
	%gnome-color-picker.xml \
	%gnome-mdi.xml \
	%gnome-mdi-child.xml \
	%gnome-mdi-generic-child.xml \
	%gnome-mdi-session.xml \
	%gnome-dialog.xml \
	%gnome-dialog-util.xml \
	%gnome-messagebox.xml \
	%gnome-propertybox.xml \
	%gnome-entry.xml \
	%gnome-font-picker.xml \
	%gnome-icon-theme.xml \
	%gnome-pixmap.xml \
	%gnome-pixmap-entry.xml \
	%gnome-theme-parse.xml \
	%gnome-popup-menu.xml
docbook_xml_files = $(filter-out $(docbook_xml_ignore_files), $(wildcard $(docbook_xml_wildcard)))

wrapset_stem = libgnomeui

GTK_DOC_COVERAGE_MODULES = (gnome gnome-ui) (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-libgnomeui.texi
guile_gnome_libgnomeui_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
