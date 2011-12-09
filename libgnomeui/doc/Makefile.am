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
	%gnome-popup-menu.xml \
	%gnome-about.xml \
	%gnome-appbar.xml \
	%gnome-app-helper.xml \
	%gnome-app.xml \
	%gnome-druid-page-edge.xml \
	%gnome-druid-page-standard.xml \
	%gnome-druid-page.xml \
	%gnome-druid.xml \
	%gnome-file-entry.xml \
	%gnome-href.xml \
	%gnome-ice.xml \
	%gnome-icon-entry.xml \
	%gnome-icon-item.xml \
	%gnome-icon-list.xml \
	%gnome-icon-lookup.xml \
	%gnome-icon-sel.xml \
	%gnome-scores.xml \
	%gnome-stock-icons.xml \
	%gnome-thumbnail.xml \
	%gnometypebuiltins.xml \
	%gnome-types.xml \
	%gnome-uidefs.xml \
	%gnome-ui-init.xml \
	%gnome-window-icon.xml \
	%gnome-window.xml
docbook_xml_files = $(filter-out $(docbook_xml_ignore_files), $(wildcard $(docbook_xml_wildcard)))

wrapset_stem = libgnomeui

GTK_DOC_COVERAGE_MODULES = (gnome gnome-ui) (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-libgnomeui.texi
guile_gnome_libgnomeui_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
