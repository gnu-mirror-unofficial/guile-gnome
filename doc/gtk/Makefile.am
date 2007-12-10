################################################################################
## guile-gnome-gtk.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/gtk+/docs/reference/gtk/xml/*.xml
docbook_xml_except = 
docbook_xml_files = $(filter-out $(docbook_xml_except),$(wildcard $(docbook_xml_wildcard)))

wrapset_stem = gtk

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-gtk.texi
guile_gnome_gtk_TEXINFOS=overview.texi $(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
