################################################################################
## guile-gnome-libglade.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/libglade/doc/xml/glade-xml.xml
docbook_xml_files = $(wildcard $(docbook_xml_wildcard))

wrapset_stem = libglade
GTK_DOC_COVERAGE_MODULES = (gnome glade) (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-libglade.texi
guile_gnome_libglade_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
