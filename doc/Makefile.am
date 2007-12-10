################################################################################
## guile-gnome-libgnome.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/libgnome/doc/reference/xml/*.xml
docbook_xml_ignore_files = \
	%gnome-config.xml
docbook_xml_files = $(filter-out $(docbook_xml_ignore_files), $(wildcard $(docbook_xml_wildcard)))

wrapset_stem = libgnome
GTK_DOC_COVERAGE_MODULES = (gnome gnome) (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-libgnome.texi
guile_gnome_libgnome_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
