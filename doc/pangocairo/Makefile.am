################################################################################
## guile-gnome-pangocairo.info

include $(top_srcdir)/generate-docs.mk

# FIXME: cairo?

# A hand-culled list of modules that we wrap.
docbook_xml = pangocairo.xml

docbook_xml_base = ~/src/gnome2/pango/docs/xml/
docbook_xml_files = $(addprefix $(docbook_xml_base),$(docbook_xml))

wrapset_stem = pangocairo
GTK_DOC_COVERAGE_MODULES = (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-pangocairo.texi
guile_gnome_pangocairo_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
