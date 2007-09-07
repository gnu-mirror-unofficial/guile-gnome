################################################################################
## guile-gnome-pango.info

include $(top_srcdir)/generate-docs.mk

# FIXME: cairo?

# A hand-culled list of modules that we wrap.
docbook_xml = coverage-maps.xml \
	fonts.xml \
	glyphs.xml \
	layout.xml \
	main.xml \
	pango-renderer.xml \
	scripts.xml \
	tab-stops.xml \
	text-attributes.xml

docbook_xml_base = ~/src/gnome2/pango/docs/xml/
docbook_xml_files = $(addprefix $(docbook_xml_base),$(docbook_xml))

wrapset_stem = pango

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-pango.texi
guile_gnome_pango_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi)
