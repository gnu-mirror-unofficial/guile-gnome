################################################################################
## guile-gnome-atk.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/atk/docs/xml/*.xml
docbook_xml_files = $(wildcard $(docbook_xml_wildcard))

wrapset_stem = atk

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-atk.texi
guile_gnome_atk_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
