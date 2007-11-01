################################################################################
## guile-gnome-libgnomecanvas.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/libgnomecanvas/docs/reference/xml/*.xml
docbook_xml_files = $(wildcard $(docbook_xml_wildcard))

wrapset_stem = canvas

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-libgnomecanvas.texi
guile_gnome_libgnomecanvas_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi)
