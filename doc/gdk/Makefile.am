################################################################################
## guile-gnome-gdk.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/gtk+/docs/reference/gdk/xml/*.xml
docbook_xml_files = $(wildcard $(docbook_xml_wildcard))

wrapset_stem = gdk

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-gdk.texi
guile_gnome_gdk_TEXINFOS=$(wildcard section-*.texi) $(wildcard defuns-*.texi)
