################################################################################
## guile-gnome-glib.info

include $(top_srcdir)/generate-docs.mk

# docbook_xml_wildcard = ~/src/gnome2/glib/docs/reference/glib/xml/*.xml
# docbook_xml_files = $(wildcard ~/src/gnome2/glib/docs/reference/glib/xml/*.xml)

# A hand-culled list of modules that we wrap.
docbook_xml = base64.xml \
	bookmarkfile.xml \
	conversions.xml \
	fileutils.xml \
	iochannels.xml \
	main.xml \
	misc_utils.xml \
	quarks.xml \
	shell.xml \
	strings.xml \
	unicode.xml

docbook_xml_base = ~/src/gnome2/glib/docs/reference/glib/xml/
docbook_xml_files = $(addprefix $(docbook_xml_base),$(docbook_xml))
wrapset_stem = glib

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-glib.texi
guile_gnome_glib_TEXINFOS=$(wildcard section-*.texi) $(wildcard defuns-*.texi)
