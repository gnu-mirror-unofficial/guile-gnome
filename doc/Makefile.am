################################################################################
## guile-gnome-gconf.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/gconf/doc/gconf/xml/*.xml
docbook_xml_ignore_files = \
	%gconf-backend.xml \
	%gconf-locale.xml \
	%gconf-sources.xml \
	%gconf-listeners.xml \
	%gconf-internals.xml
docbook_xml_files = $(filter-out $(docbook_xml_ignore_files), $(wildcard $(docbook_xml_wildcard)))

wrapset_stem = gconf

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-gconf.texi
guile_gnome_gconf_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi)
