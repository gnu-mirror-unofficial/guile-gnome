doc=guile-gnome-gobject
DOT_DOC_FILES=$(wildcard $(top_builddir)/glib/gnome/gobject/*.doc)

include docs.mk

nodist_info_TEXINFOS=guile-gnome-gobject.texi

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
# docbook_xml_wildcard = ~/src/gnome2/glib/docs/reference/glib/xml/*.xml

generate-stubs:
	$(top_builddir)/dev-environ guile -c '((@ (gnome gw support gtk-doc) gtk-doc->texi-stubs) (cdr (program-arguments)))' $(addprefix $(docbook_xml_base),$(docbook_xml))

generate-defuns:
	$(top_builddir)/dev-environ guile -c '(apply (@ (gnome gw support gtk-doc) gtk-doc->texi-defuns) (cdr (program-arguments)))' ./overrides.texi '(gnome gw glib-spec)' gnome-glib $(addprefix $(docbook_xml_base),$(docbook_xml))

EXTRA_DIST = README generate-doc-stubs generate-doc-defuns overrides.texi

info_TEXINFOS=guile-gnome-glib.texi
guile_gnome_glib_TEXINFOS=$(wildcard section-*.texi) $(wildcard defuns-*.texi)
