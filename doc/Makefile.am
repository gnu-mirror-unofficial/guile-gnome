################################################################################
## guile-gnome-gnome-vfs.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/gnome-vfs/doc/xml/*.xml
docbook_xml_ignore_files = \
	%gnome-vfs-application-registry.xml \
	%gnome-vfs-module.xml \
	%gnome-vfs-module-callback-module-api.xml \
	%gnome-vfs-module-shared.xml \
	%gnome-vfs-method.xml \
	%gnome-vfs-context.xml \
	%gnome-vfs-transform.xml \
	%gnome-vfs-parse-ls.xml \
	%gnome-vfs-standard-callbacks.xml \
	%gnome-vfs-module-callback.xml \
	%gnome-vfs-ops.xml \
	%gnome-vfs-init.xml
docbook_xml_files = $(filter-out $(docbook_xml_ignore_files), $(wildcard $(docbook_xml_wildcard)))

wrapset_stem = gnome-vfs
GTK_DOC_COVERAGE_MODULES = (gnome vfs) (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-gnome-vfs.texi
guile_gnome_gnome_vfs_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi)
