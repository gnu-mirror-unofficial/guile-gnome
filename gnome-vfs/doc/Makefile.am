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
	%gnome-vfs-init.xml \
	%gnome-vfs-async-ops.xml \
	%gnome-vfs-cancellation.xml \
	%gnome-vfs-directory-find-ops.xml \
	%gnome-vfs-directory-list-ops.xml \
	%gnome-vfs-dns-sd.xml \
	%gnome-vfs-file-advanced-ops.xml \
	%gnome-vfs-file-rw-ops.xml \
	%gnome-vfs-file-size.xml \
	%gnome-vfs-inet-connection.xml \
	%gnome-vfs-mime-database-deprecated.xml \
	%gnome-vfs-mime-monitor.xml \
	%gnome-vfs-monitor.xml \
	%gnome-vfs-resolve.xml \
	%gnome-vfs-socket-buffer.xml \
	%gnome-vfs-socket.xml \
	%gnome-vfs-ssl.xml \
	%gnome-vfs-xfer.xml
docbook_xml_files = $(filter-out $(docbook_xml_ignore_files), $(wildcard $(docbook_xml_wildcard)))

wrapset_stem = gnome-vfs
GTK_DOC_COVERAGE_MODULES = (gnome vfs) (gnome gw $(wrapset_stem))

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-gnome-vfs.texi
guile_gnome_gnome_vfs_TEXINFOS=overview.texi \
	$(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
