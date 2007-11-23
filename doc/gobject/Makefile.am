################################################################################
## guile-gnome-gobject.info

doc=guile-gnome-gobject
DOT_DOC_FILES=$(wildcard $(top_builddir)/glib/gnome/gobject/*.doc)
EXTRA_DIST=$(doc).scm make-texinfo.scm

update-docs:
	$(top_builddir)/dev-environ $(srcdir)/make-texinfo.scm $(srcdir)/$(doc).scm $(DOT_DOC_FILES) >$@.tmp
	mv $(doc).texi.tmp $(doc).texi

info_TEXINFOS=guile-gnome-gobject.texi
