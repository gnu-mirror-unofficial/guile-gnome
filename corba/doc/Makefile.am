doc=guile-gnome-corba
DOT_DOC_FILES=$(wildcard $(top_builddir)/corba/gnome/corba/*.doc)
EXTRA_DIST=$(doc).scm make-texinfo.scm

update-docs:
	$(top_builddir)/dev-environ $(srcdir)/make-texinfo.scm $(srcdir)/$(doc).scm $(DOT_DOC_FILES) > $(doc).texi.tmp
	mv $(doc).texi.tmp $(doc).texi

info_TEXINFOS=guile-gnome-corba.texi
