################################################################################
## guile-gnome-gtk.info

include $(top_srcdir)/generate-docs.mk

docbook_xml_wildcard = ~/src/gnome2/gtk+/docs/reference/gtk/xml/*.xml
docbook_xml_except = \
	%glossary.xml \
	%gtkbindings.xml \
	%gtkclist.xml \
	%gtkcombo.xml \
	%gtkctree.xml \
	%gtkenums.xml \
	%gtkgc.xml \
	%gtkitemfactory.xml \
	%gtklistitem.xml \
	%gtklist.xml \
	%gtkoldeditable.xml \
	%gtkoptionmenu.xml \
	%gtkpagesetupunixdialog.xml \
	%gtkpixmap.xml \
	%gtkpreview.xml \
	%gtkprinter.xml \
	%gtkprintjob.xml \
	%gtkprintunixdialog.xml \
	%gtkprogress.xml \
	%gtksignal.xml \
	%gtktext.xml \
	%gtkthemes.xml \
	%gtktipsquery.xml \
	%gtktreeitem.xml \
	%gtktree.xml \
	%gtktypeutils.xml
docbook_xml_files = $(filter-out $(docbook_xml_except),$(wildcard $(docbook_xml_wildcard)))
generate-defuns:
	$(GUILE) $(GUILE_FLAGS) -c "(begin (debug-set! stack 400000) (apply (@ (gnome gw support gtk-doc) gtk-doc->texi-defuns) (cadr (program-arguments)) '$(GTK_DOC_DEFUN_METHOD) '($(GTK_DOC_DEFUN_ARGS)) (cddr (program-arguments))))" ./overrides.texi $(docbook_xml_files)

wrapset_stem = gtk

EXTRA_DIST = overrides.texi

info_TEXINFOS=guile-gnome-gtk.texi
guile_gnome_gtk_TEXINFOS=overview.texi $(wildcard section-*.texi) $(wildcard defuns-*.texi) undocumented.texi
