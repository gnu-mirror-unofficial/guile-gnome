include $(top_srcdir)/common.mk

bin_SCRIPTS = guile-gnome-@API_VERSION@

all-local: $(bin_SCRIPTS)

# tried to get autoconf to do this, but failed.
guile-gnome-@API_VERSION@: guile-gnome.in Makefile
	if test -f $@; then chmod +w $@; fi
	sed -e "s|@API_VERSION\@|@API_VERSION@|g" \
	    -e "s|@running-uninstalled\@|#t|g" \
	    -e "s|@guilegnomedir\@|$(guilegnomedir)|g" \
	    -e "s|@guilegnomelibdir\@|$(guilegnomelibdir)|g" \
	    -e "s|@libdir\@|$(libdir)|g" \
	    $< > $@
	chmod +x-w $@

install-exec-local: Makefile
	@$(NORMAL_INSTALL)
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	sed -e "s|@API_VERSION\@|@API_VERSION@|g" \
	    -e "s|@running-uninstalled\@|#f|g" \
	    -e "s|@guilegnomedir\@|$(guilegnomedir)|g" \
	    -e "s|@guilegnomelibdir\@|$(guilegnomelibdir)|g" \
	    -e "s|@libdir\@|$(libdir)|g" \
	    $(srcdir)/guile-gnome.in > $(DESTDIR)$(bindir)/guile-gnome-@API_VERSION@
	chmod 755 $(DESTDIR)$(bindir)/guile-gnome-@API_VERSION@

EXTRA_DIST = guile-gnome.in
CLEANFILES = $(bin_SCRIPTS)
