include $(top_srcdir)/common.mk

bin_SCRIPTS = guile-gnome-@API_VERSION@

all-local: $(bin_SCRIPTS)

guile-gnome: guile-gnome.in
	sed -e "s|@guilegnomedir\@|$(guilegnomedir)|g" \
	    -e "s|@guilegnomelibdir\@|$(guilegnomelibdir)|g" \
	    -e "s|@libdir\@|$(libdir)|g" \
	    $(srcdir)/guile-gnome.in > guile-gnome

%-@API_VERSION@: %
	cp $< $@
	chmod +x $@

EXTRA_DIST = guile-gnome.in
CLEANFILES = $(bin_SCRIPTS) guile-gnome
