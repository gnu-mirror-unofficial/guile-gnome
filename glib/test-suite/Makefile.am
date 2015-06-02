# guile-gnome

# Copyright (C) 2007, 2011, 2015
# Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:

# Free Software Foundation           Voice:  +1-617-542-5942
# 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
# Boston, MA  02111-1307,  USA       gnu@gnu.org

include $(top_srcdir)/common.mk
include $(top_srcdir)/tests.mk

noinst_LTLIBRARIES = libgw-guile-test-gobject.la libgw-guile-test-glib.la 

std_cflags = $(GUILE_GLIB_CFLAGS) $(GOBJECT_CFLAGS) $(G_WRAP_CFLAGS)

libgw_guile_test_gobject_la_SOURCES = test-gobject.c
nodist_libgw_guile_test_gobject_la_SOURCES = gw-test-gobject.c
libgw_guile_test_gobject_la_CFLAGS = $(std_cflags)
libgw_guile_test_gobject_la_LDFLAGS = -module -rpath $(libdir) 
libgw_guile_test_gobject_la_LIBADD = \
	$(GUILE_GLIB_LIBS) $(G_WRAP_LIBS) $(GUILE_LIBS)

libgw_guile_test_glib_la_SOURCES = test-glib.c
nodist_libgw_guile_test_glib_la_SOURCES = gw-test-glib.c
libgw_guile_test_glib_la_CFLAGS = $(std_cflags)
libgw_guile_test_glib_la_LDFLAGS = -module -rpath $(libdir) 
libgw_guile_test_glib_la_LIBADD = \
	$(GUILE_GLIB_LIBS) $(G_WRAP_LIBS) $(GUILE_LIBS)

noinst_HEADERS = test-gobject.h test-glib.h

SCM_TESTS = gobject.test gw-gobject.test

EXTRA_DIST+= \
	lib.scm 			\
	exceptions.scm 			\
	gw-test-gobject-spec.scm 	\
	gw-test-glib-spec.scm 		\
	test-gobject.defs		\
	test-glib.defs			\
	guile-test-env			\
	guile-gobject-test		\
	$(SCM_TESTS)

CLEANFILES = $(wildcard gnome-*.log test-*.log) 		\
             $(wildcard gw-test-gobject.* gw-test-glib.*) 	\
	     guile-gobject.log

wrapset_stem = glib

# it is not possible to prepend a previously defined variable
# so we redefine it [see $(top_srcdir)/tests.mk]
TESTS_ENVIRONMENT = \
	API_FILE=$(srcdir)/gobject.api					\
	DOC_SCM=$(srcdir)/../doc/gobject/guile-gnome-gobject.scm	\
	$(WRAPSET_TESTS_ENV)						\
	LTDL_LIBRARY_PATH=.:${LTDL_LIBRARY_PATH}			\
	GUILE_LOAD_PATH=".:${srcdir:-.}/..:..:${GUILE_LOAD_PATH}"	\
	$(DEV_ENV) $(SCM_LOG_COMPILER)

script := '(load (getenv "DOC_SCM"))\
(for-each \
(lambda (mod) \
(display " \"") (write (car mod)) (display "\" ")) \
 *modules*)'

gobject.api.update:
	$(top_builddir)/dev-environ guile --debug -e update-api -s $(srcdir)/api.scm \
	  $(shell DOC_SCM=$(srcdir)/../doc/gobject/guile-gnome-gobject.scm \
	     guile -c $(script)) > $(srcdir)/gobject.api

%.check: %
	$(TESTS_ENVIRONMENT) $(srcdir)/$*

EXTRA_DIST+= \
	gobject.api	\
	api.scm

TESTS+= \
	guile-gobject-test \
	api.scm

gw-test-%.scm gw-test-%.c: gw-test-%-spec.scm
	echo GLP=$(GUILE_LOAD_PATH)
	guile $(GUILE_FLAGS) -c \
	  "(use-modules (g-wrap) \
	                (g-wrap guile) \
	                (gw-test-$*-spec)) \
	   (generate-wrapset 'guile 'test-$* \"gw-test-$*\")"
