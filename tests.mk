# guile-gnome
# Copyright (C) 2007, 2011 Free Software Foundation, Inc.

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


#
# Makefile snippet
#
EXTRA_DIST = wrapset.api wrapset.scm

wrapset_stem = set-wrapset_stem-in-your-makefile

top_module_name = (gnome $(wrapset_stem))
gw_module_name = (gnome gw $(wrapset_stem))
extra_module_names =
wrapset_modules = ($(top_module_name) $(gw_module_name) $(extra_module_names))
WRAPSET_TESTS_ENV = WRAPSET_MODULES="$(wrapset_modules)" WRAPSET_API_FILE=$(srcdir)/wrapset.api
DEV_ENV = $(top_builddir)/dev-environ
GUILE = guile

TESTS_ENVIRONMENT=$(WRAPSET_TESTS_ENV) $(DEV_ENV) $(GUILE) $(GUILE_FLAGS) -e main -s

wrapset.api.update:
	$(WRAPSET_TESTS_ENV) $(DEV_ENV)	$(GUILE) -e update-api -s $(srcdir)/wrapset.scm

%.check: %
	$(TESTS_ENVIRONMENT) $(srcdir)/$*

TESTS = wrapset.scm
