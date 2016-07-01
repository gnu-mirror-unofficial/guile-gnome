# guile-gnome
# Copyright (C) 2003,2004,2009,2012,2013 Free Software Foundation, Inc.

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

guilegnomedir = $(datadir)/guile-gnome-@API_VERSION@
guilemoduledir = $(guilegnomedir)/gnome
guilegwmoduledir = $(guilegnomedir)/gnome/gw

guilegnomelibdir = $(libdir)/guile-gnome-@API_VERSION@

AM_CFLAGS = -I. -I$(srcdir) $(WARN_CFLAGS) $(DEBUG_CFLAGS)

# For overriding from the command line (e.g. --debug)
GUILE_FLAGS = 

SUFFIXES = .x .doc

GUILE_SNARF_CFLAGS = $(DEFS) $(AM_CFLAGS) $(GUILE_CFLAGS) $(G_WRAP_CFLAGS)

PACKAGES = atk cairo defs gconf glib gtk libglade libgnome libgnomecanvas libgnomeui pango
# gnome-vfs corba

SOURCE_GUILE_PATH=$(shell echo $(addprefix $(abs_top_srcdir)/,$(PACKAGES)) | sed -e 's/ /:/g'):$(srcdir)
BUILD_GUILE_PATH=$(if $(filter-out $(abs_top_srcdir),$(abs_top_builddir)),$(shell echo -n $(addprefix $(abs_top_builddir)/,$(PACKAGES)): | sed -e 's/ /:/g'),)
GUILE_LOAD_PATH:=$(BUILD_GUILE_PATH)$(SOURCE_GUILE_PATH):${G_WRAP_MODULE_DIR}$(if ${GUILE_LOAD_PATH},:${GUILE_LOAD_PATH})
export GUILE_LOAD_PATH

.c.x:
	guile-snarf $(GUILE_SNARF_CFLAGS) $< > $@ \
	|| { rm $@; false; }
.c.doc:
	$(CPP) -DSCM_MAGIC_SNARF_DOCS $(GUILE_SNARF_CFLAGS) $< \
	  | grep -E '(\^\^ {|\^\^ })' > $@ || { rm $@; false; }

%.scm guile-gnome-gw-%.c: %-spec.scm
	guile $(GUILE_FLAGS) -c \
	  "(debug-set! stack 400000) \
	   (use-modules (gnome-@API_VERSION@)) \
	   (use-modules (g-wrap)) \
	   (use-modules (gnome gw $*-spec)) \
	   (generate-wrapset 'guile 'gnome-$* \"guile-gnome-gw-$*\")"
	mv guile-gnome-gw-$*.scm $*.scm

%-@API_VERSION@.pc: %.pc
	cp $< $@
%-@API_VERSION@-uninstalled.pc: %-uninstalled.pc
	cp $< $@

# Real gnu make foo
packages = $(filter-out %-uninstalled,$(patsubst %.pc.in,%,$(notdir $(wildcard $(srcdir)/*.pc.in))))
pcifiles = $(patsubst %,%-@API_VERSION@.pc,$(packages))
pcufiles = $(patsubst %,%-@API_VERSION@-uninstalled.pc,$(packages))
