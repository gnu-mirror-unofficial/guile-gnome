include $(top_srcdir)/common.mk

SUBDIRS = 
guilemodule_DATA = 

if HAVE_CORBA
guilemodule_DATA += corba.scm
SUBDIRS += corba
endif

EXTRA_DIST = corba.scm
DIST_SUBDIRS = corba
