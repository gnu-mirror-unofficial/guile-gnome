AM_CFLAGS = -I. -I$(srcdir) $(WARN_CFLAGS) -Werror -Wno-unused

CORBA_SOURCE =			\
	Foo.h			\
	Foo-common.c		\
	Foo-imodule.c

idl_flags = --imodule -I $(BONOBO_IDL_DIR) -D__FOO_COMPILATION

$(CORBA_SOURCE): $(srcdir)/Foo.idl $(ORBIT_IDL)
	$(ORBIT_IDL) $(idl_flags) $(srcdir)/Foo.idl

orbittypelib_LTLIBRARIES = Foo_module.la
orbittypelibdir = $(libdir)/orbit-2.0

Foo_module_la_CFLAGS = $(CORBA_CFLAGS) $(AM_CFLAGS)

Foo_module_la_SOURCES = Foo-imodule.c Foo-common.c Foo.h

Foo_module_la_LIBADD = $(CORBA_LIBS)

Foo_module_la_LDFLAGS = -export-dynamic -module

BUILT_SOURCES = $(CORBA_SOURCE)
CLEANFILES = $(BUILT_SOURCES)

EXTRA_DIST = Foo.idl
