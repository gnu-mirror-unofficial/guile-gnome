#include <orbit/orbit.h>

#include <Foo.h>

#define BONOBO_EX(ev) ((ev) && (ev)->_major != CORBA_NO_EXCEPTION)

static CORBA_ORB orb;
static PortableServer_POA poa;

static void
test_hello (Foo_MaximumHello hello)
{
    CORBA_Environment ev;

    CORBA_exception_init (&ev);
    Foo_MaximumHello_haveMoreFun (hello, &ev);
    CORBA_exception_free (&ev);

    CORBA_exception_init (&ev);
    Foo_MaximumHello_haveFunWithAnArgument (hello, 518L, 69, &ev);
    CORBA_exception_free (&ev);
}


int
main (int argc, gchar *argv [])
{
    CORBA_Object corba_objref;
    CORBA_Environment ev;

    CORBA_exception_init (&ev);
    orb = CORBA_ORB_init (&argc, argv, "orbit-local-orb", &ev);
    poa = (PortableServer_POA) CORBA_ORB_resolve_initial_references (orb, "RootPOA", &ev);
    PortableServer_POAManager_activate (PortableServer_POA__get_the_POAManager (poa, &ev), &ev);

    if (argc != 2)
	g_error ("Usage: %s IOR", argv [0]);

    CORBA_exception_init (&ev);
    corba_objref = CORBA_ORB_string_to_object (orb, argv [1], &ev);
    g_assert (!BONOBO_EX (&ev) && corba_objref != CORBA_OBJECT_NIL);
    g_message (G_STRLOC ": %p", corba_objref);

    test_hello (corba_objref);

    exit (0);
}
