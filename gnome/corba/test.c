/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * test.c:
 *
 * This program is free software; you can redistribute it and/or    
 * modify it under the terms of the GNU General Public License as   
 * published by the Free Software Foundation; either version 2 of   
 * the License, or (at your option) any later version.              
 *                                                                  
 * This program is distributed in the hope that it will be useful,  
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
 * GNU General Public License for more details.                     
 *                                                                  
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

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
