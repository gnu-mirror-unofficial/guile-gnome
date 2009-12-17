/* guile-gnome
 * Copyright (C) 2001, 2009 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba.c: Support routines for the GLib wrapper
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

#include <guile-gnome-corba.h>
#include <guile-gnome-corba-types.h>
#include <guile-gnome-corba-generic.h>
#include <guile-gnome-gobject.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-context.h>
#include <bonobo/bonobo-moniker-util.h>
#include <bonobo/bonobo-exception.h>

//#include <gobject/gtype.h>
#include <glib-object.h>
#include <orbit/orbit.h>

#include <string.h>

SCM_DEFINE (scm_corba_servant_to_reference, "corba-servant->reference", 1, 0, 0,
	    (SCM servant),
	    "")
#define FUNC_NAME s_scm_corba_servant_to_reference
{
    GuilePortableServer_Servant *gservant;
    CORBA_Object corba_objref;
    CORBA_Environment ev;
    SCM stub_class;

    SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE_COPY (1, servant, gservant);

    CORBA_exception_init (&ev);
    corba_objref = PortableServer_POA_servant_to_reference
	(guile_corba_poa, (PortableServer_ServantBase *) gservant, &ev);
    g_assert (!BONOBO_EX (&ev));

    stub_class = gservant->interface->stub_class;

    return scm_c_make_corba_object (stub_class, corba_objref);
}
#undef FUNC_NAME



SCM_DEFINE (scm_bonobo_get_object, "bonobo-get-object", 2, 0, 0,
	    (SCM moniker, SCM class),
	    "")
#define FUNC_NAME s_scm_bonobo_get_object
{
    char *_s;
    CORBA_Object corba_objref;
    CORBA_Environment ev;
    CORBA_TypeCode tc;

    SCM_VALIDATE_STRING (1, moniker);
    SCM_VALIDATE_CORBA_OBJECT_CLASS_COPY (1, class, tc);
    
    CORBA_exception_init (&ev);
    _s = scm_to_locale_string (moniker);
    corba_objref = bonobo_get_object (_s, tc->repo_id, &ev);
    free (_s);
    if (BONOBO_EX (&ev)) {
	g_message (G_STRLOC ": %s", bonobo_exception_get_text (&ev));
	CORBA_exception_free (&ev);
	return SCM_UNSPECIFIED;
    }

    return scm_c_make_corba_object (class, corba_objref);
}
#undef FUNC_NAME



SCM_DEFINE (scm_bonobo_object_query_interface, "bonobo-object-query-interface", 2, 0, 0,
	    (SCM object, SCM class),
	    "")
#define FUNC_NAME s_scm_bonobo_object_query_interface
{
    CORBA_Object corba_objref, queried_objref;
    CORBA_Environment ev;
    CORBA_TypeCode tc;

    SCM_VALIDATE_CORBA_OBJECT_COPY (1, object, corba_objref);
    SCM_VALIDATE_CORBA_OBJECT_CLASS_COPY (1, class, tc);

    CORBA_exception_init (&ev);
    queried_objref = Bonobo_Unknown_queryInterface (corba_objref, tc->repo_id, &ev);

    if (BONOBO_EX (&ev)) {
	g_message (G_STRLOC ": %s", bonobo_exception_get_text (&ev));
	CORBA_exception_free (&ev);
	return SCM_UNSPECIFIED;
    }

    if (!queried_objref)
	return SCM_BOOL_F;

    return scm_c_make_corba_object (class, queried_objref);
}
#undef FUNC_NAME

void
scm_init_gnome_corba (void)
{
#include "guile-gnome-corba.x"

    scm_c_export (s_scm_corba_servant_to_reference,
		  s_scm_bonobo_get_object,
		  s_scm_bonobo_object_query_interface,
		  NULL);
}
