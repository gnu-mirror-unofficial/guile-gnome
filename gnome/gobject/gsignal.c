/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * gsignal.c: Support for GSignal
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


#include <stdio.h>
#include <string.h>
#include "gsignal.h"
#include "guile-support.h"



SCM scm_gsignal_vtable;

/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



static SCM
print_gsignal_struct (SCM gsignal, SCM port)
{
    scm_display (scm_makfrom0str ("#<gsignal "), port);
    scm_write (SCM_LIST6 (SCM_GSIGNAL_ID (gsignal),
			  SCM_PACK (SCM_STRUCT_DATA (gsignal) [scm_si_gsignal_name]),
			  SCM_PACK (SCM_STRUCT_DATA (gsignal) [scm_si_gsignal_interface_type]),
			  SCM_PACK (SCM_STRUCT_DATA (gsignal) [scm_si_gsignal_return_type]),
			  SCM_GSIGNAL_FLAGS (gsignal),
			  SCM_GSIGNAL_PARAMS (gsignal)),
	       port);
    scm_display (scm_makfrom0str (">"), port);

    return SCM_UNSPECIFIED;
}



SCM_DEFINE (scm_gtype_get_signals, "gtype-get-signals", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gtype_get_signals
{
    guint *ids, n_ids, i;
    GTypeClass *type_class = NULL;
    GType gtype;
    SCM vector;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    if (!G_TYPE_IS_FUNDAMENTAL (gtype)
        && G_TYPE_FUNDAMENTAL (gtype) == G_TYPE_INTERFACE)
        /* will add signals and properties to the type */
        type_class = g_type_default_interface_ref (gtype);    
    else if (G_TYPE_IS_CLASSED (gtype))
        type_class = g_type_class_ref (gtype);

    ids = g_signal_list_ids (gtype, &n_ids);

    vector = scm_make_vector (SCM_MAKINUM (n_ids), SCM_UNDEFINED);

    for (i = 0; i < n_ids; i++) {
	GSignalQuery query;
	SCM this, param_types;
	guint j;

	g_signal_query (ids [i], &query);

	this = scm_make_struct (scm_gsignal_vtable, SCM_INUM0, SCM_EOL);

	SCM_SET_GSIGNAL_ID (this, SCM_MAKINUM (query.signal_id));
	SCM_SET_GSIGNAL_NAME (this, scm_makfrom0str (query.signal_name));
	SCM_SET_GSIGNAL_INTERFACE_TYPE (this, scm_c_register_gtype (query.itype));
	SCM_SET_GSIGNAL_RETURN_TYPE (this, scm_c_register_gtype (query.return_type));
	SCM_SET_GSIGNAL_FLAGS (this, SCM_BOOL_F);

	param_types = scm_make_vector (SCM_MAKINUM (query.n_params), SCM_UNDEFINED);

	for (j = 0; j < query.n_params; j++) {
	    SCM current = scm_c_register_gtype (query.param_types [j]);

	    scm_vector_set_x (param_types, SCM_MAKINUM (j), current);
	}

	SCM_SET_GSIGNAL_PARAMS (this, param_types);

	scm_vector_set_x (vector, SCM_MAKINUM (i), this);
    }

    if (!G_TYPE_IS_FUNDAMENTAL (gtype)
        && G_TYPE_FUNDAMENTAL (gtype) == G_TYPE_INTERFACE)
        g_type_default_interface_unref (type_class);
    else if (G_TYPE_IS_CLASSED (gtype))
        g_type_class_unref (type_class);

    g_free (ids);
    
    return vector;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_primitive_create, "gsignal-primitive-create", 2, 0, 0,
	    (SCM signal, SCM closure),
	    "")
#define FUNC_NAME s_scm_gsignal_primitive_create
{
    GClosure *gclosure;
    GValue *gvalue;
    gulong i, length;
    GType *param_types;
    SCM params;
    guint id;

    SCM_VALIDATE_GSIGNAL (1, signal);
    SCM_VALIDATE_GVALUE_TYPE_COPY (2, closure, G_TYPE_CLOSURE, gvalue);
    gclosure = g_value_get_boxed (gvalue);

    params = SCM_GSIGNAL_PARAMS (signal);
    length = SCM_INUM (scm_vector_length (params));
    for (i = 0; i < length; i++) {
	SCM this = scm_vector_ref (params, SCM_MAKINUM (i));

	SCM_VALIDATE_GTYPE (0, this);
    }

    param_types = g_new0 (GType, length);
    for (i = 0; i < length; i++) {
	SCM this = scm_vector_ref (params, SCM_MAKINUM (i));

	SCM_VALIDATE_GTYPE_COPY (0, this, param_types [i]);
    }

    id = g_signal_newv (SCM_GSIGNAL_NAME (signal),
			SCM_GSIGNAL_INTERFACE_TYPE (signal),
			G_SIGNAL_RUN_LAST,
			gclosure,
			NULL, NULL, NULL,
			SCM_GSIGNAL_RETURN_TYPE (signal),
			length, param_types);

    return SCM_MAKINUM (id);
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_instance_primitive_signal_emit, "gtype-instance-primitive-signal-emit", 3, 0, 0,
	    (SCM object, SCM id, SCM args),
	    "")
#define FUNC_NAME s_scm_gtype_instance_primitive_signal_emit
{
  GValue *params;
  GType gtype, signal_return_type;
  SCM retval = SCM_UNSPECIFIED;
  GTypeInstance *instance;
  GValue ret = { 0, };
  GSignalQuery query;
  guint i;

  SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, object, instance);
  SCM_VALIDATE_INUM (2, id);
                                                                                                                                      
  gtype = G_TYPE_FROM_INSTANCE (instance);
  g_signal_query (SCM_INUM (id), &query);
  signal_return_type = query.return_type;
  
  params = g_new0(GValue, query.n_params + 1);
  g_value_init(&params[0], gtype);
  if (g_type_is_a (gtype, G_TYPE_OBJECT))
      g_value_set_object (&params[0], G_OBJECT (SCM_SMOB_DATA (object)));
  else
      scm_c_gruntime_error
          (FUNC_NAME, "Don't know what to do with object of type ~A: ~S",
           SCM_LIST2 (scm_makfrom0str (g_type_name (gtype)), object));
      

  for (i = 0; i < query.n_params; i++){
    SCM this = scm_vector_ref (args, SCM_MAKINUM (i));
    const GValue *value = (const GValue *) SCM_SMOB_DATA (this);
    g_value_init(&params[i + 1],
		 query.param_types[i] & ~G_SIGNAL_TYPE_STATIC_SCOPE);
    g_value_copy (value, &params[i+1]);
  }
  for (i = 0; i < query.n_params; i++) {
    SCM this = scm_vector_ref (args, SCM_MAKINUM (i));
    const GValue *value;
    
    SCM_VALIDATE_GVALUE_TYPE_COPY (i + 1, this, query.param_types [i], value);
  }
  
  if (query.return_type != G_TYPE_NONE) {
    g_value_init(&ret, query.return_type & ~G_SIGNAL_TYPE_STATIC_SCOPE);
    g_signal_emitv (params, SCM_INUM (id), 0, &ret);
  } else {
    g_signal_emitv (params, SCM_INUM (id), 0, NULL);
  }
  

  for (i = 0; i < query.n_params + 1; i++)
    g_value_unset(&params[i]);
  g_free(params);
	
  return retval;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_instance_primitive_signal_connect, "gtype-instance-primitive-signal-connect", 4, 0, 0,
	    (SCM object, SCM id, SCM closure, SCM after),
	    "")
#define FUNC_NAME s_scm_gtype_instance_primitive_signal_connect
{
    GClosure *gclosure;
    GValue *gvalue;
    GTypeInstance *instance;
    GSignalQuery query;
    GType gtype;
    gulong signal_id;
#ifdef DEBUG_PRINT
    guint old_ref_count;
#endif

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, object, instance);
    SCM_VALIDATE_INUM (2, id);
    SCM_VALIDATE_GVALUE_TYPE_COPY (3, closure, G_TYPE_CLOSURE, gvalue);
    SCM_VALIDATE_BOOL (4, after);

    gtype = G_TYPE_FROM_INSTANCE (instance);
    gclosure = g_value_get_boxed (gvalue);

    g_signal_query (SCM_INUM (id), &query);
    SCM_ASSERT (g_type_is_a (gtype, query.itype), object, SCM_ARG1, FUNC_NAME);

#ifdef DEBUG_PRINT
    old_ref_count = gclosure->ref_count;
#endif
    signal_id = g_signal_connect_closure_by_id (instance, SCM_INUM (id), 0, gclosure,
						SCM_NFALSEP (after));
    DEBUG_ALLOC ("GClosure %p connecting: %u->%u",
                 gclosure, old_ref_count, gclosure->ref_count);

    return scm_ulong2num (signal_id);
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_primitive_handler_block, "gsignal-primitive-handler-block", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_primitive_handler_block
{
    GTypeInstance *ginstance;
    gulong id;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);
    SCM_VALIDATE_ULONG_COPY (2, handler_id, id);

    g_signal_handler_block (ginstance, id);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_primitive_handler_unblock, "gsignal-primitive-handler-unblock", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_primitive_handler_unblock
{
    GTypeInstance *ginstance;
    gulong id;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);
    SCM_VALIDATE_ULONG_COPY (2, handler_id, id);

    g_signal_handler_unblock (ginstance, id);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_primitive_handler_disconnect, "gsignal-primitive-handler-disconnect", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_primitive_handler_disconnect
{
    GTypeInstance *ginstance;
    gulong id;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);
    SCM_VALIDATE_ULONG_COPY (2, handler_id, id);

    g_signal_handler_disconnect (ginstance, id);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_primitive_handler_connected_p, "gsignal-primitive-handler-connected?", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_primitive_handler_connected_p
{
    GTypeInstance *ginstance;
    gulong id;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);
    SCM_VALIDATE_ULONG_COPY (2, handler_id, id);

    return g_signal_handler_is_connected (ginstance, id) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



void
scm_init_gnome_gobject_signals (void)
{
    SCM gsubr;

#ifndef SCM_MAGIC_SNARFER
#include "gsignal.x"
#endif

    gsubr = scm_c_make_gsubr ("%print-gsignal", 2, 0, 0, print_gsignal_struct);

    scm_gsignal_vtable = scm_permanent_object
	(scm_make_vtable_vtable (scm_makfrom0str ("pwpwpwpwpwpw"),
				 SCM_INUM0, SCM_LIST1 (gsubr)));

    scm_c_define ("gsignal-id", SCM_MAKINUM (scm_si_gsignal_id));
    scm_c_define ("gsignal-name", SCM_MAKINUM (scm_si_gsignal_name));
    scm_c_define ("gsignal-interface_type", SCM_MAKINUM (scm_si_gsignal_interface_type));
    scm_c_define ("gsignal-return-type", SCM_MAKINUM (scm_si_gsignal_return_type));
    scm_c_define ("gsignal-flags", SCM_MAKINUM (scm_si_gsignal_flags));
    scm_c_define ("gsignal-param-types", SCM_MAKINUM (scm_si_gsignal_params));
    scm_c_define ("gsignal-struct-vtable", scm_gsignal_vtable);
}
