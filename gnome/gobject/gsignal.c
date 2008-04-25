/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
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



static SCM _make;
static SCM scm_class_gsignal;
SCM_KEYWORD (k_id, "id");
SCM_KEYWORD (k_name, "name");
SCM_KEYWORD (k_interface_type, "interface-type");
SCM_KEYWORD (k_return_type, "return-type");
SCM_KEYWORD (k_param_types, "param-types");
SCM_SYMBOL (sym_name, "name");
SCM_SYMBOL (sym_interface_type, "interface-type");
SCM_SYMBOL (sym_return_type, "return-type");
SCM_SYMBOL (sym_param_types, "param-types");

/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



static SCM
scm_c_gsignal_query (guint id)
{
    SCM args, param_types = SCM_EOL;
    GSignalQuery q;
    gint i;

    g_signal_query (id, &q);

    for (i = q.n_params - 1; i >= 0; i--)
        param_types = scm_cons (scm_c_gtype_to_class (q.param_types [i]),
                                param_types);

    args = scm_list_n (scm_class_gsignal,
                       k_id, scm_from_uint (q.signal_id),
                       k_name, scm_from_locale_string (q.signal_name),
                       k_interface_type, scm_c_gtype_to_class (q.itype),
                       k_return_type, q.return_type == G_TYPE_NONE
                       ? SCM_BOOL_F : scm_c_gtype_to_class (q.return_type),
                       k_param_types, param_types,
                       SCM_UNDEFINED);
    return scm_apply_0 (_make, args);
}

SCM_DEFINE (scm_gsignal_query, "gsignal-query", 2, 0, 0,
	    (SCM class, SCM name),
	    "")
#define FUNC_NAME s_scm_gsignal_query
{
    GType type;
    guint id;
    char *cname;

    SCM_VALIDATE_GTYPE_CLASS_COPY (1, class, type);
    SCM_VALIDATE_SYMBOL (2, name);
    
    cname = scm_symbol_chars (name);
    id = g_signal_lookup (cname, type);
    free (cname);
    if (!id)
        scm_c_gruntime_error (FUNC_NAME, "Unknown signal ~A on class ~A",
                              SCM_LIST2 (name, class));

    return scm_c_gsignal_query (id);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_class_get_signals, "gtype-class-get-signals", 1, 1, 0,
	    (SCM class, SCM tail),
            "Returns a list of signals belonging to @var{class} and all "
            "parent types.")
#define FUNC_NAME s_scm_gtype_class_get_signals
{
    GType type;
    SCM supers;
    guint *ids, n_ids;
    glong i;

    SCM_VALIDATE_GTYPE_CLASS_COPY (1, class, type);
    if (SCM_UNBNDP (tail))
        tail = SCM_EOL;

    if (!type)
        return tail;

    if (!(G_TYPE_IS_INSTANTIATABLE (type) || G_TYPE_IS_INTERFACE (type)))
        return tail;
    
    ids = g_signal_list_ids (type, &n_ids);

    for (i = ((glong)n_ids) - 1; i >= 0; i--)
        tail = scm_cons (scm_c_gsignal_query (ids[i]), tail);

    g_free (ids);

    for (supers = scm_class_direct_supers (class); SCM_CONSP (supers);
         supers = scm_cdr (supers))
        if (SCM_GTYPE_CLASSP (scm_car (supers)))
            tail = scm_gtype_class_get_signals (scm_car (supers), tail);
    
    return tail;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_create, "gsignal-create", 2, 0, 0,
	    (SCM signal, SCM closure),
	    "")
#define FUNC_NAME s_scm_gsignal_create
{
    GClosure *gclosure;
    GValue *gvalue;
    gulong i, length;
    GType *param_types;
    SCM params, rtype;
    guint id;

#define REF(slot) scm_slot_ref (signal, sym_##slot)

    SCM_VALIDATE_GVALUE_TYPE_COPY (2, closure, G_TYPE_CLOSURE, gvalue);
    gclosure = g_value_get_boxed (gvalue);

    params = REF(param_types);
    length = scm_ilength (params);
    param_types = g_new0 (GType, length);
    for (i = 0; i < length; i++, params = scm_cdr (params))
	param_types[i] = scm_c_gtype_class_to_gtype (scm_car (params));
    rtype = REF (return_type);

    scm_dynwind_begin (0);

    id = g_signal_newv (scm_symbol_chars_dynwind (REF (name)),
			scm_c_gtype_class_to_gtype (REF (interface_type)),
			G_SIGNAL_RUN_LAST,
			gclosure,
			NULL, NULL, NULL,
			SCM_FALSEP (rtype)
                        ? G_TYPE_NONE : scm_c_gtype_class_to_gtype (rtype),
			length, param_types);

    scm_dynwind_end ();

    return scm_from_uint (id);
#undef REF
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_instance_signal_emit, "gtype-instance-signal-emit", 2, 0, 1,
	    (SCM object, SCM name, SCM args),
	    "")
#define FUNC_NAME s_scm_gtype_instance_signal_emit
{
    GValue *params;
    GType gtype;
    SCM walk, retval;
    GTypeInstance *instance;
    GValue ret = { 0, };
    GSignalQuery query;
    guint i, id;
    char *cname;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, object, instance);
    SCM_VALIDATE_SYMBOL (2, name);
                                                                                                                                      
    gtype = G_TYPE_FROM_INSTANCE (instance);
    cname = scm_symbol_chars (name);
    id = g_signal_lookup (cname, gtype);
    free (cname);
  
    if (!id)
        scm_c_gruntime_error (FUNC_NAME, "Unknown signal ~A on object ~A",
                              SCM_LIST2 (name, object));

    g_signal_query (id, &query);
  
    params = g_new0 (GValue, query.n_params + 1);
    g_value_init (&params[0], gtype);
    scm_c_gvalue_set (&params[0], object);

    for (walk = args, i = 0; i < query.n_params && SCM_CONSP (walk);
         i++, walk = scm_cdr (walk)) {
        g_value_init (&params[i+1],
                      query.param_types[i] & ~G_SIGNAL_TYPE_STATIC_SCOPE);
        scm_c_gvalue_set (&params[i+1], scm_car (walk));
    }
    SCM_ASSERT (i == query.n_params && SCM_NULLP (walk), args, 3, FUNC_NAME);
  
    if (query.return_type != G_TYPE_NONE) {
        g_value_init (&ret, query.return_type & ~G_SIGNAL_TYPE_STATIC_SCOPE);
        g_signal_emitv (params, id, 0, &ret);
        retval = scm_c_gvalue_ref (&ret);
        g_value_unset (&ret);
    } else {
        g_signal_emitv (params, id, 0, NULL);
        retval = SCM_UNSPECIFIED;
    }
  
    for (i = 0; i < query.n_params + 1; i++)
        g_value_unset (&params[i]);
    g_free(params);
	
    return retval;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_instance_signal_connect_closure,
            "gtype-instance-signal-connect-closure", 4, 0, 0,
	    (SCM object, SCM id, SCM closure, SCM after),
	    "")
#define FUNC_NAME s_scm_gtype_instance_signal_connect_closure
{
    GClosure *gclosure;
    GValue *gvalue;
    GTypeInstance *instance;
    GSignalQuery query;
    GType gtype;
    gulong signal_id, handler_id;
#ifdef DEBUG_PRINT
    guint old_ref_count;
#endif

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, object, instance);
    SCM_VALIDATE_UINT_COPY (2, id, signal_id);
    SCM_VALIDATE_GVALUE_TYPE_COPY (3, closure, G_TYPE_CLOSURE, gvalue);
    SCM_VALIDATE_BOOL (4, after);

    gtype = G_TYPE_FROM_INSTANCE (instance);
    gclosure = g_value_get_boxed (gvalue);

    g_signal_query (scm_to_ulong (id), &query);
    SCM_ASSERT (g_type_is_a (gtype, query.itype), object, SCM_ARG1, FUNC_NAME);

#ifdef DEBUG_PRINT
    old_ref_count = gclosure->ref_count;
#endif
    handler_id = g_signal_connect_closure_by_id (instance, scm_to_ulong (id), 0,
                                                 gclosure, SCM_NFALSEP (after));
    DEBUG_ALLOC ("GClosure %p connecting: %u->%u",
                 gclosure, old_ref_count, gclosure->ref_count);

    return scm_from_ulong (handler_id);
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_handler_block, "gsignal-handler-block", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_handler_block
{
    GTypeInstance *ginstance;
    gulong id;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);
    SCM_VALIDATE_ULONG_COPY (2, handler_id, id);

    g_signal_handler_block (ginstance, id);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_handler_unblock, "gsignal-handler-unblock", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_handler_unblock
{
    GTypeInstance *ginstance;
    gulong id;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);
    SCM_VALIDATE_ULONG_COPY (2, handler_id, id);

    g_signal_handler_unblock (ginstance, id);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_handler_disconnect, "gsignal-handler-disconnect", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_handler_disconnect
{
    GTypeInstance *ginstance;
    gulong id;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);
    SCM_VALIDATE_ULONG_COPY (2, handler_id, id);

    g_signal_handler_disconnect (ginstance, id);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gsignal_handler_connected_p, "gsignal-handler-connected?", 2, 0, 0,
	    (SCM instance, SCM handler_id),
	    "")
#define FUNC_NAME s_scm_gsignal_handler_connected_p
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
#ifndef SCM_MAGIC_SNARFER
#include "gsignal.x"
#endif
    scm_class_gsignal =
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gsignal>")));
    _make =
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("make")));
}
