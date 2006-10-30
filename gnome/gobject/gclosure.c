/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * gclosure.c: Support for GClosure
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
#include "gc.h"
#include "gclosure.h"



SCM scm_class_gclosure;

typedef struct _GuileGClosure      GuileGClosure;

struct _GuileGClosure {
    GClosure closure;

    SCM func;
};

/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



static void
scm_gclosure_marshal (GClosure *closure, GValue *return_value,
		      guint n_param_values, const GValue *param_values,
		      gpointer invocation_hint, gpointer marshal_data)
#define FUNC_NAME "%scm-gclosure-marshal"
{
    GuileGClosure *gclosure = (GuileGClosure *) closure;
    SCM params = SCM_EOL, retval;
    guint i;

    /* Only deals with <gvalue>s. Conversion to and from native scheme values is
     * done at a higher level (see gobject.scm) */

    for (i = 0; i < n_param_values; i++) {
	const GValue *current = &param_values [i];
	SCM this;

	this = scm_c_make_gvalue (G_VALUE_TYPE (current));
	g_value_copy (current, (GValue *) SCM_SMOB_DATA (this));

	params = scm_append_x (SCM_LIST2 (params, SCM_LIST1 (this)));
    }

    retval = scm_apply (gclosure->func, params, SCM_EOL);

    if (return_value
        && G_VALUE_TYPE (return_value) != G_TYPE_NONE
        && G_VALUE_TYPE (return_value) != G_TYPE_INVALID) {
	GValue *gvalue;

	if (retval == SCM_UNSPECIFIED) {
	    SCM return_type;

	    return_type = scm_c_register_gtype (G_VALUE_TYPE (return_value));
	    scm_c_gruntime_error
                (FUNC_NAME, "GClosure expects a return value of type ~S, "
                 "but got the unspecified value: ~S",
                 SCM_LIST2 (return_type, gclosure->func));
	}
	
	SCM_VALIDATE_GVALUE_COPY (0, retval, gvalue);
	g_value_copy (gvalue, return_value);
    }
}
#undef FUNC_NAME



static void
free_closure (gpointer data, GClosure *closure)
{
    DEBUG_ALLOC ("  unprotecting closure %p of GuileGClosure %p", 
                 ((GuileGClosure *) closure)->func, closure);
    scm_glib_gc_unprotect_object (((GuileGClosure *) closure)->func);
    ((GuileGClosure *) closure)->func = SCM_UNDEFINED;
}

SCM_DEFINE (scm_gclosure_primitive_new, "gclosure-primitive-new", 1, 0, 0,
	    (SCM func),
	    "")
#define FUNC_NAME s_scm_gclosure_primitive_new
{
    GClosure *closure;
    SCM retval;
 
    SCM_VALIDATE_PROC (1, func);

    /* <gclosure> instances fail to get unreffed, I think due to Guile's
       conservative GC. So to compensate we don't actually hold any references
       on the closure itself -- we allow it to remain floating. */
    closure = g_closure_new_simple (sizeof (GuileGClosure), NULL);

    DEBUG_ALLOC ("  protecting new closure %p of GuileGClosure %p", func, closure);
    ((GuileGClosure *) closure)->func = scm_glib_gc_protect_object (func);

    g_closure_set_marshal (closure, scm_gclosure_marshal);
    g_closure_add_invalidate_notifier (closure, NULL, free_closure);

    retval = scm_c_make_gvalue (G_TYPE_CLOSURE);
    g_value_set_static_boxed ((GValue *) SCM_SMOB_DATA (retval), closure);
    /* closure->ref_count is 1, and is floating */

    return retval;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gclosure_primitive_invoke, "gclosure-primitive-invoke", 3, 0, 0,
	    (SCM instance, SCM return_type, SCM args),
	    "")
#define FUNC_NAME s_scm_gclosure_primitive_invoke
{
    GClosure *gclosure;
    GType gtype_return = G_TYPE_NONE;
    SCM retval = SCM_UNSPECIFIED;
    GValue *gvalue, *params, *retval_param = NULL;
    guint n_params = 0, i;

    SCM_VALIDATE_GVALUE_TYPE_COPY (1, instance, G_TYPE_CLOSURE, gvalue);
    gclosure = g_value_get_boxed (gvalue);

    if (SCM_NFALSEP (return_type))
	SCM_VALIDATE_GTYPE_COPY (2, return_type, gtype_return);
    if (SCM_NFALSEP (args)) {
	SCM_VALIDATE_VECTOR (3, args);
	n_params = SCM_INUM (scm_vector_length (args));
    }

    for (i = 0; i < n_params; i++) {
	SCM this = scm_vector_ref (args, SCM_MAKINUM (i));

	SCM_VALIDATE_GVALUE (i + 1, this);
    }

    params = g_new0 (GValue, n_params);
    for (i = 0; i < n_params; i++) {
	SCM this = scm_vector_ref (args, SCM_MAKINUM (i));
	const GValue *src = (const GValue *) SCM_SMOB_DATA (this);

	params [i] = *src;
    }

    if (gtype_return != G_TYPE_NONE) {
	retval = scm_c_make_gvalue (gtype_return);
	retval_param = (GValue *) SCM_SMOB_DATA (retval);
    }

    g_closure_invoke (gclosure, retval_param, n_params, params, NULL);

    g_free (params);

    return retval;
}
#undef FUNC_NAME

void
scm_init_gnome_gobject_closures (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "gclosure.x"
#endif

    scm_class_gclosure =
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gclosure>")));
    scm_c_define_and_export_gtype_x (G_TYPE_CLOSURE);
}
