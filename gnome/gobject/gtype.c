/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * gtype.c: Base support for the GLib type system
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
#include "guile-support.h"
#include "gc.h"
#include "gutil.h"
#include "gtype.h"
#include "private.h"



SCM_GLOBAL_SYMBOL  (scm_sym_gtype,		"gtype");
SCM_GLOBAL_SYMBOL  (scm_sym_gtype_class,	"gtype-class");
SCM_GLOBAL_SYMBOL  (scm_sym_gtype_instance,	"gtype-instance");

SCM scm_class_gtype_class;
SCM scm_gtype_to_class;

scm_t_bits scm_tc16_gtype;
scm_t_bits scm_tc16_gtype_class;
scm_t_bits scm_tc16_gtype_instance;



SCM_SYMBOL  (sym_gruntime_error,"gruntime-error");
SCM_SYMBOL  (sym_name,		"name");

SCM_KEYWORD (k_real_instance,	"%real-instance");
SCM_KEYWORD (k_value,		"value");

static SCM _make;
static SCM _make_class;
static SCM _class_redefinition;

static SCM _gtype_instance_write;
static SCM _gtype_name_to_scheme_name;
static SCM _weak_smob_to_goops_hash;

static GQuark quark_class = 0;
static GQuark quark_type = 0;
static GQuark quark_guile_gtype_class = 0;
static GQuark guile_gobject_quark_smob_wrapper;
static GQuark guile_gobject_quark_goops_wrapper;



/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



/* would be nice to assume everything uses InitiallyUnowned, but that's not the
 * case... */
typedef struct {
    GType type;
    void (* sinkfunc)(gpointer instance);
} SinkFunc;

static GSList *gtype_instance_funcs = NULL;
static GArray *sink_funcs = NULL;



/**********************************************************************
 * GType
 **********************************************************************/

static size_t
scm_gtype_free (SCM smob)
{
    return 0;
}

static int
scm_gtype_print (SCM smob, SCM port, scm_print_state *pstate)
{
    GType gtype = (GType) SCM_SMOB_DATA (smob);

    scm_puts ("#<gtype ", port);
    scm_puts (g_type_name (gtype), port);
    scm_puts (">", port);

    return 1;
}

SCM_DEFINE (scm_gtype_p, "gtype?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a GType and @code{#f} if not.\n")
#define FUNC_NAME s_scm_gtype_p
{
    return SCM_TYP16_PREDICATE (scm_tc16_gtype, type) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_is_a_p, "gtype-is-a?", 2, 0, 0,
	    (SCM type, SCM is_a_type),
	    "Returns @code{#t} if @var{type} is a subtype of @var{is_a_type}.")
#define FUNC_NAME s_scm_gtype_is_a_p
{
    GType gtype, is_a_gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);
    SCM_VALIDATE_GTYPE_COPY (2, is_a_type, is_a_gtype);

    return g_type_is_a (gtype, is_a_gtype) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_basic_p, "gtype-basic?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a basic type. Basic types have\n"
            "only one possible representation in Scheme. Unless the user means\n"
            "to deal in GValues, values of basic types should be manipulated as\n"
            "Scheme values.")
#define FUNC_NAME s_scm_gtype_basic_p
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    switch (gtype) {
    case G_TYPE_CHAR:
    case G_TYPE_UCHAR:
    case G_TYPE_BOOLEAN:
    case G_TYPE_INT:
    case G_TYPE_UINT:
    case G_TYPE_LONG:
    case G_TYPE_ULONG:
    case G_TYPE_INT64:
    case G_TYPE_UINT64:
    case G_TYPE_FLOAT:
    case G_TYPE_DOUBLE:
    case G_TYPE_STRING:
	return SCM_BOOL_T;
    default:
        if (gtype == G_TYPE_BOXED_SCM ||
            gtype == G_TYPE_VALUE_ARRAY)
            return SCM_BOOL_T;
        else
            return SCM_BOOL_F;
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_classed_p, "gtype-classed?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a classed type.")
#define FUNC_NAME s_scm_gtype_classed_p
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return G_TYPE_IS_CLASSED (gtype) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_instantiatable_p, "gtype-instantiatable?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is an instantiatable type.")
#define FUNC_NAME s_scm_gtype_instantiatable_p
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return G_TYPE_IS_INSTANTIATABLE (gtype) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_fundamental_p, "gtype-fundamental?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a fundamental type and @code{#f} if not.\n"
	    "This is the same as @code{(eq? type (gtype->fundamental type))}, but\n"
	    "slightly faster.\n")
#define FUNC_NAME s_scm_gtype_fundamental_p
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return G_TYPE_IS_FUNDAMENTAL (gtype) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_to_fundamental, "gtype->fundamental", 1, 0, 0,
	    (SCM type),
	    "Returns the fundamental type of @var{type} (possibly @var{type} itself).")
#define FUNC_NAME s_scm_gtype_to_fundamental
{
    GType gtype, fundamental;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    fundamental = G_TYPE_FUNDAMENTAL (gtype);

    return scm_c_register_gtype (fundamental);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_parent, "gtype-parent", 1, 0, 0,
	    (SCM type),
	    "Returns the parent type of @var{type} (possibly @var{type} itself).")
#define FUNC_NAME s_scm_gtype_parent
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return scm_c_register_gtype (g_type_parent (gtype));
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_children, "gtype-children", 1, 0, 0,
	    (SCM type),
	    "Returns the @code{<gtype>}'s of @var{type}'s direct children, as a list.")
#define FUNC_NAME s_scm_gtype_children
{
    GType gtype, *children, *walk;
    guint n_children;
    SCM ret = SCM_EOL;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    children = walk = g_type_children (gtype, &n_children);

    while (n_children--)
        ret = scm_cons (scm_c_register_gtype (*(walk++)), ret);

    g_free (children);

    return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_interfaces, "gtype-interfaces", 1, 0, 0,
	    (SCM type),
	    "Returns the set of @code{<gtype>}'s that @var{type} implements, as a list.")
#define FUNC_NAME s_scm_gtype_interfaces
{
    GType gtype, *interfaces;
    guint n_interfaces, i;
    SCM ret = SCM_EOL;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    interfaces = g_type_interfaces (gtype, &n_interfaces);
    if (interfaces) {
        for (i=0; i<n_interfaces; i++)
            ret = scm_cons (scm_c_register_gtype (interfaces[i]), ret);
        g_free (interfaces);
    }

    return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_name, "gtype-name", 1, 0, 0,
	    (SCM type),
	    "Returns the name of @var{type}.")
#define FUNC_NAME s_scm_gtype_name
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return scm_str2string (g_type_name (gtype));
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_from_name, "gtype-from-name", 1, 0, 0,
	    (SCM name),
	    "Returns the type named @var{name}, or @code{#f} if none exists.")
#define FUNC_NAME s_scm_gtype_from_name
{
    GType gtype;

    SCM_VALIDATE_STRING (1, name);
    scm_dynwind_begin (0);
    gtype = g_type_from_name (scm_to_locale_string_dynwind (name));
    scm_dynwind_end ();

    return gtype ? scm_c_register_gtype (gtype) : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_from_instance, "gtype-from-instance", 1, 0, 0,
	    (SCM instance),
	    "Returns the type of @var{instance}, which should be a primitive @code{<%gtype-instance>}.")
#define FUNC_NAME s_scm_gtype_from_instance
{
    GTypeInstance *ginstance;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);

    return scm_c_register_gtype (G_TYPE_FROM_INSTANCE (ginstance));
}
#undef FUNC_NAME

SCM
scm_c_register_gtype (GType gtype)
{
    SCM type, object_name;
    const gchar *type_name;

    /* why? */
    gtype &= ~G_TYPE_FLAG_RESERVED_ID_BIT;

    if (!gtype)
	return SCM_BOOL_F;

    type = g_type_get_qdata (gtype, quark_type);
    if (type)
	return type;

    type_name = g_type_name (gtype);
    if (!type_name)
	return SCM_BOOL_F;
    object_name = scm_mem2symbol (type_name, strlen (type_name));

    SCM_NEWSMOB (type, scm_tc16_gtype, gtype);

    g_type_set_qdata (gtype, quark_type, scm_permanent_object (type));

    return type;
}



/**********************************************************************
 * GTypeClass
 **********************************************************************/

static int
scm_gtype_class_print (SCM smob, SCM port, scm_print_state *pstate)
{
    GTypeClass *gtype_class = (GTypeClass*) SCM_SMOB_DATA (smob);

    scm_puts ("#<%gtype-class ", port);
    scm_puts (g_type_name (G_TYPE_FROM_CLASS (gtype_class)), port);
    scm_puts (">", port);

    return 1;
}

SCM_DEFINE (scm_sys_gtype_lookup_class, "%gtype-lookup-class", 1, 0, 0,
	    (SCM type),
	    "Returns the @code{<gtype-class>} registered for @var{type}, or @code{#f} if none has been registered.")
#define FUNC_NAME s_scm_sys_gtype_lookup_class
{
    GType gtype;
    SCM class;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    class = g_type_get_qdata (gtype, quark_class);

    return class ? class : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM
scm_c_gtype_lookup_class (GType gtype)
{
    SCM class;

    class = g_type_get_qdata (gtype, quark_class);
    return class ? class : SCM_BOOL_F;
}

SCM
scm_c_gtype_to_class (GType gtype)
{
    SCM type, ret;
    
    /* Unlike lookup_class, this one will actually initialize the goops class if
       it doesn't yet exist. */

    /* Prevent a round-trip to SCM: */
    ret = scm_c_gtype_lookup_class (gtype);
    if (SCM_NFALSEP (ret))
        return ret;
    
    type = scm_c_register_gtype (gtype);
    ret = scm_call_1 (scm_gtype_to_class, type);
    
    return ret;
}

SCM_DEFINE (scm_sys_gtype_bind_to_class, "%gtype-bind-to-class", 2, 0, 0,
	    (SCM class, SCM type),
	    "A low-level procedure to bind the newly-created "
            "@code{<gtype-class>} @var{class} to @var{type}. Users should "
            "not need to call this function.")
#define FUNC_NAME s_scm_sys_gtype_bind_to_class
{
    SCM type_class;
    GType gtype;
    GTypeClass *gtype_class;

    SCM_VALIDATE_GTYPE_CLASS (1, class);
    SCM_VALIDATE_GTYPE_COPY (2, type, gtype);

    scm_slot_set_x (class, scm_sym_gtype, type);
    g_type_set_qdata (gtype, quark_class, scm_permanent_object (class));

    if (G_TYPE_IS_CLASSED (gtype)) {
        /* Note: for scheme classes, this might end up calling
           scm_c_gtype_instance_class_init in gobject.c, and we're not fully
           initialized. You just need to code around that. */
        gtype_class = g_type_class_ref (gtype);
        SCM_NEWSMOB (type_class, scm_tc16_gtype_class, gtype_class);
        scm_slot_set_x (class, scm_sym_gtype_class, type_class);
    }

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/**********************************************************************
 * GTypeInstance
 **********************************************************************/

static scm_t_gtype_instance_funcs*
get_gtype_instance_instance_funcs (gpointer instance)
{
    GSList *l;
    GType fundamental;
    fundamental = G_TYPE_FUNDAMENTAL (G_TYPE_FROM_INSTANCE (instance));
    for (l = gtype_instance_funcs; l; l = l->next) {
        scm_t_gtype_instance_funcs *ret = l->data;
        if (fundamental == ret->type)
            return ret;
    }
    return NULL;
}

void
scm_register_gtype_instance_funcs (const scm_t_gtype_instance_funcs *funcs)
{
    gtype_instance_funcs = g_slist_append (gtype_instance_funcs,
                                           (gpointer)funcs);
}

gpointer
scm_c_gtype_instance_ref (gpointer instance)
{
    scm_t_gtype_instance_funcs *funcs;
    funcs = get_gtype_instance_instance_funcs (instance);
    if (funcs && funcs->ref)
        funcs->ref (instance);
    return instance;
}

void
scm_c_gtype_instance_unref (gpointer instance)
{
    scm_t_gtype_instance_funcs *funcs;
    funcs = get_gtype_instance_instance_funcs (instance);
    if (funcs && funcs->unref)
        funcs->unref (instance);
    /* else */
    /*     g_type_free_instance (instance); */
}

SCM
scm_c_gtype_instance_get_cached_smob (gpointer instance)
{
    SCM ret;
    scm_t_gtype_instance_funcs *funcs;
    funcs = get_gtype_instance_instance_funcs (instance);
    if (funcs && funcs->get_qdata) {
        gpointer data = funcs->get_qdata ((GObject*)instance,
                                          guile_gobject_quark_smob_wrapper);
        if (data) {
            ret = GPOINTER_TO_SCM (data);
            scm_gc_mark (ret);
            return ret;
        }
    }
    return SCM_BOOL_F;
}

void
scm_c_gtype_instance_set_cached_smob (gpointer instance, SCM smob)
{
    scm_t_gtype_instance_funcs *funcs;
    funcs = get_gtype_instance_instance_funcs (instance);
    if (funcs && funcs->set_qdata)
        funcs->set_qdata ((GObject*)instance,
                          guile_gobject_quark_smob_wrapper,
                          smob == SCM_BOOL_F ? NULL : SCM_TO_GPOINTER (smob));
}

SCM
scm_c_gtype_instance_get_cached_goops (SCM smob)
{
    SCM ret;
    
    ret = scm_hashq_ref (_weak_smob_to_goops_hash, smob, SCM_BOOL_F);
    if (SCM_NFALSEP (ret))
        scm_gc_mark (ret);
    return ret;
}

void
scm_c_gtype_instance_set_cached_goops (SCM smob, SCM goops)
{
    scm_hashq_set_x (_weak_smob_to_goops_hash, smob, goops);
}

/* A GTypeInstance is a SMOB whose first word is the GTypeInstance* pointer, and
   whose second word is nothing. */
static size_t
scm_gtype_instance_free (SCM smob)
{
    gpointer instance = (gpointer)SCM_SMOB_DATA (smob);

    DEBUG_ALLOC ("freeing instance 0x%p, smob 0x%p", instance, (void*)smob);

    SCM_SET_SMOB_DATA (smob, NULL);

    if (!instance)
	return 0;

    /* cached goops will go away when this is gc'd, because it is in a weak key
       hash */
    scm_c_gtype_instance_set_cached_smob (instance, SCM_BOOL_F);
    scm_c_gtype_instance_unref (instance);

    return 0;
}

static int
scm_gtype_instance_print (SCM smob, SCM port, scm_print_state *pstate)
{
    gpointer instance = (gpointer)SCM_SMOB_DATA (smob);
    SCM class;

    class = g_type_get_qdata (G_TYPE_FROM_INSTANCE (instance), quark_class);
    if (SCM_FALSEP (class))
	class = scm_c_register_gtype (G_TYPE_FROM_INSTANCE (instance));

    scm_call_3 (_gtype_instance_write, class, smob, port);
    return 1;
}

SCM_DEFINE (scm_sys_gtype_instance_primitive_destroy_x, "%gtype-instance-primitive-destroy!", 1, 0, 0,
	    (SCM instance),
	    "Release all references that the Scheme wrapper @var{instance} "
            "has on the underlying C value, and release pointers associated "
            "with the C value that point back to Scheme.\n\n"
            "Normally not necessary. Used by the implementations of some "
            "instantiatable types that have @code{destroy} methods, "
            "notably @code{<gtk-object>}.")
#define FUNC_NAME s_scm_sys_gtype_instance_primitive_destroy_x
{
    SCM_VALIDATE_GTYPE_INSTANCE (1, instance);

    scm_gtype_instance_free (instance);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gtype_instance_primitive_to_type, "gtype-instance-primitive->type", 1, 0, 0,
	    (SCM instance),
	    "Retrieve the @code{<gtype>} object associated with the primitive "
            "@code{<%gtype-instance>} value, @var{instance}.")
#define FUNC_NAME s_scm_gtype_instance_primitive_to_type
{
    GTypeInstance *ginstance;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);

    return scm_c_register_gtype (G_TYPE_FROM_INSTANCE (ginstance));
}
#undef FUNC_NAME

gboolean
scm_c_gtype_instance_is_a_p (SCM instance, GType gtype)
{
    return scm_c_scm_to_gtype_instance (instance, gtype) != NULL;
}

/* takes EITHER a gtype-instance smob OR a gtype-instance goops object */
gpointer
scm_c_scm_to_gtype_instance (SCM instance, GType gtype)
{
    SCM type, class, pinstance;

    if (SCM_TYP16_PREDICATE (scm_tc16_gtype_instance, instance)) {
	gpointer ginstance = (gpointer)SCM_SMOB_DATA (instance);

	if (G_TYPE_CHECK_INSTANCE_TYPE (ginstance, gtype))
	    return ginstance;
	else
	    return NULL;
    }

    type = scm_c_register_gtype (gtype);
    class = scm_sys_gtype_lookup_class (type);
    if (SCM_FALSEP (class))
	return NULL;

    if (!SCM_IS_A_P (instance, class))
	return NULL;

    pinstance = scm_slot_ref (instance, scm_sym_gtype_instance);
    if (SCM_TYP16_PREDICATE (scm_tc16_gtype_instance, pinstance)) {
	gpointer ginstance = (gpointer)SCM_SMOB_DATA (pinstance);

        if (!ginstance)
            scm_c_gruntime_error ("%scm->gtype-instance",
                                  "Object ~A is either uninitialized or has been destroyed.",
                                  SCM_LIST1 (instance));

	if (G_TYPE_CHECK_INSTANCE_TYPE (ginstance, gtype))
	    return ginstance;
	else
	    return NULL;
    } else {
        scm_c_gruntime_error ("%scm->gtype-instance",
                              "Bad %gtype-instance slot value: ~A",
                              SCM_LIST1 (pinstance));
        return NULL; /* won't be reached */
    }
}

/* idea, code, and comments stolen from pygtk -- thanks, James :-) */
static inline void
sink_type_instance (gpointer instance)
{
    if (sink_funcs) {
	gint i;

	for (i = 0; i < sink_funcs->len; i++) {
	    if (g_type_is_a (G_TYPE_FROM_INSTANCE (instance),
                             g_array_index (sink_funcs, SinkFunc, i).type)) {
		g_array_index (sink_funcs, SinkFunc, i).sinkfunc (instance);

#ifdef DEBUG_PRINT
                if (G_IS_OBJECT (instance)) {
                    DEBUG_ALLOC ("sunk gobject (%p) of type %s, ->%u",
                                 instance, g_type_name (G_TYPE_FROM_INSTANCE (instance)),
                                 ((GObject*)instance)->ref_count);
                }
#endif
		break;
	    }
	}
    }
}

/**
 * As Guile handles memory management for us, the "floating reference" code in
 * GTK actually causes memory leaks for objects that are never parented. For
 * this reason, guile-gobject removes the floating references on objects on
 * construction.
 *
 * The sinkfunc should be able to remove the floating reference on
 * instances of the given type, or any subclasses.
 */
void
scm_register_gtype_instance_sinkfunc (GType type, void (*sinkfunc) (gpointer))
{
    SinkFunc sf;

    if (!sink_funcs)
	sink_funcs = g_array_new (FALSE, FALSE, sizeof(SinkFunc));

    sf.type = type;
    sf.sinkfunc = sinkfunc;
    g_array_append_val (sink_funcs, sf);
}

/* returns a goops object of class (gtype->class type). this function exists for
 * gobject.c:scm_c_gtype_instance_instance_init. all other callers should use
 * scm_c_gtype_instance_to_scm. */
SCM
scm_c_gtype_instance_to_scm_typed (gpointer ginstance, GType type)
{
    SCM instance_smob, class, object;

    instance_smob = scm_c_gtype_instance_get_cached_smob (ginstance);
    if (!scm_is_false (instance_smob)) {
        object = scm_c_gtype_instance_get_cached_goops (instance_smob);
    } else {
        instance_smob = scm_c_make_gtype_instance (ginstance);
        object = SCM_BOOL_F;
    }

    if (!scm_is_false (object))
        return object;
    
    class = scm_c_gtype_lookup_class (type);
    if (SCM_FALSEP (class))
        class = scm_c_gtype_to_class (type);
    g_assert (SCM_NFALSEP (class));

    /* call the scheme version of make, not the c version (aargh) */
    object = scm_call_3 (_make, class, k_real_instance, instance_smob);

    /* Cache the return value, so that if a callback or another function returns
     * this ginstance while the ginstance is visible elsewhere, the same wrapper
     * will be used. This qdata is unset in the SMOB's free function. */
    scm_c_gtype_instance_set_cached_goops (instance_smob, object);
    DEBUG_ALLOC ("returning new object 0x%p for i0x%p/s0x%p",
                 (void*)object, ginstance, (void*)instance_smob);
    
    return object;
}

SCM
scm_c_gtype_instance_to_scm (gpointer ginstance)
{
    if (!ginstance)
        return SCM_BOOL_F;

    return scm_c_gtype_instance_to_scm_typed
        (ginstance, G_TYPE_FROM_INSTANCE (ginstance));
}

SCM
scm_c_make_gtype_instance (gpointer ginstance)
{
    SCM ret;

    if (!ginstance)
        return SCM_BOOL_F;

    ret = scm_c_gtype_instance_get_cached_smob (ginstance);
    if (!scm_is_false (ret))
        return ret;
    
    /* see REFCOUNTING for the policy */
    scm_c_gtype_instance_ref (ginstance);

    DEBUG_ALLOC ("reffed gobject (%p) of type %s, ->%u",
                 ginstance, g_type_name (G_TYPE_FROM_INSTANCE (ginstance)),
                 ((GObject*)ginstance)->ref_count);

    /* sink the floating ref, if any */
    sink_type_instance (ginstance);
    SCM_NEWSMOB2 (ret, scm_tc16_gtype_instance, ginstance, NULL);

    /* cache the return value */
    scm_c_gtype_instance_set_cached_smob (ginstance, ret);
    
    return ret;
}



/**********************************************************************
 * Miscellaneous
 **********************************************************************/

void scm_c_gruntime_error (const char *subr, const char *message,
                           SCM args)
{
    scm_error (sym_gruntime_error, subr, message,
               args, SCM_EOL);
}

void scm_c_define_and_export_gtype_x (GType type)
{
    SCM str;

    str = scm_string_append
        (scm_list_2 (scm_str2string ("gtype:"),
                     scm_call_1 (_gtype_name_to_scheme_name,
                                 scm_str2string (g_type_name (type)))));
                     
    scm_define (scm_string_to_symbol (str), scm_c_register_gtype (type));
    scm_dynwind_begin (0);
    scm_c_export (scm_to_locale_string_dynwind (str), NULL);
    scm_dynwind_end ();
}

SCM_KEYWORD (k_name,		"name");
SCM_KEYWORD (k_class,		"class");
SCM_KEYWORD (k_metaclass,	"metaclass");
SCM_KEYWORD (k_gtype,		"gtype");

SCM_DEFINE (scm_especify_metaclass_x,
            "especify-metaclass!", 2, 0, 0,
	    (SCM class, SCM metaclass),
	    "A terrible hack that takes a class @var{class} and sets its "
            "metaclass, in-place, to @var{metaclass}. @var{metaclass} must "
            "be a subclass of @var{class}' existing metaclass.\n\n"
            "This method is useful if you want to define a method that "
            "on a particular @code{<gtype-class>}, such as "
            "@code{make-instance}. However, it would be cleaner to "
            "devise a way of making these ``class methods'' without "
            "molesting GOOPS in this way.")
#define FUNC_NAME s_scm_especify_metaclass_x
{
    SCM sgtype, new_class;
    GType gtype;
    
    SCM_VALIDATE_GTYPE_CLASS(1, class);

    if (!SCM_SUBCLASSP (metaclass, SCM_CLASS_OF (class)))
        scm_c_gruntime_error (FUNC_NAME,
                              "New metaclass ~A is not a subclass of old metaclass ~S",
                              SCM_LIST2 (metaclass, SCM_CLASS_OF (class)));

    sgtype = scm_slot_ref (class, scm_sym_gtype);
    gtype = (GType)SCM_SMOB_DATA (sgtype);

    /* unbind the type and the class */
    g_type_set_qdata (gtype, quark_class, NULL);
    
    new_class = scm_apply_0 (_make_class,
                             SCM_LIST8 (scm_class_direct_supers (class),
                                        scm_class_direct_slots (class),
                                        k_name, scm_class_name (class),
                                        k_gtype, sgtype,
                                        k_metaclass, metaclass));
    scm_call_2 (_class_redefinition, class, new_class);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_get_struct_slot, "%get-struct-slot", 2, 0, 0,
	    (SCM object, SCM offset),
	    "Access the contents of a slot directly (without the getter)")
#define FUNC_NAME s_scm_sys_get_struct_slot
{
    SCM retval;

    SCM_VALIDATE_INSTANCE (1, object);
    SCM_VALIDATE_INUM (2, offset);

    retval = SCM_SLOT (object, SCM_INUM (offset));

    return (retval == SCM_UNBOUND) ? SCM_BOOL_F : retval;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_set_struct_slot, "%set-struct-slot!", 3, 0, 0,
	    (SCM object, SCM offset, SCM value),
	    "Set the contents of a slot directly (without the setter)")
#define FUNC_NAME s_scm_sys_set_struct_slot
{
    SCM_VALIDATE_INSTANCE (1, object);
    SCM_VALIDATE_INUM (2, offset);

    SCM_SET_SLOT (object, SCM_INUM (offset), value);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/**********************************************************************
 * Initialization
 **********************************************************************/

SCM_DEFINE (scm_sys_gnome_gobject_types_post_init,
            "%gnome-gobject-types-post-init", 0, 0, 0,
            (),
            "")
#define FUNC_NAME s_scm_sys_gnome_gobject_types_post_init
{
    scm_gtype_to_class = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype->class")));
    _gtype_instance_write =
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype-instance:write")));
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_init_gnome_gobject_types (void)
{
    g_type_init ();

#ifndef SCM_MAGIC_SNARFER
#include "gtype.x"
#endif

    quark_type = g_quark_from_static_string ("%scm-gtype->type");
    quark_class = g_quark_from_static_string ("%scm-gtype->class");
    quark_guile_gtype_class = g_quark_from_static_string ("%scm-guile-gtype-class");
    guile_gobject_quark_smob_wrapper =
        g_quark_from_static_string ("%guile-gobject-smob-wrapper");
    guile_gobject_quark_goops_wrapper = 
        g_quark_from_static_string ("%guile-gobject-goops-wrapper");

    scm_tc16_gtype = scm_make_smob_type ("gtype", 0);
    scm_set_smob_free (scm_tc16_gtype, scm_gtype_free);
    scm_set_smob_print (scm_tc16_gtype, scm_gtype_print);

    /* If we just call this "gtype-class" then a goops class <gtype-class> will
       automagically be created for the smob. We don't want that of course, so
       we prefix with %. */
    scm_tc16_gtype_class = scm_make_smob_type ("%gtype-class", 0);
    scm_set_smob_print (scm_tc16_gtype_class, scm_gtype_class_print);

    /* Same reason. */
    scm_tc16_gtype_instance = scm_make_smob_type ("%gtype-instance", 0);
    scm_set_smob_free (scm_tc16_gtype_instance, scm_gtype_instance_free);
    scm_set_smob_print (scm_tc16_gtype_instance, scm_gtype_instance_print);

    _gtype_name_to_scheme_name = 
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype-name->scheme-name")));

    _weak_smob_to_goops_hash =
        scm_permanent_object (scm_make_weak_key_hash_table (scm_from_int (61)));

    scm_c_define_and_export_gtype_x (G_TYPE_NONE);
    scm_c_define_and_export_gtype_x (G_TYPE_ENUM);
    scm_c_define_and_export_gtype_x (G_TYPE_FLAGS);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM);
    scm_c_define_and_export_gtype_x (G_TYPE_OBJECT);
    scm_c_define_and_export_gtype_x (G_TYPE_INTERFACE);

    scm_class_gtype_class = scm_permanent_object
	(SCM_VARIABLE_REF (scm_c_lookup ("<gtype-class>")));

    _make = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("make")));
    _make_class = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("make-class")));
    _class_redefinition =
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("class-redefinition")));
}
