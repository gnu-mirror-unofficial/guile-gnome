/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-gobject.c: The GObject wrapper
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

#include <guile-gnome-gobject.h>
#include "guile-support.h"
#include <glib-object.h>
#include <glib.h>
#include <string.h>
#include <stdio.h>



SCM scm_module_gobject;

SCM scm_class_gparam;
SCM scm_class_gobject;
SCM scm_sym_make;
SCM scm_sym_initialize;
SCM scm_sym_gobject_set_property;
SCM scm_sym_gobject_get_property;
SCM scm_sym_gobject_class_set_properties_x;
SCM scm_sym_gtype_to_class;
SCM scm_sym_gvalue_to_scm;
SCM scm_sym_scm_to_gvalue;


/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif

#define DEBUG_REFCOUNTING



typedef struct _GuileGTypeClass GuileGTypeClass;

struct _GuileGTypeClass {
    GHashTable *properties_hash;

    guint last_property_id;
    gboolean first_instance_created;

    SCM class;
};

static GQuark quark_guile_gtype_class = 0;
static GQuark quark_instance_wrapper = 0;



SCM_SYMBOL  (sym_gruntime_error,"gruntime-error");
SCM_SYMBOL  (sym_name,"name");

SCM_KEYWORD (k_real_instance,	"%real-instance");
SCM_KEYWORD (k_value,		"value");



SCM_DEFINE (scm_gobject_scheme_dir, "gobject-scheme-dir", 0, 0, 0,
	    (),
	    "The directory where this module's data installed.")
#define FUNC_NAME s_scm_gobject_scheme_dir
{
    return scm_makfrom0str (GUILE_GOBJECT_DIR);
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_children, "gtype-children", 1, 0, 0,
	    (SCM type),
	    "Calls the C function g_type_children().")
#define FUNC_NAME s_scm_gtype_children
{
    GType gtype, *children;
    guint n_children, i;
    SCM vector;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    children = g_type_children (gtype, &n_children);

    vector = scm_c_make_vector (n_children, SCM_UNDEFINED);

    for (i = 0; i < n_children; i++)
	scm_vector_set_x (vector, SCM_MAKINUM (i), scm_c_register_gtype (children [i]));

    return vector;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_to_fundamental, "gtype->fundamental", 1, 0, 0,
	    (SCM type),
	    "Calls the C macro G_TYPE_FUNDAMENTAL().")
#define FUNC_NAME s_scm_gtype_to_fundamental
{
    GType gtype, fundamental;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    fundamental = G_TYPE_FUNDAMENTAL (gtype);

    return scm_c_register_gtype (fundamental);
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_to_class_name, "gtype->class-name", 1, 0, 0,
	    (SCM type),
	    "Converts the GType type name @var{type} into a scheme class name.")
#define FUNC_NAME s_scm_gtype_to_class_name
{
    GType gtype;
    gchar *type_name;
    SCM class_name;

    
    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    type_name = scm_c_make_gtype_name ("<%s>", g_type_name (gtype));
    class_name = scm_mem2symbol (type_name, strlen (type_name));
    g_free (type_name);

    return class_name;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_to_method_name, "gtype->method-name", 2, 0, 0,
	    (SCM type, SCM method),
	    "Converts the GType type name @var{type} into a scheme method name.")
#define FUNC_NAME s_scm_gtype_to_method_name
{
    GType gtype;
    gchar *type_name, *method_name;
    SCM retval;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);
    SCM_VALIDATE_SYMBOL (2, method);

    type_name = scm_c_make_gtype_name ("%s", g_type_name (gtype));
    method_name = g_strdup_printf ("%s:%s", type_name, SCM_SYMBOL_CHARS (method));
    retval = scm_mem2symbol (method_name, strlen (method_name));
    g_free (method_name);
    g_free (type_name);

    return retval;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_p, "gtype?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a GType and @code{#f} if not.\n")
#define FUNC_NAME s_scm_gtype_p
{
    return SCM_TYP16_PREDICATE (scm_tc16_gtype, type) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_fundamental_p, "gtype-fundamental?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a fundamental type and @code{#f} if not.\n"
	    "This is the same than using @code{(gtype-eq? type (gtype->fundamental type))}, but\n"
	    "slightly faster.\n")
#define FUNC_NAME s_scm_gtype_fundamental_p
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return G_TYPE_IS_FUNDAMENTAL (gtype) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_genum_register_static, "genum-register-static", 2, 0, 0,
	    (SCM name, SCM vtable),
	    "Creates and registers a new enum GType with name @var{name} with the C runtime. "
	    "There must be no type with name @var{name} when this function is called.\n\n"
	    "The new type can be accessed from C either by passing the returned @code{<gtype>} "
	    "object back to a C function or by using the C function @code{g_type_from_name()}.\n\n"
	    "@var{vtable} is a vector describing the new enum type, each vector element describes "
	    "one enum element and must be a list of 3 elements - the element's nick name (SYMBOL), "
	    "its name (STRING) and its integer value (INUMP).\n\n"
	    "@lisp\n"
	    "(genum-register-static \"Test\"\n"
	    "  #((foo \"Foo\" 1) (bar \"Bar\" 2) (baz \"Long name of baz\" 4)))\n"
	    "@end lisp\n")
#define FUNC_NAME s_scm_genum_register_static
{
    gulong length, i;
    GEnumValue *values;
    GType type;

    SCM_VALIDATE_STRING (1, name);
    SCM_VALIDATE_VECTOR (2, vtable);

    type = g_type_from_name (SCM_STRING_CHARS (name));
    if (type)
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "There is already a type with this name: ~S", SCM_LIST1 (name), SCM_EOL);

    length = SCM_INUM (scm_vector_length (vtable));

    for (i = 0; i < length; i++) {
	SCM this = scm_vector_ref (vtable, SCM_MAKINUM (i));

	SCM_ASSERT ((scm_ilength (this) == 3) &&
		    SCM_SYMBOLP (scm_list_ref (this, SCM_MAKINUM (0))) &&
		    SCM_STRINGP (scm_list_ref (this, SCM_MAKINUM (1))) &&
		    SCM_INUMP (scm_list_ref (this, SCM_MAKINUM (2))),
		    vtable, SCM_ARG2, FUNC_NAME);
    }

    values = g_new0 (GEnumValue, length + 1);

    for (i = 0; i < length; i++) {
	SCM this = scm_vector_ref (vtable, SCM_MAKINUM (i));

	values [i].value_nick  = g_strdup (SCM_SYMBOL_CHARS (scm_list_ref (this, SCM_MAKINUM (0))));
	values [i].value_name  = g_strdup (SCM_STRING_CHARS (scm_list_ref (this, SCM_MAKINUM (1))));
	values [i].value       = SCM_INUM (scm_list_ref (this, SCM_MAKINUM (2)));
    }

    type = g_enum_register_static (SCM_STRING_CHARS (name), values);

    return scm_c_register_gtype (type);
}
#undef FUNC_NAME



SCM_DEFINE (scm_gflags_register_static, "gflags-register-static", 2, 0, 0,
	    (SCM name, SCM vtable),
	    "Creates and registers a new flags GType with name @var{name} with the C runtime.\n\n"
	    "See @code{genum-register-static} for details.")
#define FUNC_NAME s_scm_gflags_register_static
{
    gulong length, i;
    GFlagsValue *values;
    GType type;

    SCM_VALIDATE_STRING (1, name);
    SCM_VALIDATE_VECTOR (2, vtable);

    type = g_type_from_name (SCM_STRING_CHARS (name));
    if (type)
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "There is already a type with this name: ~S", SCM_LIST1 (name), SCM_EOL);

    length = SCM_INUM (scm_vector_length (vtable));

    for (i = 0; i < length; i++) {
	SCM this = scm_vector_ref (vtable, SCM_MAKINUM (i));

	SCM_ASSERT ((scm_ilength (this) == 3) &&
		    SCM_SYMBOLP (scm_list_ref (this, SCM_MAKINUM (0))) &&
		    SCM_STRINGP (scm_list_ref (this, SCM_MAKINUM (1))) &&
		    SCM_INUMP (scm_list_ref (this, SCM_MAKINUM (2))),
		    vtable, SCM_ARG2, FUNC_NAME);
    }

    values = g_new0 (GFlagsValue, length + 1);

    for (i = 0; i < length; i++) {
	SCM this = scm_vector_ref (vtable, SCM_MAKINUM (i));

	values [i].value_nick  = g_strdup (SCM_SYMBOL_CHARS (scm_list_ref (this, SCM_MAKINUM (0))));
	values [i].value_name  = g_strdup (SCM_STRING_CHARS (scm_list_ref (this, SCM_MAKINUM (1))));
	values [i].value       = SCM_INUM (scm_list_ref (this, SCM_MAKINUM (2)));
    }

    type = g_flags_register_static (SCM_STRING_CHARS (name), values);

    return scm_c_register_gtype (type);
}
#undef FUNC_NAME



gboolean
scm_c_gtype_instance_is_a_p (SCM instance, GType gtype)
{
    return scm_c_scm_to_gtype_instance (instance, gtype) != NULL;
}



/* takes EITHER a gtype-instance smob OR a gtype-instance goops object */
GTypeInstance *
scm_c_scm_to_gtype_instance (SCM instance, GType gtype)
{
    SCM type, class, pinstance;

    if (SCM_TYP16_PREDICATE (scm_tc16_gtype_instance, instance)) {
	GTypeInstance *ginstance = (GTypeInstance *) SCM_SMOB_DATA (instance);

	if (G_TYPE_CHECK_INSTANCE_TYPE (ginstance, gtype))
	    return ginstance;
	else
	    return NULL;
    }

    type = scm_c_register_gtype (gtype);
    class = scm_sys_gtype_lookup_class (type);
    if (!class)
	return NULL;

    if (!SCM_IS_A_P (instance, class))
	return NULL;

    pinstance = scm_slot_ref (instance, scm_sym_gtype_instance);
    if (SCM_TYP16_PREDICATE (scm_tc16_gtype_instance, pinstance)) {
	GTypeInstance *ginstance = (GTypeInstance *) SCM_SMOB_DATA (pinstance);

        if (!ginstance)
            scm_error (sym_gruntime_error, "%scm->gtype-instance",
                       "Object ~A is either uninitialized or has been destroyed.",
                       SCM_LIST1 (instance), SCM_EOL);

	if (G_TYPE_CHECK_INSTANCE_TYPE (ginstance, gtype))
	    return ginstance;
	else
	    return NULL;
    }

    return NULL;
}



/* returns a goops object of class (gtype->class [type of gtypeinstance]) */
SCM
scm_c_gtype_instance_to_scm (GTypeInstance *ginstance)
{
    GType type;
    SCM instance_smob, class, object;

    g_return_val_if_fail (ginstance != NULL, SCM_BOOL_F);

    type = G_TYPE_FROM_INSTANCE (ginstance);

    switch (G_TYPE_FUNDAMENTAL (type)) {
    case G_TYPE_OBJECT:
        object = g_object_get_qdata ((GObject*)ginstance, quark_instance_wrapper);
        if (object) return object;
        break;
    }
    
    instance_smob = scm_c_make_gtype_instance (ginstance);
    
    class = scm_c_gtype_lookup_class (type);
    if (SCM_FALSEP (class))
        class = scm_call_1 (scm_sym_gtype_to_class, scm_c_register_gtype (type));
    g_assert (SCM_NFALSEP (class));

    /* call the scheme version of make, not the c version (aargh) */
    object = scm_call_3 (scm_sym_make, class, k_real_instance, instance_smob);

    /* Cache the return value, so that if a callback or another function returns
     * this ginstance while the ginstance is visible elsewhere, the same wrapper
     * will be used. Since this doesn't happen much with params, we don't cache
     * their wrappers. This qdata is unset in the SMOB's free function. */
    switch (G_TYPE_FUNDAMENTAL (type)) {
    case G_TYPE_OBJECT:
        g_object_set_qdata ((GObject*)ginstance, quark_instance_wrapper, object);
        break;
    }
    
    return object;
}



SCM scm_c_gvalue_to_scm (const GValue *value)
{
    SCM svalue;
    GValue *new = g_new0 (GValue, 1);

    g_value_init (new, G_VALUE_TYPE (value));
    g_value_copy (value, new);
    SCM_NEWSMOB (svalue, scm_tc16_gvalue, new);
    return scm_call_1 (scm_sym_gvalue_to_scm, svalue);
}

GValue* scm_c_scm_to_gvalue (GType type, SCM scm)
#define FUNC_NAME "%scm->gvalue"
{
    SCM svalue;
    GValue *value, *new = g_new0 (GValue, 1);
    
    svalue = scm_call_2 (scm_sym_scm_to_gvalue, scm_c_register_gtype (type), scm);

    /* doesn't actually copy the value. rather disingenuous i would say :P */
    SCM_VALIDATE_GVALUE_COPY (0, svalue, value);
    g_value_init (new, G_VALUE_TYPE (value));
    g_value_copy (value, new);
    return new;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_gtype_instance_value_to_scm, "%gtype-instance-value->scm", 1, 0, 0,
	    (SCM svalue),
	    "")
#define FUNC_NAME s_scm_sys_gtype_instance_value_to_scm
{
    GValue *value;
    GTypeInstance *instance = NULL;
    
    SCM_VALIDATE_GVALUE_COPY (1, svalue, value);
    
    switch (G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (value))) {
    case G_TYPE_OBJECT:
        instance = (GTypeInstance*)g_value_get_object (value);
        break;
    case G_TYPE_PARAM:
        instance = (GTypeInstance*)g_value_get_param (value);
        break;
    default:
        scm_error (sym_gruntime_error, FUNC_NAME,
                   "Don't know what to do with object of "
                   "type ~A: ~S",
                   SCM_LIST2 (scm_makfrom0str (g_type_name (G_VALUE_TYPE (value))), svalue),
                   SCM_EOL);
    }
    
    return scm_c_gtype_instance_to_scm (instance);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gobject_set_data_x, "gobject-set-data!", 3, 0, 0,
	    (SCM object, SCM key, SCM val),
	    "")
#define FUNC_NAME s_scm_gobject_set_data_x
{
    GObject *gobject;
    gchar *sym;

    SCM_VALIDATE_GOBJECT_COPY (1, object, gobject);
    SCM_VALIDATE_SYMBOL (2, key);

    sym = g_strndup (SCM_SYMBOL_CHARS (key), SCM_SYMBOL_LENGTH (key));

    if (SCM_EQ_P (val, SCM_UNBOUND))
        g_object_set_qdata (gobject, g_quark_from_string (sym), NULL);
    else
        g_object_set_qdata_full
            (gobject, g_quark_from_string (sym),
             GINT_TO_POINTER (SCM_UNPACK (scm_gc_protect_object (val))),
             (GDestroyNotify)scm_gc_unprotect_object);
        
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gobject_get_data, "gobject-get-data", 2, 0, 0,
	    (SCM object, SCM key),
	    "")
#define FUNC_NAME s_scm_gobject_get_data
{
    GObject *gobject;
    gchar *sym;
    gpointer data;

    SCM_VALIDATE_GOBJECT_COPY (1, object, gobject);
    SCM_VALIDATE_SYMBOL (2, key);

    sym = g_strndup (SCM_SYMBOL_CHARS (key), SCM_SYMBOL_LENGTH (key));

    data = g_object_get_qdata (gobject, g_quark_from_string (sym));
    
    if (data)
        return SCM_PACK (GPOINTER_TO_INT (data));
    else
        return SCM_UNBOUND;
}
#undef FUNC_NAME

static void
scm_c_gobject_get_property (GObject *gobject, guint param_id, GValue *dest_gvalue, GParamSpec *pspec)
{
    SCM object;
    GValue *gvalue;

    object = scm_c_gtype_instance_to_scm ((GTypeInstance*)gobject);

    gvalue = scm_c_scm_to_gvalue (G_VALUE_TYPE (dest_gvalue),
                                  scm_call_2 (scm_sym_gobject_get_property,
                                              object, scm_str2symbol (pspec->name)));
    g_value_copy (gvalue, dest_gvalue);
}

static void
scm_c_gobject_set_property (GObject *gobject, guint param_id, const GValue *src_value, GParamSpec *pspec)
{
    SCM object, value;

    object = scm_c_gtype_instance_to_scm ((GTypeInstance*)gobject);

    value = scm_c_make_gvalue (G_VALUE_TYPE (src_value));
    g_value_copy (src_value, (GValue *) SCM_SMOB_DATA (value));

    scm_call_3 (scm_sym_gobject_set_property, object, scm_str2symbol (pspec->name),
                scm_call_1 (scm_sym_gvalue_to_scm, value));
}

static void
scm_c_gtype_instance_instance_init (GTypeInstance *g_instance,
				    gpointer g_class)
{
    SCM class;

    class = scm_c_gtype_lookup_class (G_TYPE_FROM_CLASS (g_class));
    g_assert (SCM_NFALSEP (class));

    /* It seems that as an object is initialized, the g_class argument to the
     * init function is the same for each level of inherited classes. However --
     * and this shit tripped me up for a while -- _the class of the instance
     * changes for each level of the init process_. Thus if you want to know the
     * real type of the object, use G_TYPE_FROM_CLASS (g_class). If you want to
     * know which derived class is being initialized (as in a gobject class
     * doubly-specialized on the scheme side), use G_TYPE_FROM_INSTANCE
     * (g_instance). Fucked up! */

    switch (G_TYPE_FUNDAMENTAL (G_TYPE_FROM_CLASS (g_class))) {
    case G_TYPE_OBJECT: {
	GuileGTypeClass *guile_class;

	guile_class = g_type_get_qdata (G_TYPE_FROM_CLASS (g_class), quark_guile_gtype_class);
	guile_class->first_instance_created = TRUE;

        scm_call_2 (scm_sym_initialize, scm_c_gtype_instance_to_scm (g_instance), SCM_EOL);
	break;
    }

    default:
	break;
    }
}

static void
scm_c_gtype_instance_class_init (gpointer g_class, gpointer class_data)
{
    GuileGTypeClass *guile_class;
    SCM class;

    class = scm_c_gtype_lookup_class (G_TYPE_FROM_CLASS (g_class));
    if (SCM_FALSEP (class)) {
        /* this can happen for scheme-defined classes */
        class = scm_call_1 (scm_sym_gtype_to_class,
                            scm_c_register_gtype (G_TYPE_FROM_CLASS (g_class)));
    }
    g_assert (SCM_NFALSEP (class));

    guile_class = g_type_get_qdata (G_TYPE_FROM_CLASS (g_class), quark_guile_gtype_class);
    g_assert (guile_class != NULL);

    DEBUG_ALLOC ("  protecting class %p of %s gclass %p", class,
                 g_type_name (G_TYPE_FROM_CLASS (g_class)), class);

    guile_class->class = scm_gc_protect_object (class);

    /* Not calling a class-init generic will prevent GOOPS classes that are
     * subclassed on the scheme side from being initialized, but that's a corner
     * case. Perhaps we should support it, but I'm removing it for now. */
    /* NOTE: The proper way for supporting class-init is to override initialize
     * for gtype-instance-class. */

    if (G_TYPE_IS_OBJECT (G_TYPE_FROM_CLASS (g_class))) {
	((GObjectClass *) g_class)->get_property = scm_c_gobject_get_property;
	((GObjectClass *) g_class)->set_property = scm_c_gobject_set_property;
    }
}



SCM_DEFINE (scm_gtype_register_static, "gtype-register-static", 2, 0, 0,
	    (SCM name, SCM parent_type),
	    "")
#define FUNC_NAME s_scm_gtype_register_static
{
    GType gtype_parent, gtype;
    GTypeInfo gtype_info;
    GTypeQuery gtype_query;
    GuileGTypeClass *guile_class;

    SCM_VALIDATE_STRING (1, name);
    SCM_VALIDATE_GTYPE_COPY (2, parent_type, gtype_parent);

    gtype = g_type_from_name (SCM_STRING_CHARS (name));
    if (gtype)
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "There is already a type with this name: ~S", SCM_LIST1 (name), SCM_EOL);

    if (!G_TYPE_IS_DERIVABLE (gtype_parent))
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "Cannot derive ~S from non-derivable parent type: ~S",
		   SCM_LIST2 (name, parent_type), SCM_EOL);

    if (!G_TYPE_IS_FUNDAMENTAL (gtype_parent) && !G_TYPE_IS_DEEP_DERIVABLE (gtype_parent))
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "Cannot derive ~S from non-fundamental parent type: ~S",
		   SCM_LIST2 (name, parent_type), SCM_EOL);

    g_type_query (gtype_parent, &gtype_query);

    memset (&gtype_info, 0, sizeof (gtype_info));
    gtype_info.class_size = gtype_query.class_size;
    gtype_info.instance_size = gtype_query.instance_size;
    gtype_info.class_init = scm_c_gtype_instance_class_init;
    gtype_info.instance_init = scm_c_gtype_instance_instance_init;

    gtype = g_type_register_static (gtype_parent, SCM_STRING_CHARS (name),
				    &gtype_info, 0);

    guile_class = g_new0 (GuileGTypeClass, 1);
    guile_class->properties_hash = g_hash_table_new (NULL, NULL);

    g_type_set_qdata (gtype, quark_guile_gtype_class, guile_class);

    return scm_c_register_gtype (gtype);
}
#undef FUNC_NAME



SCM_DEFINE (scm_gobject_class_install_property, "gobject-class-install-property", 2, 0, 0,
	    (SCM class, SCM param),
	    "")
#define FUNC_NAME s_scm_gobject_class_install_property
{
    GType gtype;
    GParamSpec *gparam;
    GObjectClass *gclass;
    GuileGTypeClass *guile_class;
    guint id;

    SCM_VALIDATE_GOBJECT_CLASS_GET_TYPE (1, class, gtype);
    SCM_VALIDATE_GPARAM_COPY (2, param, gparam);

    gclass = g_type_class_ref (gtype);
    if (g_object_class_find_property (gclass, gparam->name))
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "There is already a property with this name in class ~S: ~S",
		   SCM_LIST2 (class, scm_makfrom0str (gparam->name)), SCM_EOL);

    guile_class = g_type_get_qdata (gtype, quark_guile_gtype_class);
    if (!guile_class)
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "Can't add properties to non-derived type: ~S",
		   SCM_LIST1 (class), SCM_EOL);

    if (guile_class->first_instance_created)
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "Can't add properties after intances have been created: ~S",
		   SCM_LIST1 (class), SCM_EOL);

    id = ++guile_class->last_property_id;
    g_object_class_install_property (gclass, id, gparam);

    DEBUG_ALLOC ("  protecting param %p of %s gparam %p", param,
                 g_type_name (G_TYPE_FROM_INSTANCE (gparam)), gparam);
    g_hash_table_insert (guile_class->properties_hash, GINT_TO_POINTER (id),
			 scm_gc_protect_object (param));

    scm_call_1 (scm_sym_gobject_class_set_properties_x, class);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gparam_to_value_type, "gparam->value-type", 1, 0, 0,
	    (SCM param),
	    "")
#define FUNC_NAME s_scm_gparam_to_value_type
{
    GParamSpec *gparam;

    SCM_VALIDATE_GPARAM_COPY (1, param, gparam);

    return scm_c_register_gtype (G_PARAM_SPEC_VALUE_TYPE (gparam));
}
#undef FUNC_NAME



#ifdef DEBUG_REFCOUNTING
SCM_DEFINE (scm_sys_gobject_get_refcount, "%gobject-get-refcount", 1, 0, 0,
	    (SCM object),
	    "Get the refcount of an object (for debugging purposes)")
#define FUNC_NAME s_scm_sys_gobject_get_refcount
{
    GObject *gobject;

    SCM_VALIDATE_GOBJECT_COPY (1, object, gobject);

    return SCM_MAKINUM (gobject->ref_count);
}
#undef FUNC_NAME
#endif




void scm_init_gnome_gobject_helper (GType type)
{
    gchar *scheme_name;
    scheme_name = scm_c_make_gtype_name ("gtype:%s", g_type_name (type));
    
    scm_c_define (scheme_name, scm_c_register_gtype (type));
    scm_c_export (scheme_name, NULL);
    
    g_free (scheme_name);
}


void scm_pre_init_gnome_gobject (void) 
{
    quark_guile_gtype_class = g_quark_from_static_string ("%scm-guile-gtype-class");
    quark_instance_wrapper = g_quark_from_static_string ("%scm-instance-wrapper");
}

void
scm_init_gnome_gobject (void)
{

#ifndef SCM_MAGIC_SNARFER
#include "guile-gnome-gobject.x"
#endif

    scm_c_export (s_scm_gobject_scheme_dir,
		  s_scm_gtype_children,
		  s_scm_gtype_to_fundamental, s_scm_gtype_to_class_name,
		  s_scm_gtype_to_method_name,
		  s_scm_gtype_p, s_scm_gtype_fundamental_p,
		  s_scm_genum_register_static, s_scm_gflags_register_static,
		  s_scm_gtype_register_static,
		  s_scm_gobject_set_data_x,
		  s_scm_gobject_get_data,
		  s_scm_gobject_class_install_property,
		  s_scm_gparam_to_value_type,
#ifdef DEBUG_REFCOUNTING
                  s_scm_sys_gobject_get_refcount,
#endif
		  NULL);

    scm_init_gnome_gobject_helper (G_TYPE_NONE);
    scm_init_gnome_gobject_helper (G_TYPE_INTERFACE);
    scm_init_gnome_gobject_helper (G_TYPE_CHAR);
    scm_init_gnome_gobject_helper (G_TYPE_UCHAR); 				
    scm_init_gnome_gobject_helper (G_TYPE_BOOLEAN);
    scm_init_gnome_gobject_helper (G_TYPE_INT);
    scm_init_gnome_gobject_helper (G_TYPE_UINT);
    scm_init_gnome_gobject_helper (G_TYPE_LONG);
    scm_init_gnome_gobject_helper (G_TYPE_ULONG);
    scm_init_gnome_gobject_helper (G_TYPE_INT64);
    scm_init_gnome_gobject_helper (G_TYPE_UINT64);
    scm_init_gnome_gobject_helper (G_TYPE_ENUM);
    scm_init_gnome_gobject_helper (G_TYPE_FLAGS);
    scm_init_gnome_gobject_helper (G_TYPE_FLOAT);
    scm_init_gnome_gobject_helper (G_TYPE_DOUBLE);
    scm_init_gnome_gobject_helper (G_TYPE_STRING);
    scm_init_gnome_gobject_helper (G_TYPE_POINTER);
    scm_init_gnome_gobject_helper (G_TYPE_BOXED);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM);
    scm_init_gnome_gobject_helper (G_TYPE_OBJECT);
    scm_init_gnome_gobject_helper (G_TYPE_CLOSURE);
    scm_init_gnome_gobject_helper (gboxed_scm_get_type());
    scm_init_gnome_gobject_helper (G_TYPE_NONE);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_CHAR);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_UCHAR); 				
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_BOOLEAN);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_INT);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_UINT);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_ENUM);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_FLAGS);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_LONG);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_ULONG);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_INT64);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_UINT64);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_FLOAT);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_DOUBLE);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_STRING);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_POINTER);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_BOXED);
    scm_init_gnome_gobject_helper (G_TYPE_PARAM_OBJECT);

    scm_init_gnome_gobject_helper (G_TYPE_VALUE_ARRAY);
}

void
scm_post_init_gnome_gobject (void)
{
    scm_module_gobject = scm_current_module ();

    scm_sym_make = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("make")));
    scm_sym_gobject_get_property = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:get-property")));
    scm_sym_gobject_set_property = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:set-property")));
    scm_sym_gobject_class_set_properties_x = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject-class-set-properties!")));
    scm_class_gobject = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gobject>")));
    scm_class_gparam = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gparam>")));
    scm_sym_gvalue_to_scm = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gvalue->scm")));
    scm_sym_scm_to_gvalue = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("scm->gvalue")));
    scm_sym_initialize = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("initialize")));
    scm_sym_gtype_to_class = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype->class")));
}

GType
g_type_from_instance (GTypeInstance *instance)
{
  return G_TYPE_FROM_INSTANCE (instance);
}

gboolean
g_type_is_instantiatable (GType type)
{
  return G_TYPE_IS_INSTANTIATABLE (type);
}

gboolean
g_type_is_classed (GType type)
{
  return G_TYPE_IS_CLASSED (type);
}
