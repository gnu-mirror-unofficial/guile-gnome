/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * gobject.c: Support for GObject (and GInterface)
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

#include <string.h>
#include <stdio.h>
#include "gobject.h"
#include "private.h"
#include "guile-support.h"



SCM scm_class_gobject;
static SCM _initialize;
static SCM _gobject_set_property;
static SCM _gobject_get_property;
static SCM _gobject_class_set_properties_x;

static GQuark quark_guile_gtype_class = 0;

SCM_SYMBOL  (sym_gruntime_error,"gruntime-error");

/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif

#define DEBUG_REFCOUNTING



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
                                  scm_call_2 (_gobject_get_property,
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

    scm_call_3 (_gobject_set_property, object, scm_str2symbol (pspec->name),
                scm_gvalue_to_scm (value));
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

        scm_call_2 (_initialize, scm_c_gtype_instance_to_scm (g_instance), SCM_EOL);
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
        class = scm_c_gtype_to_class (G_TYPE_FROM_CLASS (g_class));
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
	scm_c_gruntime_error (FUNC_NAME,
                              "There is already a type with this name: ~S",
                              SCM_LIST1 (name));

    if (!G_TYPE_IS_DERIVABLE (gtype_parent))
        scm_c_gruntime_error (FUNC_NAME,
                              "Cannot derive ~S from non-derivable parent type: ~S",
                              SCM_LIST2 (name, parent_type));

    if (!G_TYPE_IS_FUNDAMENTAL (gtype_parent) && !G_TYPE_IS_DEEP_DERIVABLE (gtype_parent))
        scm_c_gruntime_error (FUNC_NAME,
                              "Cannot derive ~S from non-fundamental parent type: ~S",
                              SCM_LIST2 (name, parent_type));

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

SCM_DEFINE (scm_gobject_type_get_properties, "gobject-type-get-properties", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gobject_type_get_properties
{
    gpointer class = 0;
    GParamSpec **properties;
    guint n_properties, i, count;
    GType gtype;
    SCM vector;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    if (G_TYPE_FUNDAMENTAL (gtype) == G_TYPE_OBJECT) {
        class = G_OBJECT_CLASS (g_type_class_ref (gtype));
        properties = g_object_class_list_properties (class, &n_properties);
    } else if (G_TYPE_FUNDAMENTAL (gtype) == G_TYPE_INTERFACE) {
        if (G_TYPE_IS_FUNDAMENTAL (gtype)) {
            properties = NULL;
            n_properties = 0;
        } else {
            class = g_type_default_interface_ref (gtype);
            properties = g_object_interface_list_properties (class, &n_properties);
        }
    } else {
        scm_wrong_type_arg (FUNC_NAME, 1, type);
    }

    for (i = count = 0; i < n_properties; i++)
	if (properties [i]->owner_type == gtype)
	    count++;

    vector = scm_make_vector (SCM_MAKINUM (count), SCM_UNDEFINED);

    for (i = count = 0; i < n_properties; i++) {
	SCM this;

	if (properties [i]->owner_type != gtype)
	    continue;

	this = scm_c_gtype_instance_to_scm ((GTypeInstance *) properties [i]);

	scm_vector_set_x (vector, SCM_MAKINUM (count), this);
	count++;
    }

    if (G_TYPE_FUNDAMENTAL (gtype) == G_TYPE_OBJECT)
        g_type_class_unref (class);
    else if (G_TYPE_FUNDAMENTAL (gtype) == G_TYPE_INTERFACE
             && !G_TYPE_IS_FUNDAMENTAL (gtype))
        g_type_default_interface_unref (class);

    g_free (properties);

    return vector;
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

    scm_call_1 (_gobject_class_set_properties_x, class);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gobject_primitive_get_property, "gobject-primitive-get-property", 2, 0, 0,
	    (SCM object, SCM name),
	    "")
#define FUNC_NAME s_scm_gobject_primitive_get_property
{
    GObject *gobject;
    GParamSpec *pspec;
    SCM retval;

    SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (1, object, G_TYPE_OBJECT, GObject, gobject);
    SCM_VALIDATE_SYMBOL (2, name);

    pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (gobject), SCM_SYMBOL_CHARS (name));
    if (!pspec) {
	SCM type = scm_c_register_gtype (G_TYPE_FROM_INSTANCE (gobject));

	scm_error (sym_gruntime_error, FUNC_NAME,
		   "No such property ~S in class ~S",
		   SCM_LIST2 (name, type), SCM_EOL);
    }

    retval = scm_c_make_gvalue (pspec->value_type);
    g_object_get_property (gobject, SCM_SYMBOL_CHARS (name),
			   (GValue *) SCM_SMOB_DATA (retval));

    return retval;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gobject_primitive_set_property, "gobject-primitive-set-property", 3, 0, 0,
	    (SCM object, SCM name, SCM value),
	    "")
#define FUNC_NAME s_scm_gobject_primitive_set_property
{
    GObject *gobject;
    GParamSpec *pspec;
    GValue *gvalue;

    SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (1, object, G_TYPE_OBJECT, GObject, gobject);
    SCM_VALIDATE_SYMBOL (2, name);

    pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (gobject), SCM_SYMBOL_CHARS (name));
    if (!pspec) {
	SCM type = scm_c_register_gtype (G_TYPE_FROM_INSTANCE (gobject));

	scm_error (sym_gruntime_error, FUNC_NAME,
		   "No such property ~S in class ~S",
		   SCM_LIST2 (name, type), SCM_EOL);
    }

    SCM_VALIDATE_GVALUE_TYPE_COPY (3, value, pspec->value_type, gvalue);

    g_object_set_property (gobject, SCM_SYMBOL_CHARS (name), gvalue);
    
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



typedef struct {
    GType type;
    gpointer (* postmakefunc)(gpointer object);
} PostMakeFunc;

static GArray *post_make_funcs = NULL;

static inline void
post_make_object (GObject *obj)
{
    if (post_make_funcs) {
	gint i;

	for (i = 0; i < post_make_funcs->len; i++) {
	    if (g_type_is_a (G_OBJECT_TYPE (obj),
                             g_array_index (post_make_funcs, PostMakeFunc, i).type)) {
		g_array_index (post_make_funcs, PostMakeFunc, i).postmakefunc (obj);
                DEBUG_ALLOC ("post-made gobject (%p) of type %s, ->%u",
                             obj, g_type_name (G_TYPE_FROM_INSTANCE (obj)),
                             obj->ref_count);
		break;
	    }
	}
    }
}

/**
 * guile_gobject_register_postmakefunc:
 * type: the GType the sink function applies to.
 * postmakefunc: a function to remove the floating reference on an object.
 *
 * The lengths we go to to deal with broken APIs... This function is here to
 * deal with the case where the ref you get from g_object_new is not actually
 * your own, eg GtkWindow and GtkInvisible.
 *
 * The postmakefunc should be able to remove the floating reference on
 * instances of the given type, or any subclasses.
 */
void
scm_register_gobject_postmakefunc (GType type, gpointer (*postmakefunc) (gpointer))
{
    PostMakeFunc pmf;

    if (!post_make_funcs)
	post_make_funcs = g_array_new (FALSE, FALSE, sizeof(PostMakeFunc));

    pmf.type = type;
    pmf.postmakefunc = postmakefunc;
    g_array_append_val (post_make_funcs, pmf);
}

SCM_DEFINE (scm_gobject_primitive_create_instance, "gobject-primitive-create-instance", 4, 0, 0,
	    (SCM class, SCM type, SCM object, SCM properties),
	    "")
#define FUNC_NAME s_scm_gobject_primitive_create_instance
{
    GObject *gobject;
    GType gtype;
    SCM smob;
    GParameter *params;
    guint length, i;

    SCM_VALIDATE_GTYPE_CLASS (1, class);
    SCM_VALIDATE_GTYPE_COPY (2, type, gtype);
    SCM_VALIDATE_INSTANCE (3, object);
    SCM_VALIDATE_VECTOR (4, properties);
    SCM_ASSERT (G_TYPE_IS_OBJECT (gtype), type, 2, FUNC_NAME);

    length = SCM_INUM (scm_vector_length (properties));
    for (i = 0; i < length; i++) {
	SCM this = scm_vector_ref (properties, SCM_MAKINUM (i));

	SCM_VALIDATE_SYMBOL (4, SCM_CAR (this));
        SCM_VALIDATE_GVALUE (4, SCM_CDR (this));
    }
    
    params = g_new0 (GParameter, length);

    for (i = 0; i < length; i++) {
	const GValue *gvalue;
	SCM this = scm_vector_ref (properties, SCM_MAKINUM (i));
	GParameter *current = &params [i];

	current->name = SCM_SYMBOL_CHARS (SCM_CAR (this));
	current->value.g_type = 0;
        SCM_VALIDATE_GVALUE_COPY (4, SCM_CDR (this), gvalue);
        g_value_init (&current->value, G_VALUE_TYPE (gvalue));
        g_value_copy (gvalue, &current->value);
    }

    gobject = g_object_newv (gtype, length, params);

    /* eat me, GtkWindow! */
    post_make_object (gobject);

    g_free (params);

    smob = scm_c_make_gtype_instance ((GTypeInstance *) gobject);
    /* gobject was just reffed by make_gtype_instance, but we need to unref it
       now -- see the note above */
    DEBUG_ALLOC ("unreffing guile-owned gobject %p, ->%u",
                 gobject, ((GObject*)gobject)->ref_count - 1);
    g_object_unref (gobject);
    scm_slot_set_x (object, scm_sym_gtype_instance, smob);
    
    /* cache this wrapper, like in scm_c_gtype_instance_to_scm */
    g_object_set_qdata (gobject, guile_gobject_quark_instance_wrapper, object);

    return SCM_UNSPECIFIED;
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

SCM_DEFINE (scm_sys_gnome_gobject_object_post_init,
            "%gnome-gobject-object-post-init", 0, 0, 0,
            (),
            "")
#define FUNC_NAME s_scm_sys_gnome_gobject_object_post_init
{
    _initialize = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("initialize")));
    _gobject_get_property = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:get-property")));
    _gobject_set_property = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:set-property")));
    _gobject_class_set_properties_x = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject-class-set-properties!")));
    scm_class_gobject = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gobject>")));
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_init_gnome_gobject (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "gobject.x"
#endif
    quark_guile_gtype_class = g_quark_from_static_string ("%scm-guile-gtype-class");
}
