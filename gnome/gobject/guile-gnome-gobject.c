#include <guile-gnome-gobject.h>
#include "guile-support.h"
#include <glib-object.h>
#include <glib.h>
#include <string.h>



SCM scm_class_gparam;
SCM scm_class_gobject;
SCM scm_sym_make;
SCM scm_sym_gtype_instance_instance_init;
SCM scm_sym_gtype_instance_class_init;
SCM scm_sym_gobject_instance_init;
SCM scm_sym_gobject_class_init;
SCM scm_sym_gobject_set_property;
SCM scm_sym_gobject_get_property;
SCM scm_sym_gobject_class_install_property;
SCM scm_sym_gtype_to_class;
SCM scm_sym_gvalue_to_scm;


/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_message (str, ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



typedef struct _GuileGTypeClass GuileGTypeClass;

struct _GuileGTypeClass {
    GHashTable *properties_hash;

    guint last_property_id;
    gboolean first_instance_created;

    SCM class;
};

static GQuark quark_object = 0;
static GQuark quark_guile_gtype_class = 0;



SCM_SYMBOL  (sym_gruntime_error,"gruntime-error");

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



SCM_DEFINE (scm_gobject_register_type, "gobject-register-type", 2, 0, 0,
	    (SCM symbol, SCM name),
	    "Binds scheme symbol @var{symbol} to the GType with name @var{name} "
	    "(which must already exist in the GType system).\n\n"
	    "This is a very convenient way to make an already-existing GType accessible "
	    "from scheme.\n\n"
	    "Example:\n\n"
	    "@lisp\n"
	    "(gobject-register-type 'gtype-type-object \"GObject\")\n"
	    "@end lisp\n")
#define FUNC_NAME s_scm_gobject_register_type
{
    SCM object;
    GType type;

    SCM_VALIDATE_SYMBOL (1, symbol);
    SCM_VALIDATE_STRING (2, name);

    type = g_type_from_name (SCM_STRING_CHARS (name));
    if (!type)
	scm_error (sym_gruntime_error, FUNC_NAME,
		   "No such type: ~S", SCM_LIST1 (name), SCM_EOL);

    object = scm_c_register_gtype (type);
    scm_define (symbol, object);

    return SCM_UNSPECIFIED;
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



SCM_DEFINE (scm_gtype_eq_p, "gtype-eq?", 2, 0, 0,
	    (SCM a, SCM b),
	    "Returns @code{#t} if @var{a} and @var{b} are equal and @code{#f} if not.\n"
	    "It is recommended to use this function to compare GType's, even though it's\n"
	    "the same than just using @code{eq?}.\n")
#define FUNC_NAME s_scm_gtype_eq_p
{
    GType gtype_a, gtype_b;

    SCM_VALIDATE_GTYPE_COPY (1, a, gtype_a);
    SCM_VALIDATE_GTYPE_COPY (2, b, gtype_b);

    
    return gtype_a == gtype_b ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_p, "gtype?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a GType and @code{#f} if not.\n")
#define FUNC_NAME s_scm_gtype_p
{
    GType gtype;

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
    SCM type, class;

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

    instance = scm_slot_ref (instance, scm_sym_gtype_instance);
    if (SCM_TYP16_PREDICATE (scm_tc16_gtype_instance, instance)) {
	GTypeInstance *ginstance = (GTypeInstance *) SCM_SMOB_DATA (instance);

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

    type = G_TYPE_FROM_INSTANCE (ginstance);

    instance_smob = scm_c_make_gtype_instance (ginstance);
    
    class = scm_c_gtype_lookup_class (type);
    if (SCM_FALSEP (class)) {
        class = scm_call_1 (scm_sym_gtype_to_class, scm_c_register_gtype (type));
    }
    g_assert (SCM_NFALSEP (class));

    /* call the scheme version of make, not the c version (aargh) */
    object = scm_call_3 (scm_sym_make, class, k_real_instance, instance_smob);

    return object;
}



static void
scm_c_gobject_get_property (GObject *gobject, guint param_id, GValue *dest_gvalue, GParamSpec *pspec)
#define FUNC_NAME "%gobject-get-property"
{
    SCM object, value;
    GValue *gvalue;

    object = g_object_get_qdata (gobject, quark_object);
    g_assert (object != 0);

    value = scm_c_make_gvalue (G_VALUE_TYPE (dest_gvalue));
    scm_gvalue_primitive_set (value, scm_call_2 (scm_sym_gobject_get_property,
                                                 object, scm_str2symbol (pspec->name)));
    
    SCM_VALIDATE_GVALUE_TYPE_COPY (0, value, G_PARAM_SPEC_VALUE_TYPE (pspec), gvalue);
    g_value_copy (gvalue, dest_gvalue);
}
#undef FUNC_NAME

static void
scm_c_gobject_set_property (GObject *gobject, guint param_id, const GValue *src_value, GParamSpec *pspec)
{
    SCM object, value;

    object = g_object_get_qdata (gobject, quark_object);
    g_assert (object != 0);

    value = scm_c_make_gvalue (G_VALUE_TYPE (src_value));
    g_value_copy (src_value, (GValue *) SCM_SMOB_DATA (value));

    scm_call_3 (scm_sym_gobject_set_property, object, scm_str2symbol (pspec->name),
                scm_call_1 (scm_sym_gvalue_to_scm, value));
}

static void
remove_object_quark (SCM instance)
{
    GObject *gobject = G_OBJECT (SCM_SMOB_DATA (instance));

    /* This is called immediately before the instance smob is freed
     * from GC, so remove it from the GObject. */
    g_object_steal_qdata (gobject, quark_object);
}

static void
free_object_quark (gpointer data)
{
    SCM object, instance;
    GObject *gobject;

    object = (SCM) data;
    instance = scm_slot_ref (object, scm_sym_gtype_instance);
    SCM_SET_SMOB_DATA (instance, NULL);

    DEBUG_ALLOC ("unprotecting servant %p of freed gobject %p", object, instance);

    scm_gc_unprotect_object (object);
}

static void
scm_c_gtype_instance_instance_init (GTypeInstance *g_instance,
				    gpointer g_class)
{
    SCM class, object = SCM_UNDEFINED;

    class = scm_c_gtype_lookup_class (G_TYPE_FROM_CLASS (g_class));
    g_assert (SCM_NFALSEP (class));

    switch (G_TYPE_FUNDAMENTAL (G_TYPE_FROM_INSTANCE (g_instance))) {
    case G_TYPE_OBJECT: {
	GuileGTypeClass *guile_class;
	SCM instance;

	guile_class = g_type_get_qdata (G_TYPE_FROM_CLASS (g_class), quark_guile_gtype_class);
	guile_class->first_instance_created = TRUE;

        instance = scm_c_make_gtype_instance (g_instance);

	/* The GOOPS object which we create here is only used on the servant
	 * side - when a signal handler or a property getter/setter function
	 * is called from C.
         *
         * Which is to say, it has the same primitive GTypeInstance smob as
         * every wrapper, but it is a different object than anything available
         * on the scheme side. The only time you'll see it is is in a
         * gobject:instance-init, gobject:set-property, gobject:get-property, or
         * signal handler. So don't set object properties on it.
	 */

        /* argh! scm_make not the same as calling scm_sym_make! argh! */
	object = scm_call_3 (scm_sym_make, class, k_real_instance, instance);

        DEBUG_ALLOC ("  protecting servant %p with instance %p of %s %p", object,
                     instance, g_type_name (G_TYPE_FROM_INSTANCE (g_instance)),
                     g_instance);

	g_object_set_qdata_full (G_OBJECT (g_instance), quark_object,
				 scm_gc_protect_object (object),
				 free_object_quark);
	break;
    }

    default:
	break;
    }

    if (!SCM_UNBNDP (object)) {
	if (G_TYPE_IS_OBJECT (G_TYPE_FROM_INSTANCE (g_instance)))
	    scm_call_2 (scm_sym_gobject_instance_init, class, object);
	else
	    scm_call_2 (scm_sym_gtype_instance_instance_init, class, object);
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

    if (G_TYPE_IS_OBJECT (G_TYPE_FROM_CLASS (g_class))) {
	((GObjectClass *) g_class)->get_property = scm_c_gobject_get_property;
	((GObjectClass *) g_class)->set_property = scm_c_gobject_set_property;
	scm_call_1 (scm_sym_gobject_class_init, class);
    } else
	scm_call_1 (scm_sym_gtype_instance_class_init, class);
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

    SCM_VALIDATE_GOBJECT_CLASS_COPY (1, class, gtype);
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

    scm_call_2 (scm_sym_gobject_class_install_property, class, param);

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
    quark_object = g_quark_from_static_string ("%scm-gtype->object");
    quark_guile_gtype_class = g_quark_from_static_string ("%scm-guile-gtype-class");
}

void
scm_init_gnome_gobject (void)
{

#ifndef SCM_MAGIC_SNARFER
#include "guile-gnome-gobject.x"
#endif

    scm_c_export (s_scm_gobject_scheme_dir,
		  s_scm_gobject_register_type, s_scm_gtype_children,
		  s_scm_gtype_to_fundamental, s_scm_gtype_to_class_name,
		  s_scm_gtype_to_method_name, s_scm_gtype_eq_p,
		  s_scm_gtype_p, s_scm_gtype_fundamental_p,
		  s_scm_genum_register_static, s_scm_gflags_register_static,
		  s_scm_gtype_register_static,
		  s_scm_gobject_class_install_property,
		  s_scm_gparam_to_value_type,
                  s_scm_sys_gobject_get_refcount,
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
}

void
scm_post_init_gnome_gobject (void)
{
    scm_sym_make = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("make")));
    scm_sym_gobject_get_property = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:get-property")));
    scm_sym_gobject_set_property = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:set-property")));
    scm_sym_gobject_class_install_property = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject-class:install-property")));
    scm_class_gobject = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gobject>")));
    scm_class_gparam = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gparam>")));
    scm_sym_gvalue_to_scm = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gvalue->scm")));
    scm_sym_gtype_instance_class_init = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype-instance:class-init")));
    scm_sym_gtype_instance_instance_init = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype-instance:instance-init")));
    scm_sym_gobject_class_init = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:class-init")));
    scm_sym_gobject_instance_init = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gobject:instance-init")));
    scm_sym_gtype_to_class = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype->class")));
}
