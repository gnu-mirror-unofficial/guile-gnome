/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-gobject-primitives.c: Primitive routines for the GObject wrapper
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
#include <guile-gnome-gobject-primitives.h>
#include "guile-support.h"

#include <glib-object.h>
#include <string.h>



SCM scm_class_gtype_class;
SCM scm_sym_gtype_instance_write;
SCM scm_sym_make_class;
SCM scm_sym_class_slot_ref;
SCM scm_sym_class_slot_set_x;
scm_t_bits scm_tc16_gtype;
scm_t_bits scm_tc16_gvalue;
scm_t_bits scm_tc16_gvalue_array;
scm_t_bits scm_tc16_gtype_class;
scm_t_bits scm_tc16_gtype_instance;
SCM scm_gsignal_vtable;
SCM scm_gparam_spec_vtable;
static SCM scm_sym_class_redefinition;



#define	MAX_STACK_VALUES (16)

static GQuark quark_pspec_struct = 0;
static GQuark quark_class = 0;
static GQuark quark_type = 0;
static GQuark quark_instance_wrapper = 0;
static GQuark quark_primitive_instance_wrapper = 0;

/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



SCM_SYMBOL  (sym_gruntime_error,"gruntime-error");
SCM_SYMBOL  (sym_gsignals,	"gsignals");

SCM_KEYWORD (k_name,		"name");
SCM_KEYWORD (k_class,		"class");
SCM_KEYWORD (k_specializers,	"specializers");
SCM_KEYWORD (k_procedure,	"procedure");
SCM_KEYWORD (k_accessor,	"accessor");
SCM_KEYWORD (k_getter,		"getter");
SCM_KEYWORD (k_allocation,	"allocation");
SCM_KEYWORD (k_each_subclass,	"each-subclass");
SCM_KEYWORD (k_read_only,	"read-only");
SCM_KEYWORD (k_init_value,	"init-value");
SCM_KEYWORD (k_value,		"value");
SCM_KEYWORD (k_metaclass,	"metaclass");
SCM_KEYWORD (k_gtype,		"gtype");

SCM_GLOBAL_SYMBOL  (scm_sym_gtype,		"gtype");
SCM_GLOBAL_SYMBOL  (scm_sym_gtype_class,	"gtype-class");
SCM_GLOBAL_SYMBOL  (scm_sym_gtype_instance,	"gtype-instance");
SCM_GLOBAL_SYMBOL  (scm_sym_pspec_struct,	"pspec-struct");



typedef struct {
    GType type;
    void (* sinkfunc)(GObject *object);
} SinkFunc;

static GArray *sink_funcs = NULL;

/* idea, code, and comments stolen from pygtk -- thanks, James :-) */
static inline void
sink_object (GObject *obj)
{
    if (sink_funcs) {
	gint i;

	for (i = 0; i < sink_funcs->len; i++) {
	    if (g_type_is_a (G_OBJECT_TYPE (obj),
                             g_array_index (sink_funcs, SinkFunc, i).type)) {
		g_array_index (sink_funcs, SinkFunc, i).sinkfunc (obj);

                DEBUG_ALLOC ("sunk gobject (%p) of type %s, ->%u",
                             obj, g_type_name (G_TYPE_FROM_INSTANCE (obj)),
                             obj->ref_count);
		break;
	    }
	}
    }
}

/**
 * guile_gobject_register_sinkfunc:
 * type: the GType the sink function applies to.
 * sinkfunc: a function to remove the floating reference on an object.
 *
 * As Guile handles memory management for us, the "floating reference" code in
 * GTK is not all that useful. In fact, it can cause leaks. For this reason,
 * guile-gobject removes the floating references on objects on construction.
 *
 * The sinkfunc should be able to remove the floating reference on
 * instances of the given type, or any subclasses.
 */
void
guile_gobject_register_sinkfunc (GType type, void (*sinkfunc) (GObject *))
{
    SinkFunc sf;

    if (!sink_funcs)
	sink_funcs = g_array_new (FALSE, FALSE, sizeof(SinkFunc));

    sf.type = type;
    sf.sinkfunc = sinkfunc;
    g_array_append_val (sink_funcs, sf);
}


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
guile_gobject_register_postmakefunc (GType type, gpointer (*postmakefunc) (gpointer))
{
    PostMakeFunc pmf;

    if (!post_make_funcs)
	post_make_funcs = g_array_new (FALSE, FALSE, sizeof(PostMakeFunc));

    pmf.type = type;
    pmf.postmakefunc = postmakefunc;
    g_array_append_val (post_make_funcs, pmf);
}


void
scm_c_debug_print (const gchar *pos, SCM value)
{
    SCM port;

    port = scm_current_output_port ();
    scm_display (scm_str2string (pos), port);
    scm_puts (" - ", port);
    scm_write (value, port);
    scm_newline (port);
}



static gpointer
copy_gboxed_scm (gpointer boxed)
{
    DEBUG_ALLOC (G_STRLOC ": copying gboxed %p", boxed);
    scm_gc_protect_object ((SCM) boxed);
    return boxed;
}

static void
free_gboxed_scm (gpointer boxed)
{
    DEBUG_ALLOC (G_STRLOC ": freeing gboxed %p", boxed);
    scm_gc_unprotect_object ((SCM) boxed);
}

GType
gboxed_scm_get_type (void)
{
    static GType boxed_type = 0;

    if (!boxed_type)
      boxed_type = g_boxed_type_register_static ("GBoxedSCM", copy_gboxed_scm, free_gboxed_scm); 

    return boxed_type;
}

SCM_DEFINE (scm_gboxed_scm_primitive_new, "gboxed-scm-primitive-new", 1, 0, 0,
	    (SCM scm_value),
	    "")
#define FUNC_NAME s_scm_gboxed_scm_primitive_new
{
    SCM retval;

    retval = scm_c_make_gvalue (G_TYPE_BOXED_SCM);
    g_value_set_boxed ((GValue *) SCM_SMOB_DATA (retval),
                       GINT_TO_POINTER (SCM_UNPACK (scm_value))); 

    return retval;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gboxed_scm_primitive_to_scm, "gboxed-scm-primitive->scm", 1, 0, 0,
	    (SCM value),
	    "")
#define FUNC_NAME s_scm_gboxed_scm_primitive_to_scm
{
    GValue *gvalue;
    gpointer p;

    SCM_VALIDATE_GVALUE_TYPE_COPY (1, value, G_TYPE_BOXED, gvalue);
    p = g_value_get_boxed (gvalue);
    /* prevent segfaults on uninitialized gboxed-scm values */
    return p ? SCM_PACK (GPOINTER_TO_INT (p)) : SCM_UNSPECIFIED;
}
#undef FUNC_NAME



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

static int
scm_gtype_class_print (SCM smob, SCM port, scm_print_state *pstate)
{
    GTypeClass *gtype_class = (GTypeClass*) SCM_SMOB_DATA (smob);

    scm_puts ("#<%gtype-class ", port);
    scm_puts (g_type_name (G_TYPE_FROM_CLASS (gtype_class)), port);
    scm_puts (">", port);

    return 1;
}



/* A GTypeInstance is a SMOB whose first word is the GTypeInstance* pointer, and
   whose second word is nothing. */
static size_t
scm_gtype_instance_free (SCM smob)
{
    GTypeInstance *instance = (GTypeInstance *) SCM_SMOB_DATA (smob);

    SCM_SET_SMOB_DATA (smob, NULL);

    if (!instance)
	return 0;

    switch (G_TYPE_FUNDAMENTAL (G_TYPE_FROM_INSTANCE (instance))) {
    case G_TYPE_OBJECT:
        /* unset the cached wrapper data, if there was any */
        g_object_set_qdata ((GObject*)instance, quark_instance_wrapper, NULL);
        g_object_set_qdata ((GObject*)instance, quark_primitive_instance_wrapper, NULL);
        DEBUG_ALLOC ("g_object_unref (%p) for SMOB %p: %u->%u", instance, smob,
                     ((GObject*)instance)->ref_count,
                     ((GObject*)instance)->ref_count - 1);
	g_object_unref (G_OBJECT (instance));
	break;

    case G_TYPE_PARAM:
        DEBUG_ALLOC ("g_param_spec_unref (%p) %u->%u", instance,
                     ((GParamSpec*)instance)->ref_count,
                     ((GParamSpec*)instance)->ref_count - 1);
	g_param_spec_unref (G_PARAM_SPEC (instance));
	break;

    default:
	g_type_free_instance (instance);
	break;
    }

    return 0;
}

SCM_DEFINE (scm_sys_gtype_instance_primitive_destroy_x, "%gtype-instance-primitive-destroy!", 1, 0, 0,
	    (SCM instance),
	    "")
#define FUNC_NAME s_scm_sys_gtype_instance_primitive_destroy_x
{
    SCM_VALIDATE_GTYPE_INSTANCE (1, instance);

    scm_gtype_instance_free (instance);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static int
scm_gtype_instance_print (SCM smob, SCM port, scm_print_state *pstate)
{
    GTypeInstance *instance = (GTypeInstance *) SCM_SMOB_DATA (smob);
    SCM class;

    class = g_type_get_qdata (G_TYPE_FROM_INSTANCE (instance), quark_class);
    if (!class)
	class = scm_c_register_gtype (G_TYPE_FROM_INSTANCE (instance));

    scm_call_3 (scm_sym_gtype_instance_write, class, smob, port);
    return 1;
}

static int
scm_gvalue_print (SCM smob, SCM port, scm_print_state *pstate)
{
    GValue *value = (GValue *) SCM_SMOB_DATA (smob);
    SCM class;

    class = g_type_get_qdata (G_VALUE_TYPE (value), quark_class);
    if (!class)
    class = scm_c_register_gtype (G_VALUE_TYPE (value));

    scm_call_3 (scm_sym_gtype_instance_write, class, smob, port);
    return 1;
}

static size_t
scm_gvalue_free (SCM smob)
{
    GValue *value = (GValue *) SCM_SMOB_DATA (smob);

    DEBUG_ALLOC ("freeing SMOB %p's GValue %p", smob, value);
    g_value_unset (value);
    scm_gc_free (value, sizeof (GValue), "%gvalue");

    return 0;
}

static int
scm_gvalue_array_print (SCM smob, SCM port, scm_print_state *pstate)
{
    GValueArray *varray = (GValueArray *) SCM_SMOB_DATA (smob);
    int i;
    const char *prefix = "#<<gvalue-array> (";
    const char *suffix = ")>";

    scm_c_write (port, prefix, strlen(prefix));
    for (i = 0; i < varray->n_values; i++)
    {
        GValue *val = g_value_array_get_nth(varray, i);
        SCM vsmob;

        /* [rotty:] I'm not sure this is really OK. It should be, if
         * the SMOB destructor is not called. */
        SCM_NEWSMOB (vsmob, scm_tc16_gvalue, val);
        scm_gvalue_print (vsmob, port, pstate);
    }
    scm_c_write (port, suffix, strlen(suffix));

    return 1;
}

static size_t
scm_gvalue_array_free (SCM smob)
{
    GValueArray *varray = (GValueArray *) SCM_SMOB_DATA (smob);
    scm_gc_unregister_collectable_memory (varray, sizeof (GValueArray),
                                          "%make-gvalue-array");
    g_value_array_free (varray);

    return 0;
}



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

    if (return_value) {
	GValue *gvalue;

	if (retval == SCM_UNSPECIFIED) {
	    SCM return_type;

	    return_type = scm_c_register_gtype (G_VALUE_TYPE (return_value));
	    scm_error (sym_gruntime_error, FUNC_NAME,
		       "GClosure expects a return value of type ~S, "
		       "but got the unspecified value: ~S",
		       SCM_LIST2 (return_type, gclosure->func),
		       SCM_EOL);
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
    scm_gc_unprotect_object (((GuileGClosure *) closure)->func);
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
    ((GuileGClosure *) closure)->func = scm_gc_protect_object (func);

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



gchar *
scm_c_make_gtype_name (const gchar *format, const gchar *name)
{
    gboolean on_word_start = TRUE;
    const gchar *start, *c;
    gchar *retval, *real_retval, *ptr;
    GPtrArray *words;
    size_t length, i;

    words = g_ptr_array_new ();

    length = strlen (name);

    for (c = start = name, i = 0; i <= length; c++, i++) {
	/* First character. */
	if (i == 0)
	    continue;

	if (g_ascii_islower (*c)) {
	    on_word_start = FALSE;
	    continue;
	}

	if (g_ascii_isupper (*c) || (*c == '\0') || (*c == '+')) {
	    if (on_word_start && (*c != '\0') && (*c != '+')) {
		continue;
	    } else {
		gchar *dest;
		size_t len;

		len = c-start;
		dest = g_malloc0 (len+1);
		memcpy (dest, start, len);
		g_ptr_array_add (words, g_strdown (dest));
		on_word_start = TRUE;
		start = c;
		if (*c == '+')
		    start++;
		continue;
	    }
	}
    }

    length = words->len + 1;
    for (i = 0; i < words->len; i++)
	length += strlen (g_ptr_array_index (words, i));

    retval = ptr = g_malloc0 (length);
    for (i = 0; i < words->len; i++) {
	if (i)
	    ptr = g_stpcpy (ptr, "-");
	ptr = g_stpcpy (ptr, g_ptr_array_index (words, i));
    }

    for (ptr = retval; *ptr; ptr++)
	if (*ptr == '_')
	    *ptr = '-';

    if (format) {
	real_retval = g_strdup_printf (format, retval);
	g_free (retval);
    } else
	real_retval = retval;

    for (i = 0; i < words->len; i++)
	g_free (g_ptr_array_index (words, i));
    
    g_ptr_array_free (words, TRUE);

    return real_retval;
}



SCM
scm_c_register_gtype (GType gtype)
{
    SCM type, object_name;
    const gchar *type_name;

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

SCM
scm_c_make_genum (GType gtype, gint value)
{
    SCM type, instance;

    type = scm_c_register_gtype (gtype);
    instance = scm_gtype_primitive_create_basic_instance (type);
    scm_gvalue_primitive_set_enum (instance, SCM_MAKINUM (value));

    return instance;
}

gint
scm_c_get_enum (SCM instance)
{
    GValue *gvalue = (GValue *) SCM_SMOB_DATA (instance);
    return g_value_get_enum (gvalue);
}

SCM
scm_c_make_gvalue (GType gtype)
{
    GValue *gvalue;
    SCM ret;

    gvalue = scm_gc_malloc (sizeof (GValue), "%gvalue");
    gvalue->g_type = 0;
    g_value_init (gvalue, gtype);

    SCM_NEWSMOB (ret, scm_tc16_gvalue, gvalue);
    DEBUG_ALLOC ("New GValue SMOB %p for %p of type %s", ret, gvalue, g_type_name (gtype));
    return ret;
}



SCM_DEFINE (scm_gvalue_primitive_new, "gvalue-primitive-new", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gvalue_primitive_new
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return scm_c_make_gvalue (gtype);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gvalue_array_primitive_new, "gvalue-array-primitive-new", 0, 0, 0,
	    (),
	    "")
#define FUNC_NAME s_scm_gvalue_array_primitive_new
{
    GValueArray *varray;

    varray = g_value_array_new (4);
    scm_gc_register_collectable_memory (varray, sizeof (GValueArray),
                                        "%make-gvalue-array");
    
    SCM_RETURN_NEWSMOB (scm_tc16_gvalue_array, varray);
}
#undef FUNC_NAME



/* The GObject wrapper (a short post-hoc re-design statement)
   ==========================================================

   Specifications:

     1. For each GObject wrapper alive in Guile-land, Guile needs to hold
        exactly one reference on the GObject.

     2. When a wrapper vanishes from Guile-land, i.e., is garbage-collected,
        Guile needs to drop its reference on the GObject.

     3. If a wrapper is created again after having been garbage-collected, we
        need to have a way of holding state with regards to the wrapped GObject,
        i.e. object properties.

     4. If the GObject itself goes away (not possible if there is a wrapper
        alive), any state associated with it should go away as well.

   Stipulation 4 means that the state must not hold a reference on the GObject,
   and thus cannot be associated with the wrapper. Fortunately, GObject itself
   provides such a mechanism, g_object_{set,get}_qdata. Therefore we need to
   provide a special interface so that we can attach arbitrary data to gobjects,
   as well as provide non-parameterized slots on derived GObjects.

   Stipulation 2 means that we need a way of knowing when the wrapper is
   garbage-collected. As far as I know, the only way to do this is via SMOBs. Of
   course we want the wrapper to be a GOOPS object, though. We have two options
   for this, either (somehow) representing the wrappers as GOOPS objects that
   are really SMOBs, or having a special SMOB, existing on a 1-to-1 basis with
   the wrapper, that will go away when the wrapper goes away. However, we do not
   want details of this arrangement leaking out into scheme-land.

   As the latter arrangement is easier to implement, and closer to the
   historical situation, we choose it over the former. The SMOB is then
   responsible for the ref and unref of the GObject. If the first reference of
   the GObject is actually owned by Guile, then the constructor function (i.e.
   gobject-primitive-create-instance) should unref the object. We might as well
   use the GObject as the SMOB's data. As long as we can retrieve the SMOB given
   the wrapper, this allows us to retrieve the GObject when we need to.

   It would be good, from an optimization perspective, not to unnecessarily
   create new wrappers when old ones are still alive. A cached piece of qdata on
   the GObject does the trick (see scm_c_gtype_instance_to_scm in -gobject.c).
   The cached data is removed when the SMOB is collected.

   OK, so far so good. The one remaining problem is with regards to closures. We
   have some more specifications here:

     1. A closure connected to a GObject signal should not be collected until
        either the closure is disconnected, or the object itself (not the
        wrapper) goes out of existence.

     2. Attaching closures to an object should not prevent the object from being
        freed. Otherwise, any object with a connected signal will live forever.
        Closures should only have a weak reference on an object.

   These specifications are mutually exclusive. Stipulation 2 means that a
   closure cannot be a permanent object, because its environment will always
   reference the wrapper. But for it to be invoked after the wrapper is
   collected would mean that its environment would have to be valid, which would
   always include the wrapper object. So, either the closure must be invalidated
   and collected when the object is collected (violating stipulation 1), or the
   object must be immortal as long as it has signals connected to it (violating
   stipulation 2).

   Since it is a common idiom to find an object, connect to it, and then forget
   about it, we will choose to violate stipulation 2. However, when
   %gtype-instance-primitive-destroy! is called on a GTypeInstance, we
   invalidate all guile closures.
*/

SCM
scm_c_make_gtype_instance (GTypeInstance *ginstance)
{
    SCM ret;

    switch (G_TYPE_FUNDAMENTAL (G_TYPE_FROM_INSTANCE (ginstance))) {
    case G_TYPE_OBJECT:
        if ((ret = g_object_get_qdata ((GObject*)ginstance,
                                       quark_primitive_instance_wrapper)))
            return ret;

        /* ref the object: see above */
        g_object_ref ((GObject*)ginstance);

        DEBUG_ALLOC ("reffed gobject (%p) of type %s, ->%u",
                     ginstance, g_type_name (G_TYPE_FROM_INSTANCE (ginstance)),
                     ((GObject*)ginstance)->ref_count);

        /* sink the floating ref, if any */
        sink_object ((GObject*)ginstance);
        SCM_NEWSMOB2 (ret, scm_tc16_gtype_instance, ginstance, NULL);

        /* cache the return value */
        g_object_set_qdata ((GObject*)ginstance, quark_primitive_instance_wrapper, ret);
        break;
      
    case G_TYPE_PARAM:
        /* ignoring floating status, the creation functions will sink if
         * necessary */
        g_param_spec_ref ((GParamSpec*)ginstance);

        DEBUG_ALLOC ("reffed param (%p) of type %s, ->%u", 
                     ginstance, g_type_name (G_TYPE_FROM_INSTANCE (ginstance)),
                     ((GParamSpec*)ginstance)->ref_count);
        SCM_NEWSMOB2 (ret, scm_tc16_gtype_instance, ginstance, NULL);
        break;

    default:
        SCM_NEWSMOB2 (ret, scm_tc16_gtype_instance, ginstance, NULL);
        break;
    }
        
    return ret;
}



SCM_DEFINE (scm_sys_gtype_lookup_class, "%gtype-lookup-class", 1, 0, 0,
	    (SCM type),
	    "")
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



SCM_DEFINE (scm_sys_gtype_bind_to_class, "%gtype-bind-to-class", 2, 0, 0,
	    (SCM class, SCM type),
	    "")
#define FUNC_NAME s_scm_sys_gtype_bind_to_class
{
    SCM type_class;
    GType gtype;
    GTypeClass *gtype_class;
    gint n_interfaces, i;
    GType *interfaces;

    SCM_VALIDATE_GTYPE_CLASS (1, class);
    SCM_VALIDATE_GTYPE_COPY (2, type, gtype);

    scm_slot_set_x (class, scm_sym_gtype, type);

    if (G_TYPE_IS_CLASSED (gtype)) {
        gtype_class = g_type_class_ref (gtype);
        SCM_NEWSMOB (type_class, scm_tc16_gtype_class, gtype_class);
        scm_slot_set_x (class, scm_sym_gtype_class, type_class);
    }

    g_type_set_qdata (gtype, quark_class, scm_permanent_object (class));

    /* It takes a g_type_class_ref () on an implementing class to initialize
     * signals on GInterfaces, so we loop through the implemented interfaces and
     * re-set their classes' gsignals slot. */
    interfaces = g_type_interfaces (gtype, &n_interfaces);
    for (i=0; i<n_interfaces; i++) {
        SCM class = scm_c_gtype_lookup_class (interfaces[i]);
        if (SCM_NFALSEP (class))
            scm_call_3 (scm_sym_class_slot_set_x, class, sym_gsignals,
                        scm_gtype_primitive_get_signals (scm_c_register_gtype (interfaces[i])));
    }
    if (interfaces)
        g_free (interfaces);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_especify_metaclass_x,
            "especify-metaclass!", 2, 0, 0,
	    (SCM class, SCM metaclass),
	    "")
#define FUNC_NAME s_scm_especify_metaclass_x
{
    SCM sgtype, new_class;
    GType gtype;
    
    SCM_VALIDATE_GTYPE_CLASS(1, class);

    if (!SCM_SUBCLASSP (metaclass, SCM_CLASS_OF (class)))
        scm_error (sym_gruntime_error, FUNC_NAME,
                   "New metaclass ~A is not a subclass of old metaclass ~S",
                   SCM_LIST2 (metaclass, SCM_CLASS_OF (class)),
                   SCM_EOL);

    sgtype = scm_slot_ref (class, scm_sym_gtype);
    gtype = (GType)SCM_SMOB_DATA (sgtype);

    /* unbind the type and the class */
    g_type_set_qdata (gtype, quark_class, NULL);
    
    new_class = scm_apply_0 (scm_sym_make_class,
                             SCM_LIST8 (scm_class_direct_supers (class),
                                        scm_class_direct_slots (class),
                                        k_name, scm_class_name (class),
                                        k_gtype, sgtype,
                                        k_metaclass, metaclass));
    scm_call_2 (scm_sym_class_redefinition, class, new_class);

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
    /* do i have to validate 'value' ? */

    SCM_SET_SLOT (object, SCM_INUM (offset), value);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_primitive_create_basic_instance, "gtype-primitive-create-basic-instance", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gtype_primitive_create_basic_instance
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    return scm_c_make_gvalue (gtype);
}
#undef FUNC_NAME



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



SCM_DEFINE (scm_gtype_primitive_get_signals, "gtype-primitive-get-signals", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gtype_primitive_get_signals
{
    guint *ids, n_ids, i;
    GTypeClass *type_class = NULL;
    GType gtype;
    SCM vector;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    if (G_TYPE_IS_CLASSED (gtype))
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

    if (type_class)
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



static SCM
print_gparam_spec_struct (SCM gparam_spec, SCM port)
{
    SCM args;
    guint length, i;

    length = SCM_GPARAM_SPEC_N_ARGS (gparam_spec);
    args = scm_c_make_vector (length, SCM_UNDEFINED);

    for (i = 0; i < length; i++)
	scm_vector_set_x (args, SCM_MAKINUM (i),
			  SCM_GPARAM_SPEC_ARG (gparam_spec, i));

    scm_display (scm_makfrom0str ("#<gparam-spec "), port);
    scm_write (SCM_LIST8 (SCM_PACK (SCM_STRUCT_DATA (gparam_spec) [scm_si_gparam_spec_name]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_spec) [scm_si_gparam_spec_nick]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_spec) [scm_si_gparam_spec_blurb]),
			  SCM_GPARAM_SPEC_FLAGS (gparam_spec),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_spec) [scm_si_gparam_spec_param_type]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_spec) [scm_si_gparam_spec_value_type]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_spec) [scm_si_gparam_spec_owner_type]),
			  args),
	       port);
    scm_display (scm_makfrom0str (">"), port);

    return SCM_UNSPECIFIED;
}



SCM_DEFINE (scm_gobject_primitive_get_properties, "gobject-primitive-get-properties", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gobject_primitive_get_properties
{
    GObjectClass *object_class;
    GParamSpec **properties;
    guint n_properties, i, count;
    GType gtype;
    SCM vector;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);
    object_class = G_OBJECT_CLASS (g_type_class_ref (gtype));

    properties = g_object_class_list_properties (object_class, &n_properties);

    for (i = count = 0; i < n_properties; i++)
	if (properties [i]->owner_type == gtype)
	    count++;

    vector = scm_make_vector (SCM_MAKINUM (count), SCM_UNDEFINED);

    for (i = count = 0; i < n_properties; i++) {
	SCM this;

	if (properties [i]->owner_type != gtype)
	    continue;

	this = scm_c_make_gtype_instance ((GTypeInstance *) properties [i]);

	scm_vector_set_x (vector, SCM_MAKINUM (count), this);
	count++;
    }

    g_type_class_unref (object_class);

    g_free (properties);

    return vector;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gparam_primitive_create_pspec_struct, "gparam-primitive-create-pspec-struct", 1, 0, 0,
	    (SCM param),
	    "")
#define FUNC_NAME s_scm_gparam_primitive_create_pspec_struct
{
    GParamSpec *pspec;
    guint n_args = 0;
    SCM this;
    char *blurb;
    
    SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (1, param, G_TYPE_PARAM, GParamSpec, pspec);

    if (G_IS_PARAM_SPEC_BOOLEAN (pspec) || G_IS_PARAM_SPEC_STRING (pspec) ||
	G_IS_PARAM_SPEC_OBJECT(pspec) || G_IS_PARAM_SPEC_BOXED (pspec) ||
        G_IS_PARAM_SPEC_UNICHAR (pspec) || G_IS_PARAM_SPEC_VALUE_ARRAY (pspec))
      n_args = 1;
    
    else if (G_IS_PARAM_SPEC_CHAR (pspec) || G_IS_PARAM_SPEC_UCHAR (pspec) ||
	     G_IS_PARAM_SPEC_INT (pspec) || G_IS_PARAM_SPEC_UINT (pspec) ||
	     G_IS_PARAM_SPEC_LONG (pspec) || G_IS_PARAM_SPEC_ULONG (pspec) ||
	     G_IS_PARAM_SPEC_INT64 (pspec) || G_IS_PARAM_SPEC_UINT64 (pspec) ||
	     G_IS_PARAM_SPEC_FLOAT (pspec) || G_IS_PARAM_SPEC_DOUBLE(pspec))
      n_args = 3;

    else if (G_IS_PARAM_SPEC_POINTER (pspec))
      n_args = 0;

    else if ( G_IS_PARAM_SPEC_ENUM (pspec) ||  G_IS_PARAM_SPEC_FLAGS (pspec))
      n_args = 2;

    else
      SCM_ERROR_NOT_YET_IMPLEMENTED (param);
      
    this = scm_make_struct (scm_gparam_spec_vtable, SCM_MAKINUM (n_args),
                            SCM_EOL);

    SCM_SET_GPARAM_SPEC_NAME (this, (char *) g_param_spec_get_name (pspec)); //ariel
    SCM_SET_GPARAM_SPEC_NICK (this, (char *) g_param_spec_get_nick (pspec));
    blurb = (char *) g_param_spec_get_blurb (pspec);
    SCM_SET_GPARAM_SPEC_BLURB (this, blurb ? blurb : "");
    SCM_SET_GPARAM_SPEC_FLAGS (this, SCM_MAKINUM (pspec->flags));
    SCM_SET_GPARAM_SPEC_PARAM_TYPE (this, G_TYPE_FROM_INSTANCE (pspec));
    SCM_SET_GPARAM_SPEC_VALUE_TYPE (this, pspec->value_type);
    SCM_SET_GPARAM_SPEC_OWNER_TYPE (this, pspec->owner_type);

    
    
    if (G_IS_PARAM_SPEC_BOOLEAN (pspec)){
      GParamSpecBoolean *b = (GParamSpecBoolean *)pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, SCM_BOOL(b->default_value));//(((GParamSpecBoolean *)pspec)->default_value));
    }
      
    else if (G_IS_PARAM_SPEC_CHAR (pspec)){
      GParamSpecChar *c = (GParamSpecChar *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, SCM_MAKINUM (c->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, SCM_MAKINUM (c->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, SCM_MAKINUM (c->default_value));
    }
    
    else if  (G_IS_PARAM_SPEC_UCHAR (pspec)) {
      GParamSpecUChar *u = (GParamSpecUChar *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, SCM_MAKINUM (u->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, SCM_MAKINUM (u->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, SCM_MAKINUM (u->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_INT (pspec)) {
      GParamSpecInt *i = (GParamSpecInt *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_long2num (i->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_long2num (i->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_long2num (i->default_value));
    }

    else if (G_IS_PARAM_SPEC_UINT (pspec)) {
      GParamSpecUInt *i = (GParamSpecUInt *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_ulong2num (i->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_ulong2num (i->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_ulong2num (i->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_LONG (pspec)) {
      GParamSpecLong *l = (GParamSpecLong *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_long2num (l->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_long2num (l->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_long2num (l->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_ULONG (pspec)) {
      GParamSpecULong *u = (GParamSpecULong *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_ulong2num (u->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_ulong2num (u->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_ulong2num (u->default_value));
    }

    else if (G_IS_PARAM_SPEC_INT64 (pspec)) {
      GParamSpecInt64 *l = (GParamSpecInt64 *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_long_long2num (l->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_long_long2num (l->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_long_long2num (l->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_UINT64 (pspec)) {
      GParamSpecUInt64 *u = (GParamSpecUInt64 *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_ulong_long2num (u->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_ulong_long2num (u->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_ulong_long2num (u->default_value));
    }

    else if (G_IS_PARAM_SPEC_FLOAT (pspec)) {
      GParamSpecFloat *f = (GParamSpecFloat *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_make_real (f->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_make_real (f->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_make_real (f->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_DOUBLE (pspec)) {
      GParamSpecDouble *d = (GParamSpecDouble *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_make_real (d->minimum));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, scm_make_real (d->maximum));
      SCM_SET_GPARAM_SPEC_ARG (this, 2, scm_make_real (d->default_value));
    }

    /* this is borken, but it will suffice for now... */
    else if (G_IS_PARAM_SPEC_UNICHAR (pspec)) {
      GParamSpecUnichar *u = (GParamSpecUnichar *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_long2num ((long)u->default_value));
    }

    else if (G_IS_PARAM_SPEC_POINTER (pspec)) { }
    
    else if (G_IS_PARAM_SPEC_STRING (pspec)) {
      GParamSpecString *s = (GParamSpecString *) pspec;
      SCM_SET_GPARAM_SPEC_ARG (
              this, 0, (s->default_value ? scm_str2string (s->default_value)
                        : SCM_BOOL_F));
    }
    
    else if (G_IS_PARAM_SPEC_OBJECT (pspec) || G_IS_PARAM_SPEC_BOXED (pspec)) {
        SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_c_register_gtype (pspec->value_type));
    }
    
    else if (G_IS_PARAM_SPEC_ENUM (pspec)){
      GParamSpecEnum *e = (GParamSpecEnum *) pspec;
      GType enum_type = G_TYPE_FROM_CLASS (e->enum_class);
      
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_c_register_gtype (enum_type));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, SCM_MAKINUM (e->default_value));
     }
    
    else if (G_IS_PARAM_SPEC_FLAGS (pspec)){
      GParamSpecFlags *f = (GParamSpecFlags *) pspec;
      GType flags_type = G_TYPE_FROM_CLASS (f->flags_class);
      
      SCM_SET_GPARAM_SPEC_ARG (this, 0, scm_c_register_gtype (flags_type));
      SCM_SET_GPARAM_SPEC_ARG (this, 1, SCM_MAKINUM (f->default_value));
    }
    else if (G_IS_PARAM_SPEC_VALUE_ARRAY (pspec))
    {
        GParamSpecValueArray *va = (GParamSpecValueArray *) pspec;

        SCM_SET_GPARAM_SPEC_ARG (this, 0, (va->element_spec
                                           ? scm_c_make_gtype_instance 
                                           ((GTypeInstance*)va->element_spec)
                                           : SCM_BOOL_F));
    }
    else
      SCM_ERROR_NOT_YET_IMPLEMENTED (param);
        
    return this;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gparam_spec_p, "gparam-spec?", 1, 0, 0,
	    (SCM pspec_struct),
	    "")
#define FUNC_NAME s_scm_gparam_spec_p
{
    return SCM_GPARAM_SPEC_P (pspec_struct) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gparam_primitive_to_pspec_struct, "gparam-primitive->pspec-struct", 1, 0, 0,
	    (SCM param),
	    "")
#define FUNC_NAME s_scm_gparam_primitive_to_pspec_struct
{
    GParamSpec *pspec;
    SCM pspec_struct;

    SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (1, param, G_TYPE_PARAM, GParamSpec, pspec);

    pspec_struct = g_param_spec_get_qdata (pspec, quark_pspec_struct);
    if (pspec_struct)
	return pspec_struct;

    pspec_struct = scm_gparam_primitive_create_pspec_struct (param);

/* DEBUG_ALLOC ("  protecting new pspec-struct; but when the pspec dies it will"
                " be unreffed"); */

    g_param_spec_set_qdata_full (pspec, quark_pspec_struct,
				 scm_gc_protect_object (pspec_struct),
				 (GDestroyNotify) scm_gc_unprotect_object);

    return pspec_struct;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gparam_primitive_create, "gparam-primitive-create", 4, 0, 0,
	    (SCM class, SCM type, SCM object, SCM pspec_struct),
	    "")
#define FUNC_NAME s_scm_gparam_primitive_create
{
    GParamSpec *pspec = NULL;
    GParamFlags flags;
    GType gtype, param_type, value_type;
    guint n_args = 0;
    SCM smob;

    SCM_VALIDATE_GTYPE_CLASS (1, class);
    SCM_VALIDATE_GTYPE_COPY (2, type, gtype);
    SCM_ASSERT (SCM_IS_A_P (SCM_CLASS_OF (object), scm_class_gtype_class), object, 3, FUNC_NAME);
    SCM_ASSERT (G_TYPE_IS_PARAM (gtype), type, 2, FUNC_NAME);
    SCM_VALIDATE_GPARAM_SPEC (4, pspec_struct);

    param_type = SCM_GPARAM_SPEC_PARAM_TYPE (pspec_struct);
    value_type = SCM_GPARAM_SPEC_VALUE_TYPE (pspec_struct);
    flags = SCM_NUM2INT (4, SCM_GPARAM_SPEC_FLAGS (pspec_struct));
      
    if (SCM_G_IS_PARAM_SPEC_BOOLEAN(param_type)
        || SCM_G_IS_PARAM_SPEC_STRING(param_type)
        || SCM_G_IS_PARAM_SPEC_OBJECT(param_type)
        || SCM_G_IS_PARAM_SPEC_BOXED(param_type)
        || SCM_G_IS_PARAM_SPEC_UNICHAR(param_type)
        || SCM_G_IS_PARAM_SPEC_VALUE_ARRAY (param_type))
      n_args = 1;
    
    else if (SCM_G_IS_PARAM_SPEC_CHAR (param_type)
             || SCM_G_IS_PARAM_SPEC_UCHAR (param_type)
             || SCM_G_IS_PARAM_SPEC_INT (param_type)
             || SCM_G_IS_PARAM_SPEC_UINT (param_type)
             || SCM_G_IS_PARAM_SPEC_LONG (param_type)
             || SCM_G_IS_PARAM_SPEC_ULONG (param_type)
	     || SCM_G_IS_PARAM_SPEC_INT64 (param_type)
             || SCM_G_IS_PARAM_SPEC_UINT64 (param_type)
	     || SCM_G_IS_PARAM_SPEC_FLOAT (param_type)
             || SCM_G_IS_PARAM_SPEC_DOUBLE(param_type))
      n_args = 3;

    else if (SCM_G_IS_PARAM_SPEC_POINTER (param_type))
      n_args = 0;

    else if (SCM_G_IS_PARAM_SPEC_ENUM (param_type) ||  SCM_G_IS_PARAM_SPEC_FLAGS (param_type))
      n_args = 2;

    else
      SCM_ERROR_NOT_YET_IMPLEMENTED (pspec_struct);
    
       
    SCM_ASSERT ((SCM_GPARAM_SPEC_N_ARGS (pspec_struct) == n_args), pspec_struct, 4, FUNC_NAME); 
        

    if (SCM_G_IS_PARAM_SPEC_BOOLEAN (param_type)){
      SCM_VALIDATE_BOOL (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0));
      pspec = g_param_spec_boolean (SCM_GPARAM_SPEC_NAME (pspec_struct),
				    SCM_GPARAM_SPEC_NICK (pspec_struct),
				    SCM_GPARAM_SPEC_BLURB (pspec_struct),
				    SCM_NFALSEP (SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
				    flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_CHAR (param_type)){
      SCM_VALIDATE_INUM_RANGE (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), -128, 128);
      SCM_VALIDATE_INUM_RANGE (1, SCM_GPARAM_SPEC_ARG (pspec_struct, 1), -128, 128);
      SCM_VALIDATE_INUM_RANGE (2, SCM_GPARAM_SPEC_ARG (pspec_struct, 2), -128, 128);
      pspec = g_param_spec_char (SCM_GPARAM_SPEC_NAME (pspec_struct),
				 SCM_GPARAM_SPEC_NICK (pspec_struct),
				 SCM_GPARAM_SPEC_BLURB (pspec_struct),
				 SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
				 SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				 SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
				 flags);
    }

    else if  (SCM_G_IS_PARAM_SPEC_UCHAR (param_type)){
      SCM_VALIDATE_INUM_RANGE (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), 0, 256);
      SCM_VALIDATE_INUM_RANGE (1, SCM_GPARAM_SPEC_ARG (pspec_struct, 1), 0, 256);
      SCM_VALIDATE_INUM_RANGE (2, SCM_GPARAM_SPEC_ARG (pspec_struct, 2), 0, 256);
      pspec = g_param_spec_uchar (SCM_GPARAM_SPEC_NAME (pspec_struct),
				  SCM_GPARAM_SPEC_NICK (pspec_struct),
				  SCM_GPARAM_SPEC_BLURB (pspec_struct),
				  SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
				  SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				  SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
				  flags);
    }
    
    else if (SCM_G_IS_PARAM_SPEC_INT (param_type)) {
      pspec = g_param_spec_int (SCM_GPARAM_SPEC_NAME (pspec_struct),
				SCM_GPARAM_SPEC_NICK (pspec_struct),
				SCM_GPARAM_SPEC_BLURB (pspec_struct),
				SCM_NUM2LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
				SCM_NUM2LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				SCM_NUM2LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
				flags);
    }
    
    else if (SCM_G_IS_PARAM_SPEC_UINT (param_type)) {
      pspec = g_param_spec_uint (SCM_GPARAM_SPEC_NAME (pspec_struct),
				   SCM_GPARAM_SPEC_NICK (pspec_struct),
				   SCM_GPARAM_SPEC_BLURB (pspec_struct),
				   SCM_NUM2ULONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
				   SCM_NUM2ULONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				   SCM_NUM2ULONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
				  flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_LONG (param_type)) {
      pspec = g_param_spec_long (SCM_GPARAM_SPEC_NAME (pspec_struct),
				   SCM_GPARAM_SPEC_NICK (pspec_struct),
				   SCM_GPARAM_SPEC_BLURB (pspec_struct),
				   SCM_NUM2LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
				   SCM_NUM2LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				   SCM_NUM2LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
				   flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_ULONG (param_type)) {
      pspec = g_param_spec_ulong (SCM_GPARAM_SPEC_NAME (pspec_struct),
				  SCM_GPARAM_SPEC_NICK (pspec_struct),
				  SCM_GPARAM_SPEC_BLURB (pspec_struct),
				  SCM_NUM2ULONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
				  SCM_NUM2ULONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				  SCM_NUM2ULONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
				  flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_INT64 (param_type)) {
      pspec = g_param_spec_int64 (SCM_GPARAM_SPEC_NAME (pspec_struct),
                                  SCM_GPARAM_SPEC_NICK (pspec_struct),
                                  SCM_GPARAM_SPEC_BLURB (pspec_struct),
                                  SCM_NUM2LONG_LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
                                  SCM_NUM2LONG_LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
                                  SCM_NUM2LONG_LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
                                  flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_UINT64 (param_type)) {
      pspec = g_param_spec_uint64 (SCM_GPARAM_SPEC_NAME (pspec_struct),
                                   SCM_GPARAM_SPEC_NICK (pspec_struct),
                                   SCM_GPARAM_SPEC_BLURB (pspec_struct),
                                   SCM_NUM2ULONG_LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
                                   SCM_NUM2ULONG_LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
                                   SCM_NUM2ULONG_LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 2)),
                                   flags);
    }

     else if (SCM_G_IS_PARAM_SPEC_FLOAT (param_type)) {
       float min_value, max_value, default_value;

	SCM_VALIDATE_FLOAT_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), min_value);
	SCM_VALIDATE_FLOAT_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 1), max_value);
	SCM_VALIDATE_FLOAT_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 2), default_value);

	pspec = g_param_spec_float (SCM_GPARAM_SPEC_NAME (pspec_struct),
				    SCM_GPARAM_SPEC_NICK (pspec_struct),
				    SCM_GPARAM_SPEC_BLURB (pspec_struct),
				    min_value, max_value, default_value,
				    flags);
     }

     else if (SCM_G_IS_PARAM_SPEC_DOUBLE (param_type)) {
       pspec = g_param_spec_double (SCM_GPARAM_SPEC_NAME (pspec_struct),
				     SCM_GPARAM_SPEC_NICK (pspec_struct),
				     SCM_GPARAM_SPEC_BLURB (pspec_struct),
				     scm_num2dbl (SCM_GPARAM_SPEC_ARG (pspec_struct, 0), FUNC_NAME),
				     scm_num2dbl (SCM_GPARAM_SPEC_ARG (pspec_struct, 1), FUNC_NAME),
				     scm_num2dbl (SCM_GPARAM_SPEC_ARG (pspec_struct, 2), FUNC_NAME),
				     flags);
     }

     else if (SCM_G_IS_PARAM_SPEC_UNICHAR (param_type)) {
       pspec = g_param_spec_unichar (SCM_GPARAM_SPEC_NAME (pspec_struct),
                                     SCM_GPARAM_SPEC_NICK (pspec_struct),
                                     SCM_GPARAM_SPEC_BLURB (pspec_struct),
                                     (gunichar)SCM_NUM2LONG (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0)),
                                     flags);
     }

     else if (SCM_G_IS_PARAM_SPEC_POINTER (param_type)) {
       pspec = g_param_spec_pointer (SCM_GPARAM_SPEC_NAME (pspec_struct),
				      SCM_GPARAM_SPEC_NICK (pspec_struct),
				      SCM_GPARAM_SPEC_BLURB (pspec_struct),
				      flags);
     }

    else if (SCM_G_IS_PARAM_SPEC_STRING (param_type)) {
      const gchar *string = NULL;

	if (SCM_NFALSEP (SCM_GPARAM_SPEC_ARG (pspec_struct, 0)))
	    SCM_VALIDATE_STRING_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), string);

	pspec = g_param_spec_string (SCM_GPARAM_SPEC_NAME (pspec_struct),
				     SCM_GPARAM_SPEC_NICK (pspec_struct),
				     SCM_GPARAM_SPEC_BLURB (pspec_struct),
				     string,
				     flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_OBJECT (param_type)) {
      GType object_type;

      SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), object_type);
      pspec = g_param_spec_object (SCM_GPARAM_SPEC_NAME (pspec_struct),
				   SCM_GPARAM_SPEC_NICK (pspec_struct),
				   SCM_GPARAM_SPEC_BLURB (pspec_struct),
				   object_type,
				   flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_BOXED (param_type)) {
      GType boxed_type;

	SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), boxed_type);
	pspec = g_param_spec_boxed (SCM_GPARAM_SPEC_NAME (pspec_struct),
				    SCM_GPARAM_SPEC_NICK (pspec_struct),
				    SCM_GPARAM_SPEC_BLURB (pspec_struct),
				    boxed_type,
				    flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_ENUM (param_type)){
      GType enum_type;

	SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), enum_type);
	pspec = g_param_spec_enum (SCM_GPARAM_SPEC_NAME (pspec_struct),
				   SCM_GPARAM_SPEC_NICK (pspec_struct),
				   SCM_GPARAM_SPEC_BLURB (pspec_struct),
				   enum_type,
				   SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				   flags);
    }

    else if (SCM_G_IS_PARAM_SPEC_FLAGS (param_type)){
       GType flags_type;

       SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_SPEC_ARG (pspec_struct, 0), flags_type);
       pspec = g_param_spec_flags (SCM_GPARAM_SPEC_NAME (pspec_struct),
				   SCM_GPARAM_SPEC_NICK (pspec_struct),
				   SCM_GPARAM_SPEC_BLURB (pspec_struct),
				   flags_type,
				   SCM_INUM (SCM_GPARAM_SPEC_ARG (pspec_struct, 1)),
				   flags);
    }
    else if (SCM_G_IS_PARAM_SPEC_VALUE_ARRAY (param_type))
    {
        pspec = g_param_spec_value_array (SCM_GPARAM_SPEC_NAME (pspec_struct),
                                          SCM_GPARAM_SPEC_NICK (pspec_struct),
                                          SCM_GPARAM_SPEC_BLURB (pspec_struct),
                                          NULL,
                                          flags);
    }
    else {SCM_ERROR_NOT_YET_IMPLEMENTED (pspec_struct);}
			  
    if (!pspec)
      scm_error (sym_gruntime_error, FUNC_NAME,
		   "Can't create gparam instance ~A from this pspec struct: ~A",
		   SCM_LIST2 (type, pspec_struct), SCM_EOL);

    smob = scm_c_make_gtype_instance ((GTypeInstance *) pspec);
    /* remove floating reference (we still hold the one from
     * make_gtype_instance) */
    g_param_spec_sink (pspec);
    DEBUG_ALLOC ("sunk guile-owned param spec %p of type %s, ->%u", 
                 pspec, g_type_name (G_TYPE_FROM_INSTANCE (pspec)),
                 pspec->ref_count);

    scm_slot_set_x (object, scm_sym_gtype_instance, smob);

    scm_gparam_primitive_create_pspec_struct (smob);

    return smob;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_instance_primitive_to_type, "gtype-instance-primitive->type", 1, 0, 0,
	    (SCM instance),
	    "")
#define FUNC_NAME s_scm_gtype_instance_primitive_to_type
{
    GTypeInstance *ginstance;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);

    return scm_c_register_gtype (G_TYPE_FROM_INSTANCE (ginstance));
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_instance_primitive_to_value, "gtype-instance-primitive->value", 1, 0, 0,
	    (SCM instance),
	    "")
#define FUNC_NAME s_scm_gtype_instance_primitive_to_value
{
    SCM retval = SCM_UNSPECIFIED;
    GTypeInstance *ginstance;
    GType gtype;

    SCM_VALIDATE_GTYPE_INSTANCE_COPY (1, instance, ginstance);

    gtype = G_TYPE_FROM_INSTANCE (ginstance);

    switch (G_TYPE_FUNDAMENTAL (gtype)) {
    case G_TYPE_OBJECT:
	retval = scm_c_make_gvalue (gtype);
	g_value_set_object ((GValue *) SCM_SMOB_DATA (retval), G_OBJECT (ginstance));
	break;

    default:
	SCM_ERROR_NOT_YET_IMPLEMENTED (instance);
    }

    return retval;
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
      scm_error (sym_gruntime_error, FUNC_NAME,
                 "Don't know what to do with object of "
                 "type ~A: ~S",
                 SCM_LIST2 (scm_makfrom0str (g_type_name (gtype)), object),
                 SCM_EOL);
      

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
        if (!SCM_GVALUE_ARRAYP (SCM_CDR (this)))
            SCM_VALIDATE_GVALUE (4, SCM_CDR (this));
    }
    
    params = g_new0 (GParameter, length);

    for (i = 0; i < length; i++) {
	const GValue *gvalue;
	SCM this = scm_vector_ref (properties, SCM_MAKINUM (i));
	GParameter *current = &params [i];

	current->name = SCM_SYMBOL_CHARS (SCM_CAR (this));
	current->value.g_type = 0;
        if (SCM_GVALUE_ARRAYP (SCM_CDR (this))) {
            g_value_init (&current->value, G_TYPE_VALUE_ARRAY);
            g_value_set_boxed (&current->value,
                               (GValueArray *)SCM_SMOB_DATA (SCM_CDR (this)));
        }
        else {
            SCM_VALIDATE_GVALUE_COPY (4, SCM_CDR (this), gvalue);
            g_value_init (&current->value, G_VALUE_TYPE (gvalue));
            g_value_copy (gvalue, &current->value);
        }
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
    g_object_set_qdata (gobject, quark_instance_wrapper, object);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gvalue_p, "gvalue?", 1, 0, 0,
	    (SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_p
{
    return SCM_TYP16_PREDICATE (scm_tc16_gvalue, value) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gvalue_to_type, "gvalue->type", 1, 0, 0,
	    (SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_to_type
{
    GValue *gvalue;

    SCM_VALIDATE_GVALUE_COPY (1, value, gvalue);

    return scm_c_register_gtype (G_VALUE_TYPE (gvalue));
}
#undef FUNC_NAME



SCM_DEFINE (scm_genum_primitive_get_values, "genum-primitive-get-values", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_genum_primitive_get_values
{
    GType gtype;
    GEnumClass *enum_class;
    SCM vector;
    guint i;

    SCM_VALIDATE_GTYPE_IS_A (1, type, G_TYPE_ENUM, gtype);

    enum_class = g_type_class_ref (gtype);

    vector = scm_c_make_vector (enum_class->n_values, SCM_UNDEFINED);

    for (i = 0; i < enum_class->n_values; i++) {
	GEnumValue *current = &enum_class->values [i];
	SCM this;

	this = scm_list_3 (scm_mem2symbol (current->value_nick,
					   strlen (current->value_nick)),
			   scm_makfrom0str (current->value_name),
			   SCM_MAKINUM (current->value));

	scm_vector_set_x (vector, SCM_MAKINUM (i), this);
    }

    g_type_class_unref (enum_class);

    return vector;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gflags_primitive_get_values, "gflags-primitive-get-values", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gflags_primitive_get_values
{
    GType gtype;
    GFlagsClass *flags_class;
    SCM vector;
    guint i;

    SCM_VALIDATE_GTYPE_IS_A (1, type, G_TYPE_FLAGS, gtype);

    flags_class = g_type_class_ref (gtype);

    vector = scm_c_make_vector (flags_class->n_values, SCM_UNDEFINED);

    for (i = 0; i < flags_class->n_values; i++) {
	GFlagsValue *current = &flags_class->values [i];
	SCM this;

	this = scm_list_3 (scm_mem2symbol (current->value_nick,
					   strlen (current->value_nick)),
			   scm_makfrom0str (current->value_name),
			   SCM_MAKINUM (current->value));

	scm_vector_set_x (vector, SCM_MAKINUM (i), this);
    }

    g_type_class_unref (flags_class);

    return vector;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gvalue_primitive_set_enum, "gvalue-primitive-set-enum", 2, 0, 0,
	    (SCM instance, SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_primitive_set_enum
{
    GValue *gvalue;
    GEnumClass *enum_class;

    SCM_VALIDATE_GVALUE_TYPE_COPY (1, instance, G_TYPE_ENUM, gvalue);

    enum_class = g_type_class_ref (G_VALUE_TYPE (gvalue));

    SCM_ASSERT (SCM_INUMP (value) &&
		(SCM_INUM (value) >= enum_class->minimum) &&
		(SCM_INUM (value) <= enum_class->maximum),
		value, SCM_ARG2, FUNC_NAME);

    g_value_set_enum (gvalue, SCM_INUM (value));

    g_type_class_unref (enum_class);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gvalue_primitive_set_flags, "gvalue-primitive-set-flags", 2, 0, 0,
	    (SCM instance, SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_primitive_set_flags
{
    GValue *gvalue;
    GFlagsClass *flags_class;

    SCM_VALIDATE_GVALUE_TYPE_COPY (1, instance, G_TYPE_FLAGS, gvalue);

    flags_class = g_type_class_ref (G_VALUE_TYPE (gvalue));

    SCM_ASSERT (SCM_INUMP (value) &&
		((SCM_INUM (value) & flags_class->mask) == SCM_INUM (value)),
		value, SCM_ARG2, FUNC_NAME);

    g_value_set_flags (gvalue, SCM_INUM (value));

    g_type_class_unref (flags_class);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gflags_primitive_bit_set_p, "gflags-primitive-bit-set?", 2, 0, 0,
	    (SCM value, SCM bit),
	    "")
#define FUNC_NAME s_scm_gflags_primitive_bit_set_p
{
    SCM_VALIDATE_INUM (1, value);
    SCM_VALIDATE_INUM (2, bit);

    return (SCM_INUM (value) & SCM_INUM (bit)) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gtype_interfaces, "gtype-interfaces", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gtype_interfaces
{
    GType gtype, *interfaces;
    gint n_interfaces, i;
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

SCM_DEFINE (scm_gtype_primitive_basic_p, "gtype-primitive-basic?", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gtype_primitive_basic_p
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    if (!G_TYPE_IS_FUNDAMENTAL (gtype))
	return SCM_BOOL_F;

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
	return SCM_BOOL_F;
    }
}
#undef FUNC_NAME



SCM_DEFINE (scm_gvalue_primitive_set, "gvalue-primitive-set", 2, 0, 0,
	    (SCM instance, SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_primitive_set
{
    GValue *gvalue;

    SCM_VALIDATE_GVALUE_COPY (1, instance, gvalue);

    switch (G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (gvalue))) {
    case G_TYPE_CHAR:
	SCM_VALIDATE_CHAR (2, value);
	g_value_set_char (gvalue, SCM_CHAR (value));
	break;

    case G_TYPE_UCHAR:
	SCM_VALIDATE_CHAR (2, value);
	g_value_set_uchar (gvalue, SCM_CHAR (value));
	break;

    case G_TYPE_BOOLEAN:
	SCM_VALIDATE_BOOL (2, value);
	g_value_set_boolean (gvalue, SCM_NFALSEP (value));
	break;

    case G_TYPE_INT:
	g_value_set_int (gvalue, SCM_NUM2INT (2, value));
	break;

    case G_TYPE_UINT:
	g_value_set_uint (gvalue, SCM_NUM2UINT (2, value));
	break;

    case G_TYPE_LONG:
	g_value_set_long (gvalue, SCM_NUM2LONG (2, value));
	break;

    case G_TYPE_ULONG:
	g_value_set_ulong (gvalue, SCM_NUM2ULONG (2, value));
	break;

    case G_TYPE_INT64:
	g_value_set_int64 (gvalue, SCM_NUM2INT (2, value));
	break;

    case G_TYPE_UINT64:
	g_value_set_uint64 (gvalue, SCM_NUM2UINT (2, value));
	break;

    case G_TYPE_FLOAT: {
	double x = scm_num2dbl (value, FUNC_NAME);
	SCM_ASSERT_RANGE (2, value, (- G_MAXFLOAT < x) && (x < G_MAXFLOAT));
	g_value_set_float (gvalue, (float) x);
	break;
    }

    case G_TYPE_DOUBLE:
	g_value_set_double (gvalue, scm_num2dbl (value, FUNC_NAME));
	break;

    case G_TYPE_STRING:
	SCM_ASSERT (SCM_STRINGP (value) || SCM_FALSEP (value),
		    value, SCM_ARG2, FUNC_NAME);
	if (SCM_FALSEP (value))
	    g_value_set_string (gvalue, NULL);
	else
	    g_value_set_string (gvalue, g_strdup (SCM_STRING_CHARS (value)));
	break;

    default:
	scm_wrong_type_arg (FUNC_NAME, SCM_ARG1, instance);
	break;
    }

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gvalue_primitive_get, "gvalue-primitive-get", 1, 0, 0,
	    (SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_primitive_get
{
    GValue *gvalue;

    SCM_VALIDATE_GVALUE_COPY (1, value, gvalue);

    switch (G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (gvalue))) {
    case G_TYPE_CHAR:
	return SCM_MAKE_CHAR (g_value_get_char (gvalue));

    case G_TYPE_UCHAR:
	return SCM_MAKE_CHAR (g_value_get_uchar (gvalue));

    case G_TYPE_BOOLEAN:
	return SCM_BOOL (g_value_get_boolean (gvalue));

    case G_TYPE_INT:
	return SCM_MAKINUM (g_value_get_int (gvalue));

    case G_TYPE_UINT:
	return SCM_MAKINUM (g_value_get_uint (gvalue));

    case G_TYPE_LONG:
	return scm_long2num (g_value_get_long (gvalue));

    case G_TYPE_ULONG:
	return scm_ulong2num (g_value_get_ulong (gvalue));

    case G_TYPE_INT64:
	return scm_long_long2num (g_value_get_int64 (gvalue));

    case G_TYPE_UINT64:
	return scm_ulong_long2num (g_value_get_uint64 (gvalue));

    case G_TYPE_FLOAT:
	return scm_make_real ((double) g_value_get_float (gvalue));

    case G_TYPE_DOUBLE:
	return scm_make_real (g_value_get_double (gvalue));

    case G_TYPE_ENUM:
	return SCM_MAKINUM (g_value_get_enum (gvalue));

    case G_TYPE_FLAGS:
	return SCM_MAKINUM (g_value_get_flags (gvalue));

    case G_TYPE_STRING:
	return scm_makfrom0str (g_value_get_string (gvalue));

    case G_TYPE_OBJECT:
        /* scm_c_make_gtype_instance will ref the object for us */
	return scm_c_make_gtype_instance ((GTypeInstance *) g_value_get_object (gvalue));

    case G_TYPE_PARAM:
        /* scm_c_make_gtype_instance will ref the object for us */
	return scm_c_make_gtype_instance ((GTypeInstance *) g_value_get_param (gvalue));

    default:
	scm_wrong_type_arg (FUNC_NAME, SCM_ARG1, value);
	break;
    }

    return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_gvalue_array_primitive_append, "gvalue-array-primitive-append", 2, 0, 0,
	    (SCM array, SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_array_primitive_append
{
    GValueArray *garray;
    GValue *gvalue;
    
    SCM_VALIDATE_SMOB (1, array, gvalue_array);
    SCM_VALIDATE_GVALUE_COPY (2, value, gvalue);

    garray = (GValueArray *) SCM_SMOB_DATA (array);
    
    g_value_array_append (garray, gvalue);

    return SCM_UNSPECIFIED;
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



void guile_gobject_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
                                const gchar *message, gpointer user_data)
{
    scm_error (sym_gruntime_error, NULL,
               "~A: ~A",
               SCM_LIST2 (scm_str2string (log_domain),
                          scm_str2string (message)),
               SCM_EOL);
}

void
scm_pre_init_gnome_gobject_primitives (void)
{
    g_type_init ();

    gboxed_scm_get_type ();

    /* handle the application, GLib, and GLib-GObject domains by default */
    g_log_set_handler (NULL, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                       | G_LOG_FLAG_RECURSION, guile_gobject_log_handler,
                       NULL);
    g_log_set_handler ("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                       | G_LOG_FLAG_RECURSION, guile_gobject_log_handler,
                       NULL);
    g_log_set_handler ("GLib-GObject", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                       | G_LOG_FLAG_RECURSION, guile_gobject_log_handler,
                       NULL);

    quark_type = g_quark_from_static_string ("%scm-gtype->type");
    quark_class = g_quark_from_static_string ("%scm-gtype->class");
    quark_pspec_struct = g_quark_from_static_string ("%scm-pspec-struct");
    quark_instance_wrapper = g_quark_from_static_string ("%scm-instance-wrapper");
    quark_primitive_instance_wrapper = g_quark_from_static_string ("%scm-primitive-instance-wrapper");

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

    scm_tc16_gvalue = scm_make_smob_type ("gvalue", 0);
    scm_set_smob_free (scm_tc16_gvalue, scm_gvalue_free);
    scm_set_smob_print (scm_tc16_gvalue, scm_gvalue_print);

    scm_tc16_gvalue_array = scm_make_smob_type ("gvalue-array", 0);
    scm_set_smob_free (scm_tc16_gvalue_array, scm_gvalue_array_free);
    scm_set_smob_print (scm_tc16_gvalue_array, scm_gvalue_array_print);
}

void
scm_init_gnome_gobject_primitives (void)
{
    SCM gsubr;

#ifndef SCM_MAGIC_SNARFER
#include "guile-gnome-gobject-primitives.x"
#endif

    scm_sym_gtype_instance_write = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype-instance:write")));
    scm_sym_make_class = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("make-class")));
    scm_sym_class_slot_ref = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("class-slot-ref")));
    scm_sym_class_slot_set_x = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("class-slot-set!")));
    scm_sym_class_redefinition = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("class-redefinition")));

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

    scm_c_export ("gsignal-struct-vtable", NULL);

    gsubr = scm_c_make_gsubr ("%print-gparamspec", 2, 0, 0, print_gparam_spec_struct);

    scm_gparam_spec_vtable = scm_permanent_object
	(scm_make_vtable_vtable (scm_makfrom0str ("pwpwpwpwpwpwpwpW"),
				 SCM_INUM0, SCM_LIST1 (gsubr)));

    scm_c_define ("gparam-spec-name", SCM_MAKINUM (scm_si_gparam_spec_name));
    scm_c_define ("gparam-spec-nick", SCM_MAKINUM (scm_si_gparam_spec_nick));
    scm_c_define ("gparam-spec-blurb", SCM_MAKINUM (scm_si_gparam_spec_blurb));
    scm_c_define ("gparam-spec-flags", SCM_MAKINUM (scm_si_gparam_spec_flags));
    scm_c_define ("gparam-spec-param-type", SCM_MAKINUM (scm_si_gparam_spec_param_type));
    scm_c_define ("gparam-spec-value-type", SCM_MAKINUM (scm_si_gparam_spec_value_type));
    scm_c_define ("gparam-spec-owner-type", SCM_MAKINUM (scm_si_gparam_spec_owner_type));
    scm_c_define ("gparam-spec-n-args", SCM_MAKINUM (scm_si_gparam_spec_n_args));
    scm_c_define ("gparam-spec-args", SCM_MAKINUM (scm_si_gparam_spec_args));
    scm_c_define ("gparam-spec-struct-vtable", scm_gparam_spec_vtable);

    scm_c_export ("gparam-spec-struct-vtable", NULL);

    scm_c_define ("gruntime:uint-max", scm_ulong2num (G_MAXUINT));
    scm_c_define ("gruntime:int-min", scm_long2num (G_MININT));
    scm_c_define ("gruntime:int-max", scm_long2num (G_MAXINT));
    scm_c_define ("gruntime:ulong-max", scm_ulong2num (G_MAXULONG));
    scm_c_define ("gruntime:long-min", scm_long2num (G_MINLONG));
    scm_c_define ("gruntime:long-max", scm_long2num (G_MAXLONG));
    scm_c_define ("gruntime:uint64-max", scm_ulong_long2num (G_MAXUINT64));
    scm_c_define ("gruntime:int64-min", scm_long_long2num (G_MININT64));
    scm_c_define ("gruntime:int64-max", scm_long_long2num (G_MAXINT64));
    scm_c_define ("gruntime:float-max", scm_make_real (G_MAXFLOAT));
    scm_c_define ("gruntime:float-min", scm_make_real (G_MINFLOAT));
    scm_c_define ("gruntime:double-max", scm_make_real (G_MAXDOUBLE));
    scm_c_define ("gruntime:double-min", scm_make_real (G_MINDOUBLE));

    scm_c_define ("gruntime:byte-order", scm_long2num (G_BYTE_ORDER));

    scm_c_export ("gruntime:uint-max", "gruntime:int-min", "gruntime:int-max",
		  "gruntime:ulong-max", "gruntime:long-min", "gruntime:long-max",
		  "gruntime:uint64-max", "gruntime:int64-min", "gruntime:int64-max",
		  "gruntime:float-max", "gruntime:float-min", "gruntime:double-max",
		  "gruntime:double-min", "gruntime:byte-order",
		  NULL);

    scm_class_gtype_class = scm_permanent_object
	(SCM_VARIABLE_REF (scm_c_lookup ("<gtype-class>")));

    scm_c_export (s_scm_gvalue_p, s_scm_gvalue_to_type,
		  NULL);

    scm_c_export (s_scm_gtype_primitive_create_basic_instance,
		  s_scm_gobject_primitive_create_instance,
		  s_scm_gtype_instance_primitive_to_type,
		  s_scm_gtype_instance_primitive_to_value,
                  s_scm_sys_gtype_instance_primitive_destroy_x,
		  s_scm_gtype_primitive_get_signals,
		  s_scm_gtype_instance_primitive_signal_emit,
		  s_scm_gtype_instance_primitive_signal_connect,
                  s_scm_sys_gtype_lookup_class,
                  s_scm_sys_gtype_bind_to_class,
		  s_scm_gobject_primitive_get_properties,
		  s_scm_gobject_primitive_get_property,
		  s_scm_gobject_primitive_set_property,
		  s_scm_genum_primitive_get_values,
		  s_scm_gflags_primitive_get_values,
		  s_scm_gvalue_primitive_set_enum,
		  s_scm_gvalue_primitive_set_flags,
		  s_scm_gclosure_primitive_new,
		  s_scm_gclosure_primitive_invoke,
		  s_scm_gtype_interfaces,
		  s_scm_gtype_primitive_basic_p,
		  s_scm_gvalue_primitive_new,
		  s_scm_gvalue_primitive_get,
		  s_scm_gvalue_primitive_set,
		  s_scm_gvalue_array_primitive_new,
		  s_scm_gvalue_array_primitive_append,
		  s_scm_gflags_primitive_bit_set_p,
		  s_scm_gsignal_primitive_handler_block,
		  s_scm_gsignal_primitive_handler_unblock,
		  s_scm_gsignal_primitive_handler_disconnect,
		  s_scm_gsignal_primitive_handler_connected_p,
		  s_scm_gsignal_primitive_create,
		  s_scm_gparam_primitive_to_pspec_struct,
		  s_scm_gparam_primitive_create,
		  s_scm_gparam_spec_p,
		  s_scm_gboxed_scm_primitive_new,
		  s_scm_gboxed_scm_primitive_to_scm,
                  s_scm_especify_metaclass_x,
		  NULL);
}
