/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * gvalue.c: Support for GValue-based types
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
#include "gvalue.h"
#include "guile-support.h"



scm_t_bits scm_tc16_gvalue;

SCM_SYMBOL (sym_closure, "closure");
SCM_KEYWORD (k_value, "value");

static SCM _make;
static SCM _gtype_instance_write;



/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



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



static int
scm_gvalue_print (SCM smob, SCM port, scm_print_state *pstate)
{
    GValue *value = (GValue *) SCM_SMOB_DATA (smob);
    SCM class;

    class = scm_c_gtype_lookup_class (G_VALUE_TYPE (value));
    if (!class)
    class = scm_c_register_gtype (G_VALUE_TYPE (value));

    scm_call_3 (_gtype_instance_write, class, smob, port);
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



SCM_DEFINE (scm_gtype_valued_p, "gtype-valued?", 1, 0, 0,
	    (SCM type),
	    "Returns @code{#t} if @var{type} is a valued type. For valued types,\n"
            "Scheme values can be made into GValues by\n"
            "@code{make (gtype->class @var{type}) #:value @var{val}}.\n"
            "Note that valued types are a strict superset of basic types, adding\n"
            "on certain types that can have multiple representations in Scheme.")
#define FUNC_NAME s_scm_gtype_valued_p
{
    GType gtype;

    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);

    if (SCM_NFALSEP (scm_gtype_basic_p (type)))
        return SCM_BOOL_T;

    switch (G_TYPE_FUNDAMENTAL (gtype)) {
    case G_TYPE_ENUM:
    case G_TYPE_FLAGS:
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

    case G_TYPE_ENUM: {
        GEnumClass *enum_class = g_type_class_ref (G_VALUE_TYPE (gvalue));

        SCM_ASSERT (SCM_INUMP (value) &&
                    (SCM_INUM (value) >= enum_class->minimum) &&
                    (SCM_INUM (value) <= enum_class->maximum),
                    value, SCM_ARG2, FUNC_NAME);

        g_value_set_enum (gvalue, SCM_INUM (value));

        g_type_class_unref (enum_class);
        break;
    }

    case G_TYPE_FLAGS: {
        GFlagsClass *flags_class = g_type_class_ref (G_VALUE_TYPE (gvalue));

        SCM_ASSERT (SCM_INUMP (value) &&
                    ((SCM_INUM (value) & flags_class->mask) == SCM_INUM (value)),
                    value, SCM_ARG2, FUNC_NAME);

        g_value_set_flags (gvalue, SCM_INUM (value));

        g_type_class_unref (flags_class);
        break;
    }

    /* could probably add G_TYPE_PARAM as well */
    case G_TYPE_OBJECT:
    case G_TYPE_INTERFACE: /* assuming GObject is a prereq */ {
        GTypeInstance *ginstance;
        GType gtype;

        SCM_VALIDATE_GTYPE_INSTANCE_COPY (2, value, ginstance);
        gtype = G_TYPE_FROM_INSTANCE (ginstance);

        switch (G_TYPE_FUNDAMENTAL (gtype)) {
        case G_TYPE_OBJECT:
            g_value_set_object (gvalue, G_OBJECT (ginstance));
            break;
        default:
            SCM_ERROR_NOT_YET_IMPLEMENTED (value);
        }
        break;
    }

    case G_TYPE_BOXED:
        if (G_VALUE_TYPE (gvalue) == G_TYPE_BOXED_SCM) {
            /* the copy func will protect the scm_value from GC */
            g_value_set_boxed (gvalue,
                               GINT_TO_POINTER (SCM_UNPACK (value)));
        } else if (G_VALUE_TYPE (gvalue) == G_TYPE_VALUE_ARRAY) {
            GValueArray *arr;
            gint len;
            
            SCM_ASSERT (scm_list_p (value),
                        value, SCM_ARG2, FUNC_NAME);

            len = SCM_INUM (scm_length (value));
            arr = g_value_array_new (len);
            while (len--) {
                GType value_type;
                SCM val, v;

                v = SCM_CAR (value);

                if (SCM_STRINGP (v))
                    value_type = G_TYPE_STRING;
                else if (SCM_BOOLP (v))
                    value_type = G_TYPE_BOOLEAN;
                else if (SCM_INUMP (v))
                    value_type = G_TYPE_LONG;
                else if (SCM_REALP (v))
                    value_type = G_TYPE_DOUBLE;
                else if (SCM_CHARP (v))
                    value_type = G_TYPE_CHAR;
                else if (scm_list_p (v))
                    value_type = G_TYPE_VALUE_ARRAY;
                else
                    scm_wrong_type_arg (FUNC_NAME, SCM_ARG1, v);

                val = scm_c_make_gvalue (value_type);
                scm_gvalue_primitive_set (val, v);
                /* will copy the val */
                g_value_array_append (arr, (GValue*) SCM_SMOB_DATA (val));
                value = SCM_CDR (value);
            }
                
            g_value_set_boxed_take_ownership (gvalue, arr);
        } else {
            scm_wrong_type_arg (FUNC_NAME, SCM_ARG1, instance);
        }
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

    case G_TYPE_BOXED: {
        gpointer p = g_value_get_boxed (gvalue);
        
        if (G_VALUE_TYPE (gvalue) == G_TYPE_BOXED_SCM) {
            if (!p)
                return SCM_UNSPECIFIED;
            return SCM_PACK (GPOINTER_TO_INT (p));
        } else if (G_VALUE_TYPE (gvalue) == G_TYPE_VALUE_ARRAY) {
            GValueArray *arr = p;
            gint i = arr ? arr->n_values : 0;
            SCM l = SCM_EOL;
            
            while (i--) {
                /* have to copy -- could fix this with a helper
                   scm_c_gvalue_primitive_get, but I'm lazy */
                SCM val = scm_c_make_gvalue (G_VALUE_TYPE (&arr->values[i]));
                g_value_copy (&arr->values[i], (GValue*)SCM_SMOB_DATA (val));
                /* maybe should somehow use scm_c_gvalue_to_scm */
                l = scm_cons (scm_gvalue_primitive_get (val), l);
            }
            return l;
        } else {
            scm_wrong_type_arg (FUNC_NAME, SCM_ARG1, value);
        }
        break;
    }

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



SCM_DEFINE (scm_gvalue_to_scm, "gvalue->scm", 1, 0, 0,
	    (SCM value),
	    "")
#define FUNC_NAME s_scm_gvalue_to_scm
{
    GValue *gvalue;

    GType type, fundamental;
    SCM stype;

    SCM_VALIDATE_GVALUE_COPY (1, value, gvalue);

    type = G_VALUE_TYPE (gvalue);
    fundamental = G_TYPE_FUNDAMENTAL (type);

    stype = scm_c_register_gtype (type);

    if (SCM_NFALSEP (scm_gtype_basic_p (stype))) {
        return scm_gvalue_primitive_get (value);
    }
    else if (fundamental == G_TYPE_OBJECT) {
        return scm_c_gtype_instance_to_scm ((GTypeInstance*)g_value_get_object (gvalue));
    }
    else if (fundamental == G_TYPE_PARAM) {
        return scm_c_gtype_instance_to_scm ((GTypeInstance*)g_value_get_param (gvalue));
    }
    else {
        /* Enums and flags are natively represented as GValues. Also most boxed
         * and pointer values will fall here. */
        return value;
    }
}
#undef FUNC_NAME

SCM scm_c_gvalue_to_scm (const GValue *gvalue)
{
    GType fundamental = G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (gvalue));
    
    /* try to avoid needless allocation of a gvalue smob */
    if (fundamental == G_TYPE_OBJECT) {
        return scm_c_gtype_instance_to_scm ((GTypeInstance*)g_value_get_object (gvalue));
    }
    else if (fundamental == G_TYPE_PARAM) {
        return scm_c_gtype_instance_to_scm ((GTypeInstance*)g_value_get_param (gvalue));
    }
    else {
        /* fall back on the normal version */
        SCM svalue;
        GValue *new;

        new = g_new0 (GValue, 1);
        g_value_init (new, G_VALUE_TYPE (gvalue));
        g_value_copy (gvalue, new);
        SCM_NEWSMOB (svalue, scm_tc16_gvalue, new);

        return scm_gvalue_to_scm (svalue);
    }
}

SCM_DEFINE (scm_scm_to_gvalue, "scm->gvalue", 2, 0, 0,
	    (SCM type, SCM scm),
	    "")
#define FUNC_NAME s_scm_scm_to_gvalue
{
    GType gtype, fundamental;
    
    SCM_VALIDATE_GTYPE_COPY (1, type, gtype);
    fundamental = G_TYPE_FUNDAMENTAL (gtype);    

    if (SCM_NFALSEP (scm_gvalue_p (scm))) {
        return scm;
    }
    else if (SCM_EQ_P (scm, SCM_UNSPECIFIED) || gtype == G_TYPE_NONE) {
        /* Make things easy when you're ignoring a return val. */
        return SCM_UNSPECIFIED;
    }
    else if (SCM_NFALSEP (scm_gtype_valued_p (type))) {
        /* Some types can be converted to GValues with the same incantation. */
        return scm_call_3 (_make, scm_c_gtype_to_class (gtype), k_value, scm);
    }
    else if (fundamental == G_TYPE_OBJECT || fundamental == G_TYPE_INTERFACE) {
        SCM ret = scm_gvalue_primitive_new (type);
        scm_gvalue_primitive_set (ret, scm_slot_ref (scm, scm_sym_gtype_instance));
        return ret;
    }
    else if (gtype == G_TYPE_CLOSURE) {
        /* GOOPS closures only -- the primitive ones were already caught
           above with the <gvalue> check. */
        /* this is a hack. really, this function should be a primitive generic,
           and then closures.scm specializes for closures. */
        if (!SCM_NFALSEP (scm_slot_exists_p (scm, sym_closure)))
            scm_wrong_type_arg (FUNC_NAME, 2, scm);
        return scm_slot_ref (scm, sym_closure);
    }
    else {
        scm_c_gruntime_error (FUNC_NAME,
                              "Don't know how to make values of type ~A",
                              SCM_LIST1 (type));
        return SCM_UNSPECIFIED; /* won't get here */
    }
}
#undef FUNC_NAME

GValue* scm_c_scm_to_gvalue (GType gtype, SCM scm)
{
    SCM type, ret;
    GValue *value, *new = g_new0 (GValue, 1);
    
    type = scm_c_register_gtype (gtype);
    ret = scm_scm_to_gvalue (type, scm);
    
    /* doesn't actually copy the value. rather disingenuous i would say :P */
#define FUNC_NAME "%scm->gvalue"
    SCM_VALIDATE_GVALUE_COPY (0, ret, value);
#undef FUNC_NAME
    g_value_init (new, G_VALUE_TYPE (value));
    g_value_copy (value, new);
    return new;
}



/**********************************************************************
 * Defining new enum and flags types
 **********************************************************************/

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
        scm_c_gruntime_error (FUNC_NAME,
                              "There is already a type with this name: ~S",
                              SCM_LIST1 (name));

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
	scm_c_gruntime_error (FUNC_NAME,
                              "There is already a type with this name: ~S",
                              SCM_LIST1 (name));

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



/**********************************************************************
 * GEnum and GFlags class support
 **********************************************************************/

SCM_DEFINE (scm_genum_type_get_values, "genum-type-get-values", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_genum_type_get_values
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

SCM_DEFINE (scm_gflags_type_get_values, "gflags-type-get-values", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_gflags_type_get_values
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



void
scm_init_gnome_gobject_values (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "gvalue.x"
#endif

    scm_tc16_gvalue = scm_make_smob_type ("gvalue", 0);
    scm_set_smob_free (scm_tc16_gvalue, scm_gvalue_free);
    scm_set_smob_print (scm_tc16_gvalue, scm_gvalue_print);

    _gtype_instance_write =
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("gtype-instance:write")));
    _make =
        scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("make")));

    scm_c_define_and_export_gtype_x (G_TYPE_CHAR);
    scm_c_define_and_export_gtype_x (G_TYPE_UCHAR); 				
    scm_c_define_and_export_gtype_x (G_TYPE_BOOLEAN);
    scm_c_define_and_export_gtype_x (G_TYPE_INT);
    scm_c_define_and_export_gtype_x (G_TYPE_UINT);
    scm_c_define_and_export_gtype_x (G_TYPE_LONG);
    scm_c_define_and_export_gtype_x (G_TYPE_ULONG);
    scm_c_define_and_export_gtype_x (G_TYPE_INT64);
    scm_c_define_and_export_gtype_x (G_TYPE_UINT64);
    scm_c_define_and_export_gtype_x (G_TYPE_ENUM);
    scm_c_define_and_export_gtype_x (G_TYPE_FLAGS);
    scm_c_define_and_export_gtype_x (G_TYPE_FLOAT);
    scm_c_define_and_export_gtype_x (G_TYPE_DOUBLE);
    scm_c_define_and_export_gtype_x (G_TYPE_STRING);
    scm_c_define_and_export_gtype_x (G_TYPE_POINTER);
    scm_c_define_and_export_gtype_x (G_TYPE_BOXED);
    scm_c_define_and_export_gtype_x (gboxed_scm_get_type());
    scm_c_define_and_export_gtype_x (G_TYPE_VALUE_ARRAY);
}
