/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba-types.c: Support routines for the GLib wrapper
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

#include <guile-gnome-corba-types.h>
#include <guile-gnome-corba-generic.h>
#include <guile-gnome-corba.h>
#include <guile-gnome-gobject.h>
#include <guile/gh.h>
#include <bonobo/Bonobo.h>
#include <bonobo/bonobo-exception.h>
#include <string.h>
#include <bonobo/bonobo-arg.h>

SCM scm_corba_struct_vtable;
SCM scm_corba_sequence_vtable;
scm_t_bits scm_tc16_corba_typecode;
scm_t_bits scm_tc16_orbit_object;
scm_t_bits scm_tc16_corba_data;
scm_t_bits scm_tc16_corba_object;
scm_t_bits scm_tc16_orbit_imethod;
scm_t_bits scm_tc16_orbit_iinterface;

SCM_GLOBAL_SYMBOL (scm_sym_corba_objref, "corba-objref");
SCM_GLOBAL_SYMBOL (scm_sym_corba_typecode, "corba-typecode");
SCM_GLOBAL_SYMBOL (scm_sym_orbit_iinterface, "%orbit-iinterface");
SCM_GLOBAL_SYMBOL (scm_sym_servant, "servant");
SCM_GLOBAL_SYMBOL (scm_sym_stub_class, "stub-class");
SCM_GLOBAL_SYMBOL (scm_sym_corba_system_exception, "corba-system-exception");
SCM_GLOBAL_SYMBOL (scm_sym_corba_user_exception, "corba-user-exception");



static SCM
print_corba_struct (SCM corba_struct, SCM port)
{
    SCM retval, printer;
    CORBA_TypeCode tc;
    gchar *message;

    printer = SCM_PACK (SCM_STRUCT_DATA (corba_struct) [scm_vtable_index_printer]);
    if (SCM_NIMP (printer) && scm_procedure_p (printer))
	return scm_call_2 (printer, corba_struct, port);

    tc = SCM_CORBA_STRUCT_TYPECODE (corba_struct);

    message = g_strdup_printf ("<CORBA-struct %p - %s>", corba_struct, tc->repo_id);
    retval = scm_simple_format (port, scm_makfrom0str (message), SCM_EOL);
    g_free (message);

    return retval;
}

static SCM
print_corba_sequence (SCM corba_sequence, SCM port)
{
    SCM retval, printer;
    CORBA_TypeCode tc;
    gchar *message;

    printer = SCM_PACK (SCM_STRUCT_DATA (corba_sequence) [scm_vtable_index_printer]);
    if (SCM_NIMP (printer) && scm_procedure_p (printer))
	return scm_call_2 (printer, corba_sequence, port);

    tc = SCM_CORBA_SEQUENCE_TYPECODE (corba_sequence);

    message = g_strdup_printf ("<CORBA-sequence %p - %s>", corba_sequence, tc->subtypes [0]->repo_id);
    retval = scm_simple_format (port, scm_makfrom0str (message), SCM_EOL);
    g_free (message);

    return retval;
}



SCM_DEFINE (scm_corba_struct_fields, "corba-struct-fields", 1, 0, 0,
	    (SCM typecode),
	    "")
#define FUNC_NAME s_scm_corba_struct_fields
{
    CORBA_TypeCode tc;
    SCM fields;
    gulong i;

    SCM_VALIDATE_CORBA_TYPECODE_COPY (1, typecode, tc);

    fields = SCM_EOL;
    for (i = 0; i < tc->sub_parts; i++) {
	SCM sym = scm_mem2symbol (tc->subnames [i], strlen (tc->subnames [i]));
	fields = scm_append_x (SCM_LIST2 (fields, SCM_LIST1 (sym)));
    }

    return fields;
}
#undef FUNC_NAME



SCM
scm_c_make_corba_struct (CORBA_TypeCode tc, guint nfields,
			 DynamicAny_DynStruct dyn)
{
    DynamicAny_NameValuePairSeq *members;
    SCM type, fields, typecode_smob;
    CORBA_Environment ev;

    CORBA_exception_init (&ev);
    if (!dyn)
	dyn = (DynamicAny_DynStruct)
	    DynamicAny_DynAnyFactory_create_dyn_any_from_type_code
	    (guile_corba_dynany_factory, tc, &ev);
    g_assert (!BONOBO_EX (&ev));

    members = DynamicAny_DynStruct_get_members (dyn, &ev);

    type = scm_make_struct (scm_corba_struct_vtable, SCM_MAKINUM (nfields), SCM_EOL);
    SCM_SET_CORBA_STRUCT_TYPECODE (type, tc);
    SCM_SET_CORBA_STRUCT_DATA (type, dyn);
    SCM_SET_CORBA_STRUCT_MEMBERS (type, members);

    typecode_smob = SCM_PACK (SCM_STRUCT_DATA (type) [scm_si_corba_typecode]);

    fields = scm_corba_struct_fields (typecode_smob);

    SCM_SET_CORBA_STRUCT_FIELDS (type, fields);
    SCM_SET_CORBA_STRUCT_N_FIELDS (type, SCM_MAKINUM (tc->sub_parts));

    return type;
}



SCM_DEFINE (scm_make_corba_struct, "make-corba-struct", 2, 1, 0,
	    (SCM typecode, SCM num_tail_elts, SCM init_struct),
	    "")
#define FUNC_NAME s_scm_make_corba_struct
{
    CORBA_TypeCode tc, real_tc;
    DynamicAny_DynStruct dyn = NULL;

    SCM_VALIDATE_CORBA_TYPECODE_COPY (1, typecode, tc);
    SCM_VALIDATE_INUM (2, num_tail_elts);
    SCM_ASSERT (SCM_UNBNDP (init_struct) ||
		SCM_CORBA_STRUCTP (init_struct), init_struct,
		SCM_ARG3, FUNC_NAME);

    real_tc = tc;
    while (real_tc->kind == CORBA_tk_alias)
	real_tc = real_tc->subtypes [0];

    SCM_ASSERT (real_tc->kind == CORBA_tk_struct, typecode,
		SCM_ARG1, FUNC_NAME);

    if (!SCM_UNBNDP (init_struct)) {
	CORBA_TypeCode smob_tc = SCM_CORBA_STRUCT_TYPECODE (init_struct);

	SCM_ASSERT (CORBA_TypeCode_equal (smob_tc, tc, NULL), init_struct,
		    SCM_ARG3, FUNC_NAME);

	dyn = (DynamicAny_DynStruct) SCM_CORBA_STRUCT_DATA (init_struct);
    }

    return scm_c_make_corba_struct (tc, SCM_INUM (num_tail_elts), dyn);
}
#undef FUNC_NAME



SCM
scm_c_make_corba_sequence (CORBA_TypeCode tc, guint nfields, gpointer data)
{
    DynamicAny_DynSequence dyn;
    DynamicAny_AnySeq *elements;
    CORBA_TypeCode real_tc;
    CORBA_Environment ev;
    gulong length;
    SCM type;

    real_tc = tc;
    while (real_tc->kind == CORBA_tk_alias)
	real_tc = real_tc->subtypes [0];

    CORBA_exception_init (&ev);
    if (!data)
	dyn = (DynamicAny_DynSequence)
	    DynamicAny_DynAnyFactory_create_dyn_any_from_type_code
	    (guile_corba_dynany_factory, real_tc, &ev);
    else {
	CORBA_any any = { real_tc, data, FALSE };

	dyn = (DynamicAny_DynSequence)
	    DynamicAny_DynAnyFactory_create_dyn_any
	    (guile_corba_dynany_factory, &any, &ev);
    }
    g_assert (!BONOBO_EX (&ev));

    length = DynamicAny_DynSequence_get_length (dyn, &ev);
    g_assert (!BONOBO_EX (&ev));

    elements = DynamicAny_DynSequence_get_elements (dyn, &ev);
    g_assert (!BONOBO_EX (&ev));

    type = scm_make_struct (scm_corba_sequence_vtable, SCM_MAKINUM (nfields), SCM_EOL);
    SCM_SET_CORBA_SEQUENCE_TYPECODE (type, tc);
    SCM_SET_CORBA_SEQUENCE_DATA (type, dyn);
    SCM_SET_CORBA_SEQUENCE_LENGTH (type, SCM_MAKINUM (length));
    SCM_SET_CORBA_SEQUENCE_MEMBERS (type, elements);

    return type;
}



SCM_DEFINE (scm_make_corba_sequence, "make-corba-sequence", 2, 0, 1,
	    (SCM typecode, SCM num_tail_elts, SCM init_smob),
	    "")
#define FUNC_NAME s_scm_make_corba_sequence
{
    CORBA_TypeCode tc, real_tc;
    gpointer data = NULL;

    SCM_VALIDATE_CORBA_TYPECODE_COPY (1, typecode, tc);
    SCM_VALIDATE_INUM (2, num_tail_elts);
    SCM_ASSERT (SCM_UNBNDP (init_smob) || scm_list_p (init_smob) ||
		SCM_TYP16_PREDICATE (scm_tc16_corba_data, init_smob), init_smob,
		SCM_ARG3, FUNC_NAME);

    real_tc = tc;
    while (real_tc->kind == CORBA_tk_alias)
	real_tc = real_tc->subtypes [0];

    SCM_ASSERT (real_tc->kind == CORBA_tk_sequence, typecode,
		SCM_ARG1, FUNC_NAME);

    if (SCM_TYP16_PREDICATE (scm_tc16_corba_data, init_smob)) {
	CORBA_TypeCode smob_tc = (CORBA_TypeCode) SCM_CELL_WORD_1 (init_smob);

	SCM_ASSERT (CORBA_TypeCode_equal (smob_tc, tc, NULL), init_smob,
		    SCM_ARG3, FUNC_NAME);

	data = (gpointer) SCM_CELL_WORD_2 (init_smob);
    } else if (scm_list_p (init_smob)) {
	DynamicAny_DynSequence dyn;
	DynamicAny_AnySeq *elements;
	CORBA_Environment ev;
	CORBA_any *any;
	gulong i;

	CORBA_exception_init (&ev);
	dyn = (DynamicAny_DynSequence)
	    DynamicAny_DynAnyFactory_create_dyn_any_from_type_code
	    (guile_corba_dynany_factory, real_tc, &ev);
	g_assert (!BONOBO_EX (&ev));

	DynamicAny_DynSequence_set_length (dyn, scm_ilength (init_smob), &ev);
	g_assert (!BONOBO_EX (&ev));

	elements = DynamicAny_AnySeq__alloc ();
	elements->_length = elements->_maximum = scm_ilength (init_smob);
	elements->_buffer = DynamicAny_AnySeq_allocbuf (elements->_length);

	for (i = 0; i < elements->_length; i++) {
	    CORBA_any *this = &elements->_buffer [i];
	    SCM value;

	    this->_type = real_tc->subtypes [0];
	    this->_value = ORBit_alloc_tcval (this->_type, 1);
	    this->_release = TRUE;
	    value = scm_list_ref (init_smob, SCM_MAKINUM (i));
	    scm_c_corba_marshal_any (this, value);
	}

	DynamicAny_DynSequence_set_elements (dyn, elements, &ev);
	g_assert (!BONOBO_EX (&ev));

	any = DynamicAny_DynAny_to_any ((DynamicAny_DynAny) dyn, &ev);
	g_assert (!BONOBO_EX (&ev));

	data = any->_value;
    }

    return scm_c_make_corba_sequence (tc, SCM_INUM (num_tail_elts), data);
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_sequence_length, "corba-sequence-length", 1, 0, 0,
	    (SCM corba_sequence),
	    "")
#define FUNC_NAME s_scm_corba_sequence_length
{
    SCM_VALIDATE_CORBA_SEQUENCE (1, corba_sequence);

    return SCM_CORBA_SEQUENCE_LENGTH (corba_sequence);
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_sequence_set_length_x, "corba-sequence-set-length!", 2, 0, 0,
	    (SCM corba_sequence, SCM length),
	    "")
#define FUNC_NAME s_scm_corba_sequence_set_length_x
{
    DynamicAny_DynSequence dyn;
    DynamicAny_AnySeq *elements;
    CORBA_Environment ev;

    SCM_VALIDATE_CORBA_SEQUENCE (1, corba_sequence);
    SCM_ASSERT (SCM_INUMP (length) && SCM_INUM (length) >= 0, length, SCM_ARG1, FUNC_NAME);

    CORBA_exception_init (&ev);
    dyn = (DynamicAny_DynSequence) SCM_CORBA_SEQUENCE_DATA (corba_sequence);
    DynamicAny_DynSequence_set_length (dyn, SCM_INUM (length), &ev);
    g_assert (!BONOBO_EX (&ev));

    elements = DynamicAny_DynSequence_get_elements (dyn, &ev);
    g_assert (!BONOBO_EX (&ev));

    SCM_SET_CORBA_SEQUENCE_LENGTH (corba_sequence, length);
    SCM_SET_CORBA_SEQUENCE_MEMBERS (corba_sequence, elements);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_sequence_ref, "corba-sequence-ref", 2, 0, 0,
	    (SCM corba_sequence, SCM index),
	    "")
#define FUNC_NAME s_scm_corba_sequence_ref
{
    DynamicAny_AnySeq *elements;
    CORBA_any *any;

    SCM_VALIDATE_CORBA_SEQUENCE (1, corba_sequence);
    SCM_ASSERT (SCM_CORBA_SEQUENCEP (corba_sequence), corba_sequence,
		SCM_ARG1, FUNC_NAME);
    SCM_ASSERT (SCM_INUMP (index) && (SCM_INUM (index) >= 0) &&
		(SCM_INUM (index) < SCM_INUM (SCM_CORBA_SEQUENCE_LENGTH (corba_sequence))),
		 index, SCM_ARG2, FUNC_NAME);

    elements = SCM_CORBA_SEQUENCE_MEMBERS (corba_sequence);
    any = &elements->_buffer [SCM_INUM (index)];

    return scm_c_corba_demarshal_any (any);
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_sequence_set_x, "corba-sequence-set!", 3, 0, 0,
	    (SCM corba_sequence, SCM index, SCM value),
	    "")
#define FUNC_NAME s_scm_corba_sequence_set_x
{
    DynamicAny_DynSequence dyn;
    DynamicAny_AnySeq *elements;
    CORBA_Environment ev;
    CORBA_any *any;

    SCM_VALIDATE_CORBA_SEQUENCE (1, corba_sequence);
    SCM_ASSERT (SCM_INUMP (index) && (SCM_INUM (index) >= 0) &&
		(SCM_INUM (index) < SCM_INUM (SCM_CORBA_SEQUENCE_LENGTH (corba_sequence))),
		 index, SCM_ARG2, FUNC_NAME);

    elements = SCM_CORBA_SEQUENCE_MEMBERS (corba_sequence);
    any = &elements->_buffer [SCM_INUM (index)];

    scm_c_corba_marshal_any (any, value);

    CORBA_exception_init (&ev);
    dyn = (DynamicAny_DynSequence) SCM_CORBA_SEQUENCE_DATA (corba_sequence);
    DynamicAny_DynSequence_set_elements (dyn, elements, &ev);
    g_assert (!BONOBO_EX (&ev));

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_sequence_type, "corba-sequence-type", 1, 0, 0,
	    (SCM corba_sequence),
	    "")
#define FUNC_NAME s_scm_corba_sequence_type
{
    CORBA_TypeCode tc;

    SCM_VALIDATE_CORBA_SEQUENCE (1, corba_sequence);

    tc = SCM_CORBA_SEQUENCE_TYPECODE (corba_sequence);

    /* internal API? */
    SCM_RETURN_NEWSMOB (scm_tc16_corba_typecode, ORBit_RootObject_duplicate
                        (tc->subtypes [0]));
}
#undef FUNC_NAME



SCM
scm_c_corba_demarshal_any (const CORBA_any *any)
{
    CORBA_TypeCode tc = any->_type;
    SCM retval = SCM_BOOL_F;

    while (tc->kind == CORBA_tk_alias)
	tc = tc->subtypes [0];
    
    switch (tc->kind) {
      /*
#define _HANDLE_BASIC_VALUE(k,t,f)						\
case CORBA_tk_ ## k:								\
	retval =  ## f (*(CORBA_ ## t *) any->_value);				\
	break;

	_HANDLE_BASIC_VALUE (short,     short,              gh_int2scm);
	_HANDLE_BASIC_VALUE (long,      long,               gh_long2scm);
	_HANDLE_BASIC_VALUE (ushort,    unsigned_short,     gh_int2scm);
	_HANDLE_BASIC_VALUE (ulong,     unsigned_long,      gh_long2scm);
	_HANDLE_BASIC_VALUE (longlong,  long_long,          scm_long_long2num);
	_HANDLE_BASIC_VALUE (ulonglong, unsigned_long_long, scm_long_long2num);
	_HANDLE_BASIC_VALUE (float,     float,              gh_double2scm);
	_HANDLE_BASIC_VALUE (double,    double,             gh_double2scm);

#undef _HANDLE_BASIC_VALUE
      */
    case CORBA_tk_TypeCode:
	SCM_NEWSMOB (retval, scm_tc16_corba_typecode,
		     * (CORBA_TypeCode *) any->_value);
	break;

    case CORBA_tk_objref: {
	CORBA_Object corba_objref;
	SCM class;

	corba_objref = * (CORBA_Object *) any->_value;
	CORBA_Object_duplicate (corba_objref, NULL);
	class = scm_c_corba_typecode_primitive_to_class (any->_type);

	retval = scm_c_make_corba_object (class, corba_objref);

	break;
    }

    case CORBA_tk_sequence:
	retval = scm_c_make_corba_sequence
	    (tc, 0, (CORBA_sequence_CORBA_octet *) any->_value);
	break;

    case CORBA_tk_struct: {
	DynamicAny_DynStruct dyn;
	CORBA_Environment ev;

	CORBA_exception_init (&ev);

	dyn = (DynamicAny_DynStruct)
	    DynamicAny_DynAnyFactory_create_dyn_any_from_type_code
	    (guile_corba_dynany_factory, tc, &ev);

	DynamicAny_DynAny_from_any ((DynamicAny_DynAny) dyn, any, &ev);
	
	retval = scm_c_make_corba_struct (tc, 0, dyn);
	break;
    }

    case CORBA_tk_string:
	retval = scm_mem2string (* (CORBA_char **) any->_value,
				 strlen (* (CORBA_char **) any->_value));
	break;

    case CORBA_tk_any:
	retval = scm_c_corba_demarshal_any (* (CORBA_any **) any->_value);
	break;

    case CORBA_tk_enum: {
	GType gtype;

	gtype = guile_corba_generic_typecode_to_type (tc);
	g_message (G_STRLOC ": %ld", (unsigned long)gtype);

	retval = scm_c_make_gvalue (gtype);
        scm_gvalue_primitive_set (retval, SCM_MAKINUM(*(CORBA_unsigned_long*)any->_value));

	break;
    }

    default:
	g_message (G_STRLOC ": %p - |%s| - %d", any->_value,
		   any->_type->repo_id, tc->kind);
	break;
    }

    return retval;
}



void
scm_c_corba_marshal_any (CORBA_any *any, SCM value)
{
    switch (any->_type->kind) {
#define _HANDLE_BASIC_VALUE(k,t,f)						\
case CORBA_tk_ ## k:								\
	(*(CORBA_ ## t *) any->_value) = f (value);				\
	break;

	_HANDLE_BASIC_VALUE (short,     short,              gh_scm2int);
	_HANDLE_BASIC_VALUE (long,      long,               gh_scm2long);
	_HANDLE_BASIC_VALUE (ushort,    unsigned_short,     gh_scm2int);
	_HANDLE_BASIC_VALUE (ulong,     unsigned_long,      gh_scm2long);
	_HANDLE_BASIC_VALUE (float,     float,              gh_scm2double);
	_HANDLE_BASIC_VALUE (double,    double,             gh_scm2double);

#undef _HANDLE_BASIC_VALUE

    case CORBA_tk_TypeCode:
	SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_corba_typecode, value),
		    value, SCM_ARG2, "%marshal-any");

	(* (CORBA_TypeCode *) any->_value) = (CORBA_TypeCode) SCM_SMOB_DATA (value);
	break;

    case CORBA_tk_string:
	SCM_ASSERT (SCM_STRINGP (value), value, SCM_ARG2, "%marshal-any");

	(* (CORBA_char **) any->_value) = CORBA_string_dup (SCM_STRING_CHARS (value));
	break;

    default:
	g_message (G_STRLOC ": %p - |%s|", any->_value, any->_type->repo_id);
	break;
    }
}



SCM_DEFINE (scm_corba_struct_ref, "corba-struct-ref", 2, 0, 0,
	    (SCM corba_struct, SCM index),
	    "")
#define FUNC_NAME s_scm_corba_struct_ref
{
    DynamicAny_NameValuePairSeq *members;
    DynamicAny_NameValuePair *this;

    SCM_VALIDATE_CORBA_STRUCT (1, corba_struct);
    SCM_ASSERT (SCM_INUMP (index) && (SCM_INUM (index) >= 0) &&
		(SCM_INUM (index) < SCM_INUM (SCM_CORBA_STRUCT_N_FIELDS (corba_struct))),
		 index, SCM_ARG2, FUNC_NAME);

    members = SCM_CORBA_STRUCT_MEMBERS (corba_struct);
    this = &members->_buffer [SCM_INUM (index)];

    return scm_c_corba_demarshal_any (&this->value);
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_struct_set_x, "corba-struct-set!", 3, 0, 0,
	    (SCM corba_struct, SCM index, SCM value),
	    "")
#define FUNC_NAME s_scm_corba_struct_set_x
{
    DynamicAny_NameValuePairSeq *members;
    DynamicAny_NameValuePair *this;

    SCM_VALIDATE_CORBA_STRUCT (1, corba_struct);
    SCM_ASSERT (SCM_INUMP (index) && (SCM_INUM (index) >= 0) &&
		(SCM_INUM (index) < SCM_INUM (SCM_CORBA_STRUCT_N_FIELDS (corba_struct))),
		index, SCM_ARG2, FUNC_NAME);

    members = SCM_CORBA_STRUCT_MEMBERS (corba_struct);
    this = &members->_buffer [SCM_INUM (index)];

    scm_c_corba_marshal_any (&this->value, value);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_struct_p, "corba-struct?", 1, 0, 0,
	    (SCM corba_struct),
	    "")
#define FUNC_NAME s_scm_corba_struct_p
{
    return SCM_BOOL (SCM_CORBA_STRUCTP (corba_struct));
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_struct_is_a_p, "corba-struct-is-a?", 2, 0, 0,
	    (SCM corba_struct, SCM typecode),
	    "")
#define FUNC_NAME s_scm_corba_struct_is_a_p
{
    CORBA_TypeCode struct_tc, tc;

    SCM_VALIDATE_CORBA_STRUCT (1, corba_struct);
    SCM_VALIDATE_CORBA_TYPECODE_COPY (2, typecode, tc);

    struct_tc = SCM_CORBA_STRUCT_TYPECODE (corba_struct);

    return SCM_BOOL (CORBA_TypeCode_equal (struct_tc, tc, NULL));
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_struct_type, "corba-struct-type", 1, 0, 0,
	    (SCM corba_struct),
	    "")
#define FUNC_NAME s_scm_corba_struct_type
{
    SCM_VALIDATE_CORBA_STRUCT (1, corba_struct);

    return SCM_PACK (SCM_STRUCT_DATA (corba_struct) [scm_si_corba_typecode]);
}
#undef FUNC_NAME



static size_t
guile_orbit_object_free (SCM type)
{
    /* RootObject_release is internal API? */
       
    gpointer objptr = (gpointer) SCM_SMOB_DATA (type);
    ORBit_RootObject_release (objptr);
    return 0;
}

static int
guile_corba_typecode_print (SCM typecode_smob, SCM port, scm_print_state *pstate)
{
    CORBA_TypeCode tc = (CORBA_TypeCode) SCM_SMOB_DATA (typecode_smob);
     
    scm_puts ("#<CORBA-TypeCode ", port);
    scm_display (scm_makfrom0str (tc->repo_id), port);
    scm_puts (">", port);
     
    /* non-zero means success */
    return 1;
}

G_GNUC_UNUSED static size_t
guile_corba_data_free (SCM data)
{
    gpointer dataptr = (gpointer) SCM_SMOB_DATA (data);
    CORBA_free (dataptr);
    return 0;
}



SCM_DEFINE (scm_corba_typecode_to_gtype, "corba-typecode->gtype", 1, 0, 0,
	    (SCM typecode),
	    "")
#define FUNC_NAME s_scm_corba_typecode_to_gtype
{
    CORBA_TypeCode tc;
    GType gtype;

    SCM_VALIDATE_CORBA_TYPECODE_COPY (1, typecode, tc);

    gtype = guile_corba_generic_typecode_to_type (tc);

    return scm_c_register_gtype (gtype);
}
#undef FUNC_NAME



SCM
scm_c_make_corba_object (SCM class, CORBA_Object corba_objref)
{
    SCM smob, object;

    ORBit_RootObject_duplicate (corba_objref);
    SCM_NEWSMOB (smob, scm_tc16_corba_object, corba_objref);

    object = scm_make (SCM_LIST1 (class));
    scm_slot_set_x (object, scm_sym_corba_objref, smob);

    return object;
}

SCM
scm_c_make_corba_typecode (CORBA_TypeCode corba_typecode)
{
    SCM smob;

    ORBit_RootObject_duplicate (corba_typecode);
    SCM_NEWSMOB (smob, scm_tc16_corba_typecode, corba_typecode);

    return smob;
}



SCM
scm_c_corba_typecode_to_class (CORBA_TypeCode tc)
{
    CORBA_TypeCode real_tc;

    real_tc = tc;
    while (real_tc->kind == CORBA_tk_alias)
	real_tc = real_tc->subtypes [0];

    switch (tc->kind) {
    case CORBA_tk_short:
    case CORBA_tk_long:
    case CORBA_tk_ushort:
    case CORBA_tk_ulong:
	return scm_class_integer;

    case CORBA_tk_longlong:
    case CORBA_tk_ulonglong:
	return scm_class_number;

    case CORBA_tk_float:
    case CORBA_tk_double:
	return scm_class_real;

    case CORBA_tk_string:
	return scm_class_string;

    default:
	return scm_class_top;
    }
}



SCM_DEFINE (scm_corba_object_class_to_typecode, "corba-object-class->typecode", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_corba_object_class_to_typecode
{
    SCM tc_smob;

    SCM_VALIDATE_CORBA_OBJECT_CLASS (1, class);

    tc_smob = scm_call_2 (scm_class_slot_ref, class, scm_sym_corba_typecode);
    SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_corba_typecode, tc_smob),
		class, SCM_ARG3, FUNC_NAME);

    return tc_smob;
}
#undef FUNC_NAME



void
scm_pre_init_gnome_corba_types (void)
{
    scm_tc16_orbit_object = scm_make_smob_type ("%orbit-object", 0);
    scm_set_smob_free (scm_tc16_orbit_object, guile_orbit_object_free);

    scm_tc16_corba_typecode = scm_make_smob_type ("%corba-typecode", 0);
    scm_set_smob_free (scm_tc16_corba_typecode, guile_orbit_object_free);
    scm_set_smob_print (scm_tc16_corba_typecode, guile_corba_typecode_print);

    scm_tc16_corba_object = scm_make_smob_type ("%corba-object", 0);
    // scm_set_smob_print (scm_tc16_corba_object, guile_corba_object_print);
    scm_set_smob_free (scm_tc16_corba_object, guile_orbit_object_free);

    scm_tc16_orbit_iinterface = scm_make_smob_type ("%orbit-iinterface", 0);
    scm_set_smob_free (scm_tc16_orbit_iinterface, NULL);

    scm_tc16_orbit_imethod = scm_make_smob_type ("%orbit-imethod", 0);
    scm_set_smob_free (scm_tc16_orbit_imethod, NULL);

    scm_tc16_corba_data = scm_make_smob_type ("%corba-data", 0);
    // scm_set_smob_free (scm_tc16_corba_data, guile_corba_data_free);
}

void
scm_init_gnome_corba_types (void)
{
    SCM gsubr;

#include "guile-gnome-corba-types.x"

    gsubr = scm_c_make_gsubr ("%print-corba-struct", 2, 0, 0, print_corba_struct);
    scm_corba_struct_vtable = scm_permanent_object
	(scm_make_vtable_vtable (scm_makfrom0str ("srprprprpopopW"), SCM_INUM0, SCM_LIST1 (gsubr)));
    SCM_SET_CORBA_STRUCT_TYPECODE (scm_corba_struct_vtable, TC_CORBA_TypeCode);
    scm_c_define ("%corba-struct-vtable", scm_corba_struct_vtable);
    scm_c_define ("%corba-struct-vtable-offset-user", SCM_MAKINUM (scm_corba_struct_vtable_offset_user));
    scm_c_define ("%corba-struct-vtable-offset-printer", SCM_MAKINUM (scm_vtable_index_printer));

    gsubr = scm_c_make_gsubr ("%print-corba-sequence", 2, 0, 0, print_corba_sequence);
    scm_corba_sequence_vtable = scm_permanent_object
	(scm_make_vtable_vtable (scm_makfrom0str ("srprprprpopW"), SCM_INUM0, SCM_LIST1 (gsubr)));
    SCM_SET_CORBA_SEQUENCE_TYPECODE (scm_corba_sequence_vtable, TC_CORBA_TypeCode);
    scm_c_define ("%corba-sequence-vtable", scm_corba_sequence_vtable);
    scm_c_define ("%corba-sequence-vtable-offset-user", SCM_MAKINUM (scm_corba_sequence_vtable_offset_user));
    scm_c_define ("%corba-sequence-vtable-offset-printer", SCM_MAKINUM (scm_vtable_index_printer));

    scm_c_export ("%corba-struct-vtable",
		  "%corba-struct-vtable-offset-user",
		  "%corba-struct-vtable-offset-printer",
		  "%corba-sequence-vtable",
		  "%corba-sequence-vtable-offset-user",
		  "%corba-sequence-vtable-offset-printer",
		  NULL);

    scm_c_export (s_scm_corba_object_class_to_typecode,
		  s_scm_corba_typecode_to_gtype,
		  s_scm_make_corba_struct,
		  s_scm_corba_struct_fields,
		  s_scm_corba_struct_ref,
		  s_scm_corba_struct_set_x,
		  s_scm_corba_struct_p,
		  s_scm_corba_struct_is_a_p,
		  s_scm_corba_struct_type,
		  s_scm_make_corba_sequence,
		  s_scm_corba_sequence_length,
		  s_scm_corba_sequence_set_length_x,
		  s_scm_corba_sequence_set_x,
		  s_scm_corba_sequence_ref,
		  s_scm_corba_sequence_type,
		  NULL);
}
