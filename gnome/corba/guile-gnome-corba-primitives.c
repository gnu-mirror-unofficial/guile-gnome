/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba-primitives.c:
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

#include "guile-support.h"

#include "guile-gnome-corba-primitives.h"
#include "guile-gnome-corba-types.h"
#include "guile-gnome-corba-generic.h"
#include "guile-gnome-gobject-primitives.h"

#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-context.h>
#include <bonobo/bonobo-moniker-util.h>
#include <bonobo/bonobo-exception.h>

#include <glib-object.h>
#include <orbit/orbit.h>

#include <string.h>

#define BONOBO_EX(ev) ((ev) && (ev)->_major != CORBA_NO_EXCEPTION)

SCM scm_class_corba_object;
SCM scm_class_portable_server_servant_base;
SCM scm_f_skel_marshal_func;

scm_t_bits scm_tc16_guile_corba_interface;
scm_t_bits scm_tc16_guile_portable_server_servant;

DynamicAny_DynAnyFactory guile_corba_dynany_factory;
PortableServer_POA guile_corba_poa;
CORBA_ORB guile_corba_orb;
static GMainLoop *guile_corba_main_loop = NULL;

#define CLASSP(x) (SCM_STRUCTP (x) && SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_METACLASS)

SCM_KEYWORD (k_ior, "ior");
SCM_KEYWORD (k_unknown, "unknown");

SCM_KEYWORD (k_metaclass,	"metaclass");
SCM_KEYWORD (k_name,		"name");
SCM_KEYWORD (k_specializers,	"specializers");
SCM_KEYWORD (k_procedure,	"procedure");

G_GNUC_UNUSED static SCM
make_scm_module_name (const gchar *module_name)
{
    gchar **parts, **ptr;
    SCM scm_name = SCM_EOL;

    parts = g_strsplit (module_name, ":", 0);
    for (ptr = parts; *ptr; ptr++)
	scm_name = scm_append_x (SCM_LIST2 (scm_name, SCM_LIST1 (
                                                    scm_str2symbol (*ptr))));
    g_strfreev (parts);

    return scm_name;
}

SCM_SYMBOL (sym_o, "o");
SCM_SYMBOL (sym_x, "x");
SCM_SYMBOL (sym_object, "%object");
SCM_SYMBOL (sym_name, "%name");
SCM_SYMBOL (sym_imethod, "%imethod");
SCM_SYMBOL (sym_args, "%args");
SCM_SYMBOL (sym_class, "%class");

SCM_KEYWORD (k_class, "class");
SCM_KEYWORD (k_accessor, "accessor");
SCM_KEYWORD (k_getter, "getter");
SCM_KEYWORD (k_allocation, "allocation");
SCM_KEYWORD (k_each_subclass, "each-subclass");

void
scm_c_corba_handle_exception (CORBA_Environment *ev)
{
    SCM name;

    name = scm_mem2string (ev->_id, strlen (ev->_id));

    if (ev->_major == CORBA_SYSTEM_EXCEPTION) {
	CORBA_SystemException *se = CORBA_exception_value (ev);
	SCM minor, completed;

	minor = scm_long2num (se->minor);;
	switch (se->completed) {
	case CORBA_COMPLETED_YES:
	    completed = scm_str2symbol ("completed");
	    break;
	case CORBA_COMPLETED_NO:
	    completed = scm_str2symbol ("not-completed");
	    break;
	default:
	    completed = scm_str2symbol ("maybe-completed");
	    break;
	}

	CORBA_exception_free (ev);
	scm_ithrow (scm_sym_corba_system_exception,
		    SCM_LIST3 (name, minor, completed), 1);

    }  else {
	CORBA_exception_free (ev);
	scm_ithrow (scm_sym_corba_user_exception, SCM_LIST1 (name), 1);
    }
}

SCM_DEFINE (scm_corba_primitive_invoke_method, "corba-primitive-invoke-method", 3, 0, 1,
	    (SCM method_name, SCM imethod, SCM class, SCM args),
	    "")
#define FUNC_NAME s_scm_corba_primitive_invoke_method
{
    CORBA_Object corba_objref;
    ORBit_IMethod *method;
    CORBA_Environment ev;
    gpointer ret = NULL;
    gpointer *arg = NULL;
    SCM retval = SCM_UNSPECIFIED;
    gulong num_args, i;

    SCM_VALIDATE_STRING (1, method_name);
    SCM_VALIDATE_ORBIT_IMETHOD_COPY (2, imethod, method);
    SCM_VALIDATE_CORBA_OBJECT_COPY (3, class, corba_objref);

    num_args = method->arguments._length ? method->arguments._length-1 : 0;
    if (scm_ilength (args) != num_args)
	scm_error_num_args_subr (SCM_STRING_CHARS (method_name));

    CORBA_exception_init (&ev);

    if (method->ret)
	ret = ORBit_small_alloc (method->ret);

    arg = g_new0 (gpointer, num_args);
    for (i = 0; i < num_args; i++)
	arg [i] = ORBit_small_alloc (method->arguments._buffer [i].tc);

    for (i = 0; i < num_args; i++) {
	CORBA_any any = { method->arguments._buffer [i].tc, arg [i], FALSE };

	scm_c_corba_marshal_any (&any, scm_list_ref (args, SCM_MAKINUM (i)));
    }

    ORBit_small_invoke_stub (corba_objref, method, ret, arg,
			     CORBA_OBJECT_NIL, &ev);

    if (BONOBO_EX (&ev)) {
	g_free (arg);
	CORBA_free (ret);
	scm_c_corba_handle_exception (&ev);
	return SCM_UNSPECIFIED;
    }

    CORBA_exception_free (&ev);

    if (ret) {
	CORBA_any any = { method->ret, ret, FALSE };

	retval = scm_c_corba_demarshal_any (&any);
    }

    // CORBA_free (ret);
    // g_free (arg);

    return retval;
}
#undef FUNC_NAME


/*
static SCM
guile_corba_value_to_scm (CORBA_TypeCode tc, gpointer value)
{
    SCM retval = SCM_UNDEFINED;

    switch (tc->kind) {
#define _HANDLE_BASIC_VALUE(k,t,f)						\
case CORBA_tk_ ## k:								\
	retval =  ## f (*(CORBA_ ## t *) value);				\
	break;

	_HANDLE_BASIC_VALUE (short,     short,              scm_short2num);
	_HANDLE_BASIC_VALUE (long,      long,               scm_long2num);
	_HANDLE_BASIC_VALUE (ushort,    unsigned_short,     scm_int2num);
	_HANDLE_BASIC_VALUE (ulong,     unsigned_long,      scm_ulong2num);
	_HANDLE_BASIC_VALUE (longlong,  long_long,          scm_long_long2num);
	_HANDLE_BASIC_VALUE (ulonglong, unsigned_long_long, scm_long_long2num);
	_HANDLE_BASIC_VALUE (float,     float,              scm_float2num);
	_HANDLE_BASIC_VALUE (double,    double,             scm_double2num);

#undef _HANDLE_BASIC_VALUE
    }

    return retval;
}
*/
static SCM
guile_corba_portable_server_servant_mark (SCM servant)
{
    GuilePortableServer_Servant *gservant;

    gservant = (GuilePortableServer_Servant *) SCM_SMOB_DATA (servant);
    return gservant->this;
}

static size_t
guile_corba_portable_server_servant_free (SCM smob_servant)
{
    GuilePortableServer_Servant *gservant;
    PortableServer_ServantBase *servant;
    CORBA_Environment ev;

    gservant = (GuilePortableServer_Servant *) SCM_SMOB_DATA (smob_servant);
    servant = (PortableServer_ServantBase *) gservant;

    CORBA_exception_init (&ev);
    PortableServer_POA_deactivate_object (guile_corba_poa, gservant->objid, &ev);
    g_assert (!BONOBO_EX (&ev));

    PortableServer_ServantBase__fini (servant, &ev);
    g_assert (!BONOBO_EX (&ev));

    g_free (servant);

    return sizeof (GuilePortableServer_Servant);
}



SCM_DEFINE (scm_corba_primitive_find_poa_class, "corba-primitive-find-poa-class", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_corba_primitive_find_poa_class
{
    SCM cpl;
    long i;

    SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE_CLASS (1, class);

    cpl = scm_class_precedence_list (class);

    for (i = 0; i < scm_ilength (cpl); i++) {
	SCM this = scm_list_ref (cpl, SCM_MAKINUM (i));
	SCM slots, slot;

	slots = scm_class_slots (this);
	slot = scm_assq (scm_sym_orbit_iinterface, slots);

	if (SCM_NFALSEP (scm_slot_bound_using_class_p (this, class, scm_sym_orbit_iinterface)))
	    return this;
    }

    SCM_ASSERT (FALSE, class, SCM_ARG1, FUNC_NAME);
    return SCM_UNDEFINED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_primitive_make_poa_instance, "corba-primitive-make-poa-instance", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_corba_primitive_make_poa_instance
{
    GuilePortableServer_Servant *gservant;
    PortableServer_ServantBase *servant;
    GuileCorbaInterface *interface;
    CORBA_Environment ev;
    SCM object, smob;

    SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE_CLASS_COPY (1, class, interface);

    gservant = g_new0 (GuilePortableServer_Servant, 1);
    gservant->this = SCM_UNDEFINED;
    gservant->interface = interface;
    servant = (PortableServer_ServantBase *) gservant;
    servant->vepv = interface->vepv;
    ORBIT_SERVANT_SET_CLASSINFO (servant, &interface->class_info);

    CORBA_exception_init (&ev);
    PortableServer_ServantBase__init (servant, &ev);
    g_assert (!BONOBO_EX (&ev));

    gservant->objid = PortableServer_POA_activate_object (guile_corba_poa, servant, &ev);
    g_assert (!BONOBO_EX (&ev));

    SCM_NEWSMOB (smob, scm_tc16_guile_portable_server_servant, servant);

    object = scm_make (SCM_LIST1 (class));
    scm_slot_set_x (object, scm_sym_servant, smob);

    gservant->this = object;

    return object;
}
#undef FUNC_NAME



static SCM
repo_id_to_symbol (const gchar *format, const gchar *repo_id)
{
    gchar *new_repo_id;
    SCM retval;

    new_repo_id = guile_corba_generic_repo_id_to_name (format, repo_id);
    retval = scm_mem2symbol (new_repo_id, strlen (new_repo_id));
    g_free (new_repo_id);
    return retval;
}



SCM_DEFINE (scm_corba_typecode_primitive_p, "corba-typecode-primitive?", 1, 0, 0,
	    (SCM typecode),
	    "")
#define FUNC_NAME s_scm_corba_typecode_primitive_p
{
    return SCM_BOOL (SCM_TYP16_PREDICATE (scm_tc16_corba_typecode, typecode));
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_typecode_primitive_to_name, "corba-typecode-primitive->name", 1, 0, 0,
	    (SCM typecode),
	    "")
#define FUNC_NAME s_scm_corba_typecode_primitive_to_name
{
    CORBA_TypeCode tc;

    SCM_VALIDATE_CORBA_TYPECODE_COPY (1, typecode, tc);

    return repo_id_to_symbol ("TC:%s", tc->repo_id);
}
#undef FUNC_NAME



static SCM
scm_skel_marshal_func (SCM cclo)
{
    SCM proc, args;

    proc = SCM_VELTS (cclo)[1];
    args = SCM_VELTS (cclo)[2];

    return scm_apply (proc, args, SCM_EOL);
}

static SCM
scm_c_generic_skel_func_exception (void *data, SCM tag, SCM throw_args)
{
    CORBA_Environment *ev = data;
    SCM cur_outp = scm_current_output_port();
    
    scm_display (tag, cur_outp); scm_newline (cur_outp);
    scm_display (throw_args, cur_outp); scm_newline (cur_outp);

    if (SCM_EQ_P (tag, scm_sym_corba_system_exception)) {
	g_message (G_STRLOC ": CORBA system exception");
    } else if (SCM_EQ_P (tag, scm_sym_corba_user_exception)) {
	g_message (G_STRLOC ": CORBA user exception");
    } else {
	g_message (G_STRLOC ": Unknown exception");
	CORBA_exception_set_system (ev, ex_CORBA_UNKNOWN, CORBA_COMPLETED_MAYBE);
    }

    return SCM_UNSPECIFIED;
}

static void
scm_c_generic_skel_func (PortableServer_ServantBase *servant,
                         gpointer retvalptr, gpointer *argptr,
			 gpointer ctx, CORBA_Environment *ev,
                         gpointer implementation)
{
    GuilePortableServer_Servant *gservant = (GuilePortableServer_Servant *) servant;
    ORBit_IMethod *imethod;
    SCM poa_vector, generic, func, proc, args, thunk, retval;
    struct scm_body_thunk_data thunk_data;
    gulong i, length;
    SCM cur_outp = scm_current_output_port();

    g_message (G_STRLOC ": %p - %p", servant, implementation);

    poa_vector = (SCM) implementation;
    scm_display (poa_vector, cur_outp); scm_newline (cur_outp);
    scm_display (gservant->this, cur_outp); scm_newline (cur_outp);

    imethod = (ORBit_IMethod *) SCM_SMOB_DATA (SCM_VELTS (poa_vector)[1]);
    generic = SCM_VELTS (poa_vector)[3];

    args = SCM_LIST1 (gservant->this);

    length = imethod->arguments._length;
    if (length) length--;

    for (i = 0; i < length; i++) {
	ORBit_IArg *arg = &imethod->arguments._buffer [i];
	CORBA_any any = { arg->tc, argptr [i], FALSE };
	SCM value;

	value = scm_c_corba_demarshal_any (&any);
	args = scm_append_x (SCM_LIST2 (args, SCM_LIST1 (value)));
    }

    func = scm_compute_applicable_methods (generic, args, scm_ilength (args), 1);
    if (SCM_FALSEP (func)) {
	CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);
	return;
    }

    proc = scm_method_procedure (SCM_CAR (func));

    if (!scm_procedure_p (proc)) {
	CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);
	return;
    }

    thunk = scm_makcclo (scm_f_skel_marshal_func, 3L);
    SCM_VECTOR_SET (thunk, 1, generic);
    SCM_VECTOR_SET (thunk, 2, args);

    thunk_data.tag = SCM_BOOL_T;
    thunk_data.body_proc = thunk;

    retval = scm_internal_catch (SCM_BOOL_T, scm_body_thunk, &thunk_data,
				 scm_c_generic_skel_func_exception, ev);

    scm_display (retval, cur_outp); scm_newline (cur_outp);
}

static ORBitSmallSkeleton
impl_finder_func (PortableServer_ServantBase *servant, const gchar *opname,
                  gpointer *m_data, gpointer *impl)
{
    GuilePortableServer_Servant *gservant = (GuilePortableServer_Servant *) servant;
    SCM poa_vector;
    gpointer value;

    g_message (G_STRLOC ": %p - |%s|", servant, opname);

    if (!g_hash_table_lookup_extended (gservant->interface->epv_hash, opname, NULL, &value)) {
	g_warning (G_STRLOC ": Invalid operation '%s'", opname);
	return NULL;
    }

    poa_vector = (SCM) value;

    *m_data = (ORBit_IMethod *) SCM_SMOB_DATA (SCM_VECTOR_REF (poa_vector, 1));
    *impl = poa_vector;

    return scm_c_generic_skel_func;
}

G_GNUC_UNUSED static void
init_vepvmap_func (ORBit_VepvIdx * map)
{
    /* PortableServer_ServantBase__vepv *fakevepv = 0; */
}



static void
guile_corba_register_type (CORBA_TypeCode tc)
{
    SCM name;
    SCM typecode;

    name = repo_id_to_symbol ("TC:%s", tc->repo_id);
    SCM_NEWSMOB (typecode, scm_tc16_corba_typecode, ORBit_RootObject_duplicate (tc));
    scm_define (name, typecode);

    guile_corba_generic_typecode_to_type (tc);
}

static void
guile_corba_sys_register_interface (ORBit_IInterface *iinterface)
{
    static GHashTable *iinterface_hash = NULL;
    GuileCorbaInterface *interface;
    SCM poa_class, poa_class_name, poa_parent_classes, poa_meta_class;
    SCM stub_class, stub_class_name, stub_parent_classes, stub_meta_class;
    SCM iinterface_smob;
    gulong length, i, j;

    if (!iinterface_hash)
	iinterface_hash = g_hash_table_new (g_str_hash, g_str_equal);

    if (g_hash_table_lookup (iinterface_hash, iinterface->tc->repo_id)) {
	g_warning (G_STRLOC ": Already registered interface `%s'",
		   iinterface->tc->repo_id);
	return;
    }

    guile_corba_register_type (iinterface->tc);

    stub_class_name = repo_id_to_symbol ("<%s>", iinterface->tc->repo_id);
    poa_class_name = repo_id_to_symbol ("<POA:%s>", iinterface->tc->repo_id);

    interface = g_new0 (GuileCorbaInterface, 1);
    interface->iinterface = iinterface;

    interface->class_info.small_relay_call = &impl_finder_func;
    //interface->class_info.vepvmap = &init_vepvmap_func; //init_
    interface->class_info.class_name = g_strdup (iinterface->tc->repo_id);
    interface->class_info.class_id = g_new0 (CORBA_unsigned_long,1 );
    ORBit_classinfo_register (&interface->class_info);

    interface->epv_hash = g_hash_table_new (g_str_hash, g_str_equal);

    interface->epv_size = iinterface->methods._length;
    interface->epv = g_new0 (gpointer, interface->epv_size);

    g_assert (iinterface->base_interfaces._length >= 1);
    length = iinterface->base_interfaces._length - 1;

    interface->vepv = (PortableServer_ServantBase__vepv *) g_new0 (gpointer, 2);
    interface->vepv [0] = g_new0 (PortableServer_ServantBase__epv, 1);

    SCM_NEWSMOB (iinterface_smob, scm_tc16_guile_corba_interface, interface);

    poa_meta_class = scm_class_portable_server_servant_base;
    poa_parent_classes = SCM_LIST1 (poa_meta_class);

    stub_meta_class = scm_class_corba_object;
    stub_parent_classes = SCM_LIST1 (stub_meta_class);

    for (i = 0; i < length; i++) {
	GuileCorbaInterface *base_interface;
	const CORBA_char *repo_id;

	repo_id = iinterface->base_interfaces._buffer [i];
	base_interface = g_hash_table_lookup (iinterface_hash, repo_id);
	if (!base_interface) {
	    g_warning (G_STRLOC ": Unknown base interface `%s' in interface `%s'",
		       repo_id, iinterface->tc->repo_id);
	    continue;
	}

	interface->vepv [i+1] = (PortableServer_ServantBase__epv *) base_interface->epv;

	poa_meta_class = base_interface->poa_class;
	poa_parent_classes = scm_append_x (SCM_LIST2 (SCM_LIST1 (poa_meta_class), poa_parent_classes));

	stub_meta_class = base_interface->stub_class;
	stub_parent_classes = scm_append_x (SCM_LIST2 (SCM_LIST1 (stub_meta_class), stub_parent_classes));

	for (j = 0; j < base_interface->iinterface->methods._length; j++) {
	    ORBit_IMethod *imethod = &base_interface->iinterface->methods._buffer [j];

	    g_hash_table_insert (interface->epv_hash, imethod->name,
				 base_interface->epv [j]);
	}
    }

    stub_class = scm_apply (scm_sym_make_class,
			    scm_cons2 (stub_parent_classes, SCM_EOL,
				       SCM_LIST4 (k_name, stub_class_name,
						  k_metaclass, stub_meta_class)),
			    SCM_EOL);

    scm_call_3 (scm_sym_class_slot_set_x, stub_class, scm_sym_corba_typecode,
		scm_c_make_corba_typecode (iinterface->tc));

    scm_define (stub_class_name, stub_class);

    poa_class = scm_apply (scm_sym_make_class,
			   scm_cons2 (poa_parent_classes, SCM_EOL,
				      SCM_LIST4 (k_name, poa_class_name,
						 k_metaclass, poa_meta_class)),
			   SCM_EOL);

    scm_call_3 (scm_sym_class_slot_set_x, poa_class, scm_sym_orbit_iinterface,
		iinterface_smob);

    scm_define (poa_class_name, poa_class);

    interface->poa_class = scm_gc_protect_object (poa_class);
    interface->stub_class = scm_gc_protect_object (stub_class);

    for (i = 0; i < iinterface->methods._length; i++) {
	ORBit_IMethod *imethod = &iinterface->methods._buffer [i];
	SCM method_name, method_gsubr, method_formals, method_args;
	SCM imethod_smob, method_closure, method;
	SCM specializers, poa_vector;
	gulong num_args;

	{
	    gchar *format = g_strdup_printf ("%%s:%s", imethod->name);
	    method_name = repo_id_to_symbol (format, iinterface->tc->repo_id);
	    g_free (format);
	}

	method_gsubr = scm_c_define_gsubr (SCM_SYMBOL_CHARS (method_name), 4, 0, 0,
					   scm_corba_primitive_invoke_method);

	SCM_NEWSMOB (imethod_smob, scm_tc16_orbit_imethod, imethod);

	method_formals = scm_cons (sym_object, sym_args);
	method_args = SCM_LIST5 (method_gsubr, scm_symbol_to_string (method_name),
				 imethod_smob, sym_object, sym_args);

	specializers = SCM_LIST1 (stub_class);

	num_args = imethod->arguments._length;
	if (num_args) --num_args;

	for (j = 0; j < num_args; j++) {
	    ORBit_IArg *arg = &imethod->arguments._buffer [j];
	    SCM class;

	    class = scm_c_corba_typecode_to_class (arg->tc);
	    specializers = scm_append_x (SCM_LIST2 (specializers, SCM_LIST1 (class)));
	}

	method_closure = scm_closure (SCM_LIST2 (method_formals, method_args),
				      SCM_EOL);

	method = scm_make (SCM_LIST3 (scm_class_generic,
				      k_name, method_name));

	scm_add_method (method, scm_make (SCM_LIST5 (scm_class_method,
						     k_procedure, method_closure,
						     k_specializers, specializers)));
	scm_define (method_name, method);
	
	poa_vector = scm_c_make_vector (4L, SCM_UNDEFINED);
	SCM_VECTOR_SET (poa_vector, 0, iinterface_smob);
	SCM_VECTOR_SET (poa_vector, 1, imethod_smob);
	SCM_VECTOR_SET (poa_vector, 2, SCM_MAKINUM (i));
	SCM_VECTOR_SET (poa_vector, 3, method);

	interface->epv [i] = (gpointer)(scm_gc_protect_object (poa_vector));

	g_hash_table_insert (interface->epv_hash, imethod->name, interface->epv [i]);
    }

    g_hash_table_insert (iinterface_hash, iinterface->tc->repo_id, interface);
}



SCM
scm_c_corba_typecode_primitive_to_class (CORBA_TypeCode tc)
{
    SCM class_name;

    class_name = repo_id_to_symbol ("<%s>", tc->repo_id);
    return SCM_VARIABLE_REF (scm_lookup (class_name));
}

SCM_DEFINE (scm_corba_typecode_primitive_to_class, "corba-primitive-typecode->class", 1, 0, 0,
	    (SCM type),
	    "")
#define FUNC_NAME s_scm_corba_typecode_primitive_to_class
{
    CORBA_TypeCode tc;

    SCM_VALIDATE_CORBA_TYPECODE_COPY (1, type, tc);

    return scm_c_corba_typecode_primitive_to_class (tc);
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_primitive_open_module, "corba-primitive-open-module", 1, 0, 0,
	    (SCM name),
	    "")
#define FUNC_NAME s_scm_corba_primitive_open_module
{
    CORBA_sequence_ORBit_IInterface *iinterfaces;
    CORBA_sequence_CORBA_TypeCode *types;
    gulong i;

    SCM_VALIDATE_STRING (1, name);

    if (!ORBit_small_load_typelib (SCM_STRING_CHARS (name)))
	return SCM_BOOL_F;

    types = ORBit_small_get_types (SCM_STRING_CHARS (name));
    g_assert (types != NULL);

    iinterfaces = ORBit_small_get_iinterfaces (SCM_STRING_CHARS (name));
    g_assert (iinterfaces != NULL);

    for (i = 0; i < iinterfaces->_length; i++)
	guile_corba_sys_register_interface (&iinterfaces->_buffer [i]);

    for (i = 0; i < types->_length; i++)
	guile_corba_register_type (types->_buffer [i]);

    return SCM_BOOL_T;
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_primitive_register_interface, "corba-primitive-register-interface", 1, 0, 0,
	    (SCM name),
	    "")
#define FUNC_NAME s_scm_corba_primitive_register_interface
{
    ORBit_IInterface *iinterface;
    CORBA_Environment ev;

    SCM_VALIDATE_STRING (1, name);

    CORBA_exception_init (&ev);
    iinterface = ORBit_small_get_iinterface (CORBA_OBJECT_NIL, SCM_STRING_CHARS (name), &ev);
    if (BONOBO_EX (&ev)) {
	CORBA_exception_free (&ev);
	return SCM_UNSPECIFIED;
    }

    guile_corba_sys_register_interface (iinterface);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_corba_primitive_main, "corba-primitive-main", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_corba_primitive_main
{
    g_main_loop_run (guile_corba_main_loop);

    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_pre_init_gnome_corba_primitives (void)
{
    CORBA_Environment ev;
    gchar *my_argv[] = { "guile-corba", NULL };
    int my_argc = 1;
 
    g_print ("\n\ninitializing corba primitives... ");

    // g_log_set_always_fatal (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL);

    scm_tc16_guile_corba_interface = scm_make_smob_type ("%guile-corba-interface", 0);
    scm_set_smob_free (scm_tc16_guile_corba_interface, NULL);

    scm_tc16_guile_portable_server_servant = scm_make_smob_type
	("PortableServer-Servant", sizeof (GuilePortableServer_Servant));
    scm_set_smob_free (scm_tc16_guile_portable_server_servant,
		       guile_corba_portable_server_servant_free);
    scm_set_smob_mark (scm_tc16_guile_portable_server_servant,
		       guile_corba_portable_server_servant_mark);

    guile_corba_register_type (TC_ORBit_IInterface);
    guile_corba_register_type (TC_ORBit_ITypes);

    bonobo_init (&my_argc,my_argv);

    CORBA_exception_init (&ev);
    guile_corba_orb = CORBA_ORB_init (&my_argc, my_argv, "orbit-local-orb", &ev);
    g_assert (!BONOBO_EX (&ev));
    guile_corba_poa = (PortableServer_POA)CORBA_ORB_resolve_initial_references
	(guile_corba_orb, "RootPOA", &ev);
    g_assert (!BONOBO_EX (&ev));
    PortableServer_POAManager_activate (PortableServer_POA__get_the_POAManager (guile_corba_poa, &ev), &ev);
    g_assert (!BONOBO_EX (&ev));
    guile_corba_dynany_factory = (DynamicAny_DynAnyFactory)
	CORBA_ORB_resolve_initial_references (guile_corba_orb, "DynAnyFactory", &ev);
    g_assert (!BONOBO_EX (&ev));
    CORBA_exception_free (&ev);

    guile_corba_main_loop = g_main_loop_new (NULL, FALSE);

    g_print ("done.\n\n");
}

void
scm_init_gnome_corba_primitives (void)
{
#include "guile-gnome-corba-primitives.x"

    scm_f_skel_marshal_func = scm_c_make_subr ("skel-marshal-func", scm_tc7_subr_1,
					       scm_skel_marshal_func);

    scm_class_corba_object = scm_permanent_object
	(SCM_VARIABLE_REF (scm_c_lookup ("<CORBA:Object>")));
#if 0
    scm_class_corba_object_meta = scm_permanent_object
	(SCM_VARIABLE_REF (scm_c_lookup ("<%CORBA:MetaObject>")));
#endif

    scm_class_portable_server_servant_base = scm_permanent_object
	(SCM_VARIABLE_REF (scm_c_lookup ("<PortableServer-ServantBase>")));

    scm_c_export (
		  s_scm_corba_primitive_invoke_method,
		  s_scm_corba_primitive_find_poa_class,
		  s_scm_corba_primitive_make_poa_instance,
		  s_scm_corba_typecode_primitive_p,
		  s_scm_corba_typecode_primitive_to_name,
		  s_scm_corba_typecode_primitive_to_class,
		  s_scm_corba_primitive_open_module,
		  s_scm_corba_primitive_register_interface,
		  s_scm_corba_primitive_main,
		  NULL);
}
