#ifndef __GUILE_GNOME_GOBJECT_PRIMITIVES_H__
#define __GUILE_GNOME_GOBJECT_PRIMITIVES_H__

#include <glib.h>
#include <glib-object.h> 
#include <libguile.h>

G_BEGIN_DECLS

typedef struct _GuileGClosure      GuileGClosure;

struct _GuileGClosure {
    GClosure closure;

    SCM func;
};



extern SCM scm_class_gtype_class;
extern SCM scm_gobject_module;
extern SCM scm_sym_gclosure;
extern SCM scm_sym_gtype_instance;
extern SCM scm_sym_gtype_instance_write;
extern SCM scm_sym_gtype_instance_instance_init;
extern SCM scm_sym_gtype_instance_class_init;
extern SCM scm_sym_gobject_instance_init;
extern SCM scm_sym_gobject_class_init;
extern SCM scm_sym_make_class;
extern SCM scm_sym_class_slot_ref;
extern SCM scm_sym_class_slot_set_x;
extern SCM scm_sym_gtype;
extern SCM scm_sym_pspec_struct;
extern scm_bits_t scm_tc16_gtype;
extern scm_bits_t scm_tc16_gvalue;
extern scm_bits_t scm_tc16_gtype_instance;
extern SCM scm_gsignal_vtable;
extern SCM scm_gparam_spec_vtable;



#define SCM_ERROR_NOT_YET_IMPLEMENTED(what)	scm_error (sym_gruntime_error, FUNC_NAME, \
							   "Not yet implemented: file ~S line ~S: ~A", \
							   SCM_LIST3 (scm_makfrom0str (__FILE__), \
								      SCM_MAKINUM (__LINE__), \
								      what), \
							   SCM_EOL)



#define scm_si_gsignal_id			(scm_vtable_offset_user)
#define scm_si_gsignal_name			(scm_vtable_offset_user + 1)
#define scm_si_gsignal_interface_type		(scm_vtable_offset_user + 2)
#define scm_si_gsignal_return_type		(scm_vtable_offset_user + 3)
#define scm_si_gsignal_flags			(scm_vtable_offset_user + 4)
#define scm_si_gsignal_params			(scm_vtable_offset_user + 5)

#define SCM_GSIGNAL_STRUCTP(x)			(SCM_STRUCTP (x) && \
						 SCM_EQ_P (scm_struct_vtable (x), scm_gsignal_vtable))

#define SCM_SET_GSIGNAL_ID(x,f)			{SCM_STRUCT_DATA (x) [scm_si_gsignal_id] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_NAME(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gsignal_name] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_INTERFACE_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_si_gsignal_interface_type] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_RETURN_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_si_gsignal_return_type] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_FLAGS(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gsignal_flags] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_PARAMS(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gsignal_params] = SCM_UNPACK (f);}

#define SCM_GSIGNAL_ID(x)			SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_id]))
#define SCM_GSIGNAL_NAME(x)			SCM_SYMBOL_CHARS (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_name])))
#define SCM_GSIGNAL_INTERFACE_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_interface_type]))))
#define SCM_GSIGNAL_RETURN_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_return_type]))))
#define SCM_GSIGNAL_FLAGS(x)			SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_flags]))
#define SCM_GSIGNAL_PARAMS(x)			SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_params]))



#define scm_si_gparam_spec_name			(scm_vtable_offset_user)
#define scm_si_gparam_spec_nick			(scm_vtable_offset_user + 1)
#define scm_si_gparam_spec_blurb		(scm_vtable_offset_user + 2)
#define scm_si_gparam_spec_flags		(scm_vtable_offset_user + 3)
#define scm_si_gparam_spec_param_type		(scm_vtable_offset_user + 4)
#define scm_si_gparam_spec_value_type		(scm_vtable_offset_user + 5)
#define scm_si_gparam_spec_owner_type		(scm_vtable_offset_user + 6)
#define scm_si_gparam_spec_n_args		(scm_vtable_offset_user + 7)
#define scm_si_gparam_spec_args			(scm_vtable_offset_user + 8)

#define SCM_GPARAM_SPEC_STRUCTP(x)		(SCM_STRUCTP (x) && \
						 SCM_EQ_P (scm_struct_vtable (x), scm_gparam_spec_vtable))

#define SCM_SET_GPARAM_SPEC_NAME(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_name] = SCM_UNPACK (scm_str2symbol (f));}
#define SCM_SET_GPARAM_SPEC_NICK(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_nick] = SCM_UNPACK (scm_str2string (f));}
#define SCM_SET_GPARAM_SPEC_BLURB(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_blurb] = SCM_UNPACK (scm_str2string (f));}
#define SCM_SET_GPARAM_SPEC_FLAGS(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_flags] = SCM_UNPACK (f);}
#define SCM_SET_GPARAM_SPEC_PARAM_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_param_type] = SCM_UNPACK (scm_c_register_gtype (f));}
#define SCM_SET_GPARAM_SPEC_VALUE_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_value_type] = SCM_UNPACK (scm_c_register_gtype (f));}
#define SCM_SET_GPARAM_SPEC_OWNER_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_owner_type] = SCM_UNPACK (scm_c_register_gtype (f));}
#define SCM_SET_GPARAM_SPEC_ARG(x,i,f)		{SCM_STRUCT_DATA (x) [scm_si_gparam_spec_args + i] = SCM_UNPACK (f);}

#define SCM_GPARAM_SPEC_NAME(x)			SCM_SYMBOL_CHARS (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gparam_spec_name])))
#define SCM_GPARAM_SPEC_NICK(x)			(SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_gparam_spec_nick])) ? \
						NULL : \
						SCM_STRING_CHARS (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_gparam_spec_nick])))
#define SCM_GPARAM_SPEC_BLURB(x)		(SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_gparam_spec_blurb])) ? \
						NULL : \
						SCM_STRING_CHARS (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_gparam_spec_blurb])))
#define SCM_GPARAM_SPEC_FLAGS(x)		SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gparam_spec_flags]))
#define SCM_GPARAM_SPEC_PARAM_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gparam_spec_param_type]))))
#define SCM_GPARAM_SPEC_VALUE_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gparam_spec_value_type]))))
#define SCM_GPARAM_SPEC_OWNER_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gparam_spec_owner_type]))))
#define SCM_GPARAM_SPEC_N_ARGS(x)		(SCM_STRUCT_DATA (x) [scm_si_gparam_spec_n_args])
#define SCM_GPARAM_SPEC_ARG(x,i)		SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gparam_spec_args + i]))


#define SCM_G_IS_PARAM_SPEC_BOOLEAN(x) (x == G_TYPE_PARAM_BOOLEAN)
#define SCM_G_IS_PARAM_SPEC_STRING(x) (x == G_TYPE_PARAM_STRING)
#define SCM_G_IS_PARAM_SPEC_OBJECT(x) (x == G_TYPE_PARAM_OBJECT)
#define SCM_G_IS_PARAM_SPEC_BOXED(x) (x == G_TYPE_PARAM_BOXED)
#define SCM_G_IS_PARAM_SPEC_UNICHAR(x) (x == G_TYPE_PARAM_UNICHAR)
#define SCM_G_IS_PARAM_SPEC_CHAR(x) (x == G_TYPE_PARAM_CHAR)
#define SCM_G_IS_PARAM_SPEC_UCHAR(x) (x == G_TYPE_PARAM_UCHAR)
#define SCM_G_IS_PARAM_SPEC_INT(x) (x == G_TYPE_PARAM_INT)
#define SCM_G_IS_PARAM_SPEC_UINT(x) (x == G_TYPE_PARAM_UINT)
#define SCM_G_IS_PARAM_SPEC_LONG(x) (x == G_TYPE_PARAM_LONG)
#define SCM_G_IS_PARAM_SPEC_ULONG(x) (x == G_TYPE_PARAM_ULONG)
#define SCM_G_IS_PARAM_SPEC_INT64(x) (x == G_TYPE_PARAM_INT64)
#define SCM_G_IS_PARAM_SPEC_UINT64(x) (x == G_TYPE_PARAM_UINT64)
#define SCM_G_IS_PARAM_SPEC_FLOAT(x) (x == G_TYPE_PARAM_FLOAT)
#define SCM_G_IS_PARAM_SPEC_DOUBLE(x) (x == G_TYPE_PARAM_DOUBLE)
#define SCM_G_IS_PARAM_SPEC_POINTER(x) (x == G_TYPE_PARAM_POINTER)
#define SCM_G_IS_PARAM_SPEC_ENUM(x) (x == G_TYPE_PARAM_ENUM)
#define SCM_G_IS_PARAM_SPEC_FLAGS(x) (x == G_TYPE_PARAM_FLAGS)


#define SCM_GTYPEP(scm)				SCM_TYP16_PREDICATE (scm_tc16_gtype, scm)
#define SCM_GTYPE_CLASSP(scm)			SCM_IS_A_P (scm, scm_class_gtype_class)

#define SCM_VALIDATE_GTYPE(pos, scm)		SCM_VALIDATE_SMOB (pos, scm, gtype)
#define SCM_VALIDATE_GTYPE_CLASS(pos, scm)	SCM_MAKE_VALIDATE (pos, scm, GTYPE_CLASSP)
#define SCM_VALIDATE_GVALUE(pos, scm)		SCM_VALIDATE_SMOB (pos, scm, gvalue)
#define SCM_VALIDATE_GTYPE_INSTANCE(pos, scm)	SCM_VALIDATE_SMOB (pos, scm, gtype_instance)

#define SCM_VALIDATE_GTYPE_COPY(pos, type, cvar) \
  do { \
    SCM_VALIDATE_GTYPE (pos, type); \
    cvar = (GType) SCM_SMOB_DATA (type); \
  } while (0)

#define SCM_VALIDATE_GTYPE_IS_A(pos, scm, is_a, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_COPY (pos, type, cvar); \
    SCM_ASSERT (g_type_is_a (cvar, is_a), type, pos, FUNC_NAME); \
  } while (0)


#define SCM_VALIDATE_GVALUE_COPY(pos, value, cvar) \
  do { \
    SCM_VALIDATE_GVALUE (pos, value); \
    cvar = (GValue *) SCM_SMOB_DATA (value); \
  } while (0)
   
#define SCM_VALIDATE_GVALUE_TYPE_COPY(pos, value, type, cvar) \
  do { \
    SCM_VALIDATE_GVALUE_COPY (pos, value, cvar); \
    SCM_ASSERT (G_TYPE_CHECK_VALUE_TYPE (cvar, type), value, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_GTYPE_INSTANCE_COPY(pos, value, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_INSTANCE (pos, value); \
    cvar = (GTypeInstance *) SCM_SMOB_DATA (value); \
  } while (0)

#define SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY(pos, value, type, ctype, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_INSTANCE (pos, value); \
    cvar = (ctype *) SCM_SMOB_DATA (value); \
    SCM_ASSERT (G_TYPE_CHECK_INSTANCE_TYPE (cvar, type), value, pos, FUNC_NAME); \
  } while (0)

#define SCM_GPARAM_SPEC_P(value) \
    (SCM_EQ_P (scm_struct_vtable (value), scm_gparam_spec_vtable) && \
     SCM_SYMBOLP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_name])) && \
     (SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_nick])) || \
      SCM_STRINGP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_nick]))) && \
     (SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_blurb])) || \
      SCM_STRINGP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_blurb]))) && \
     SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_param_type])) && \
     SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_value_type])) && \
     SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gparam_spec_owner_type])) && \
     TRUE)

#define SCM_VALIDATE_GPARAM_SPEC(pos, value) \
  do { \
    SCM_VALIDATE_STRUCT (pos, value); \
    SCM_ASSERT (SCM_GPARAM_SPEC_P (value), value, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_GSIGNAL(pos, value) \
  do { \
    SCM_VALIDATE_STRUCT (pos, value); \
    SCM_ASSERT (SCM_EQ_P (scm_struct_vtable (value), scm_gsignal_vtable) && \
		SCM_INUMP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_id])) && \
		SCM_SYMBOLP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_name])) && \
		SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_interface_type])) && \
		SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_return_type])) && \
		SCM_VECTORP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_params])) && \
		TRUE, \
		value, pos, FUNC_NAME); \
  } while (0)

#ifndef SCM_VALIDATE_FLOAT_COPY
#define SCM_VALIDATE_FLOAT_COPY(pos,z,cvar) \
  do { \
    double x; \
    SCM_ASSERT (SCM_INUMP (z) || SCM_BIGP (z) || SCM_REALP (z), z, pos, FUNC_NAME); \
    x = scm_num2dbl (z, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, z, (- G_MAXFLOAT <= x) && (x <= G_MAXFLOAT)); \
    cvar = (float) x; \
  } while (0)
#endif


void scm_pre_init_gnome_gobject_primitives (void);
void scm_init_gnome_gobject_primitives (void);



GType gboxed_scm_get_type (void) G_GNUC_CONST;
#define G_TYPE_GBOXED_SCM (gboxed_scm_get_type ())



SCM scm_gtype_primitive_create_basic_instance (SCM type);
SCM scm_gobject_primitive_create_instance (SCM class, SCM type, SCM object, SCM properties);
SCM scm_gtype_instance_primitive (SCM object);
SCM scm_gtype_instance_primitive_to_type (SCM instance);
SCM scm_gtype_instance_primitive_to_value (SCM instance);
SCM scm_gobject_primitive_get_signals (SCM type);
SCM scm_gobject_primitive_get_properties (SCM type);
SCM scm_gobject_primitive_signal_emit (SCM object, SCM signal, SCM args);
SCM scm_gobject_primitive_signal_connect (SCM object, SCM id, SCM closure, SCM after);
SCM scm_gobject_primitive_get_property (SCM object, SCM name);
SCM scm_gobject_primitive_set_property (SCM object, SCM name, SCM value);
SCM scm_genum_primitive_get_values (SCM type);
SCM scm_gflags_primitive_get_values (SCM type);
SCM scm_gvalue_primitive_set_enum (SCM instance, SCM value);
SCM scm_gvalue_primitive_set_flags (SCM instance, SCM value);
SCM scm_gclosure_primitive_new (SCM func);
SCM scm_gclosure_primitive_invoke (SCM instance, SCM return_type, SCM args);
SCM scm_gtype_primitive_basic_p (SCM instance);
SCM scm_gvalue_primitive_new (SCM type);
SCM scm_gvalue_primitive_get (SCM instance);
SCM scm_gvalue_primitive_set (SCM instance, SCM value);
SCM scm_gflags_primitive_bit_set_p (SCM value, SCM bit);
SCM scm_gsignal_primitive_handler_block (SCM instance, SCM handler_id);
SCM scm_gsignal_primitive_handler_unblock (SCM instance, SCM handler_id);
SCM scm_gsignal_primitive_handler_disconnect (SCM instance, SCM handler_id);
SCM scm_gsignal_primitive_handler_connected_p (SCM instance, SCM handler_id);
SCM scm_gsignal_primitive_create (SCM signal, SCM closure);
SCM scm_gparam_primitive_create_pspec_struct (SCM param);
SCM scm_gparam_primitive_to_pspec_struct (SCM param);
SCM scm_gparam_primitive_create (SCM class, SCM type, SCM object, SCM pspec_struct);
SCM scm_gparam_spec_p (SCM pspect_struct);
SCM scm_gboxed_scm_primitive_new (SCM scm_value);
SCM scm_gboxed_scm_primitive_to_scm (SCM value);



SCM scm_sys_gtype_lookup_class (SCM type);
SCM scm_sys_gtype_bind_to_class (SCM class, SCM type);
SCM scm_sys_gtype_get_roslot (SCM object, SCM offset);



gchar *scm_c_make_gtype_name (const gchar *format, const gchar *name);
SCM scm_c_register_gtype (GType type);
SCM scm_c_make_genum (GType type, gint value);
gint scm_c_get_enum (SCM obj);
SCM scm_c_make_gvalue (GType gtype);
SCM scm_c_make_gtype_instance (GTypeInstance *instance);
void scm_c_gtype_instance_bind_to_instance (SCM object, SCM instance);
SCM scm_c_gtype_lookup_class (GType gtype);
void scm_c_debug_print (const gchar *pos, SCM value);

#define SCM_DEBUG_PRINT(obj) scm_c_debug_print (G_STRLOC, obj)



SCM scm_gtype_class_to_type (SCM class);
SCM scm_gvalue_p (SCM class);
SCM scm_gvalue_to_type (SCM value);

void guile_gobject_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
                                const gchar *message, gpointer user_data);
void guile_gobject_register_sinkfunc (GType type, void (*sinkfunc) (GObject *));
void guile_gobject_register_postmakefunc (GType type, gpointer (*postmakefunc) (gpointer));

G_END_DECLS

#endif
