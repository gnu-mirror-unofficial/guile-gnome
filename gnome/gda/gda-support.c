#include "gda-support.h"
#include "guile-gnome-gobject.h"

static SCM scm_sym_null;

static void
scm_c_gda_value_set (GdaValue *gda_value, SCM value, int pos,
                     const char *func_name)
{
  if (SCM_SYMBOLP (value))
  {
    if (SCM_EQ_P (value, scm_sym_null))
      gda_value_set_null (gda_value);
    else
      scm_wrong_type_arg (func_name, 1, value);
  }
  else if (SCM_BOOLP (value))
    gda_value_set_boolean (gda_value, SCM_NFALSEP (value));
  else if (SCM_STRINGP (value))
    gda_value_set_string (gda_value, SCM_STRING_CHARS (value));
  else if (SCM_INUMP (value))
    gda_value_set_integer (gda_value, SCM_INUM (value));
#if SCM_SIZEOF_LONG_LONG != 0
  else if (SCM_NFALSEP (scm_integer_p (value)))
    gda_value_set_bigint (gda_value, scm_num2long_long (value, pos,
                                                        func_name));
#endif
  else if (SCM_NFALSEP (scm_number_p (value)))
    gda_value_set_double (gda_value, scm_num2double (value, pos,
                                                     func_name));
  else
    scm_wrong_type_arg (func_name, 1, value);
}

SCM_DEFINE (scm_gda_value_primitive_new, "gda-value-primitive-new", 1, 0, 0,
            (SCM value), "")
#define FUNC_NAME s_scm_gda_value_primitive_new
{
  SCM retval;
  GdaValue *gda_val = gda_value_new_null();
  
  scm_c_gda_value_set (gda_val, value, 1, FUNC_NAME);

  retval = scm_c_make_gvalue (GDA_TYPE_VALUE);
  g_value_set_boxed ((GValue *) SCM_SMOB_DATA (retval), gda_val);
  
  return retval;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gda_value_primitive_set_x, "gda-value-primitive-set!", 2, 0, 0,
            (SCM gda_value, SCM value), "")
#define FUNC_NAME s_scm_gda_value_primitive_set_x
{
  GdaValue *val;
  
  SCM_VALIDATE_GDA_VALUE_COPY (1, gda_value, val);
  
  scm_c_gda_value_set (val, value, 1, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gda_value_primitive_get, "gda-value-primitive-get", 1, 0, 0,
            (SCM gda_value), "")
#define FUNC_NAME s_scm_gda_value_primitive_get
{
  GdaValue *val;
  
  SCM_VALIDATE_GDA_VALUE_COPY (1, gda_value, val);

  switch (gda_value_get_type (val))
  {
    case GDA_VALUE_TYPE_NULL:
      return scm_sym_null;
      
    case GDA_VALUE_TYPE_BOOLEAN:
      return SCM_BOOL (gda_value_get_boolean (val));

    case GDA_VALUE_TYPE_STRING:
      return scm_makfrom0str (gda_value_get_string (val));

    case GDA_VALUE_TYPE_INTEGER:
      return scm_int2num (gda_value_get_integer (val));
      
    case GDA_VALUE_TYPE_UINTEGER:
      return scm_int2num (gda_value_get_uinteger (val));
      
    case GDA_VALUE_TYPE_SMALLINT:
      return SCM_MAKINUM (gda_value_get_smallint (val));

    case GDA_VALUE_TYPE_SMALLUINT:
      return SCM_MAKINUM (gda_value_get_smalluint (val));
      
#if SCM_SIZEOF_LONG_LONG != 0
    case GDA_VALUE_TYPE_BIGINT:
      return scm_long_long2num (gda_value_get_bigint (val));
        
    case GDA_VALUE_TYPE_BIGUINT:
      return scm_ulong_long2num (gda_value_get_bigint (val));
#endif

    case GDA_VALUE_TYPE_SINGLE:
      return scm_float2num (gda_value_get_single (val));

    case GDA_VALUE_TYPE_DOUBLE:
      return scm_double2num (gda_value_get_double (val));
      
    default:
      scm_misc_error (FUNC_NAME, "Unsupported GdaValueType", SCM_EOL);
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_gda_init, "gda-init", 2, 0, 1,
            (SCM name, SCM version, SCM args), "")
#define FUNC_NAME s_scm_gda_init
{
  int i;
  int argc;
  char **argv;
  
  SCM_VALIDATE_STRING (1, name);
  SCM_VALIDATE_STRING (2, version);
  
  /* Use guile command line if no args were given */
  if (SCM_NULLP (args))
    args = scm_program_arguments ();

  /* Build up argv for gda_init() */
  argc = scm_ilength (args);
  argv = g_new (char *, argc);
  for (i = 0; i < argc; i++)
  {
    SCM arg = SCM_CAR (args);
    
    if (!SCM_STRINGP (arg))
      scm_wrong_type_arg (FUNC_NAME, 2 + i, arg);
    
    argv[i] = SCM_STRING_CHARS (arg);
    
    args = SCM_CDR (args);
  }

  gda_init (SCM_STRING_CHARS (name), SCM_STRING_CHARS (version), argc, argv);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM make_instance;
SCM_KEYWORD (k_sys_primitive_instance, "%primitive-instance");

SCM
scm_c_make_gtype_primitive_instance (const char *class_name,
                                     GType type, gpointer val)
{
  SCM klass = SCM_VARIABLE_REF (scm_c_lookup (class_name));
  SCM primitive_instance = scm_c_make_gvalue (type);

  g_value_set_boxed ((GValue *) SCM_SMOB_DATA (primitive_instance), val);
  
  return scm_call_3 (make_instance, klass,
                     k_sys_primitive_instance, primitive_instance);
}

void
scm_init_gnome_gda_support (void)
{

#ifndef SCM_MAGIC_SNARFER
#include "gda-support.x"
#endif

  make_instance = SCM_VARIABLE_REF (
          scm_c_module_lookup (scm_module_goops, "make-instance"));
  
  scm_sym_null = scm_permanent_object (scm_str2symbol ("null"));
  
/*   scm_tc16_gda_value = scm_make_smob_type ("%gda-value", 0); */
/*   scm_set_smob_free (scm_tc16_gda_value, scm_gda_value_free); */
/*   scm_set_smob_print (scm_tc16_gda_value, scm_gda_value_print); */

  scm_init_gnome_gobject_helper (GDA_TYPE_VALUE);
  scm_init_gnome_gobject_helper (GDA_TYPE_COMMAND);
  
  scm_c_export (s_scm_gda_value_primitive_new,
                s_scm_gda_value_primitive_set_x,
                s_scm_gda_value_primitive_get,
                s_scm_gda_init,
                NULL);
}
