#include "test-gobject.h"

static GObjectClass *parent_class = NULL;

static void
test_obj_instance_init (TestObj *self)
{
  self->arr = g_value_array_new (0);
}

static void
test_obj_instance_finalize (TestObj *self)
{
  g_value_array_free (self->arr);
}

guint
test_obj_foo (TestObj *obj, guint arg)
{
  return arg;
}



/*
 * TestObj class.
 */

static GObject*
test_obj_constructor (GType                  type,
                      guint                  n_construct_properties,
                      GObjectConstructParam *construct_properties)
{
  GObject *object;
  TestObj *self;
  
  object = G_OBJECT_CLASS (parent_class)->constructor (type,
                                                       n_construct_properties,
                                                       construct_properties);

  self = TEST_OBJ (object);
  
  return object;
}

enum
{
  PROP_ARR = 1,
  PROP_END
};

static void
test_obj_set_property (GObject         *object,
                       guint            prop_id,
                       const GValue    *value,
                       GParamSpec      *pspec)
{
  TestObj *self = (TestObj *) object;

  switch (prop_id)
  {
    case PROP_ARR:
    {
      GValueArray *arr = (GValueArray *)g_value_get_boxed (value);

      if (!arr)
        break;

      g_value_array_free (self->arr);
      self->arr = g_value_array_copy (arr);
      
      break;
    }
    default:
      break;
  }
}

static void
test_obj_get_property (GObject         *object,
                       guint            prop_id,
                       GValue          *value,
                       GParamSpec      *pspec)
{
  TestObj *self = (TestObj *) object;
  
  switch (prop_id)
  {
    case PROP_ARR:
    {
      g_value_set_boxed (value, self->arr);
      break;
    }
    default:
      break;
  }
}

static void
test_obj_class_init (TestObjClass *klass)
{
  GParamSpec *param_spec;
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);
  
  object_class->constructor = test_obj_constructor;
  object_class->finalize = (void (*)(GObject *))test_obj_instance_finalize;
  
  /* Properties */
  object_class->set_property = test_obj_set_property;
  object_class->get_property = test_obj_get_property;

  param_spec = g_param_spec_boxed (
          "arr",
          "Array",
          "An array",
          G_TYPE_VALUE_ARRAY,
          G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);
  
  g_object_class_install_property (object_class,
				   PROP_ARR,
				   param_spec);
}

GType 
test_obj_get_type (void)
{
  static GType type = 0;
  
  if (type == 0)
  {
    static const GTypeInfo info = {
      sizeof (TestObjClass),
      NULL,   /* base_init */
      NULL,   /* base_finalize */
      (GClassInitFunc) test_obj_class_init,   /* class_init */
      NULL,   /* class_finalize */
      NULL,   /* class_data */
      sizeof (TestObj),
      0,      /* n_preallocs */
      (GInstanceInitFunc)test_obj_instance_init    /* instance_init */
    };
    type = g_type_register_static (G_TYPE_OBJECT,
                                   "TestObj",
                                   &info, 0);
  }
  return type;
}
