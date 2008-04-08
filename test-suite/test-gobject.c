/* guile-gnome
 * Copyright (C) 2003,2004 Andreas Rottmann <a.rottmann gmx.at>
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

  param_spec = g_param_spec_value_array (
          "arr",
          "Array",
          "An array",
          g_param_spec_int ("int", "Int", "An int", 0, 100, 0, G_PARAM_READWRITE),
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
