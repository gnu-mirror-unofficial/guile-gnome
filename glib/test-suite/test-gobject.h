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

#if !defined(_INC_TEST_SUITE_TEST_GOBJECT_H)
#define _INC_TEST_SUITE_TEST_GOBJECT_H

#include <glib-object.h>

G_BEGIN_DECLS

#define TEST_TYPE_OBJ	     (test_obj_get_type ())
#define TEST_OBJ(obj)	     (G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_OBJ, TestObj))
#define TEST_OBJ_CLASS(vtable)    (G_TYPE_CHECK_CLASS_CAST ((vtable), TEST_TYPE_OBJ, TestObjClass))
#define TEST_IS_OBJ(obj)	     (G_TYPE_CHECK_TYPE ((obj), TEST_TYPE_OBJ))
#define TEST_OBJ_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), TEST_TYPE_OBJ, TestObjClass))

typedef struct _TestObj      TestObj;
typedef struct _TestObjClass TestObjClass;

struct _TestObj
{
    GObject super;

    GValueArray *arr;
};

struct _TestObjClass
{
    GObjectClass super;
};
 
GType    test_obj_get_type(void);

guint    test_obj_foo (TestObj *obj, guint arg);

G_END_DECLS


#endif
