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
