/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gtk-tree-model.c: A tree model for guile-gtk
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
 *
 * Based on work Copyright (C) 2003 James Henstridge <james@daa.com.au>
 */

#include <string.h>
#include <libguile.h>
#include <glib-object.h>
#include "guile-gnome-gobject.h"

#include <gtk/gtk.h>
#include "guile-gtk-tree-model.h"
#include "gtk-support.h"


#define PROC_FROM_INSTANCE(instance, member) (((GuileGtkGenericTreeModel*)instance)->member)

#define GET_ITER_VAL(iter) (GPOINTER_TO_SCM (iter->user_data))

#define GET_ITER_VAL_OR_FALSE(iter) (iter ? GET_ITER_VAL (iter) : SCM_BOOL_F)

#define SET_ITER_VAL(iter, val)                                                         \
G_STMT_START{                                                                           \
    if (iter->stamp == ((GuileGtkGenericTreeModel*)tree_model)->stamp && iter->user_data)\
        scm_gc_unprotect_object (GET_ITER_VAL (iter));                                  \
    iter->stamp = ((GuileGtkGenericTreeModel*)tree_model)->stamp;                       \
    iter->user_data = SCM_TO_GPOINTER (scm_gc_protect_object (val));       \
}G_STMT_END

#define SET_ITER_NULL(iter)                                                     \
G_STMT_START{                                                                   \
    if (iter->stamp == ((GuileGtkGenericTreeModel*)tree_model)->stamp && iter->user_data)\
        scm_gc_unprotect_object (GET_ITER_VAL (iter));                          \
    iter->stamp = ((GuileGtkGenericTreeModel*)tree_model)->stamp;               \
    iter->user_data = NULL;                                                     \
}G_STMT_END

#define RETURN_IF_INVALID_ITER(iter)                                            \
G_STMT_START{                                                                   \
    g_return_if_fail (iter != NULL);                                            \
    g_return_if_fail (iter->stamp == ((GuileGtkGenericTreeModel*)tree_model)->stamp);\
    g_return_if_fail (iter->user_data != NULL);                                 \
}G_STMT_END

#define RETURN_VAL_IF_INVALID_ITER(iter, val)                                   \
G_STMT_START{                                                                   \
    g_return_val_if_fail (iter != NULL, val);                                   \
    g_return_val_if_fail (iter->stamp == ((GuileGtkGenericTreeModel*)tree_model)->stamp, val);\
    g_return_val_if_fail (iter->user_data != NULL, val);                        \
}G_STMT_END

static void guile_gtk_generic_tree_model_init (GuileGtkGenericTreeModel *self);
static void guile_gtk_generic_tree_model_iface_init (GtkTreeModelIface *iface);


GType
guile_gtk_generic_tree_model_get_type(void)
{
    static GType object_type = 0;

    if (!object_type) {
	static const GTypeInfo object_info = {
	    sizeof(GuileGtkGenericTreeModelClass),
	    (GBaseInitFunc) NULL,
	    (GBaseFinalizeFunc) NULL,
	    (GClassInitFunc) NULL,
	    NULL, /* class_finalize */
	    NULL, /* class_data */
	    sizeof(GuileGtkGenericTreeModel),
	    0, /* n_preallocs */
	    (GInstanceInitFunc) guile_gtk_generic_tree_model_init,
	};
	static const GInterfaceInfo tree_model_info = {
	    (GInterfaceInitFunc) guile_gtk_generic_tree_model_iface_init,
	    NULL,
	    NULL,
	};

	object_type = g_type_register_static(G_TYPE_OBJECT,
					     "GuileGtkGenericTreeModel",
					     &object_info, 0);
	g_type_add_interface_static(object_type,
				    GTK_TYPE_TREE_MODEL,
				    &tree_model_info);
    }
    return object_type;
}

static void
guile_gtk_generic_tree_model_init (GuileGtkGenericTreeModel *tree_model)
{
    SCM current_module;

    tree_model->stamp = g_random_int ();

    current_module =
        scm_set_current_module (scm_c_resolve_module ("gnome gtk"));
    /* these must not error, or the current module will stay as (gnome gtk) */

    /* we know they aren't going to go away, so no permanent objects */
    tree_model->on_get_flags = SCM_VARIABLE_REF (scm_c_lookup ("on-get-flags"));
    tree_model->on_get_n_columns = SCM_VARIABLE_REF (scm_c_lookup ("on-get-n-columns"));
    tree_model->on_get_column_type = SCM_VARIABLE_REF (scm_c_lookup ("on-get-column-type"));
    tree_model->on_get_iter = SCM_VARIABLE_REF (scm_c_lookup ("on-get-iter"));
    tree_model->on_get_path = SCM_VARIABLE_REF (scm_c_lookup ("on-get-path"));
    tree_model->on_get_value = SCM_VARIABLE_REF (scm_c_lookup ("on-get-value"));
    tree_model->on_iter_next = SCM_VARIABLE_REF (scm_c_lookup ("on-iter-next"));
    tree_model->on_iter_children = SCM_VARIABLE_REF (scm_c_lookup ("on-iter-children"));
    tree_model->on_iter_has_child = SCM_VARIABLE_REF (scm_c_lookup ("on-iter-has-child"));
    tree_model->on_iter_n_children = SCM_VARIABLE_REF (scm_c_lookup ("on-iter-n-children"));
    tree_model->on_iter_nth_child = SCM_VARIABLE_REF (scm_c_lookup ("on-iter-nth-child"));
    tree_model->on_iter_parent = SCM_VARIABLE_REF (scm_c_lookup ("on-iter-parent"));

    scm_set_current_module (current_module);
}


static guint guile_gtk_generic_tree_model_get_flags (GtkTreeModel *tree_model);
static gint guile_gtk_generic_tree_model_get_n_columns (GtkTreeModel *tree_model);
static GType guile_gtk_generic_tree_model_get_column_type (GtkTreeModel *tree_model,
                                                           gint index);
static gboolean guile_gtk_generic_tree_model_get_iter (GtkTreeModel *tree_model,
                                                       GtkTreeIter *iter,
                                                       GtkTreePath *path);
static GtkTreePath *guile_gtk_generic_tree_model_get_path (GtkTreeModel *tree_model,
                                                           GtkTreeIter *iter);
static void guile_gtk_generic_tree_model_get_value (GtkTreeModel *tree_model,
                                                    GtkTreeIter *iter,
                                                    gint column, GValue *value);
static gboolean guile_gtk_generic_tree_model_iter_next (GtkTreeModel *tree_model,
                                                        GtkTreeIter *iter);
static gboolean guile_gtk_generic_tree_model_iter_children (GtkTreeModel *tree_model,
                                                            GtkTreeIter *iter,
                                                            GtkTreeIter *parent);
static gboolean guile_gtk_generic_tree_model_iter_has_child (GtkTreeModel *tree_model,
                                                             GtkTreeIter *iter);
static gint guile_gtk_generic_tree_model_iter_n_children (GtkTreeModel *tree_model,
                                                          GtkTreeIter *iter);
static gboolean guile_gtk_generic_tree_model_iter_nth_child (GtkTreeModel *tree_model,
                                                             GtkTreeIter  *iter,
                                                             GtkTreeIter  *parent,
                                                             gint n);
static gboolean guile_gtk_generic_tree_model_iter_parent (GtkTreeModel *tree_model,
                                                          GtkTreeIter *iter,
                                                          GtkTreeIter *child);

static void
guile_gtk_generic_tree_model_iface_init(GtkTreeModelIface *iface)
{
  iface->get_flags = guile_gtk_generic_tree_model_get_flags;
  iface->get_n_columns = guile_gtk_generic_tree_model_get_n_columns;
  iface->get_column_type = guile_gtk_generic_tree_model_get_column_type;
  iface->get_iter = guile_gtk_generic_tree_model_get_iter;
  iface->get_path = guile_gtk_generic_tree_model_get_path;
  iface->get_value = guile_gtk_generic_tree_model_get_value;
  iface->iter_next = guile_gtk_generic_tree_model_iter_next;
  iface->iter_children = guile_gtk_generic_tree_model_iter_children;
  iface->iter_has_child = guile_gtk_generic_tree_model_iter_has_child;
  iface->iter_n_children = guile_gtk_generic_tree_model_iter_n_children;
  iface->iter_nth_child = guile_gtk_generic_tree_model_iter_nth_child;
  iface->iter_parent = guile_gtk_generic_tree_model_iter_parent;
}

/* format of GtkTreeIter's for GuileGtkGenericTreeModel:
 *  user_data == SCM value
 */


struct with_guile_args {
    GtkTreeModel *tree_model;
    gint i;
    GType t;
    guint u;
    GtkTreeIter *iter;
    GtkTreeIter *iter2;
    GtkTreePath *path;
    gboolean b;
    GValue *v;
};
    
static void*
_with_guile_gtk_generic_tree_model_get_flags (void *p)
#define FUNC_NAME "guile-gtk-generic-tree-model-get-flags"
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    SCM scm_obj, scm_ret;
    GValue *flags_val = NULL;

    a->u = 0;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*) (tree_model));
    scm_ret = scm_call_1 (PROC_FROM_INSTANCE (tree_model, on_get_flags), scm_obj);

    if (SCM_NFALSEP (scm_ret)) {
        SCM_VALIDATE_GVALUE_TYPE_COPY (0, scm_ret, GTK_TYPE_TREE_MODEL_FLAGS, flags_val);
        a->u = g_value_get_flags (flags_val);
    }
    return NULL;
}
static guint
guile_gtk_generic_tree_model_get_flags (GtkTreeModel *tree_model)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    scm_with_guile (_with_guile_gtk_generic_tree_model_get_flags, &args);
    return args.u;
}
#undef FUNC_NAME

static void*
_with_guile_gtk_generic_tree_model_get_n_columns (void *p)
#define FUNC_NAME "guile-gtk-generic-tree-model-get-n-columns"
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    SCM scm_obj, scm_ret;

    a->i = 0;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_1 (PROC_FROM_INSTANCE (tree_model, on_get_n_columns), scm_obj);

    a->i = SCM_NUM2INT (0, scm_ret);
    return NULL;
}
static gint
guile_gtk_generic_tree_model_get_n_columns (GtkTreeModel *tree_model)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    scm_with_guile (_with_guile_gtk_generic_tree_model_get_n_columns, &args);
    return args.i;
}
#undef FUNC_NAME

static void*
_with_guile_gtk_generic_tree_model_get_column_type (void *p)
#define FUNC_NAME "guile-gtk-generic-tree-model-get-column-type"
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    SCM scm_obj, scm_ret;
    GType gtype;

    a->t = 0;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_get_column_type),
                          scm_obj, SCM_MAKINUM (a->i));
    SCM_VALIDATE_GTYPE_CLASS_COPY (0, scm_ret, gtype);
    a->t = gtype;
    return NULL;
}
static GType
guile_gtk_generic_tree_model_get_column_type (GtkTreeModel *tree_model, gint index)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.i = index;
    scm_with_guile (_with_guile_gtk_generic_tree_model_get_column_type, &args);
    return args.t;
}
#undef FUNC_NAME

static void*
_with_guile_gtk_generic_tree_model_get_iter (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    SCM scm_obj, scm_ret, scm_path;

    a->b = FALSE;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_path = guile_gtk_tree_path_to_scm (a->path);
    scm_ret = scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_get_iter),
                          scm_obj, scm_path);

    if (SCM_NFALSEP (scm_ret)) {
        SET_ITER_VAL (iter, scm_ret);
        a->b = TRUE;
    } else {
        SET_ITER_NULL (iter);
        a->b = FALSE;
    }
    return NULL;
}
static gboolean
guile_gtk_generic_tree_model_get_iter (GtkTreeModel *tree_model,
                                       GtkTreeIter *iter, GtkTreePath *path)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    args.path = path;
    scm_with_guile (_with_guile_gtk_generic_tree_model_get_iter, &args);
    return args.b;
}

static void*
_with_guile_gtk_generic_tree_model_get_path (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    SCM scm_obj, scm_ret;

    a->path = NULL;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL(tree_model), NULL);
    RETURN_VAL_IF_INVALID_ITER (iter, NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_get_path),
                          scm_obj, GET_ITER_VAL (iter));
    a->path = guile_gtk_scm_to_tree_path (scm_ret);

    if (!a->path)
        g_warning("could not convert return value of `on-get-path' to "
                  "a GtkTreePath");

    return NULL;
}
static GtkTreePath *
guile_gtk_generic_tree_model_get_path (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    scm_with_guile (_with_guile_gtk_generic_tree_model_get_path, &args);
    return args.path;
}

static void*
_with_guile_gtk_generic_tree_model_get_value (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    SCM scm_obj, scm_ret;
    GValue *tmp;
    GType type;

    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);
    RETURN_VAL_IF_INVALID_ITER (iter, NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_3 (PROC_FROM_INSTANCE (tree_model, on_get_value),
                          scm_obj, GET_ITER_VAL (iter), SCM_MAKINUM (a->i));

    /* oh god this is terrible */
    _with_guile_gtk_generic_tree_model_get_column_type (a);
    type = a->t;
    tmp = scm_c_scm_to_gvalue (type, scm_ret);
    g_value_init (a->v, type);
    g_value_copy (tmp, a->v);
    g_value_unset (tmp);
    g_free (tmp);
    return NULL;
}
static void
guile_gtk_generic_tree_model_get_value (GtkTreeModel *tree_model, GtkTreeIter *iter, gint column, GValue *value)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    args.i = column;
    args.v = value;
    scm_with_guile (_with_guile_gtk_generic_tree_model_get_value, &args);
}

static void*
_with_guile_gtk_generic_tree_model_iter_next (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    SCM scm_obj, scm_ret;
    
    a->b = FALSE;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);
    RETURN_VAL_IF_INVALID_ITER (iter, NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_iter_next),
                          scm_obj, GET_ITER_VAL (iter));

    if (SCM_NFALSEP (scm_ret)) {
        SET_ITER_VAL (iter, scm_ret);
        a->b = TRUE;
    } else {
        SET_ITER_NULL (iter);
        a->b = FALSE;
    }
    return NULL;
}
static gboolean
guile_gtk_generic_tree_model_iter_next (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    scm_with_guile (_with_guile_gtk_generic_tree_model_iter_next, &args);
    return args.b;
}

static void*
_with_guile_gtk_generic_tree_model_iter_children (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    GtkTreeIter *parent = a->iter2;
    SCM scm_obj, scm_ret;

    a->b = FALSE;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);
    g_return_val_if_fail (!parent ||
                          GUILE_GTK_GENERIC_TREE_MODEL (tree_model)->stamp == parent->stamp,
                          NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_iter_children), scm_obj,
                          GET_ITER_VAL_OR_FALSE (parent));

    if (SCM_NFALSEP (scm_ret)) {
        SET_ITER_VAL (iter, scm_ret);
        a->b = TRUE;
    } else {
        SET_ITER_NULL (iter);
        a->b = FALSE;
    }
    return NULL;
}
static gboolean
guile_gtk_generic_tree_model_iter_children (GtkTreeModel *tree_model, GtkTreeIter *iter,GtkTreeIter *parent)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    args.iter2 = parent;
    scm_with_guile (_with_guile_gtk_generic_tree_model_iter_children, &args);
    return args.b;
}

static void*
_with_guile_gtk_generic_tree_model_iter_has_child (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    SCM scm_obj;

    a->b = FALSE;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);
    RETURN_VAL_IF_INVALID_ITER (iter, FALSE);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);

    a->b = SCM_NFALSEP (scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_iter_has_child),
                                    scm_obj, GET_ITER_VAL (iter)));
    return NULL;
}
static gboolean
guile_gtk_generic_tree_model_iter_has_child (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    scm_with_guile (_with_guile_gtk_generic_tree_model_iter_has_child, &args);
    return args.b;
}

static void*
_with_guile_gtk_generic_tree_model_iter_n_children (void *p)
#define FUNC_NAME "guile-gtk-generic-tree-model-iter-n-children"
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    SCM scm_obj, scm_ret;

    a->i = 0;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);
    g_return_val_if_fail (!iter ||
                          iter->stamp == ((GuileGtkGenericTreeModel*)tree_model)->stamp, NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_iter_n_children),
                          scm_obj, GET_ITER_VAL_OR_FALSE (iter));

    a->i = SCM_NUM2INT (0, scm_ret);
    return NULL;
}
static gint
guile_gtk_generic_tree_model_iter_n_children (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    scm_with_guile (_with_guile_gtk_generic_tree_model_iter_n_children, &args);
    return args.i;
}
#undef FUNC_NAME

static void*
_with_guile_gtk_generic_tree_model_iter_nth_child (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    GtkTreeIter *parent = a->iter2;
    SCM scm_obj, scm_ret;

    a->b = FALSE;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);
    g_return_val_if_fail (!parent ||
                          ((GuileGtkGenericTreeModel*)tree_model)->stamp == parent->stamp,
                          NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_3 (PROC_FROM_INSTANCE (tree_model, on_iter_nth_child),
                          scm_obj, GET_ITER_VAL_OR_FALSE (parent),
                          SCM_MAKINUM (a->i));

    if (SCM_NFALSEP (scm_ret)) {
        SET_ITER_VAL (iter, scm_ret);
        a->b = TRUE;
    } else {
        SET_ITER_NULL (iter);
        a->b = FALSE;
    }
    return NULL;
}
static gboolean
guile_gtk_generic_tree_model_iter_nth_child (GtkTreeModel *tree_model, GtkTreeIter *iter,GtkTreeIter *parent, gint n)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    args.iter2 = parent;
    args.i = n;
    scm_with_guile (_with_guile_gtk_generic_tree_model_iter_nth_child, &args);
    return args.b;
}

static void*
_with_guile_gtk_generic_tree_model_iter_parent (void *p)
{
    struct with_guile_args *a = p;
    GtkTreeModel *tree_model = a->tree_model;
    GtkTreeIter *iter = a->iter;
    GtkTreeIter *child = a->iter2;
    SCM scm_obj, scm_ret;

    a->b = FALSE;
    g_return_val_if_fail (GUILE_GTK_IS_GENERIC_TREE_MODEL (tree_model), NULL);
    RETURN_VAL_IF_INVALID_ITER (child, NULL);

    scm_obj = scm_c_gtype_instance_to_scm ((GTypeInstance*)tree_model);
    scm_ret = scm_call_2 (PROC_FROM_INSTANCE (tree_model, on_iter_parent),
                          scm_obj, GET_ITER_VAL (child));

    if (SCM_NFALSEP (scm_ret)) {
        SET_ITER_VAL (iter, scm_ret);
        a->b = TRUE;
    } else {
        SET_ITER_NULL (iter);
        a->b = FALSE;
    }
    return NULL;
}
static gboolean
guile_gtk_generic_tree_model_iter_parent (GtkTreeModel *tree_model, GtkTreeIter *iter,GtkTreeIter *child)
{
    struct with_guile_args args;
    memset (&args, 0, sizeof(args));
    args.tree_model = tree_model;
    args.iter = iter;
    args.iter2 = child;
    scm_with_guile (_with_guile_gtk_generic_tree_model_iter_parent, &args);
    return args.b;
}
