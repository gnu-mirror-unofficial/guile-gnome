/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gtk-tree-model.h: A tree model for guile-gtk
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

#include <gtk/gtk.h>

#define GUILE_GTK_TYPE_GENERIC_TREE_MODEL (guile_gtk_generic_tree_model_get_type())
#define GUILE_GTK_GENERIC_TREE_MODEL(object) \
    (G_TYPE_CHECK_INSTANCE_CAST((object), GUILE_GTK_TYPE_GENERIC_TREE_MODEL, GuileGtkGenericTreeModel))
#define GUILE_GTK_GENERIC_TREE_MODEL_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), GUILE_GTK_TYPE_GENERIC_TREE_MODEL, GuileGtkGenericTreeModelClass))
#define GUILE_GTK_IS_GENERIC_TREE_MODEL(object) \
    (G_TYPE_CHECK_INSTANCE_TYPE((object), GUILE_GTK_TYPE_GENERIC_TREE_MODEL))
#define GUILE_GTK_IS_GENERIC_TREE_MODEL_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_TYPE((klass), GUILE_GTK_TYPE_GENERIC_TREE_MODEL))
#define GUILE_GTK_GENERIC_TREE_MODEL_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), GUILE_GTK_TYPE_GENERIC_TREE_MODEL, GuileGtkGenericTreeModelClass))

typedef struct _GuileGtkGenericTreeModel GuileGtkGenericTreeModel;
typedef struct _GuileGtkGenericTreeModelClass GuileGtkGenericTreeModelClass;

struct _GuileGtkGenericTreeModel {
    GObject parent_instance;

    gint stamp;

    SCM on_get_flags;
    SCM on_get_n_columns;
    SCM on_get_column_type;
    SCM on_get_iter;
    SCM on_get_path;
    SCM on_get_value;
    SCM on_iter_next;
    SCM on_iter_children;
    SCM on_iter_has_child;
    SCM on_iter_n_children;
    SCM on_iter_nth_child;
    SCM on_iter_parent;
};

struct _GuileGtkGenericTreeModelClass {
    GObjectClass parent_class;
};

GType guile_gtk_generic_tree_model_get_type (void);
