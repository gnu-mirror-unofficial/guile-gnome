/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * gtk-support.c: Customizations for guile-gtk
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
#include "gtk-support.h"


/* python-gtk defines a nice series of classes to access rows as elements of a
   sequence and to access their fields via a sequence interface. I wish we could
   do that in guile. Food for thought. */

SCM
guile_gtk_tree_path_to_scm (GtkTreePath *path)
{
    gint len, i, *indices;
    SCM ret;

    len = gtk_tree_path_get_depth(path);
    indices = gtk_tree_path_get_indices(path);
    ret = SCM_EOL;
    for (i = len - 1; i >= 0; i--)
	ret = scm_cons (SCM_MAKINUM (indices[i]), ret);
    return ret;
}

GtkTreePath*
guile_gtk_scm_to_tree_path (SCM scm)
#define FUNC_NAME "guile-gtk-scm-to-tree-path"
{
    if (SCM_STRINGP (scm)) {
	return gtk_tree_path_new_from_string (SCM_STRING_CHARS (scm));
    } else if (SCM_INUMP (scm)) {
        GtkTreePath *ret = gtk_tree_path_new ();
        gtk_tree_path_append_index (ret, SCM_INUM (scm));
        return ret;
    } else if (SCM_EQ_P (scm, SCM_EOL)) {
        return NULL;
    } else if (SCM_NFALSEP (scm_list_p (scm))) {
	GtkTreePath *path = gtk_tree_path_new ();
        
        for (; !SCM_EQ_P (scm, SCM_EOL); scm = SCM_CDR (scm))
            gtk_tree_path_append_index (path, SCM_NUM2INT (0, SCM_CAR (scm)));

	return path;
    } else if (SCM_GVALUEP (scm)) {
        /* we have to allow for GValues, because the code that takes a
           marshalled GValue doesn't know how to convert that into a scm */
        GValue *value;
        SCM_VALIDATE_GVALUE_TYPE_COPY (1, scm, GTK_TYPE_TREE_PATH, value);
	return g_value_dup_boxed (value);
    }
    return NULL;
}
#undef FUNC_NAME

GtkWidget*
gtk_combo_get_entry (GtkCombo *combo)
{
    return combo->entry;
}

gint
_wrap_gtk_editable_insert_text (GtkEditable *editable, const gchar *text, gint pos)
{
    gtk_editable_insert_text (editable, text, strlen (text), &pos);
    return pos;
}

GtkWidget*
_wrap_gtk_dialog_get_vbox (GtkDialog *dialog)
{
  return dialog->vbox;
}

GtkWidget*
_wrap_gtk_dialog_get_action_area (GtkDialog *dialog)
{
  return dialog->action_area;
}

GtkWidget*
_wrap_gtk_file_selection_get_ok_button (GtkFileSelection* selection)
{
    return selection->ok_button;
}

GtkWidget*
_wrap_gtk_file_selection_get_cancel_button (GtkFileSelection* selection)
{
    return selection->cancel_button;
}

GtkListStore*
_wrap_gtk_list_store_new (SCM col_types) 
#define FUNC_NAME "gtk-list-store-new"
{
    GType *col_gtypes;
    gint len, i;
    GtkListStore *ret;
    
    SCM_VALIDATE_NONEMPTYLIST (1, col_types);
    len = scm_ilength (col_types);
    col_gtypes = g_new (GType, len); /* fixme: if there's an error, this memory
                                      * is leaked */
    for (i=0; i<len; i++) {
        SCM_VALIDATE_GTYPE_COPY (i, SCM_CAR (col_types), col_gtypes[i]);
        col_types = SCM_CDR (col_types);
    }
    
    ret = gtk_list_store_newv (len, col_gtypes);
    g_free (col_gtypes);
    return ret;
}
#undef FUNC_NAME

void
_wrap_gtk_list_store_set_value (GtkListStore *store, GtkTreeIter *iter,
                                gint column, SCM scm) 
{
    GValue *value;
    GType type;

    SCM_ASSERT (column < gtk_tree_model_get_n_columns (GTK_TREE_MODEL (store)),
                SCM_MAKINUM (column), 3, "gtk-list-store-set-value");
    type = gtk_tree_model_get_column_type (GTK_TREE_MODEL (store), column);
    value = scm_c_scm_to_gvalue (type, scm);

    gtk_list_store_set_value (store, iter, column, value);
    g_value_unset (value);
    g_free (value);
}

GtkTreeIter*
_wrap_gtk_list_store_remove (GtkListStore *store, GtkTreeIter *iter) 
{
    if (!gtk_list_store_remove (store, iter))
        return NULL;
    return gtk_tree_iter_copy (iter);
}

GtkTreeIter*
_wrap_gtk_list_store_insert (GtkListStore *store, gint position)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_list_store_insert (store, new, position);
    return new;
}

GtkTreeIter*
_wrap_gtk_list_store_insert_before (GtkListStore *store, GtkTreeIter *sibling)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_list_store_insert_before (store, new, sibling);
    return new;
}

GtkTreeIter*
_wrap_gtk_list_store_insert_after (GtkListStore *store, GtkTreeIter *sibling)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_list_store_insert_after (store, new, sibling);
    return new;
}

GtkTreeIter*
_wrap_gtk_list_store_prepend (GtkListStore *store)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_list_store_prepend (store, new);
    return new;
}

GtkTreeIter*
_wrap_gtk_list_store_append (GtkListStore *store)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_list_store_append (store, new);
    return new;
}

SCM
_wrap_gtk_message_dialog_new (GtkWindow* parent, GtkDialogFlags flags, GtkMessageType type,
                              GtkButtonsType buttons, const gchar *text)
{
    SCM ret;
    GtkWidget *w = gtk_message_dialog_new (parent, flags, type, buttons, "%s", text);
    g_object_ref (w); /* the initial ref belongs to GTK+ */
    SCM_NEWSMOB2 (ret, scm_tc16_gtype_instance, w, NULL);
    return ret;
}

void
_wrap_gtk_text_buffer_set_text (GtkTextBuffer *buf, SCM stext)
#define FUNC_NAME "gtk-text-buffer-set-text"
{
    SCM_VALIDATE_STRING (2, stext);
    gtk_text_buffer_set_text (buf, SCM_STRING_CHARS (stext), SCM_STRING_LENGTH (stext));
}
#undef FUNC_NAME

void
_wrap_gtk_text_buffer_insert (GtkTextBuffer *buf, GtkTextIter* iter, SCM stext)
#define FUNC_NAME "gtk-text-buffer-insert"
{
    SCM_VALIDATE_STRING (3, stext);
    gtk_text_buffer_insert (buf, iter, SCM_STRING_CHARS (stext), SCM_STRING_LENGTH (stext));
}
#undef FUNC_NAME

void
_wrap_gtk_text_buffer_insert_at_cursor (GtkTextBuffer *buf, SCM stext)
#define FUNC_NAME "gtk-text-buffer-insert-at-cursor"
{
    SCM_VALIDATE_STRING (2, stext);
    gtk_text_buffer_insert_at_cursor (buf, SCM_STRING_CHARS (stext), SCM_STRING_LENGTH (stext));
}
#undef FUNC_NAME

gboolean
_wrap_gtk_text_buffer_insert_interactive (GtkTextBuffer *buf, GtkTextIter* iter,
                                                   SCM stext, gboolean default_editable)
#define FUNC_NAME "gtk-text-buffer-insert-interactive"
{
    SCM_VALIDATE_STRING (3, stext);
    return gtk_text_buffer_insert_interactive (buf, iter, SCM_STRING_CHARS (stext),
                                               SCM_STRING_LENGTH (stext), default_editable);
}
#undef FUNC_NAME

gboolean
_wrap_gtk_text_buffer_insert_interactive_at_cursor (GtkTextBuffer *buf, SCM stext,
                                                    gboolean default_editable)
#define FUNC_NAME "gtk-text-buffer-insert-interactive-at-cursor"
{
    SCM_VALIDATE_STRING (2, stext);
    return gtk_text_buffer_insert_interactive_at_cursor (buf, SCM_STRING_CHARS (stext),
                                                         SCM_STRING_LENGTH (stext),
                                                         default_editable);
}
#undef FUNC_NAME

void
_wrap_gtk_text_buffer_insert_with_tags (GtkTextBuffer *buf, GtkTextIter* iter,
                                        SCM stext, GList* tag_list)
#define FUNC_NAME "gtk-text-buffer-insert-with-tags"
{
    GtkTextIter start;
    GList *walk;
    gint start_offset;
    
    /* based on gtktextbuffer.c (LGPL) */

    SCM_VALIDATE_STRING (3, stext);
    start_offset = gtk_text_iter_get_offset (iter);
    gtk_text_buffer_insert (buf, iter, SCM_STRING_CHARS (stext),
                            SCM_STRING_LENGTH (stext));
    gtk_text_buffer_get_iter_at_offset (buf, &start, start_offset);
    
    for (walk = tag_list; walk; walk = walk->next)
        gtk_text_buffer_apply_tag (buf, (GtkTextTag*)walk->data, &start, iter);
    g_list_free (tag_list);
}
#undef FUNC_NAME

void
_wrap_gtk_text_buffer_insert_with_tags_by_name (GtkTextBuffer *buf, GtkTextIter* iter,
                                                SCM stext, GList* tag_list)
#define FUNC_NAME "gtk-text-buffer-insert-with-tags-by-name"
{
    GtkTextIter start;
    GList *walk;
    gint start_offset;
    
    /* based on gtktextbuffer.c (LGPL) */

    SCM_VALIDATE_STRING (3, stext);
    start_offset = gtk_text_iter_get_offset (iter);
    gtk_text_buffer_insert (buf, iter, SCM_STRING_CHARS (stext),
                            SCM_STRING_LENGTH (stext));
    gtk_text_buffer_get_iter_at_offset (buf, &start, start_offset);
    
    for (walk = tag_list; walk; walk = walk->next)
        gtk_text_buffer_apply_tag (buf,
                                   gtk_text_tag_table_lookup (buf->tag_table,
                                                              (gchar*)walk->data),
                                   &start, iter);
    g_list_free (tag_list);
}
#undef FUNC_NAME

GtkTextIter*
_wrap_gtk_text_buffer_get_iter_at_line_offset (GtkTextBuffer *buf, gint line_number,
                                               gint char_offset)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_iter_at_line_offset (buf, iter, line_number, char_offset);
    return iter;
}

GtkTextIter*
_wrap_gtk_text_buffer_get_iter_at_line_index (GtkTextBuffer *buf, gint line_number,
                                              gint byte_index)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_iter_at_line_index (buf, iter, line_number, byte_index);
    return iter;
}

GtkTextIter*
_wrap_gtk_text_buffer_get_iter_at_offset (GtkTextBuffer *buf, gint char_offset)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_iter_at_offset (buf, iter, char_offset);
    return iter;
}

GtkTextIter*
_wrap_gtk_text_buffer_get_iter_at_line (GtkTextBuffer *buf, gint line_number)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_iter_at_line (buf, iter, line_number);
    return iter;
}

GtkTextIter*
_wrap_gtk_text_buffer_get_start_iter (GtkTextBuffer *buf)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_start_iter (buf, iter);
    return iter;
}

GtkTextIter*
_wrap_gtk_text_buffer_get_end_iter (GtkTextBuffer *buf)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_end_iter (buf, iter);
    return iter;
}

SCM
_wrap_gtk_text_buffer_get_bounds (GtkTextBuffer *buf)
{
    GtkTextIter *start, *end;
    SCM sstart, send;
    
    start = g_new0 (GtkTextIter, 1);
    end = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_bounds (buf, start, end);
    sstart = scm_c_make_gvalue (GTK_TYPE_TEXT_ITER);
    send = scm_c_make_gvalue (GTK_TYPE_TEXT_ITER);
    g_value_set_boxed_take_ownership ((GValue*)SCM_SMOB_DATA (sstart), start);
    g_value_set_boxed_take_ownership ((GValue*)SCM_SMOB_DATA (send), end);
    return scm_values (SCM_LIST2 (sstart, send));
}

SCM
_wrap_gtk_text_buffer_get_selection_bounds (GtkTextBuffer *buf)
{
    GtkTextIter *start, *end;
    SCM sstart, send;
    
    start = g_new0 (GtkTextIter, 1);
    end = g_new0 (GtkTextIter, 1);
    if (gtk_text_buffer_get_selection_bounds (buf, start, end)) {
        sstart = scm_c_make_gvalue (GTK_TYPE_TEXT_ITER);
        send = scm_c_make_gvalue (GTK_TYPE_TEXT_ITER);
        g_value_set_boxed_take_ownership ((GValue*)SCM_SMOB_DATA (sstart), start);
        g_value_set_boxed_take_ownership ((GValue*)SCM_SMOB_DATA (send), end);
        return scm_values (SCM_LIST2 (sstart, send));
    }
    g_free (start);
    g_free (end);
    return scm_values (SCM_LIST2 (SCM_BOOL_F, SCM_BOOL_F));
}

GtkTextIter*
_wrap_gtk_text_buffer_get_iter_at_mark (GtkTextBuffer *buf, GtkTextMark* mark)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_iter_at_mark (buf, iter, mark);
    return iter;
}

GtkTextIter*
_wrap_gtk_text_buffer_get_iter_at_child_anchor (GtkTextBuffer *buf,
                                                GtkTextChildAnchor* anchor)
{
    GtkTextIter* iter = g_new0 (GtkTextIter, 1);
    gtk_text_buffer_get_iter_at_child_anchor (buf, iter, anchor);
    return iter;
}

GtkTreeIter*
_wrap_gtk_tree_model_get_iter (GtkTreeModel *model, GtkTreePath *path)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    
    if (gtk_tree_model_get_iter (model, new, path))
        return new;
    g_free (new);
    return NULL;
}

GtkTreeIter*
_wrap_gtk_tree_model_get_iter_first (GtkTreeModel *model) 
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    
    if (gtk_tree_model_get_iter_first (model, new))
        return new;
    g_free (new);
    return NULL;
}

SCM
_wrap_gtk_tree_model_get_value (GtkTreeModel *model, GtkTreeIter *iter, gint column) 
{
    GValue value = { 0, };
    
    gtk_tree_model_get_value (model, iter, column, &value);

    return scm_c_gvalue_to_scm (&value);
}

GtkTreeIter*
_wrap_gtk_tree_model_iter_next (GtkTreeModel *model, GtkTreeIter *iter) 
{
    GtkTreeIter *new = gtk_tree_iter_copy (iter);

    if (gtk_tree_model_iter_next (model, new))
        return new;
    g_free (new);
    return NULL;
}
    
GList*
_wrap_gtk_tree_model_iter_children (GtkTreeModel *model, GtkTreeIter *iter)
{
    GList *list = NULL;
    GtkTreeIter *prev = g_new0 (GtkTreeIter, 1);
    
    if (gtk_tree_model_iter_children (model, prev, iter)) {
        while (1) {
            GtkTreeIter *new = g_new0 (GtkTreeIter, 1);

            list = g_list_prepend (list, prev);
            *new = *prev;

            if (!gtk_tree_model_iter_next (model, new)) {
                g_free (new);
                return g_list_reverse (list);
            }
            prev = new;
        }
    }
    g_free (prev);
    return NULL;
}

GtkTreeIter*
_wrap_gtk_tree_model_iter_parent (GtkTreeModel *model, GtkTreeIter *iter)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    
    if (gtk_tree_model_iter_parent (model, new, iter))
        return new;
    g_free (new);
    return NULL;
}

GtkTreeIter*
_wrap_gtk_tree_model_iter_nth_child (GtkTreeModel *model, GtkTreeIter *iter, gint n)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    
    if (gtk_tree_model_iter_nth_child (model, new, iter, n))
        return new;
    g_free (new);
    return NULL;
}

SCM
_wrap_gtk_tree_selection_get_selected (GtkTreeSelection *selection)
{
    GtkTreeIter *iter = g_new0 (GtkTreeIter, 1);
    GtkTreeModel *model = NULL;
    
    if (gtk_tree_selection_get_selected (selection, &model, iter)) {
        SCM smodel, siter;
        g_object_ref (model);
        smodel = scm_c_gtype_instance_to_scm ((GTypeInstance*)model);
        siter = scm_c_make_gvalue (GTK_TYPE_TREE_ITER);
        g_value_set_boxed_take_ownership ((GValue*)SCM_SMOB_DATA (siter), iter);
        return scm_values (SCM_LIST2 (smodel, siter));
    }
    return scm_values (SCM_LIST2 (SCM_BOOL_F, SCM_BOOL_F));
}

GtkTreeStore*
_wrap_gtk_tree_store_new (SCM col_types) 
#define FUNC_NAME "gtk-tree-store-new"
{
    GType *col_gtypes;
    gint len, i;
    GtkTreeStore *ret;
    
    SCM_VALIDATE_NONEMPTYLIST (1, col_types);
    len = scm_ilength (col_types);
    col_gtypes = g_new (GType, len); /* fixme: if there's an error, this memory
                                      * is leaked */
    for (i=0; i<len; i++) {
        SCM_VALIDATE_GTYPE_COPY (i, SCM_CAR (col_types), col_gtypes[i]);
        col_types = SCM_CDR (col_types);
    }
    
    ret = gtk_tree_store_newv (len, col_gtypes);
    g_free (col_gtypes);
    return ret;
}
#undef FUNC_NAME

void
_wrap_gtk_tree_store_set_value (GtkTreeStore *store, GtkTreeIter *iter,
                                gint column, SCM scm) 
{
    GValue *value;
    GType type;

    SCM_ASSERT (column < gtk_tree_model_get_n_columns (GTK_TREE_MODEL (store)),
                SCM_MAKINUM (column), 3, "gtk-tree-store-set-value");
    type = gtk_tree_model_get_column_type (GTK_TREE_MODEL (store), column);
    value = scm_c_scm_to_gvalue (type, scm);

    gtk_tree_store_set_value (store, iter, column, value);
    g_value_unset (value);
    g_free (value);
}

GtkTreeIter*
_wrap_gtk_tree_store_remove (GtkTreeStore *store, GtkTreeIter *iter) 
{
    if (!gtk_tree_store_remove (store, iter))
        return NULL;
    return gtk_tree_iter_copy (iter);
}

GtkTreeIter*
_wrap_gtk_tree_store_insert (GtkTreeStore *store, GtkTreeIter *parent, gint position)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_tree_store_insert (store, new, parent, position);
    return new;
}

GtkTreeIter*
_wrap_gtk_tree_store_insert_before (GtkTreeStore *store, GtkTreeIter *parent,
                                    GtkTreeIter *sibling)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_tree_store_insert_before (store, new, parent, sibling);
    return new;
}

GtkTreeIter*
_wrap_gtk_tree_store_insert_after (GtkTreeStore *store, GtkTreeIter *parent,
                                   GtkTreeIter *sibling)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_tree_store_insert_after (store, new, parent, sibling);
    return new;
}

GtkTreeIter*
_wrap_gtk_tree_store_prepend (GtkTreeStore *store, GtkTreeIter *parent)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_tree_store_prepend (store, new, parent);
    return new;
}

GtkTreeIter*
_wrap_gtk_tree_store_append (GtkTreeStore *store, GtkTreeIter *parent)
{
    GtkTreeIter *new = g_new0 (GtkTreeIter, 1);
    gtk_tree_store_append (store, new, parent);
    return new;
}
