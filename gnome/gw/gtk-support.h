/* guile-gnome
 * Copyright (C) 2003,2004,2010 Andy Wingo <wingo at pobox dot com>
 *
 * gtk-support.h: Customizations for guile-gtk
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

#include <libguile.h>
#include "guile-gnome-gobject.h"
#include <gtk/gtk.h>


SCM guile_gtk_tree_path_to_scm (GtkTreePath *path);
GtkTreePath* guile_gtk_scm_to_tree_path (SCM scm);

void _wrap_gtk_action_group_add_actions (GtkActionGroup* action_group, SCM entries);
void _wrap_gtk_action_group_add_toggle_actions (GtkActionGroup *action_group, SCM entries);
void _wrap_gtk_action_group_add_radio_actions (GtkActionGroup *action_group, SCM entries, gint value, SCM on_change);

void _wrap_gtk_clipboard_set_text (GtkClipboard *clipboard, const gchar *text);

GtkWidget* _wrap_gtk_dialog_get_vbox (GtkDialog *dialog);
GtkWidget* _wrap_gtk_dialog_get_action_area (GtkDialog *dialog);

GtkWidget* _wrap_gtk_color_selection_dialog_get_colorsel (GtkColorSelectionDialog *dialog);
GtkWidget* _wrap_gtk_color_selection_dialog_get_ok_button (GtkColorSelectionDialog *dialog);
GtkWidget* _wrap_gtk_color_selection_dialog_get_cancel_button (GtkColorSelectionDialog *dialog);
GtkWidget* _wrap_gtk_color_selection_dialog_get_help_button (GtkColorSelectionDialog *dialog);

gint _wrap_gtk_editable_insert_text (GtkEditable *editable, const gchar *text, gint pos);

GtkWidget* _wrap_gtk_file_selection_get_ok_button (GtkFileSelection* selection);
GtkWidget* _wrap_gtk_file_selection_get_cancel_button (GtkFileSelection* selection);

GtkListStore* _wrap_gtk_list_store_new (SCM col_types);
void _wrap_gtk_list_store_set_value (GtkListStore *store, GtkTreeIter *iter, gint column, SCM scm);
GtkTreeIter* _wrap_gtk_list_store_remove (GtkListStore *store, GtkTreeIter *iter);
GtkTreeIter* _wrap_gtk_list_store_insert (GtkListStore *store, gint position);
GtkTreeIter* _wrap_gtk_list_store_insert_before (GtkListStore *store, GtkTreeIter *sibling);
GtkTreeIter* _wrap_gtk_list_store_insert_after (GtkListStore *store, GtkTreeIter *sibling);
GtkTreeIter* _wrap_gtk_list_store_prepend (GtkListStore *store);
GtkTreeIter* _wrap_gtk_list_store_append (GtkListStore *store);

void _wrap_gtk_menu_popup (GtkMenu *menu, GtkWidget *parent_menu_shell,
                           GtkWidget *parent_menu_item, SCM func,
                           guint button, guint32 activate_time);

SCM _wrap_gtk_message_dialog_new (GtkWindow* parent, GtkDialogFlags flags, GtkMessageType type, GtkButtonsType buttons, const gchar *text);

gchar* _gtk_selection_data_get_as_string (GtkSelectionData *data);

void _wrap_gtk_stock_add (SCM items);
SCM _wrap_gtk_stock_lookup (const gchar *stock_id);

GdkGC* gtk_style_get_fg_gc (GtkStyle *style, GtkStateType state);
GdkGC* gtk_style_get_bg_gc (GtkStyle *style, GtkStateType state);
GdkGC* gtk_style_get_white_gc (GtkStyle *style);
GdkGC* gtk_style_get_black_gc (GtkStyle *style);

void _wrap_gtk_text_buffer_set_text (GtkTextBuffer *buf, SCM stext);
void _wrap_gtk_text_buffer_insert (GtkTextBuffer *buf, GtkTextIter* iter, SCM stext);
void _wrap_gtk_text_buffer_insert_at_cursor (GtkTextBuffer *buf, SCM stext);
gboolean _wrap_gtk_text_buffer_insert_interactive (GtkTextBuffer *buf, GtkTextIter* iter, SCM stext, gboolean default_editable);
gboolean _wrap_gtk_text_buffer_insert_interactive_at_cursor (GtkTextBuffer *buf, SCM stext, gboolean default_editable);
void _wrap_gtk_text_buffer_insert_with_tags (GtkTextBuffer *buf, GtkTextIter* iter, SCM stext, GList* tag_list);
void _wrap_gtk_text_buffer_insert_with_tags_by_name (GtkTextBuffer *buf, GtkTextIter* iter, SCM stext, GList* tag_list);
GtkTextIter* _wrap_gtk_text_buffer_get_iter_at_line_offset (GtkTextBuffer *buf, gint line_number, gint char_offset);
GtkTextIter* _wrap_gtk_text_buffer_get_iter_at_line_index (GtkTextBuffer *buf, gint line_number, gint byte_index);
GtkTextIter* _wrap_gtk_text_buffer_get_iter_at_offset (GtkTextBuffer *buf, gint char_offset);
GtkTextIter* _wrap_gtk_text_buffer_get_iter_at_line (GtkTextBuffer *buf, gint line_number);
GtkTextIter* _wrap_gtk_text_buffer_get_start_iter (GtkTextBuffer *buf);
GtkTextIter* _wrap_gtk_text_buffer_get_end_iter (GtkTextBuffer *buf);
SCM _wrap_gtk_text_buffer_get_bounds (GtkTextBuffer *buf);
SCM _wrap_gtk_text_buffer_get_selection_bounds (GtkTextBuffer *buf);
GtkTextIter* _wrap_gtk_text_buffer_get_iter_at_mark (GtkTextBuffer *buf, GtkTextMark* mark);
GtkTextIter* _wrap_gtk_text_buffer_get_iter_at_child_anchor (GtkTextBuffer *buf, GtkTextChildAnchor* anchor);

GtkTreeIter* _wrap_gtk_tree_model_get_iter (GtkTreeModel *model, GtkTreePath *path);
GtkTreeIter* _wrap_gtk_tree_model_get_iter_first (GtkTreeModel *model);
SCM _wrap_gtk_tree_model_get_value (GtkTreeModel *model, GtkTreeIter *iter, gint column);
GtkTreeIter* _wrap_gtk_tree_model_iter_next (GtkTreeModel *model, GtkTreeIter *iter);
GList* _wrap_gtk_tree_model_iter_children (GtkTreeModel *model, GtkTreeIter *iter);
GtkTreeIter* _wrap_gtk_tree_model_iter_parent (GtkTreeModel *model, GtkTreeIter *child);
GtkTreeIter* _wrap_gtk_tree_model_iter_nth_child (GtkTreeModel *model, GtkTreeIter *iter, gint n);

SCM _wrap_gtk_tree_selection_get_selected (GtkTreeSelection *selection);

GtkTreeStore* _wrap_gtk_tree_store_new (SCM col_types);
void _wrap_gtk_tree_store_set_value (GtkTreeStore *store, GtkTreeIter *iter, gint column, SCM scm);
GtkTreeIter* _wrap_gtk_tree_store_remove (GtkTreeStore *store, GtkTreeIter *iter);
GtkTreeIter* _wrap_gtk_tree_store_insert (GtkTreeStore *store, GtkTreeIter *parent, gint position);
GtkTreeIter* _wrap_gtk_tree_store_insert_before (GtkTreeStore *store, GtkTreeIter *parent, GtkTreeIter *sibling);
GtkTreeIter* _wrap_gtk_tree_store_insert_after (GtkTreeStore *store, GtkTreeIter *parent, GtkTreeIter *sibling);
GtkTreeIter* _wrap_gtk_tree_store_prepend (GtkTreeStore *store, GtkTreeIter *parent);
GtkTreeIter* _wrap_gtk_tree_store_append (GtkTreeStore *store, GtkTreeIter *parent);

SCM _wrap_gtk_tree_view_get_path_at_pos (GtkTreeView *treeview, gint x, gint y);

void _wrap_gtk_tree_view_column_set_cell_data_func (GtkTreeViewColumn *tree_column, GtkCellRenderer *cell_renderer, SCM proc);

guint _wrap_gtk_ui_manager_add_ui_from_string (GtkUIManager *ui, const gchar *string, GError **error);

GdkWindow* gtk_widget_get_window (GtkWidget *widget);
GdkRectangle* _wrap_gtk_widget_get_allocation (GtkWidget *widget);
void _wrap_gtk_drag_dest_set (GtkWidget *widget, GtkDestDefaults flags, const GList *types, GdkDragAction actions);
GtkStateType gtk_widget_get_state (GtkWidget *widget);
