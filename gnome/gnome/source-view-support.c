/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * source-view-support.c: Customizations for GtkSourceView wrappers
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

#include "source-view-support.h"
#include "guile-gnome-gobject.h"

GtkTextIter*
_wrap_gtk_source_buffer_get_iter_at_marker (GtkSourceBuffer *buf, GtkSourceMarker *marker)
{
    GtkTextIter *new = g_new0 (GtkTextIter, 1);

    gtk_source_buffer_get_iter_at_marker (buf, new, marker);
    return new;
}

SCM
_wrap_gtk_source_iter_backward_search (const GtkTextIter *iter, const gchar *string,
                                       GtkSourceSearchFlags flags, const GtkTextIter *limit)
{
    GtkTextIter *start, *end;
    start = g_new0 (GtkTextIter, 1);
    end = g_new0 (GtkTextIter, 1);

    if (gtk_source_iter_backward_search (iter, string, flags, start, end, limit)) {
        SCM s_start, s_end;
        s_start = scm_c_make_gvalue (GTK_TYPE_TEXT_ITER);
        s_end = scm_c_make_gvalue (GTK_TYPE_TEXT_ITER);
        g_value_set_boxed_take_ownership ((GValue*)SCM_SMOB_DATA (s_start), start);
        g_value_set_boxed_take_ownership ((GValue*)SCM_SMOB_DATA (s_end), end);
        return scm_values (SCM_LIST2 (s_start, s_end));
    }
    g_free (start);
    g_free (end);
    return scm_values (SCM_LIST2 (SCM_BOOL_F, SCM_BOOL_F));
}

GtkTextIter*
_wrap_gtk_source_iter_find_matching_bracket (GtkTextIter *bracket)
{
    GtkTextIter *new = g_new0 (GtkTextIter, 1);
    *new = *bracket;

    if (gtk_source_iter_find_matching_bracket (new))
        return new;
    g_free (new);
    return NULL;
}
