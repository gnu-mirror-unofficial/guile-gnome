/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * source-view-support.h: Customizations for GtkSourceView wrappers
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

#include <libguile.h>
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourceview-typebuiltins.h>
#include <gtksourceview/gtksourceprintjob.h>
#include <gtksourceview/gtksourcetag.h>
#include <gtksourceview/gtksourcelanguagesmanager.h>


GtkTextIter* _wrap_gtk_source_buffer_get_iter_at_marker (GtkSourceBuffer *buf, GtkSourceMarker *marker);
SCM _wrap_gtk_source_iter_backward_search (const GtkTextIter *iter, const gchar *string,
                                           GtkSourceSearchFlags flags, const GtkTextIter *limit);
GtkTextIter* _wrap_gtk_source_iter_find_matching_bracket (GtkTextIter *bracket);
