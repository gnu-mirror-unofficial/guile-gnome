/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * glade-support.h:
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

#include <glade/glade.h>
#include <libguile.h>
#include "guile-gnome-gobject.h"


GladeXML* _wrap_glade_xml_new_from_buffer (const char *buffer, const char *root,
                                           const char *domain);

void _wrap_glade_xml_signal_connect (GladeXML *xml, const char *handlername, SCM proc);
void _wrap_glade_xml_signal_autoconnect (GladeXML *xml, SCM module);

GtkWidget* guile_glade_custom_handler (GladeXML *xml, gchar *func, gchar *name,
                                       gchar *string1, gchar *string2,
                                       gint int1, gint int2, gpointer user_data);

