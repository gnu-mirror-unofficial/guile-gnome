/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * glib-support.h: Support for the GLib binding
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

#if !defined(_GUILE_GOBJECT_GLIB_SUPPORT_H)
#define _GUILE_GOBJECT_GLIB_SUPPORT_H

#include <glib.h>
#include <libguile.h>
#include "guile-gnome-gobject.h"

G_BEGIN_DECLS

void scm_init_glib (void);
void _wrap_g_main_loop_run (GMainLoop *loop);
SCM  _wrap_g_string_get_str (GString *str);
guint _wrap_g_io_add_watch (GIOChannel *channel, GIOCondition condition, SCM func);

G_END_DECLS

#endif
