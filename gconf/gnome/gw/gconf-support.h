/* guile-gnome
 * Copyright (C) 2004 Free Software Foundation, Inc.
 *
 * gconf-support.h:
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

#include <gconf/gconf.h>
#include <gconf/gconf-client.h>
#include <libguile.h>
#include "guile-gnome-gobject.h"

#define _GCONF_TYPE_SCHEMA (_gconf_schema_get_type ())
#define _GCONF_TYPE_VALUE (_gconf_value_get_type ())

GType _gconf_schema_get_type (void);

GType _gconf_value_get_type (void);

SCM scm_c_gconf_value_to_scm (const GConfValue *value);
GConfValue *scm_c_scm_to_gconf_value (SCM value);

guint _wrap_gconf_client_notify_add (GConfClient *client,
                                     const gchar *namespace_section,
                                     SCM proc, GError **err);

