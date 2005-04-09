/* guile-gnome
 * Copyright (C) 2005 Andreas Rottmann
 *
 * gutil.h: Some GLib-related utilties
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

#ifndef __GUILE_GNOME_GOBJECT_GUTIL_H__
#define __GUILE_GNOME_GOBJECT_GUTIL_H__

#include <glib.h>
#include <libguile.h>


G_BEGIN_DECLS

SCM scm_c_gerror_to_scm (GError *error);
void scm_c_raise_gerror (GError *error);

G_END_DECLS

#endif
