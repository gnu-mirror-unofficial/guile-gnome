/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba-generic.h:
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

#ifndef __GUILE_CORBA_GENERIC_H__
#define __GUILE_CORBA_GENERIC_H__ 1

#include <orbit/orbit.h>

G_BEGIN_DECLS

gchar *
guile_corba_generic_repo_id_to_name (const gchar *format, const gchar *repo_id);

gchar *
guile_corba_generic_make_type_name (const gchar *StudlyCaps);

GType
guile_corba_generic_typecode_to_type (CORBA_TypeCode tc);

CORBA_TypeCode
guile_corba_generic_type_to_typecode (GType type);

void
scm_pre_init_gnome_corba_generic (void);

G_END_DECLS

#endif
