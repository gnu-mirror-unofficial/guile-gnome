/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * private.h: Declarations private to guile-gobject
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

#ifndef __GUILE_GNOME_GOBJECT_PRIVATE_H__
#define __GUILE_GNOME_GOBJECT_PRIVATE_H__


G_BEGIN_DECLS


typedef struct _GuileGTypeClass GuileGTypeClass;
struct _GuileGTypeClass {
    /*< private >*/
    GHashTable *properties_hash;

    guint last_property_id;
    gboolean first_instance_created;

    SCM class;
};


extern GQuark guile_gobject_quark_instance_wrapper;


G_END_DECLS


#endif /* __GUILE_GNOME_GOBJECT_PRIVATE_H__ */
