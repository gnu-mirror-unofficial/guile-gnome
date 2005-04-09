/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * gobject.h: Support for GObject and GInterface
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

#ifndef __GUILE_GNOME_GOBJECT_OBJECT_H__
#define __GUILE_GNOME_GOBJECT_OBJECT_H__

#include <guile-gnome-gobject/gtype.h>
#include <guile-gnome-gobject/gparameter.h>
#include <guile-gnome-gobject/gvalue.h>
#include <guile-gnome-gobject/gsignal.h>
#include <guile-gnome-gobject/gutil.h>

G_BEGIN_DECLS


extern SCM scm_class_gobject;




#define SCM_GOBJECTP(scm) \
SCM_INSTANCEP (scm) && SCM_GTYPE_CLASS_SUBCLASSP (SCM_CLASS_OF (scm), scm_class_gobject)

#define SCM_VALIDATE_GOBJECT(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, GOBJECTP)

#define SCM_VALIDATE_GOBJECT_COPY(pos, scm, cvar) \
  do { \
    SCM tmp_instance; \
    SCM_VALIDATE_GOBJECT (pos, scm); \
    tmp_instance = scm_slot_ref (scm, scm_sym_gtype_instance); \
    SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (pos, tmp_instance, G_TYPE_OBJECT, GObject, cvar); \
  } while (0)

#define SCM_GOBJECT_CLASSP(scm) \
SCM_GTYPE_CLASS_SUBCLASSP (scm, scm_class_gobject)

#define SCM_VALIDATE_GOBJECT_CLASS(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, GOBJECT_CLASSP)

#define SCM_VALIDATE_GOBJECT_CLASS_GET_TYPE(pos, scm, cvar) \
  do { \
    SCM tmp_type; \
    SCM_VALIDATE_GOBJECT_CLASS (pos, scm); \
    tmp_type = scm_slot_ref (scm, scm_sym_gtype); \
    SCM_VALIDATE_GTYPE_COPY (0, tmp_type, cvar); \
  } while (0)




SCM scm_gobject_set_data_x (SCM object, SCM key, SCM val);
SCM scm_gobject_get_data (SCM object, SCM key);

SCM scm_gtype_register_static (SCM name, SCM parent_type);
SCM scm_scheme_gclass_p (SCM class);
SCM scm_gobject_class_install_property (SCM type, SCM param);

SCM scm_gobject_type_get_properties (SCM type);
SCM scm_gobject_primitive_create_instance (SCM class, SCM type, SCM object, SCM properties);
SCM scm_gobject_primitive_get_property (SCM object, SCM name);
SCM scm_gobject_primitive_set_property (SCM object, SCM name, SCM value);

void scm_register_gobject_postmakefunc (GType type, gpointer (*postmakefunc) (gpointer));



G_END_DECLS

#endif
