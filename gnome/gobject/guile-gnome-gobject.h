/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-gobject.h: The GObject wrapper
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

#ifndef __GUILE_GNOME_GOBJECT_H__
#define __GUILE_GNOME_GOBJECT_H__

#include <guile-gnome-gobject-primitives.h>
#include <libguile.h>
#include <g-wrap/core-runtime.h>

G_BEGIN_DECLS

extern SCM scm_module_gobject;

extern SCM scm_class_gparam;
extern SCM scm_class_gobject;
extern SCM scm_sym_gtype_to_class;



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



#define SCM_GPARAMP(scm) \
SCM_INSTANCEP (scm) && SCM_IS_A_P (scm, scm_class_gparam)

#define SCM_VALIDATE_GPARAM(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, GPARAMP)

#define SCM_VALIDATE_GPARAM_COPY(pos, scm, cvar) \
  do { \
    SCM tmp_instance; \
    SCM_VALIDATE_GPARAM (pos, scm); \
    tmp_instance = scm_slot_ref (scm, scm_sym_gtype_instance); \
    SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (0, tmp_instance, G_TYPE_PARAM, GParamSpec, cvar); \
  } while (0)



void scm_init_gnome_gobject_module (void);
void scm_pre_init_gnome_gobject (void);
void scm_init_gnome_gobject (void);
void scm_post_init_gnome_gobject (void);



SCM scm_gobject_scheme_dir (void);
SCM scm_gtype_children (SCM type);
SCM scm_gtype_to_fundamental (SCM type);
SCM scm_gtype_to_class_name (SCM type);
SCM scm_gtype_to_method_name (SCM type, SCM method);
SCM scm_gtype_p (SCM type);
SCM scm_gtype_fundamental_p (SCM type);

SCM scm_genum_register_static (SCM name, SCM vtable);
SCM scm_gflags_register_static (SCM name, SCM vtable);

SCM scm_gtype_register_static (SCM name, SCM parent_type);
SCM scm_gobject_set_data_x (SCM object, SCM key, SCM val);
SCM scm_gobject_get_data (SCM object, SCM key);
SCM scm_gobject_class_install_property (SCM type, SCM param);
SCM scm_gparam_to_value_type (SCM param);



gboolean scm_c_gtype_instance_is_a_p (SCM instance, GType gtype);
GTypeInstance* scm_c_scm_to_gtype_instance (SCM instance, GType gtype);
SCM scm_c_gtype_instance_to_scm (GTypeInstance *ginstance);
SCM scm_c_gvalue_to_scm (const GValue *value);
GValue* scm_c_scm_to_gvalue (GType type, SCM scm);

void scm_init_gnome_gobject_helper (GType type);



/* Macros that are not available as functions (needed for glueless wrapping) */

GType    g_type_from_instance (GTypeInstance *);
gboolean g_type_is_instantiatable (GType type);
gboolean g_type_is_classed (GType type);

G_END_DECLS

#endif
