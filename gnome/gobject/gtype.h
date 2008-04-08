/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * gtype.h: Base support for the GLib type system
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

#ifndef __GUILE_GNOME_GOBJECT_GTYPE_H__
#define __GUILE_GNOME_GOBJECT_GTYPE_H__


#include <glib-object.h>
#include <libguile.h>


G_BEGIN_DECLS


typedef gpointer (*scm_t_gtype_instance_ref)(gpointer instance);
typedef void (*scm_t_gtype_instance_unref)(gpointer instance);
typedef gpointer (*scm_t_gtype_instance_get_qdata)(gpointer instance,
                                                   GQuark quark);
typedef void (*scm_t_gtype_instance_set_qdata)(gpointer instance, GQuark quark,
                                               gpointer data);
typedef void* (*scm_t_gtype_instance_construct)(SCM object, SCM initargs);
typedef void (*scm_t_gtype_instance_initialize_scm)(SCM object, gpointer instance);
typedef struct {
    GType type;
    scm_t_gtype_instance_ref ref;
    scm_t_gtype_instance_unref unref;
    scm_t_gtype_instance_get_qdata get_qdata;
    scm_t_gtype_instance_set_qdata set_qdata;
    scm_t_gtype_instance_construct construct;
    scm_t_gtype_instance_initialize_scm initialize_scm;
} scm_t_gtype_instance_funcs;





extern SCM scm_class_gtype_class;
extern SCM scm_class_gtype_instance;
extern SCM scm_sys_gtype_to_class;






#define SCM_GTYPE_CLASSP(scm)			SCM_NFALSEP (scm_memq (scm_class_gtype_class, \
                                                                       scm_class_precedence_list (scm_class_of ((scm)))))
#define SCM_GTYPE_INSTANCEP(scm)		SCM_IS_A_P (scm, scm_class_gtype_instance)

#define SCM_VALIDATE_GTYPE_CLASS(pos, scm)	SCM_MAKE_VALIDATE (pos, scm, GTYPE_CLASSP)
#define SCM_VALIDATE_GTYPE_INSTANCE(pos, scm)	SCM_MAKE_VALIDATE (pos, scm, GTYPE_INSTANCEP)

#define SCM_VALIDATE_GTYPE_CLASS_COPY(pos, scm, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_CLASS (pos, scm); \
    cvar = scm_c_gtype_class_to_gtype (scm);   \
  } while (0)

#define SCM_VALIDATE_GTYPE_CLASS_IS_A(pos, scm, is_a, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_CLASS_COPY (pos, scm, cvar); \
    SCM_ASSERT (g_type_is_a (cvar, is_a), scm, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_GTYPE_INSTANCE_COPY(pos, value, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_INSTANCE (pos, value); \
    cvar = scm_c_scm_to_gtype_instance (value); \
  } while (0)

#define SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY(pos, value, type, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_INSTANCE (pos, value); \
    cvar = scm_c_scm_to_gtype_instance_typed (value, type); \
    SCM_ASSERT (cvar != NULL, value, pos, FUNC_NAME); \
  } while (0)




/* SCM API */

/* GTypeInstance */
SCM scm_sys_gtype_instance_primitive_destroy_x (SCM instance);

/* Misc */
SCM scm_especify_metaclass_x (SCM class, SCM metaclass);
SCM scm_sys_function_to_method_public (SCM proc, SCM of_object,
                                       SCM generic_name);





/* C-only API */

GType gboxed_scm_get_type (void) G_GNUC_CONST;
#define G_TYPE_BOXED_SCM (gboxed_scm_get_type ())

/* GTypeClass */
SCM scm_c_gtype_lookup_class (GType gtype);
SCM scm_c_gtype_to_class (GType gtype);
gboolean scm_c_gtype_class_is_a_p (SCM instance, GType gtype);
GType scm_c_gtype_class_to_gtype (SCM klass);

/* GTypeInstance use */
gpointer scm_c_gtype_instance_ref (gpointer instance);
void scm_c_gtype_instance_unref (gpointer instance);
gboolean scm_c_gtype_instance_is_a_p (SCM instance, GType gtype);
gpointer scm_c_scm_to_gtype_instance (SCM instance);
gpointer scm_c_scm_to_gtype_instance_typed (SCM instance, GType gtype);
SCM scm_c_gtype_instance_to_scm (gpointer ginstance);

/* GTypeInstance implementations */
void scm_c_gruntime_error (const char *subr, const char *message, SCM args);
void scm_register_gtype_instance_funcs (const scm_t_gtype_instance_funcs *funcs);
void scm_register_gtype_instance_sinkfunc (GType type, void (*sinkfunc) (gpointer));


G_END_DECLS


#endif /* __GUILE_GNOME_GOBJECT_GTYPE_H__ */
