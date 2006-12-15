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
typedef struct {
    GType type;
    scm_t_gtype_instance_ref ref;
    scm_t_gtype_instance_unref unref;
    scm_t_gtype_instance_get_qdata get_qdata;
    scm_t_gtype_instance_set_qdata set_qdata;
} scm_t_gtype_instance_funcs;





extern SCM scm_sym_gtype;
extern SCM scm_sym_gtype_class;
extern SCM scm_sym_gtype_instance;

extern SCM scm_class_gtype_class;
extern SCM scm_gtype_to_class;

extern scm_t_bits scm_tc16_gtype;
extern scm_t_bits scm_tc16_gtype_class;
extern scm_t_bits scm_tc16_gtype_instance;





/* For some reason, SCM_IS_A_P doesn't act like is-a? in GOOPS. So we implement
   the GOOPS version an an extra-hacky macro. */
#define SCM_HACKY_IS_A_P(scm,class) \
  (SCM_INSTANCEP (scm) && scm_memq (class, scm_class_precedence_list (SCM_CLASS_OF (scm))))

#define SCM_ERROR_NOT_YET_IMPLEMENTED(what) \
  scm_c_gruntime_error (FUNC_NAME, "Not yet implemented: file ~S line ~S: ~A", \
			SCM_LIST3 (scm_makfrom0str (__FILE__), SCM_MAKINUM (__LINE__), what))

#define SCM_GTYPEP(scm)				SCM_TYP16_PREDICATE (scm_tc16_gtype, scm)
#define SCM_GTYPE_CLASSP(scm)			SCM_HACKY_IS_A_P (scm, scm_class_gtype_class)
/* Roll our own SUBCLASSP, the GOOPS one only works on metaclasses */
#define SCM_GTYPE_CLASS_SUBCLASSP(scm,class)	(SCM_GTYPE_CLASSP (scm) && (!SCM_FALSEP (scm_c_memq (class, scm_class_precedence_list (scm)))))

#define SCM_VALIDATE_GTYPE(pos, scm)		SCM_VALIDATE_SMOB (pos, scm, gtype)
#define SCM_VALIDATE_GTYPE_CLASS(pos, scm)	SCM_MAKE_VALIDATE (pos, scm, GTYPE_CLASSP)

#define SCM_VALIDATE_GTYPE_INSTANCE(pos, scm)	SCM_VALIDATE_SMOB (pos, scm, gtype_instance)

#define SCM_VALIDATE_GTYPE_COPY(pos, type, cvar) \
  do { \
    SCM_VALIDATE_GTYPE (pos, type); \
    cvar = (GType) SCM_SMOB_DATA (type); \
  } while (0)

#define SCM_VALIDATE_GTYPE_IS_A(pos, scm, is_a, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_COPY (pos, type, cvar); \
    SCM_ASSERT (g_type_is_a (cvar, is_a), type, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_GTYPE_INSTANCE_COPY(pos, value, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_INSTANCE (pos, value); \
    cvar = (GTypeInstance *) SCM_SMOB_DATA (value); \
  } while (0)

#define SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY(pos, value, type, ctype, cvar) \
  do { \
    SCM_VALIDATE_GTYPE_INSTANCE (pos, value); \
    cvar = (ctype *) SCM_SMOB_DATA (value); \
    SCM_ASSERT (G_TYPE_CHECK_INSTANCE_TYPE (cvar, type), value, pos, FUNC_NAME); \
  } while (0)




/* SCM API */

/* GType */
SCM scm_gtype_p (SCM type);
SCM scm_gtype_is_a_p (SCM type, SCM is_a_type);
SCM scm_gtype_basic_p (SCM type);
SCM scm_gtype_classed_p (SCM type);
SCM scm_gtype_instantiatable_p (SCM type);
SCM scm_gtype_fundamental_p (SCM type);
SCM scm_gtype_to_fundamental (SCM type);
SCM scm_gtype_parent (SCM type);
SCM scm_gtype_children (SCM type);
SCM scm_gtype_interfaces (SCM type);
SCM scm_gtype_name (SCM type);
SCM scm_gtype_from_name (SCM name);
SCM scm_gtype_from_instance (SCM instance);

/* GTypeClass */
SCM scm_sys_gtype_lookup_class (SCM type);
SCM scm_sys_gtype_bind_to_class (SCM class, SCM type);

/* GTypeInstance */
SCM scm_gtype_instance_primitive_to_type (SCM instance);
SCM scm_gtype_instance_primitive_to_value (SCM instance);
SCM scm_sys_gtype_instance_primitive_destroy_x (SCM instance);

/* Misc */
SCM scm_especify_metaclass_x (SCM class, SCM metaclass);
SCM scm_sys_function_to_method_public (SCM proc, SCM of_object,
                                       SCM generic_name);





/* C-only API */

GType gboxed_scm_get_type (void) G_GNUC_CONST;
#define G_TYPE_BOXED_SCM (gboxed_scm_get_type ())

/* GType */
SCM scm_c_register_gtype (GType type);

/* GTypeClass */
SCM scm_c_gtype_lookup_class (GType gtype);
SCM scm_c_gtype_to_class (GType gtype);

/* GTypeInstance use */
gpointer scm_c_gtype_instance_ref (gpointer instance);
void scm_c_gtype_instance_unref (gpointer instance);
SCM scm_c_make_gtype_instance (gpointer ginstance);
gboolean scm_c_gtype_instance_is_a_p (SCM instance, GType gtype);
gpointer scm_c_scm_to_gtype_instance (SCM instance, GType gtype);
SCM scm_c_gtype_instance_to_scm (gpointer ginstance);

/* GTypeInstance implementations */
void scm_c_define_and_export_gtype_x (GType type);
void scm_c_gruntime_error (const char *subr, const char *message, SCM args);
void guile_gobject_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
                                const gchar *message, gpointer user_data);
void scm_register_gtype_instance_funcs (const scm_t_gtype_instance_funcs *funcs);
void scm_register_gtype_instance_sinkfunc (GType type, void (*sinkfunc) (gpointer));
SCM scm_c_gtype_instance_get_cached_smob (gpointer instance);
void scm_c_gtype_instance_set_cached_smob (gpointer instance, SCM smob);
SCM scm_c_gtype_instance_get_cached_goops (gpointer instance);
void scm_c_gtype_instance_set_cached_goops (gpointer instance, SCM goops);


G_END_DECLS


#endif /* __GUILE_GNOME_GOBJECT_GTYPE_H__ */
