#ifndef __GUILE_GNOME_GOBJECT_H__
#define __GUILE_GNOME_GOBJECT_H__

#include <guile-gnome-gobject-primitives.h>
#include <libguile.h>

G_BEGIN_DECLS

extern SCM scm_class_gparam;
extern SCM scm_class_gobject;
extern SCM scm_sym_gtype_to_class;



#define SCM_GOBJECTP(scm) \
SCM_INSTANCEP (scm) && SCM_IS_A_P (SCM_CLASS_OF (scm), scm_class_gobject)

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
SCM_CLASSP (scm) && SCM_IS_A_P (scm, scm_class_gobject)

#define SCM_VALIDATE_GOBJECT_CLASS(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, GOBJECT_CLASSP)

#define SCM_VALIDATE_GOBJECT_CLASS_COPY(pos, scm, cvar) \
  do { \
    SCM tmp_type; \
    SCM_VALIDATE_GOBJECT_CLASS (pos, scm); \
    tmp_type = scm_call_2 (scm_sym_class_slot_ref, scm, scm_sym_gtype); \
    SCM_VALIDATE_GTYPE_COPY (0, tmp_type, cvar); \
  } while (0)



#define SCM_GPARAMP(scm) \
SCM_INSTANCEP (scm) && SCM_IS_A_P (SCM_CLASS_OF (scm), scm_class_gparam)

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
SCM scm_gobject_register_type (SCM symbol, SCM name);
SCM scm_gtype_children (SCM type);
SCM scm_gtype_to_fundamental (SCM type);
SCM scm_gtype_to_class_name (SCM type);
SCM scm_gtype_to_method_name (SCM type, SCM method);
SCM scm_gtype_eq_p (SCM a, SCM b);
SCM scm_gtype_p (SCM type);
SCM scm_gtype_fundamental_p (SCM type);

SCM scm_genum_register_static (SCM name, SCM vtable);
SCM scm_gflags_register_static (SCM name, SCM vtable);

SCM scm_gtype_register_static (SCM name, SCM parent_type);
SCM scm_gobject_class_install_property (SCM type, SCM param);
SCM scm_gparam_to_value_type (SCM param);



gboolean scm_c_gtype_instance_is_a_p (SCM instance, GType gtype);
GTypeInstance*scm_c_scm_to_gtype_instance (SCM instance, GType gtype);
SCM scm_c_gtype_instance_to_scm (GTypeInstance *ginstance);

G_END_DECLS

#endif
