#ifndef __GUILE_GNOME_GDA_SUPPORT_H__
#define __GUILE_GNOME_GDA_SUPPORT_H__

#include <libgda/libgda.h>
#include <libguile.h>

#include "guile-gnome-gobject-primitives.h"

G_BEGIN_DECLS

#define SCM_VALIDATE_GDA_VALUE_COPY(pos, scm, cvar) \
  do { \
    GValue *gvalue; \
    SCM_VALIDATE_GVALUE_TYPE_COPY (pos, scm, GDA_TYPE_VALUE, gvalue); \
    cvar = (GdaValue*) g_value_get_boxed (gvalue); \
  } while (0);

void scm_init_gnome_gda_support (void);

SCM scm_gda_value_primitive_new (SCM value);
SCM scm_gda_value_primitive_set_x (SCM gda_value, SCM value);
SCM scm_gda_value_primitive_get (SCM gda_value);
SCM scm_gda_init (SCM name, SCM version, SCM args);

SCM scm_c_make_gtype_primitive_instance (const char *class_name,
                                         GType type, gpointer val);

G_END_DECLS

#endif
