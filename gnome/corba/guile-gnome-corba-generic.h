#ifndef __GUILE_CORBA_GENERIC_H__
#define __GUILE_CORBA_GENERIC_H__ 1

#include <orbit/orbit.h>

G_BEGIN_DECLS

gchar *
guile_corba_generic_repo_id_to_name (const gchar *format, const gchar *repo_id);

gchar *
guile_corba_generic_make_type_name (const gchar *format, const gchar *name);

GType
guile_corba_generic_typecode_to_type (CORBA_TypeCode tc);

CORBA_TypeCode
guile_corba_generic_type_to_typecode (GType type);

void
scm_pre_init_gnome_corba_generic (void);

G_END_DECLS

#endif
