#ifndef __GUILE_CORBA_H__
#define __GUILE_CORBA_H__ 1

#include <guile-gnome-corba-primitives.h>
#include <guile-gnome-corba-types.h>
#include <guile-gnome-corba-generic.h>



SCM guile_corba_typecode_to_class (SCM type);
SCM scm_corba_servant_to_reference (SCM smob_servant);
SCM scm_bonobo_get_object (SCM moniker, SCM class);
SCM scm_bonobo_object_query_interface (SCM object, SCM class);

void scm_init_gnome_corba (void);
void scm_init_gnome_corba_module (void);

#endif
