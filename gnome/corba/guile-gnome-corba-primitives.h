/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba-primitives.h:
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

#ifndef __GUILE_CORBA_PRIMITIVES_H__
#define __GUILE_CORBA_PRIMITIVES_H__ 1

#include <libguile.h>
#include <orbit/orbit.h>
#include <orbit/poa/poa-types.h>

typedef struct {
    ORBit_IInterface *iinterface;
    PortableServer_ClassInfo class_info;

    GHashTable *epv_hash;

    SCM poa_class, stub_class;

    glong epv_size;
    PortableServer_ServantBase__vepv *vepv;
    gpointer *epv;

} GuileCorbaInterface;

typedef struct {

    PortableServer_ServantBase servant;
    PortableServer_ObjectId *objid;
    
    GuileCorbaInterface *interface;
    
    SCM this;

}GuilePortableServer_Servant;



extern DynamicAny_DynAnyFactory guile_corba_dynany_factory;
extern PortableServer_POA guile_corba_poa;
extern CORBA_ORB guile_corba_orb;

extern SCM scm_class_corba_object;
extern SCM scm_class_portable_server_servant_base;

extern scm_t_bits scm_tc16_guile_corba_interface;
extern scm_t_bits scm_tc16_guile_portable_server_servant;



#define SCM_PORTABLE_SERVER_SERVANT_BASEP(scm) \
SCM_INSTANCEP (scm) && SCM_IS_A_P (SCM_CLASS_OF (scm), scm_class_portable_server_servant_base)

#define SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, PORTABLE_SERVER_SERVANT_BASEP)

#define SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE_COPY(pos, scm, cvar) \
  do { \
    SCM tmp_smob; \
    SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE (pos, scm); \
    tmp_smob = scm_slot_ref (scm, scm_sym_servant); \
    SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_guile_portable_server_servant, tmp_smob), \
		tmp_smob, pos, FUNC_NAME); \
    cvar = (GuilePortableServer_Servant *) SCM_SMOB_DATA (tmp_smob); \
  } while (0)



#define SCM_PORTABLE_SERVER_SERVANT_BASE_CLASSP(scm) \
SCM_CLASSP (scm) && SCM_IS_A_P (scm, scm_class_portable_server_servant_base)

#define SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE_CLASS(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, PORTABLE_SERVER_SERVANT_BASE_CLASSP)

#define SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE_CLASS_COPY(pos, scm, cvar) \
  do { \
    SCM tmp_smob, tmp_class; \
    SCM_VALIDATE_PORTABLE_SERVER_SERVANT_BASE_CLASS (pos, scm); \
    tmp_class = scm_corba_primitive_find_poa_class (scm); \
    tmp_smob = scm_call_2 (scm_sym_class_slot_ref, tmp_class, scm_sym_orbit_iinterface); \
    SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_guile_corba_interface, tmp_smob), \
		tmp_smob, pos, FUNC_NAME); \
    cvar = (GuileCorbaInterface *) SCM_SMOB_DATA (tmp_smob); \
  } while (0)



SCM scm_corba_primitive_invoke_method (SCM method_name, SCM imethod, SCM class, SCM args);
SCM scm_corba_primitive_find_poa_class (SCM class);
SCM scm_corba_primitive_make_poa_instance (SCM class);
SCM scm_corba_typecode_primitive_p (SCM typecode);
SCM scm_corba_typecode_primitive_to_name (SCM typecode);
SCM scm_corba_typecode_primitive_to_class (SCM type);
SCM scm_corba_primitive_open_module (SCM scm_name);
SCM scm_corba_primitive_register_interface (SCM name);
SCM scm_corba_primitive_main (void);



SCM scm_c_corba_typecode_primitive_to_class (CORBA_TypeCode tc);
void scm_c_corba_handle_exception (CORBA_Environment *ev);

void scm_pre_init_gnome_corba_primitives (void);
void scm_init_gnome_corba_primitives (void);

#endif
