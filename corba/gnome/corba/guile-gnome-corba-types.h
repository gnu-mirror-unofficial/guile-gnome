/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba-types.h:
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

#ifndef __GUILE_CORBA_TYPES_H__
#define __GUILE_CORBA_TYPES_H__ 1

#include <libguile.h>
#define ORBIT2_INTERNAL_API
/* we need RootObject_duplicate/release */
#include <orbit/orb-core/orbit-object.h>
#undef ORBIT2_INTERNAL_API
#include <orbit/orbit.h>

extern SCM scm_corba_struct_vtable;
extern SCM scm_corba_sequence_vtable;
extern scm_t_bits scm_tc16_corba_typecode;
extern scm_t_bits scm_tc16_orbit_object;
extern scm_t_bits scm_tc16_corba_data;
extern scm_t_bits scm_tc16_corba_object;
extern scm_t_bits scm_tc16_orbit_imethod;
extern scm_t_bits scm_tc16_orbit_iinterface;

extern SCM scm_sym_corba_objref;
extern SCM scm_sym_corba_objref;
extern SCM scm_sym_corba_typecode;
extern SCM scm_sym_orbit_iinterface;
extern SCM scm_sym_servant;
extern SCM scm_sym_stub_class;
extern SCM scm_sym_corba_system_exception;
extern SCM scm_sym_corba_user_exception;



#define scm_si_corba_typecode                 (scm_vtable_offset_user + 1)
#define scm_si_corba_struct_data              (scm_vtable_offset_user + 2)
#define scm_si_corba_struct_n_fields          (scm_vtable_offset_user + 3)
#define scm_si_corba_struct_members           (scm_vtable_offset_user + 4)
#define scm_si_corba_struct_fields            (scm_vtable_offset_user + 5)
#define scm_corba_struct_vtable_offset_user   (scm_vtable_offset_user + 6)
#define scm_corba_sequence_vtable_offset_user (scm_vtable_offset_user + 2)


#define SCM_CORBA_STRUCTP(x)                (SCM_STRUCTP (x) \
                                             && SCM_EQ_P (scm_struct_vtable (x), scm_corba_struct_vtable))

#define SCM_VALIDATE_CORBA_STRUCT(pos, scm) SCM_MAKE_VALIDATE (pos, scm, CORBA_STRUCTP)

#define SCM_CORBA_STRUCT_TYPECODE(x)        (CORBA_TypeCode) SCM_SMOB_DATA (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_corba_typecode]))
#define SCM_SET_CORBA_STRUCT_TYPECODE(x,tc) G_STMT_START{			\
   SCM z;									\
   SCM_NEWSMOB (z, scm_tc16_corba_typecode, ORBit_RootObject_duplicate (tc));	\
   SCM_STRUCT_DATA (x) [scm_si_corba_typecode] = SCM_UNPACK (z);		\
}G_STMT_END

#define SCM_CORBA_STRUCT_FIELDS(x)          (SCM_STRUCT_DATA (x) [scm_si_corba_struct_fields])
#define SCM_SET_CORBA_STRUCT_FIELDS(x,f)    {SCM_STRUCT_DATA (x) [scm_si_corba_struct_fields] = SCM_UNPACK (f);}

#define SCM_CORBA_STRUCT_N_FIELDS(x)        SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_corba_struct_n_fields]))
#define SCM_SET_CORBA_STRUCT_N_FIELDS(x,n)  {SCM_STRUCT_DATA (x) [scm_si_corba_struct_n_fields] = SCM_UNPACK (n);}

#define SCM_CORBA_STRUCT_DATA(x)            SCM_SMOB_DATA (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_corba_struct_data]))
#define SCM_SET_CORBA_STRUCT_DATA(x,d)      G_STMT_START{			\
   SCM z;									\
   SCM_NEWSMOB (z, scm_tc16_orbit_object, ORBit_RootObject_duplicate (d)); \
   SCM_STRUCT_DATA (x) [scm_si_corba_struct_data] = SCM_UNPACK (z);		\
}G_STMT_END

#define SCM_CORBA_STRUCT_MEMBERS(x)         (DynamicAny_NameValuePairSeq *) SCM_SMOB_DATA (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_corba_struct_members]))
#define SCM_SET_CORBA_STRUCT_MEMBERS(x,d)   G_STMT_START{			\
   SCM z;									\
   SCM_NEWSMOB (z, scm_tc16_corba_data, d);				\
   SCM_STRUCT_DATA (x) [scm_si_corba_struct_members] = SCM_UNPACK (z);		\
}G_STMT_END

#define SCM_CORBA_SEQUENCEP(x)                (SCM_STRUCTP (x) \
                                               && SCM_EQ_P (scm_struct_vtable (x), scm_corba_sequence_vtable))

#define SCM_VALIDATE_CORBA_SEQUENCE(pos, scm) SCM_MAKE_VALIDATE (pos, scm, CORBA_SEQUENCEP)

#define SCM_CORBA_SEQUENCE_TYPECODE(x)        SCM_CORBA_STRUCT_TYPECODE(x)
#define SCM_SET_CORBA_SEQUENCE_TYPECODE(x,tc) SCM_SET_CORBA_STRUCT_TYPECODE(x,tc)

#define SCM_CORBA_SEQUENCE_DATA(x)            SCM_CORBA_STRUCT_DATA(x)
#define SCM_SET_CORBA_SEQUENCE_DATA(x,tc)     SCM_SET_CORBA_STRUCT_DATA(x,tc)

#define SCM_CORBA_SEQUENCE_LENGTH(x)          SCM_CORBA_STRUCT_N_FIELDS(x)
#define SCM_SET_CORBA_SEQUENCE_LENGTH(x,l)    SCM_SET_CORBA_STRUCT_N_FIELDS(x,l)

#define SCM_CORBA_SEQUENCE_MEMBERS(x)         (DynamicAny_AnySeq *) SCM_SMOB_DATA (SCM_PACK (SCM_STRUCT_DATA (x) [scm_si_corba_struct_members]))
#define SCM_SET_CORBA_SEQUENCE_MEMBERS(x,d)   G_STMT_START{			\
   SCM z;									\
   SCM_NEWSMOB (z, scm_tc16_corba_data, d);				\
   SCM_STRUCT_DATA (x) [scm_si_corba_struct_members] = SCM_UNPACK (z);		\
}G_STMT_END



#define SCM_CORBA_OBJECTP(scm) \
SCM_INSTANCEP (scm) && SCM_IS_A_P (SCM_CLASS_OF (scm), scm_class_corba_object)

#define SCM_VALIDATE_CORBA_OBJECT(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, CORBA_OBJECTP)

#define SCM_VALIDATE_CORBA_OBJECT_COPY(pos, scm, cvar) \
  do { \
    SCM smob; \
    SCM_VALIDATE_CORBA_OBJECT (pos, scm); \
    smob = scm_slot_ref (scm, scm_sym_corba_objref); \
    SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_corba_object, smob), smob, \
		pos, FUNC_NAME); \
    cvar = (CORBA_Object) SCM_SMOB_DATA (smob); \
  } while (0)



#define SCM_CORBA_OBJECT_CLASSP(scm) \
SCM_CLASSP (scm) && SCM_IS_A_P (scm, scm_class_corba_object)

#define SCM_VALIDATE_CORBA_OBJECT_CLASS(pos, scm) \
SCM_MAKE_VALIDATE (pos, scm, CORBA_OBJECT_CLASSP)

#define SCM_VALIDATE_CORBA_OBJECT_CLASS_COPY(pos, scm, cvar) \
  do { \
    SCM smob; \
    SCM_VALIDATE_CORBA_OBJECT_CLASS (pos, scm); \
    smob = scm_call_2 (scm_class_slot_ref, scm, scm_sym_corba_typecode); \
    SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_corba_typecode, smob), smob, \
		pos, FUNC_NAME); \
    cvar = (CORBA_TypeCode) SCM_SMOB_DATA (smob); \
  } while (0)



#define SCM_VALIDATE_ORBIT_IMETHOD(pos, scm) \
SCM_VALIDATE_SMOB (pos, scm, orbit_imethod)

#define SCM_VALIDATE_ORBIT_IMETHOD_COPY(pos, type, cvar) \
  do { \
    SCM_VALIDATE_ORBIT_IMETHOD (pos, type); \
    cvar = (ORBit_IMethod *) SCM_SMOB_DATA (type); \
  } while (0)



#define SCM_VALIDATE_CORBA_TYPECODE(pos, scm) \
SCM_VALIDATE_SMOB (pos, scm, corba_typecode)

#define SCM_VALIDATE_CORBA_TYPECODE_COPY(pos, type, cvar) \
  do { \
    SCM_VALIDATE_CORBA_TYPECODE (pos, type); \
    cvar = (CORBA_TypeCode) SCM_SMOB_DATA (type); \
  } while (0)



SCM scm_make_corba_struct (SCM typecode, SCM num_tail_elts, SCM init_smob);
SCM scm_corba_struct_fields (SCM typecode);
SCM scm_corba_struct_ref (SCM corba_struct, SCM index);
SCM scm_corba_struct_set_x (SCM corba_struct, SCM index, SCM value);
SCM scm_corba_struct_p (SCM corba_struct);
SCM scm_corba_struct_is_a_p (SCM corba_struct, SCM typecode);
SCM scm_corba_struct_type (SCM corba_struct);

SCM scm_make_corba_sequence (SCM typecode, SCM length, SCM init_smob);
SCM scm_corba_sequence_length (SCM corba_sequence);
SCM scm_corba_sequence_set_length_x (SCM corba_sequence, SCM length);
SCM scm_corba_sequence_set_x (SCM corba_sequence, SCM index, SCM value);
SCM scm_corba_sequence_ref (SCM corba_sequence, SCM index);
SCM scm_corba_sequence_type (SCM corba_sequence);

SCM scm_corba_typecode_to_gtype (SCM typecode);



SCM scm_c_make_corba_struct (CORBA_TypeCode tc, guint nfields, DynamicAny_DynStruct dyn);
SCM scm_c_make_corba_sequence (CORBA_TypeCode tc, guint length, gpointer data);
SCM scm_c_corba_demarshal_any (const CORBA_any *any);
void scm_c_corba_marshal_any (CORBA_any *any, SCM value);
SCM scm_c_make_corba_object (SCM class, CORBA_Object corba_objref);
SCM scm_c_make_corba_typecode (CORBA_TypeCode tc);
SCM scm_corba_object_class_to_typecode (SCM class);
SCM scm_c_corba_typecode_to_class (CORBA_TypeCode tc);



void scm_pre_init_gnome_corba_types (void);
void scm_init_gnome_corba_types (void);


#endif
