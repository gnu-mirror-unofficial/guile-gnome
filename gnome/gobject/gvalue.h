/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * values.h: Support for GValue-based types
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

#ifndef __GUILE_GNOME_GOBJECT_GVALUE_H__
#define __GUILE_GNOME_GOBJECT_GVALUE_H__

#include <guile-gnome-gobject/gtype.h>


G_BEGIN_DECLS


extern scm_t_bits scm_tc16_gvalue;




#define SCM_GVALUEP(scm)			SCM_TYP16_PREDICATE (scm_tc16_gvalue, scm)

#define SCM_VALIDATE_GVALUE(pos, scm)		SCM_VALIDATE_SMOB (pos, scm, gvalue)

#define SCM_VALIDATE_GVALUE_COPY(pos, value, cvar) \
  do { \
    SCM_VALIDATE_GVALUE (pos, value); \
    cvar = (GValue *) SCM_SMOB_DATA (value); \
  } while (0)
   
#define SCM_VALIDATE_GVALUE_TYPE_COPY(pos, value, type, cvar) \
  do { \
    SCM_VALIDATE_GVALUE_COPY (pos, value, cvar); \
    SCM_ASSERT (G_TYPE_CHECK_VALUE_TYPE (cvar, type), value, pos, FUNC_NAME); \
  } while (0)




SCM scm_gtype_valued_p (SCM instance);

SCM scm_gvalue_p (SCM class);
SCM scm_gvalue_to_type (SCM value);
SCM scm_gvalue_primitive_new (SCM type);
SCM scm_gvalue_primitive_get (SCM instance);
SCM scm_gvalue_primitive_set (SCM instance, SCM value);

SCM scm_gvalue_to_scm (SCM value);
SCM scm_scm_to_gvalue (SCM type, SCM scm);

SCM scm_genum_register_static (SCM name, SCM vtable);
SCM scm_gflags_register_static (SCM name, SCM vtable);
SCM scm_genum_type_get_values (SCM type);
SCM scm_gflags_type_get_values (SCM type);

/* C only */

SCM scm_c_make_gvalue (GType gtype);
SCM scm_c_gvalue_to_scm (const GValue *gvalue);
GValue* scm_c_scm_to_gvalue (GType gtype, SCM scm);



G_END_DECLS

#endif
