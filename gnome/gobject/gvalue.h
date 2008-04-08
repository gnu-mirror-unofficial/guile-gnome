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


extern SCM scm_class_gvalue;




#define SCM_GVALUEP(scm)			SCM_IS_A_P (scm, scm_class_gvalue)

#define SCM_VALIDATE_GVALUE(pos, scm)		SCM_MAKE_VALIDATE (pos, scm, GVALUEP)

#define SCM_VALIDATE_GVALUE_COPY(pos, value, cvar) \
  do { \
    SCM_VALIDATE_GVALUE (pos, value); \
    cvar = scm_c_gvalue_peek_value (value); \
  } while (0)
   
#define SCM_VALIDATE_GVALUE_TYPE_COPY(pos, value, type, cvar) \
  do { \
    SCM_VALIDATE_GVALUE_COPY (pos, value, cvar); \
    SCM_ASSERT (G_TYPE_CHECK_VALUE_TYPE (cvar, type), value, pos, FUNC_NAME); \
  } while (0)




SCM scm_gvalue_to_scm (SCM value);
SCM scm_scm_to_gvalue (SCM type, SCM scm);

SCM scm_genum_register_static (SCM name, SCM vtable);
SCM scm_gflags_register_static (SCM name, SCM vtable);

/* C only */

SCM scm_c_make_gvalue (GType gtype);
GValue* scm_c_gvalue_peek_value (SCM value);
SCM scm_c_gvalue_to_scm (const GValue *gvalue);
SCM scm_c_gvalue_ref (const GValue *gvalue);
void scm_c_gvalue_set (GValue *gvalue, SCM value);
GValue* scm_c_scm_to_gvalue (GType gtype, SCM scm);

void scm_c_register_gvalue_wrappers (GType type,
                                     SCM (*wrap) (const GValue*),
                                     void (*unwrap) (SCM, GValue*));

typedef gpointer (*SCMGValueGetTypeInstanceFunc) (const GValue*);
typedef void (*SCMGValueSetTypeInstanceFunc) (GValue*, gpointer instance);
void scm_c_register_gtype_instance_gvalue_wrappers (GType type,
    SCMGValueGetTypeInstanceFunc getter, SCMGValueSetTypeInstanceFunc setter);

/* Helpers */
gboolean scm_c_gvalue_holds (SCM maybe_gvalue, GType type);
SCM scm_c_gvalue_new_from_boxed (GType type, const gpointer boxed);
SCM scm_c_gvalue_new_take_boxed (GType type, gpointer boxed);
gpointer scm_c_gvalue_peek_boxed (SCM value);
gpointer scm_c_gvalue_dup_boxed (SCM value);

// should these go into gobject-support or something?
GValue* scm_c_gvalue_dup_value (SCM scm);
SCM scm_c_gvalue_from_value (const GValue *value);
SCM scm_c_gvalue_take_value (GValue *value);




G_END_DECLS

#endif
