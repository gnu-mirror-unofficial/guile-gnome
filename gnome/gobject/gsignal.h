/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * gsignal.h: Support for GSignal
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

#ifndef __GUILE_GNOME_GOBJECT_GSIGNAL_H__
#define __GUILE_GNOME_GOBJECT_GSIGNAL_H__

#include <guile-gnome-gobject/gclosure.h>

G_BEGIN_DECLS

extern SCM scm_gsignal_vtable;



#define scm_si_gsignal_id			(scm_vtable_offset_user)
#define scm_si_gsignal_name			(scm_vtable_offset_user + 1)
#define scm_si_gsignal_interface_type		(scm_vtable_offset_user + 2)
#define scm_si_gsignal_return_type		(scm_vtable_offset_user + 3)
#define scm_si_gsignal_flags			(scm_vtable_offset_user + 4)
#define scm_si_gsignal_params			(scm_vtable_offset_user + 5)

#define SCM_GSIGNAL_STRUCTP(x)			(SCM_STRUCTP (x) && \
						 SCM_EQ_P (scm_struct_vtable (x), scm_gsignal_vtable))

#define SCM_SET_GSIGNAL_ID(x,f)			{SCM_STRUCT_DATA (x) [scm_si_gsignal_id] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_NAME(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gsignal_name] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_INTERFACE_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_si_gsignal_interface_type] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_RETURN_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_si_gsignal_return_type] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_FLAGS(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gsignal_flags] = SCM_UNPACK (f);}
#define SCM_SET_GSIGNAL_PARAMS(x,f)		{SCM_STRUCT_DATA (x) [scm_si_gsignal_params] = SCM_UNPACK (f);}

#define SCM_GSIGNAL_ID(x)			SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_id]))
#define SCM_GSIGNAL_NAME(x)			SCM_SYMBOL_CHARS (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_name])))
#define SCM_GSIGNAL_INTERFACE_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_interface_type]))))
#define SCM_GSIGNAL_RETURN_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_return_type]))))
#define SCM_GSIGNAL_FLAGS(x)			SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_flags]))
#define SCM_GSIGNAL_PARAMS(x)			SCM_PACK ((SCM_STRUCT_DATA (x) [scm_si_gsignal_params]))

#define SCM_VALIDATE_GSIGNAL(pos, value) \
  do { \
    SCM_VALIDATE_STRUCT (pos, value); \
    SCM_ASSERT (SCM_EQ_P (scm_struct_vtable (value), scm_gsignal_vtable) && \
                SCM_INUMP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_id])) && \
                SCM_SYMBOLP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_name])) && \
                SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_interface_type])) && \
                SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_return_type])) && \
                SCM_VECTORP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_si_gsignal_params])) && \
                TRUE, \
                value, pos, FUNC_NAME); \
  } while (0)

SCM scm_gtype_get_signals (SCM type);
SCM scm_gsignal_primitive_create (SCM signal, SCM closure);
SCM scm_gsignal_primitive_handler_block (SCM instance, SCM handler_id);
SCM scm_gsignal_primitive_handler_unblock (SCM instance, SCM handler_id);
SCM scm_gsignal_primitive_handler_disconnect (SCM instance, SCM handler_id);
SCM scm_gsignal_primitive_handler_connected_p (SCM instance, SCM handler_id);

G_END_DECLS

#endif
