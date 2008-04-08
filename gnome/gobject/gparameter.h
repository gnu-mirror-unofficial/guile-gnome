/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * gparameter.h: Support for GParamSpec
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

#ifndef __GUILE_GNOME_GOBJECT_GPARAMETER_H__
#define __GUILE_GNOME_GOBJECT_GPARAMETER_H__

#include <guile-gnome-gobject/gtype.h>

G_BEGIN_DECLS

extern SCM scm_class_gparam;

#define SCM_GPARAMP(scm) \
  scm_c_gtype_instance_is_a_p (scm, G_TYPE_PARAM)

#define SCM_VALIDATE_GPARAM(pos, scm) \
  SCM_MAKE_VALIDATE (pos, scm, GPARAMP)

#define SCM_VALIDATE_GPARAM_COPY(pos, scm, cvar) \
  SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (pos, scm, G_TYPE_PARAM, cvar)

G_END_DECLS

#endif /* __GUILE_GNOME_GOBJECT_PARAMETERS_H__ */
