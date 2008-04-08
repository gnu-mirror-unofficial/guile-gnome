/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * gobject.h: Support for GObject and GInterface
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

#ifndef __GUILE_GNOME_GOBJECT_OBJECT_H__
#define __GUILE_GNOME_GOBJECT_OBJECT_H__

#include <guile-gnome-gobject/gtype.h>
#include <guile-gnome-gobject/gparameter.h>
#include <guile-gnome-gobject/gvalue.h>
#include <guile-gnome-gobject/gsignal.h>
#include <guile-gnome-gobject/gutil.h>

G_BEGIN_DECLS




#define SCM_GOBJECT_CLASSP(scm) \
  scm_c_gtype_class_is_a_p (scm, G_TYPE_OBJECT)

#define SCM_VALIDATE_GOBJECT_CLASS(pos, scm) \
  SCM_MAKE_VALIDATE (pos, scm, GOBJECT_CLASSP)

#define SCM_VALIDATE_GOBJECT_CLASS_COPY(pos, scm, cvar) \
  do { \
    SCM_VALIDATE_GOBJECT_CLASS (pos, scm); \
    SCM_VALIDATE_GTYPE_CLASS_COPY (pos, scm, cvar); \
  } while (0)

#define SCM_GOBJECTP(scm) \
  scm_c_gtype_instance_is_a_p (scm, G_TYPE_OBJECT)

#define SCM_VALIDATE_GOBJECT(pos, scm) \
  SCM_MAKE_VALIDATE (pos, scm, GOBJECTP)

#define SCM_VALIDATE_GOBJECT_COPY(pos, scm, cvar) \
  SCM_VALIDATE_GTYPE_INSTANCE_TYPE_COPY (pos, scm, G_TYPE_OBJECT, cvar)




void scm_register_gobject_postmakefunc (GType type, gpointer (*postmakefunc) (gpointer));



G_END_DECLS

#endif
