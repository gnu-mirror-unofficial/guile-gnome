/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * guile-support.h: Support routines for old Guile versions
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

#ifndef __GUILE_SUPPORT_H__
#define __GUILE_SUPPORT_H__

#include <glib/gmacros.h>

#include <libguile.h>

G_BEGIN_DECLS

/* Define this macro if Guile 1.7.x or better is in use. */
#if defined (SCM_MINOR_VERSION) && (SCM_MINOR_VERSION >= 7) && \
    defined (SCM_MAJOR_VERSION) && (SCM_MAJOR_VERSION >= 1)
#define SCM_VERSION_17X 1
#endif

/* Support for coding against Guile 1.7 */
#ifndef SCM_VERSION_17X

#define scm_gc_malloc(size, what) scm_must_malloc((size), (what))
#define scm_gc_free(mem, size, what) \
  do{ scm_must_free (mem); scm_done_free (size); } while (0)

#define SCM_VECTOR_SET(x, idx, val) (SCM_VELTS(x)[(idx)] = (val))
#define SCM_VECTOR_REF(x, idx) (SCM_VELTS(x)[(idx)])

#define scm_gc_register_collectable_memory(mem, size, what)
#define scm_gc_unregister_collectable_memory(mem, size, what)

SCM scm_str2string (const char *src);

#endif

G_END_DECLS

#endif /* __GUILE_SUPPORT_H__ */
