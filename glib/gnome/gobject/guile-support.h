/* guile-gnome
 * Copyright (C) 2003,2004,2011 Andy Wingo <wingo at pobox dot com>
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

/* We have Guile >= 1.8.0, so we certainly have Guile 1.7.x.  */
#define SCM_VERSION_17X 1

char* scm_to_locale_string_dynwind (SCM s);
char* scm_to_utf8_stringn_dynwind (SCM s, size_t *lenp);
char* scm_symbol_chars (SCM s);
char* scm_symbol_chars_dynwind (SCM s);
char* scm_keyword_chars (SCM s);
char* scm_keyword_chars_dynwind (SCM s);
void scm_dynwind_guile_v__p_p (void* (*dynwind)(void*(*)(void*), void*), void *func,
                               void *arg1, void *arg2);
void scm_dynwind_guile_v__p_p_p_p_p (void* (*dynwind)(void*(*)(void*), void*), void *func,
                                     void *arg1, void *arg2, void *arg3,
                                     void *arg4, void *arg5);
void scm_dynwind_guile_v__p_u_p_p (void* (*dynwind)(void*(*)(void*), void*), void *func,
                                   void *arg1, unsigned int arg2, void *arg3,
                                   void *arg4);
void scm_dynwind_guile_v__p_u_c_p (void* (*dynwind)(void*(*)(void*), void*), void *func,
                                   void *arg1, unsigned int arg2, const void *arg3,
                                   void *arg4);

G_END_DECLS

#endif /* __GUILE_SUPPORT_H__ */
