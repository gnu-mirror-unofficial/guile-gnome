/* guile-gnome
 * Copyright (C) 2005 Andreas Rottmann
 *
 * gutil.c: Some GLib-related utilties
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

#include "gutil.h"

SCM
scm_c_gerror_to_scm (GError *error)
{
  return scm_list_3 (scm_from_ulong(error->domain),
                     scm_from_ulong(error->code),
                     scm_from_locale_string(error->message));
}

void
scm_c_raise_gerror (GError *error)
{
  SCM scm_gerror = scm_c_gerror_to_scm (error);
  g_error_free (error);
  scm_throw (scm_from_locale_symbol("g-error"), scm_gerror);
}
