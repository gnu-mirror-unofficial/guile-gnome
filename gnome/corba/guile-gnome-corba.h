/* guile-gnome
 * Copyright (C) 2001 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba.h:
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

#ifndef __GUILE_CORBA_H__
#define __GUILE_CORBA_H__ 1

#include <guile-gnome-corba-primitives.h>
#include <guile-gnome-corba-types.h>
#include <guile-gnome-corba-generic.h>



SCM guile_corba_typecode_to_class (SCM type);
SCM scm_corba_servant_to_reference (SCM smob_servant);
SCM scm_bonobo_get_object (SCM moniker, SCM class);
SCM scm_bonobo_object_query_interface (SCM object, SCM class);

void scm_init_gnome_corba (void);
void scm_init_gnome_corba_module (void);

#endif
