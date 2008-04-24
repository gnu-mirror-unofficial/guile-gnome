/* guile-gnome
 * Copyright (C) 2007 Andy Wingo <wingo at pobox dot com>
 *
 * pango-support.c: Support routines for the Pango wrapper
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

#include <libguile.h>
#include "guile-gnome-gobject.h"

#include "pango-support.h"

SCM
scm_pango_rectangle_to_scm (PangoRectangle *rect)
{
    SCM ret = scm_c_make_vector (4, SCM_BOOL_F);

    scm_c_vector_set_x (ret, 0, scm_int2num (rect->x));
    scm_c_vector_set_x (ret, 1, scm_int2num (rect->y));
    scm_c_vector_set_x (ret, 2, scm_int2num (rect->width));
    scm_c_vector_set_x (ret, 3, scm_int2num (rect->height));

    return ret;
}

void
scm_scm_to_pango_rectangle (SCM scm, PangoRectangle* rect)
#define FUNC_NAME "%scm->pango-rectangle"
{
#define GET_VINT(v,i) \
    scm_num2int (scm_c_vector_ref (v, i), 0, FUNC_NAME)

    rect->x = GET_VINT (scm, 0);
    rect->y = GET_VINT (scm, 1);
    rect->width = GET_VINT (scm, 2);
    rect->height = GET_VINT (scm, 3);
}
#undef FUNC_NAME
