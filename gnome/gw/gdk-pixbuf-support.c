/* guile-gnome
 * Copyright (C) 2004 Free Software Foundation, Inc.
 *
 * gdk-pixbuf-support.c: Support routines for the gdk-pixbuf wrapper
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

#include "gdk-pixbuf-support.h"

static gboolean
port_write_cb (const gchar *buf, gsize count, GError **error,
               gpointer data) 
{
    SCM port = SCM_PACK (GPOINTER_TO_INT (data));
    scm_c_write (port, buf, count);
    return TRUE;
}

gboolean
gdk_pixbuf_save_to_port (GdkPixbuf *pixbuf, SCM port, const char *type,
                         SCM options_alist, GError **error)
#define FUNC_NAME "gdk-pixbuf-save-to-port"
{
    gboolean res;

    SCM_VALIDATE_PORT (1, port);
    /* ignoring options for now */

    res = gdk_pixbuf_save_to_callback (pixbuf, port_write_cb,
                                       GINT_TO_POINTER (SCM_UNPACK (port)),
                                       type, error, NULL);
    scm_remember_upto_here_1 (port);
    return res;
}
#undef FUNC_NAME
