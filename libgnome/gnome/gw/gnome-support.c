/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * gnome-support.c: Customizations for libgnome wrappers
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

#include "gnome-support.h"
#include "guile-gnome-gobject.h"

GnomeProgram*
_wrap_gnome_program_init (const gchar *app_id, const gchar *app_version)
{
    GnomeProgram *program;
    char **argv, **cwalk;
    int argc, i;
    SCM args, walk;
    
    args = scm_program_arguments ();
    argc = scm_to_int (scm_length (args));
    argv = g_new0 (char*, argc);
    walk = args;
    cwalk = argv;
    while (walk != SCM_EOL) {
        *cwalk = scm_to_locale_string (SCM_CAR (walk));
        cwalk++;
        walk = SCM_CDR (walk);
    }
    
    program = gnome_program_init (app_id, app_version, LIBGNOME_MODULE,
                                  argc, argv, NULL);

    for (i = 0; i < argc; i++)
        free (argv[i]);
    g_free (argv);
    
    return program;
}
