/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * gdk-support.c: Support routines for the GDK wrapper
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

#include "gdk-support.h"

#define SLOT(n) (n+scm_vtable_offset_user)
#define MAKSLOT(n) (SCM_MAKINUM (SLOT (n)))

SCM
gdk_event_to_event_struct (GdkEvent *event)
{
    switch (event->type) {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
        {
            GdkEventKey ekey = event->key;
            SCM layout, ret;
            char *fields = "pwpwpwpwpwpwpwpw"; /* 8 fields (we ignore key.string
                                                * and key.length) */

            /* not so efficient, but this can be fixed later */
            layout = scm_make_vtable_vtable (scm_makfrom0str (fields), SCM_INUM0, SCM_EOL);
            ret = scm_make_struct (layout, SCM_INUM0, SCM_EOL);
            
            /* can be make more efficient with macros */
            scm_struct_set_x (ret, MAKSLOT (0), SCM_MAKINUM (event->type));
            scm_struct_set_x (ret, MAKSLOT (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ekey.window));
            scm_struct_set_x (ret, MAKSLOT (2), ekey.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_struct_set_x (ret, MAKSLOT (3),
                              ekey.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (ekey.time) : SCM_MAKINUM (ekey.time));
            scm_struct_set_x (ret, MAKSLOT (4), SCM_MAKINUM ((int)ekey.state));
            scm_struct_set_x (ret, MAKSLOT (5), SCM_MAKINUM (ekey.keyval));
            scm_struct_set_x (ret, MAKSLOT (6), SCM_MAKINUM (ekey.hardware_keycode));
            scm_struct_set_x (ret, MAKSLOT (7), SCM_MAKINUM (ekey.group));
            return ret;
        }
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
        {
            GdkEventButton ebutton = event->button;
            SCM layout, ret;
            char *fields = "pwpwpwpwpwpwpwpwpwpwpw"; /* 11 fields (we ignore axes) */

            /* not so efficient, but this can be fixed later */
            layout = scm_make_vtable_vtable (scm_makfrom0str (fields), SCM_INUM0, SCM_EOL);
            ret = scm_make_struct (layout, SCM_INUM0, SCM_EOL);
            
            /* can be make more efficient with macros */
            scm_struct_set_x (ret, MAKSLOT (0), SCM_MAKINUM (event->type));
            scm_struct_set_x (ret, MAKSLOT (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ebutton.window));
            scm_struct_set_x (ret, MAKSLOT (2), ebutton.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_struct_set_x (ret, MAKSLOT (3),
                              ebutton.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (ebutton.time) : SCM_MAKINUM (ebutton.time));
            scm_struct_set_x (ret, MAKSLOT (4), scm_double2num (ebutton.x));
            scm_struct_set_x (ret, MAKSLOT (5), scm_double2num (ebutton.y));
            scm_struct_set_x (ret, MAKSLOT (6), SCM_MAKINUM ((int)ebutton.state));
            scm_struct_set_x (ret, MAKSLOT (7), SCM_MAKINUM ((int)ebutton.button));
            scm_struct_set_x (ret, MAKSLOT (8), 
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ebutton.device));
            scm_struct_set_x (ret, MAKSLOT (9), scm_double2num (ebutton.x_root));
            scm_struct_set_x (ret, MAKSLOT (10), scm_double2num (ebutton.y_root));
            return ret;
        }
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
        {
            GdkEventCrossing ecrossing = event->crossing;
            SCM layout, ret;
            char *fields = "pwpwpwpwpwpwpwpwpwpwpwpwpw"; /* 13 fields */

            /* not so efficient, but this can be fixed later */
            layout = scm_make_vtable_vtable (scm_makfrom0str (fields), SCM_INUM0, SCM_EOL);
            ret = scm_make_struct (layout, SCM_INUM0, SCM_EOL);
            
            /* can be make more efficient with macros */
            scm_struct_set_x (ret, MAKSLOT (0), SCM_MAKINUM (event->type));
            scm_struct_set_x (ret, MAKSLOT (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ecrossing.window));
            scm_struct_set_x (ret, MAKSLOT (2), ecrossing.send_event ? SCM_BOOL_T : SCM_BOOL_F);
	    /* subwindow may be NULL. --jcn */
	    if (ecrossing.subwindow)
	      scm_struct_set_x (ret, MAKSLOT (3),
				scm_c_gtype_instance_to_scm ((GTypeInstance*)ecrossing.subwindow));
	    else
	      scm_struct_set_x (ret, MAKSLOT (3), SCM_BOOL_F);
	      
            scm_struct_set_x (ret, MAKSLOT (4),
                              ecrossing.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (ecrossing.time) : SCM_MAKINUM (ecrossing.time));
            scm_struct_set_x (ret, MAKSLOT (5), scm_double2num (ecrossing.x));
            scm_struct_set_x (ret, MAKSLOT (6), scm_double2num (ecrossing.y));
            scm_struct_set_x (ret, MAKSLOT (7), scm_double2num (ecrossing.x_root));
            scm_struct_set_x (ret, MAKSLOT (8), scm_double2num (ecrossing.y_root));
            scm_struct_set_x (ret, MAKSLOT (9), SCM_MAKINUM ((int)ecrossing.mode));
            scm_struct_set_x (ret, MAKSLOT (10), SCM_MAKINUM ((int)ecrossing.detail));
            scm_struct_set_x (ret, MAKSLOT (11), SCM_BOOL ((int)ecrossing.focus));
            scm_struct_set_x (ret, MAKSLOT (12), SCM_MAKINUM ((int)ecrossing.state));
            return ret;
        }
    case GDK_SELECTION_CLEAR:
    case GDK_SELECTION_NOTIFY:
    case GDK_SELECTION_REQUEST:
        {
            GdkEventSelection eselection = event->selection;
            SCM layout, ret;
            char *fields = "pwpwpwpwpwpwpwpw"; /* 8 fields */

            /* not so efficient, but this can be fixed later */
            layout = scm_make_vtable_vtable (scm_makfrom0str (fields), SCM_INUM0, SCM_EOL);
            ret = scm_make_struct (layout, SCM_INUM0, SCM_EOL);
            
            /* can be make more efficient with macros */
            scm_struct_set_x (ret, MAKSLOT (0), SCM_MAKINUM (event->type));
            scm_struct_set_x (ret, MAKSLOT (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)eselection.window));
            scm_struct_set_x (ret, MAKSLOT (2), eselection.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_struct_set_x (ret, MAKSLOT (3), scm_take0str (gdk_atom_name (eselection.selection)));
            scm_struct_set_x (ret, MAKSLOT (4), scm_take0str (gdk_atom_name (eselection.target)));
            scm_struct_set_x (ret, MAKSLOT (5), scm_take0str (gdk_atom_name (eselection.property)));
            scm_struct_set_x (ret, MAKSLOT (6),
                              eselection.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (eselection.time) : SCM_MAKINUM (eselection.time));
            scm_struct_set_x (ret, MAKSLOT (7), SCM_MAKINUM (eselection.requestor));

            return ret;
        }
    default:
        g_print ("Conversions for events of type %d are not implemented.\n"
                 "How about doing it yourself?\n", event->type);
        return SCM_BOOL_F;
    }
}

