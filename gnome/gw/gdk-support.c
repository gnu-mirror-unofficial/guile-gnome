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

SCM
gdk_event_to_vector (GdkEvent *event)
{
    switch (event->type) {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
        {
            GdkEventKey ekey = event->key;
            SCM ret;

            /* 8 fields (we ignore key.string and key.length) */
            ret = scm_c_make_vector (8, SCM_BOOL_F);
            
            scm_vector_set_x (ret, SCM_MAKINUM (0), SCM_MAKINUM (event->type));
            scm_vector_set_x (ret, SCM_MAKINUM (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ekey.window));
            scm_vector_set_x (ret, SCM_MAKINUM (2), ekey.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_vector_set_x (ret, SCM_MAKINUM (3),
                              ekey.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (ekey.time) : SCM_MAKINUM (ekey.time));
            scm_vector_set_x (ret, SCM_MAKINUM (4), SCM_MAKINUM ((int)ekey.state));
            scm_vector_set_x (ret, SCM_MAKINUM (5), SCM_MAKINUM (ekey.keyval));
            scm_vector_set_x (ret, SCM_MAKINUM (6), SCM_MAKINUM (ekey.hardware_keycode));
            scm_vector_set_x (ret, SCM_MAKINUM (7), SCM_MAKINUM (ekey.group));
            return ret;
        }
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
        {
            GdkEventButton ebutton = event->button;
            SCM ret;

            /* 11 fields (we ignore axes) */
            ret = scm_c_make_vector (11, SCM_BOOL_F);
            
            scm_vector_set_x (ret, SCM_MAKINUM (0), SCM_MAKINUM (event->type));
            scm_vector_set_x (ret, SCM_MAKINUM (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ebutton.window));
            scm_vector_set_x (ret, SCM_MAKINUM (2), ebutton.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_vector_set_x (ret, SCM_MAKINUM (3),
                              ebutton.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (ebutton.time) : SCM_MAKINUM (ebutton.time));
            scm_vector_set_x (ret, SCM_MAKINUM (4), scm_double2num (ebutton.x));
            scm_vector_set_x (ret, SCM_MAKINUM (5), scm_double2num (ebutton.y));
            scm_vector_set_x (ret, SCM_MAKINUM (6), SCM_MAKINUM ((int)ebutton.state));
            scm_vector_set_x (ret, SCM_MAKINUM (7), SCM_MAKINUM ((int)ebutton.button));
            scm_vector_set_x (ret, SCM_MAKINUM (8), 
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ebutton.device));
            scm_vector_set_x (ret, SCM_MAKINUM (9), scm_double2num (ebutton.x_root));
            scm_vector_set_x (ret, SCM_MAKINUM (10), scm_double2num (ebutton.y_root));
            return ret;
        }
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
        {
            GdkEventCrossing ecrossing = event->crossing;
            SCM ret;

            /* 13 fields */
            ret = scm_c_make_vector (13, SCM_BOOL_F);

            scm_vector_set_x (ret, SCM_MAKINUM (0), SCM_MAKINUM (event->type));
            scm_vector_set_x (ret, SCM_MAKINUM (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)ecrossing.window));
            scm_vector_set_x (ret, SCM_MAKINUM (2), ecrossing.send_event ? SCM_BOOL_T : SCM_BOOL_F);
	    /* subwindow may be NULL. --jcn */
	    if (ecrossing.subwindow)
	      scm_vector_set_x (ret, SCM_MAKINUM (3),
				scm_c_gtype_instance_to_scm ((GTypeInstance*)ecrossing.subwindow));
	    else
	      scm_vector_set_x (ret, SCM_MAKINUM (3), SCM_BOOL_F);
	      
            scm_vector_set_x (ret, SCM_MAKINUM (4),
                              ecrossing.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (ecrossing.time) : SCM_MAKINUM (ecrossing.time));
            scm_vector_set_x (ret, SCM_MAKINUM (5), scm_double2num (ecrossing.x));
            scm_vector_set_x (ret, SCM_MAKINUM (6), scm_double2num (ecrossing.y));
            scm_vector_set_x (ret, SCM_MAKINUM (7), scm_double2num (ecrossing.x_root));
            scm_vector_set_x (ret, SCM_MAKINUM (8), scm_double2num (ecrossing.y_root));
            scm_vector_set_x (ret, SCM_MAKINUM (9), SCM_MAKINUM ((int)ecrossing.mode));
            scm_vector_set_x (ret, SCM_MAKINUM (10), SCM_MAKINUM ((int)ecrossing.detail));
            scm_vector_set_x (ret, SCM_MAKINUM (11), SCM_BOOL ((int)ecrossing.focus));
            scm_vector_set_x (ret, SCM_MAKINUM (12), SCM_MAKINUM ((int)ecrossing.state));
            return ret;
        }
    case GDK_SELECTION_CLEAR:
    case GDK_SELECTION_NOTIFY:
    case GDK_SELECTION_REQUEST:
        {
            GdkEventSelection eselection = event->selection;
            SCM ret;

            /* 8 fields */
            ret = scm_c_make_vector (8, SCM_BOOL_F);
            
            scm_vector_set_x (ret, SCM_MAKINUM (0), SCM_MAKINUM (event->type));
            scm_vector_set_x (ret, SCM_MAKINUM (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)eselection.window));
            scm_vector_set_x (ret, SCM_MAKINUM (2), eselection.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_vector_set_x (ret, SCM_MAKINUM (3), scm_take0str (gdk_atom_name (eselection.selection)));
            scm_vector_set_x (ret, SCM_MAKINUM (4), scm_take0str (gdk_atom_name (eselection.target)));
            scm_vector_set_x (ret, SCM_MAKINUM (5), scm_take0str (gdk_atom_name (eselection.property)));
            scm_vector_set_x (ret, SCM_MAKINUM (6),
                              eselection.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (eselection.time) : SCM_MAKINUM (eselection.time));
            scm_vector_set_x (ret, SCM_MAKINUM (7), SCM_MAKINUM (eselection.requestor));

            return ret;
        }
    case GDK_MOTION_NOTIFY:
        {
            GdkEventMotion emotion = event->motion;
            SCM ret;

            /* 11 fields (we ignore axes) */
            ret = scm_c_make_vector (11, SCM_BOOL_F);
            
            scm_vector_set_x (ret, SCM_MAKINUM (0), SCM_MAKINUM (event->type));
            scm_vector_set_x (ret, SCM_MAKINUM (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)emotion.window));
            scm_vector_set_x (ret, SCM_MAKINUM (2), emotion.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_vector_set_x (ret, SCM_MAKINUM (3),
                              emotion.time > SCM_MOST_POSITIVE_FIXNUM
                              ? scm_i_ulong2big (emotion.time) : SCM_MAKINUM (emotion.time));
            scm_vector_set_x (ret, SCM_MAKINUM (4), scm_double2num (emotion.x));
            scm_vector_set_x (ret, SCM_MAKINUM (5), scm_double2num (emotion.y));
            scm_vector_set_x (ret, SCM_MAKINUM (6), SCM_MAKINUM ((int)emotion.state));
            scm_vector_set_x (ret, SCM_MAKINUM (7), emotion.is_hint ? SCM_BOOL_T : SCM_BOOL_F);
            scm_vector_set_x (ret, SCM_MAKINUM (8), 
                              scm_c_gtype_instance_to_scm ((GTypeInstance*)emotion.device));
            scm_vector_set_x (ret, SCM_MAKINUM (9), scm_double2num (emotion.x_root));
            scm_vector_set_x (ret, SCM_MAKINUM (10), scm_double2num (emotion.y_root));
            return ret;
        }
    case GDK_WINDOW_STATE:
        {
            GdkEventWindowState ewinstate = event->window_state;
            SCM ret;

            /* 5 fields */
            ret = scm_c_make_vector (5, SCM_BOOL_F);

            scm_vector_set_x (ret, SCM_MAKINUM (0), SCM_MAKINUM (event->type));
            scm_vector_set_x (ret, SCM_MAKINUM (1),
                              scm_c_gtype_instance_to_scm ((GTypeInstance *)ewinstate.window));
            scm_vector_set_x (ret, SCM_MAKINUM (2),
                              ewinstate.send_event ? SCM_BOOL_T : SCM_BOOL_F);
            scm_vector_set_x (ret, SCM_MAKINUM (3),
                              SCM_MAKINUM ((int)ewinstate.changed_mask));
            scm_vector_set_x (ret, SCM_MAKINUM (4),
                              SCM_MAKINUM ((int)ewinstate.new_window_state));
            
            return ret;
        }
    default:
        g_print ("Conversions for events of type %d are not implemented.\n"
                 "How about doing it yourself?\n", event->type);
        return SCM_BOOL_F;
    }
}

