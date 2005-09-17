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
            scm_vector_set_x (ret, SCM_MAKINUM (3), scm_ulong2num (ekey.time));
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
            scm_vector_set_x (ret, SCM_MAKINUM (3), scm_ulong2num (ebutton.time));
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
	      
            scm_vector_set_x (ret, SCM_MAKINUM (4), scm_ulong2num (ecrossing.time));
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
            scm_vector_set_x (ret, SCM_MAKINUM (6), scm_ulong2num (eselection.time));
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
            scm_vector_set_x (ret, SCM_MAKINUM (3), scm_ulong2num (emotion.time));
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
    case GDK_EXPOSE:
        {
            GdkEventExpose expose = event->expose;
            SCM ret;

            /* 6 fields */
            ret = scm_c_make_vector (6, SCM_BOOL_F);

            scm_vector_set_x (ret, SCM_MAKINUM (0), SCM_MAKINUM (event->type));
            scm_vector_set_x (ret, SCM_MAKINUM (1),
                              scm_c_gtype_instance_to_scm (
                                  (GTypeInstance *)expose.window));
            scm_vector_set_x (ret, SCM_MAKINUM (2), SCM_BOOL (expose.send_event));
            scm_vector_set_x (ret, SCM_MAKINUM (3),
                              scm_gdk_rectangle_to_scm (&expose.area));
            scm_vector_set_x (ret, SCM_MAKINUM (4), SCM_BOOL_F); /* FIXME: region */
            scm_vector_set_x (ret, SCM_MAKINUM (5), SCM_MAKINUM (expose.count));
            return ret;
        }
    default:
        g_print ("Conversions for events of type %d are not implemented.\n"
                 "How about doing it yourself?\n", event->type);
        return SCM_BOOL_F;
    }
}

SCM
scm_gdk_rectangle_to_scm (GdkRectangle *rect)
{
    SCM ret = scm_c_make_vector (4, SCM_BOOL_F);

    scm_vector_set_x (ret, SCM_MAKINUM(0), scm_int2num (rect->x));
    scm_vector_set_x (ret, SCM_MAKINUM(1), scm_int2num (rect->y));
    scm_vector_set_x (ret, SCM_MAKINUM(2), scm_int2num (rect->width));
    scm_vector_set_x (ret, SCM_MAKINUM(3), scm_int2num (rect->height));

    return ret;
}

GdkRectangle*
scm_scm_to_gdk_rectangle (SCM scm)
#define FUNC_NAME "%scm->gdk-rectangle"
{
    GdkRectangle *ret = g_new0 (GdkRectangle, 1);
    
#define GET_VINT(v,i) \
  scm_num2int (scm_vector_ref (v, SCM_MAKINUM(i)), 0, FUNC_NAME)

    ret->x = GET_VINT (scm, 0);
    ret->y = GET_VINT (scm, 1);
    ret->width = GET_VINT (scm, 2);
    ret->height = GET_VINT (scm, 3);

    return ret;
}
#undef FUNC_NAME

SCM
scm_gdk_color_to_scm (GdkColor *c)
{
    SCM ret = scm_c_make_vector (3, SCM_BOOL_F);

    scm_vector_set_x (ret, SCM_MAKINUM(0), scm_ushort2num (c->red));
    scm_vector_set_x (ret, SCM_MAKINUM(1), scm_ushort2num (c->green));
    scm_vector_set_x (ret, SCM_MAKINUM(2), scm_ushort2num (c->blue));

    return ret;
}

GdkColor*
scm_scm_to_gdk_color (SCM scm)
#define FUNC_NAME "%scm->gdk-rectangle"
{
    GdkColor *ret = g_new0 (GdkColor, 1);
    
    if (SCM_STRINGP (scm)) {
        if (gdk_color_parse (SCM_STRING_CHARS (scm), ret))
            return ret;
        /* FIXME: give a proper error */
    }
    
#define GET_VUSHORT(v,i) \
  scm_num2ushort (scm_vector_ref (v, SCM_MAKINUM(i)), 0, FUNC_NAME)

    ret->red = GET_VUSHORT (scm, 0);
    ret->green = GET_VUSHORT (scm, 1);
    ret->blue = GET_VUSHORT (scm, 2);

    return ret;
}
#undef FUNC_NAME
