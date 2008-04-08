/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * glade-support.c: Support routines for the libglade wrapper
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

#include "glade-support.h"
#include <string.h>
#include "guile-gnome-gobject.h"

#define GRUNTIME_ERROR(format, func_name, args...) \
  scm_error_scm (scm_str2symbol ("gruntime-error"), scm_makfrom0str (func_name), \
                 scm_simple_format (SCM_BOOL_F, scm_makfrom0str (format), \
                                    scm_list_n (args, SCM_UNDEFINED)), \
                 SCM_EOL, SCM_EOL)

GladeXML*
_wrap_glade_xml_new_from_buffer (const char *buffer, const char *root,
                                 const char *domain)
{
    return glade_xml_new_from_buffer (buffer, strlen (buffer), root, domain);
}

static void
connect_one (const gchar *handler_name, GObject *object, const gchar *signal_name,
             const gchar *signal_data, GObject *connect_object, gboolean after,
             gpointer user_data)
{
    static SCM gtype_instance_signal_connect_data = SCM_BOOL_F;
    SCM proc;

    if (SCM_FALSEP (gtype_instance_signal_connect_data))
        gtype_instance_signal_connect_data =
            SCM_VARIABLE_REF (scm_c_module_lookup (scm_glade_module,
                                                   "gtype-instance-signal-connect-data"));

    proc = GPOINTER_TO_SCM (user_data);
    scm_call_4 (gtype_instance_signal_connect_data,
                scm_c_gtype_instance_to_scm (object),
                scm_str2symbol (signal_name),
                proc,
                after ? SCM_BOOL_T : SCM_BOOL_F);
}

void
_wrap_glade_xml_signal_connect (GladeXML *xml, const char *handlername, SCM proc)
#define FUNC_NAME "glade-xml-signal-connect"
{
    SCM_VALIDATE_PROC (3, proc);
    glade_xml_signal_connect_full (xml, handlername, connect_one,
                                   SCM_TO_GPOINTER (proc));
}
#undef FUNC_NAME

SCM handle_read_error (char *handler_name, SCM tag, SCM throw_args) 
{
    GRUNTIME_ERROR ("Error while reading signal handler ~S: ~A: ~S",
                    "glade-xml-signal-autoconnect", scm_makfrom0str (handler_name),
                    tag, throw_args);
}

static void
connect_many (const gchar *handler_name, GObject *object, const gchar *signal_name,
              const gchar *signal_data, GObject *connect_object, gboolean after,
              gpointer user_data)
{
    SCM module = GPOINTER_TO_SCM (user_data);
    SCM proc = SCM_BOOL_F;

    proc = scm_eval (scm_internal_catch (SCM_BOOL_T,
                                         (scm_t_catch_body)scm_c_read_string,
                                         (void*)handler_name,
                                         (scm_t_catch_handler)handle_read_error,
                                         (void*)handler_name),
                     module);
    if (SCM_FALSEP (scm_procedure_p (proc)))
        GRUNTIME_ERROR ("Tried to set `~A' to handle signal `~A', but it's not a procedure",
                        "glade-xml-signal-autoconnect", scm_makfrom0str (handler_name),
                        scm_makfrom0str (signal_name));

    connect_one (NULL, object, signal_name, NULL, NULL, after,
                 SCM_TO_GPOINTER (proc));
}

void
_wrap_glade_xml_signal_autoconnect (GladeXML *xml, SCM module)
{
    glade_xml_signal_autoconnect_full (xml, connect_many,
                                       SCM_TO_GPOINTER (module));
}

GtkWidget*
guile_glade_custom_handler (GladeXML *xml, gchar *func, gchar *name, gchar *string1,
                            gchar *string2, gint int1, gint int2, gpointer user_data)
#define FUNC_NAME "%guile-glade-custom-handler"
{
    SCM ret;
    GtkWidget *widget;
    
    ret = scm_c_eval_string (func);
    SCM_VALIDATE_GOBJECT (0, ret);
    widget = (GtkWidget*)scm_c_scm_to_gtype_instance_typed (ret, GTK_TYPE_WIDGET);
    gtk_widget_show (widget);
    return widget;
}
#undef FUNC_NAME
