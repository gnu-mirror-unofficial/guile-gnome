/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * glib-support.c: Support routines for the GLib wrapper
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

#include "glib-support.h"

#define GRUNTIME_ERROR(format, func_name, args...) \
  scm_error (scm_str2symbol ("gruntime-error"), func_name, format, \
             ##args, SCM_EOL)

#if (SCM_MAJOR_VERSION == 1) && (SCM_MINOR_VERSION == 6)

static SCM deliver_signals;

void
scm_init_glib (void)
{
    deliver_signals = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_lookup ("%deliver-signals")));
}

static gboolean
handle_signals (gpointer unused) 
{
    scm_call_0 (deliver_signals);
    return TRUE;
}

static void
add_interrupt_handler (guint *timeout)
{
    /* process interrupts every tenth of a second */
    *timeout = g_timeout_add (100, handle_signals, NULL);
}

static SCM
run_loop (GMainLoop *loop)
{
    g_main_loop_run (loop);
    return SCM_UNSPECIFIED;
}

static void
remove_interrupt_handler (guint *timeout)
{
    g_source_remove (*timeout);
    *timeout = 0;
}

void
_wrap_g_main_loop_run (GMainLoop *loop)
{
    guint timeout;

    scm_internal_dynamic_wind ((scm_t_guard)add_interrupt_handler,
                               (scm_t_inner)run_loop,
                               (scm_t_guard)remove_interrupt_handler,
                               loop,
                               &timeout);
}
#else

void
scm_init_glib (void)
{
}

void
_wrap_g_main_loop_run (GMainLoop *loop)
{
  g_main_loop_run (loop);
}

#endif
