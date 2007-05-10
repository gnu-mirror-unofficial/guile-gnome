/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *		 2004 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include <signal.h>
#include <string.h>

#include "glib-support.h"
#include <g-wrap/guile-wct.h>

#define GRUNTIME_ERROR(format, func_name, args...) \
  scm_error (scm_str2symbol ("gruntime-error"), func_name, format, \
             ##args, SCM_EOL)

static SCM iochannel_type = SCM_BOOL_F;

static gboolean caught_intr = FALSE;
static GMainContext *wakeup_context = NULL;

typedef struct
{
    GSource source;
    GMainLoop *loop;
    struct sigaction prev_sigaction;
    int signum;
} SignalSource;

static gboolean
signal_source_prepare (GSource * source, gint * timeout)
{
    SignalSource *ssrc = (SignalSource *) source;

    *timeout = -1;
    return ssrc->signum != 0;
}

static gboolean
signal_source_check (GSource * source)
{
    SignalSource *ssrc = (SignalSource *) source;

    if (caught_intr)
        g_main_loop_quit (ssrc->loop);

    return FALSE;
}

static gboolean
signal_source_dispatch (GSource * source, GSourceFunc callback,
                        gpointer user_data)
{
    g_assert_not_reached ();
}

static void
signal_source_finalize (GSource * source)
{
    SignalSource *ssrc = (SignalSource *) source;

    sigaction (SIGINT, &ssrc->prev_sigaction, NULL);
    caught_intr = FALSE;

    g_main_loop_unref (ssrc->loop);
    ssrc->loop = NULL;
}

static GSourceFuncs signal_source_funcs = {
    signal_source_prepare,
    signal_source_check,
    signal_source_dispatch,
    signal_source_finalize
};

static void
sigint_handler (int signum)
{
    caught_intr = TRUE;
    g_main_context_wakeup (wakeup_context);
}

static SignalSource*
signal_source_new (GMainLoop *loop)
{
    SignalSource *source;
    struct sigaction action;
    GMainContext *ctx, *prev_ctx;

    g_return_val_if_fail (loop != NULL, NULL);

    source = (SignalSource *) g_source_new (&signal_source_funcs,
                                            sizeof (SignalSource));
    g_main_loop_ref (loop);
    source->loop = loop;

    memset (&action, 0, sizeof (action));
    memset (&source->prev_sigaction, 0, sizeof (source->prev_sigaction));
    action.sa_handler = sigint_handler;
    sigaction (SIGINT, &action, &source->prev_sigaction);

    /* not fully threadsafe :/ */
    prev_ctx = wakeup_context;
    ctx = g_main_loop_get_context (loop);
    g_main_context_ref (ctx);
    wakeup_context = ctx;
    if (prev_ctx)
        g_main_context_unref (prev_ctx);

    /* context acquires a ref on the source */
    g_source_attach ((GSource *) source, ctx);
    g_source_unref ((GSource *) source);

    return source;
}

void
scm_init_glib (void)
{
    /* noop */
}

void
_wrap_g_main_loop_run (GMainLoop *loop)
{
    GSource *source = NULL;

    scm_dynwind_begin (0);

    caught_intr = FALSE;
    source = (GSource*)signal_source_new (loop);
    scm_dynwind_unwind_handler ((void*)(void*)g_source_destroy, source,
                                SCM_F_WIND_EXPLICITLY);

    g_main_loop_run (loop);
    
    if (caught_intr)
        scm_error (scm_from_locale_symbol ("signal"),
                   "g-main-loop-run", NULL, SCM_BOOL_F,
                   scm_list_1 (scm_from_int (SIGINT)));
    
    scm_dynwind_end();
}


SCM
_wrap_g_string_get_str (GString *str)
{
    return scm_mem2string (str->str, str->len);
}

static gboolean
g_io_func (GIOChannel *source,
	   GIOCondition condition,
	   gpointer data)
{
    SCM proc;
    SCM result;

    proc = GPOINTER_TO_SCM (data);
    result = scm_call_2 (proc,
                         gw_wcp_assimilate_ptr (source, iochannel_type),
                         scm_long2num (condition));
    return result == SCM_BOOL_T;
}

guint
_wrap_g_io_add_watch (GIOChannel *channel,
		      GIOCondition condition,
		      SCM func)
#define FUNC_NAME "g-io-add-watch"
{
    if (SCM_FALSEP (iochannel_type))
        iochannel_type = scm_permanent_object
            (SCM_VARIABLE_REF (scm_c_module_lookup (scm_c_resolve_module ("gnome glib"),
                                                    "<gio-channel*>")));

    SCM_VALIDATE_PROC (3, func);
    return g_io_add_watch (channel,
                           condition,
                           ((GIOFunc) (g_io_func)),
                           SCM_TO_GPOINTER (func));
}
#undef FUNC_NAME
