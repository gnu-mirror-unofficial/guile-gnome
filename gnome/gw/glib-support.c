/* guile-gnome
 * Copyright (C) 2003,2004,2007 Andy Wingo <wingo at pobox dot com>
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

size_t
_wrap_g_bookmark_file_free (void *wcp) 
{
    g_bookmark_file_free (wcp);
    return 0;
}

gboolean
_wrap_g_bookmark_file_load_from_data (GBookmarkFile *bookmark, const char *data,
                                      GError **error)
{
    return g_bookmark_file_load_from_data (bookmark, data, strlen (data),
                                           error);
}

char*
_wrap_g_bookmark_file_to_data (GBookmarkFile *bookmark, GError **error)
{
    return g_bookmark_file_to_data (bookmark, NULL, error);
}

SCM
_wrap_g_bookmark_file_get_uris (GBookmarkFile *bookmark)
{
    SCM ret = SCM_EOL;
    char **strs, *str;
    gint i = 0;
    
    strs = g_bookmark_file_get_uris (bookmark, NULL);
    for (str = strs[i]; str; i++)
        ret = scm_cons (scm_from_locale_string (str), ret);
    g_strfreev (strs);

    return scm_reverse_x (ret, SCM_EOL);
}

SCM
_wrap_g_bookmark_file_get_groups (GBookmarkFile *bookmark, const char *uri,
                                  GError **error)
{
    SCM ret = SCM_EOL;
    char **strs, *str;
    gint i = 0;
    
    strs = g_bookmark_file_get_groups (bookmark, uri, NULL, error);
    if (strs) {
        for (str = strs[i]; str; i++)
            ret = scm_cons (scm_from_locale_string (str), ret);
        g_strfreev (strs);
    }

    return scm_reverse_x (ret, SCM_EOL);
}

SCM
_wrap_g_bookmark_file_get_applications (GBookmarkFile *bookmark,
                                        const char *uri,
                                        GError **error)
{
    SCM ret = SCM_EOL;
    char **strs, *str;
    gint i = 0;
    
    strs = g_bookmark_file_get_applications (bookmark, uri, NULL, error);
    if (strs) {
        for (str = strs[i]; str; i++)
            ret = scm_cons (scm_from_locale_string (str), ret);
        g_strfreev (strs);
    }

    return scm_reverse_x (ret, SCM_EOL);
}

gchar*
_wrap_g_convert (const gchar* str, const gchar* to_codeset,
                 const gchar* from_codeset, GError** error)
{
    return g_convert (str, -1, to_codeset, from_codeset, NULL, NULL, error);
}

gchar*
_wrap_g_convert_with_fallback (const gchar* str, const gchar* to_codeset,
                               const gchar* from_codeset, gchar* fallback,
                               GError** error)
{
    return g_convert_with_fallback (str, -1, to_codeset, from_codeset,
                                    fallback, NULL, NULL, error);
}

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
                                                    "<g-iochannel*>")));

    SCM_VALIDATE_PROC (3, func);
    return g_io_add_watch (channel,
                           condition,
                           ((GIOFunc) (g_io_func)),
                           SCM_TO_GPOINTER (func));
}
#undef FUNC_NAME

gunichar
_wrap_g_utf8_get_char (const gchar *p) 
{
    return g_utf8_get_char_validated (p, -1);
}

const char*
_wrap_g_utf8_find_next_char (const gchar *p) 
{
    return g_utf8_find_next_char (p, NULL);
}

long _wrap_g_utf8_strlen (const gchar *p) 
{
    return g_utf8_strlen (p, -1);
}

const char*
_wrap_g_utf8_strchr (const gchar *p, gunichar c) 
{
    return g_utf8_strchr (p, -1, c);
}

const char*
_wrap_g_utf8_strrchr (const gchar *p, gunichar c)
{
    return g_utf8_strrchr (p, -1, c);
}

char* _wrap_g_utf8_strreverse (const gchar *p)
{
    return g_utf8_strreverse (p, -1);
}

gboolean _wrap_g_utf8_validate (const gchar *p)
{
    return g_utf8_validate (p, -1, NULL);
}

char* _wrap_g_utf8_strup (const gchar *p)
{
    return g_utf8_strup (p, -1);
}

char* _wrap_g_utf8_strdown (const gchar *p)
{
    return g_utf8_strdown (p, -1);
}

char* _wrap_g_utf8_casefold (const gchar *p)
{
    return g_utf8_casefold (p, -1);
}

char* _wrap_g_utf8_normalize (const gchar *p, GNormalizeMode mode)
{
    return g_utf8_normalize (p, -1, mode);
}

char* _wrap_g_utf8_collate_key (const gchar *p)
{
    return g_utf8_collate_key (p, -1);
}

char* _wrap_g_utf8_collate_key_for_filename (const gchar *p)
{
    return g_utf8_collate_key_for_filename (p, -1);
}

char* _wrap_g_unichar_to_utf8 (gunichar c)
{
    char *ret;
    int n;
    
    ret = g_malloc(8);
    n = g_unichar_to_utf8 (c, ret);
    ret[n] = '\0';
    return ret;
}

gunichar2*
_wrap_g_utf8_to_utf16 (const gchar *str, GError **error)
{
    return g_utf8_to_utf16 (str, -1, NULL, NULL, error);
}

gunichar*
_wrap_g_utf8_to_ucs4 (const gchar *str, GError **error)
{
    return g_utf8_to_ucs4 (str, -1, NULL, NULL, error);
}

gunichar*
_wrap_g_utf16_to_ucs4 (const gunichar2* str, GError **error) 
{
    return g_utf16_to_ucs4 (str, -1, NULL, NULL, error);
}

gchar*
_wrap_g_utf16_to_utf8 (const gunichar2* str, GError **error)
{
    return g_utf16_to_utf8 (str, -1, NULL, NULL, error);
}

gunichar2*
_wrap_g_ucs4_to_utf16 (const gunichar* str, GError **error)
{
    return g_ucs4_to_utf16 (str, -1, NULL, NULL, error);
}

gchar*
_wrap_g_ucs4_to_utf8 (const gunichar* str, GError **error) 
{
    return g_ucs4_to_utf8 (str, -1, NULL, NULL, error);
}
