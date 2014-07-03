/* guile-gnome
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * glib-support.h: Support for the GLib binding
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

#if !defined(_GUILE_GOBJECT_GLIB_SUPPORT_H)
#define _GUILE_GOBJECT_GLIB_SUPPORT_H

#include <glib.h>
#include <libguile.h>
#include "guile-gnome-gobject.h"

G_BEGIN_DECLS

void scm_init_glib (void);
size_t _wrap_g_bookmark_file_free (void *wcp);
gboolean _wrap_g_bookmark_file_load_from_data (GBookmarkFile *bookmark,
                                               const char *data,
                                               GError **error);
char* _wrap_g_bookmark_file_to_data (GBookmarkFile *bookmark, GError **error);
SCM _wrap_g_bookmark_file_get_uris (GBookmarkFile *bookmark);
SCM _wrap_g_bookmark_file_get_groups (GBookmarkFile *bookmark, const char *uri,
                                      GError **error);
SCM _wrap_g_bookmark_file_get_applications (GBookmarkFile *bookmark,
                                            const char *uri,
                                            GError **error);
gchar* _wrap_g_convert (const gchar* str, const gchar* to_codeset,
                        const gchar* from_codeset, GError** error);
gchar* _wrap_g_convert_with_fallback (const gchar* str, const gchar* to_codeset,
                                      const gchar* from_codeset, gchar* fallback,
                                      GError** error);
void _wrap_g_main_loop_run (GMainLoop *loop);
SCM  _wrap_g_string_get_str (GString *str);
guint _wrap_g_io_add_watch (GIOChannel *channel, GIOCondition condition, SCM func);
GIOStatus _wrap_g_io_channel_read_line (GIOChannel *channel, gchar **str_return,
                                        GError **error);
gunichar _wrap_g_utf8_get_char (const gchar *p);
const char* _wrap_g_utf8_find_next_char (const gchar *p);
long _wrap_g_utf8_strlen (const gchar *p);
const char* _wrap_g_utf8_strchr (const gchar *p, gunichar c);
const char* _wrap_g_utf8_strrchr (const gchar *p, gunichar c);
char* _wrap_g_utf8_strreverse (const gchar *p);
gboolean _wrap_g_utf8_validate (const gchar *p);
char* _wrap_g_utf8_strup (const gchar *p);
char* _wrap_g_utf8_strdown (const gchar *p);
char* _wrap_g_utf8_casefold (const gchar *p);
char* _wrap_g_utf8_normalize (const gchar *p, GNormalizeMode mode);
char* _wrap_g_utf8_collate_key (const gchar *p);
char* _wrap_g_utf8_collate_key_for_filename (const gchar *p);
char* _wrap_g_unichar_to_utf8 (gunichar c);
gunichar2* _wrap_g_utf8_to_utf16 (const gchar *str, GError **error);
gunichar* _wrap_g_utf8_to_ucs4 (const gchar *str, GError **error);
gunichar* _wrap_g_utf16_to_ucs4 (const gunichar2 *str, GError **error);
gchar* _wrap_g_utf16_to_utf8 (const gunichar2 *str, GError **error);
gunichar2* _wrap_g_ucs4_to_utf16 (const gunichar *str, GError **error);
gchar* _wrap_g_ucs4_to_utf8 (const gunichar *str, GError **error);

G_END_DECLS

#endif
