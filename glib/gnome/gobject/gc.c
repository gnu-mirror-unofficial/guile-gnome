/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2006 Andy Wingo <wingo at pobox dot com>
 *
 * gc.c: A sweep-safe replacement for scm_gc_protect_object
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


#include "guile-support.h"
#include "gc.h"
#include "gutil.h"


static GMutex *glib_gc_marker_hash_lock = NULL;

static GHashTable *glib_gc_marker_hash = NULL;

static SCM scm_sys_glib_gc_marker;
static scm_t_bits scm_tc16_glib_gc_marker;


/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif


gpointer
scm_glib_gc_protect_object (SCM obj)
{
    gpointer key = SCM_TO_GPOINTER (obj);
    gpointer val;
    
    g_mutex_lock (glib_gc_marker_hash_lock);
    val = g_hash_table_lookup (glib_gc_marker_hash, key);
    g_hash_table_insert (glib_gc_marker_hash, key,
                         GINT_TO_POINTER (GPOINTER_TO_INT (val)+1));
    g_mutex_unlock (glib_gc_marker_hash_lock);

    return key;
}

void
scm_glib_gc_unprotect_object (gpointer key)
{
    gpointer val;
    
    g_mutex_lock (glib_gc_marker_hash_lock);
    val = g_hash_table_lookup (glib_gc_marker_hash, key);
    /* FIXME: is this right? */
    if (val)
        g_hash_table_insert (glib_gc_marker_hash, key,
                             GINT_TO_POINTER (GPOINTER_TO_INT (val)-1));
    else
        g_hash_table_remove (glib_gc_marker_hash, key);
    g_mutex_unlock (glib_gc_marker_hash_lock);
}

static void
mark (gpointer key, gpointer val, gpointer user_data)
{
    scm_gc_mark (GPOINTER_TO_SCM (key));
}

static SCM
glib_gc_marker_mark (SCM smob)
{
    g_mutex_lock (glib_gc_marker_hash_lock);
    g_hash_table_foreach (glib_gc_marker_hash, mark, NULL);
    g_mutex_unlock (glib_gc_marker_hash_lock);

    return SCM_BOOL_F;
}

static int
glib_gc_marker_print (SCM smob, SCM port, scm_print_state *print_state)
{
    scm_puts ("#<%glib-gc-marker>", port);
    return 1;
}

void
scm_init_gnome_gobject_gc (void)
{
    scm_tc16_glib_gc_marker = scm_make_smob_type ("%glib-gc-marker", 0);
    scm_set_smob_mark (scm_tc16_glib_gc_marker, glib_gc_marker_mark);
    scm_set_smob_print (scm_tc16_glib_gc_marker, glib_gc_marker_print);

    if (!g_thread_supported ())
        g_thread_init (NULL);

    glib_gc_marker_hash = g_hash_table_new (NULL, NULL);
    glib_gc_marker_hash_lock = g_mutex_new ();

    SCM_NEWSMOB (scm_sys_glib_gc_marker, scm_tc16_glib_gc_marker, NULL);
    scm_permanent_object (scm_sys_glib_gc_marker);
}
