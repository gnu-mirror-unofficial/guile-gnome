/* guile-gnome
 * Copyright (C) 2004 Free Software Foundation, Inc.
 *
 * gnome-vfs-support.c: Support routines for the gnome-vfs wrapper
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

#include "gnome-vfs-port.h"
#include "gnome-vfs-support.h"
#include "guile-gnome-gobject.h"

GnomeVFSDirectoryHandle*
_wrap_gnome_vfs_directory_open (const gchar *text_uri,
                                GnomeVFSFileInfoOptions options)
{
    GnomeVFSDirectoryHandle *handle = NULL;
    GnomeVFSResult res;
    
    res = gnome_vfs_directory_open (&handle, text_uri, options);
    if (res != GNOME_VFS_OK)
        RESULT_ERROR (res);
    
    return handle;
}

GnomeVFSDirectoryHandle*
_wrap_gnome_vfs_directory_open_from_uri (GnomeVFSURI *uri,
                                         GnomeVFSFileInfoOptions options)
{
    GnomeVFSDirectoryHandle *handle = NULL;
    GnomeVFSResult res;
    
    res = gnome_vfs_directory_open_from_uri (&handle, uri, options);
    if (res != GNOME_VFS_OK)
        RESULT_ERROR (res);
    
    return handle;
}

SCM
_wrap_gnome_vfs_open (const gchar *text_uri, GnomeVFSOpenMode open_mode)
{
    GnomeVFSHandle *handle = NULL;
    GnomeVFSResult res;
    
    res = gnome_vfs_open (&handle, text_uri, open_mode);
    if (res != GNOME_VFS_OK)
        RESULT_ERROR (res);
    
    return scm_gnome_vfs_handle_to_port (handle, open_mode, text_uri);
}

SCM
_wrap_gnome_vfs_open_uri (GnomeVFSURI *uri, GnomeVFSOpenMode open_mode)
{
    GnomeVFSHandle *handle = NULL;
    GnomeVFSResult res;
    gchar *name;
    SCM ret;
    
    res = gnome_vfs_open_uri (&handle, uri, open_mode);
    if (res != GNOME_VFS_OK)
        RESULT_ERROR (res);
    
    name = gnome_vfs_uri_to_string (uri, GNOME_VFS_URI_HIDE_PASSWORD);
    ret = scm_gnome_vfs_handle_to_port (handle, open_mode, name);
    g_free (name);
    return ret;
}

SCM
_wrap_gnome_vfs_create (const gchar *text_uri, GnomeVFSOpenMode open_mode,
                        gboolean exclusive, guint perm)
{
    GnomeVFSHandle *handle = NULL;
    GnomeVFSResult res;
    
    res = gnome_vfs_create (&handle, text_uri, open_mode, exclusive, perm);
    if (res != GNOME_VFS_OK)
        RESULT_ERROR (res);
    
    return scm_gnome_vfs_handle_to_port (handle, open_mode, text_uri);
}

SCM
_wrap_gnome_vfs_create_uri (GnomeVFSURI *uri, GnomeVFSOpenMode open_mode,
                            gboolean exclusive, guint perm)
{
    GnomeVFSHandle *handle = NULL;
    GnomeVFSResult res;
    gchar *name;
    SCM ret;
    
    res = gnome_vfs_create_uri (&handle, uri, open_mode, exclusive, perm);
    if (res != GNOME_VFS_OK)
        RESULT_ERROR (res);
    
    name = gnome_vfs_uri_to_string (uri, GNOME_VFS_URI_HIDE_PASSWORD);
    ret = scm_gnome_vfs_handle_to_port (handle, open_mode, name);
    g_free (name);
    return ret;
}
