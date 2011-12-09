/* guile-gnome
 * Copyright (C) 2004 Free Software Foundation, Inc.
 *
 * gnome-vfs-support.h:
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

#include <libgnomevfs/gnome-vfs.h>
#include <libguile.h>
#include "guile-gnome-gobject.h"

#define RESULT_ERROR(result) \
  scm_throw (scm_from_locale_symbol ("gnome-vfs-error"), \
             scm_list_1 \
             (scm_from_locale_symbol (g_enum_get_value \
                              ((GEnumClass*)g_type_class_peek \
                               (GNOME_VFS_TYPE_VFS_RESULT), \
                               result)->value_nick)))

GnomeVFSDirectoryHandle *_wrap_gnome_vfs_directory_open (const gchar *text_uri, GnomeVFSFileInfoOptions options);
GnomeVFSDirectoryHandle *_wrap_gnome_vfs_directory_open_from_uri (GnomeVFSURI *uri, GnomeVFSFileInfoOptions options);

SCM _wrap_gnome_vfs_open (const gchar *text_uri, GnomeVFSOpenMode open_mode);
SCM _wrap_gnome_vfs_open_uri (GnomeVFSURI *uri, GnomeVFSOpenMode open_mode);
SCM _wrap_gnome_vfs_create (const gchar *text_uri, GnomeVFSOpenMode open_mode, gboolean exclusive, guint perm);
SCM _wrap_gnome_vfs_create_uri (GnomeVFSURI *uri, GnomeVFSOpenMode open_mode, gboolean exclusive, guint perm);
