/* guile-gnome
 *
 * Copyright (C) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
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
 *
 * Based on work Copyright (C) 2003 James Henstridge <james@daa.com.au>
 */

#ifndef LIBGNOMECANVAS_SUPPORT_H
#define LIBGNOMECANVAS_SUPPORT_H

void _wrap_gnome_canvas_item_affine_absolute (GnomeCanvasItem *item,
					      double x1, double y1,
					      double x2, double y2,
					      double x3, double y3);

void _wrap_gnome_canvas_item_affine_relative (GnomeCanvasItem *item,
					      double x1, double y1,
					      double x2, double y2,
					      double x3, double y3);
#endif /* LIBGNOMECANVAS_SUPPORT_H */

