/* guile-gnome
 * Copyright (C) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
 *
 * libgnomecanvas-support.c: Customizations for guile-libgnomecanvas
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

#include <stdlib.h>
#include <stdio.h>
#include <libguile.h>
#include "guile-gnome-gobject.h"
#include <libgnomecanvas/libgnomecanvas.h>
#include "libgnomecanvas-support.h"

static double*
fill_affine (double affine[6],
	     double x1, double y1, double x2, double y2, double x3, double y3)
{
  affine[0] = x1;
  affine[1] = y1;
  affine[2] = x2;
  affine[3] = y2;
  affine[4] = x3;
  affine[5] = y3;
  return affine;
}

void
_wrap_gnome_canvas_item_affine_absolute (GnomeCanvasItem *item,
					 double x1, double y1,
					 double x2, double y2,
					 double x3, double y3)
#define FUNC_NAME "gnome-canvas-item-affine-absolute"
{
  double affine[6];
  gnome_canvas_item_affine_absolute (item,
				     fill_affine (affine,
						  x1, y1, x2, y2, x3, y3));
}
#undef FUNC_NAME

void
_wrap_gnome_canvas_item_affine_relative (GnomeCanvasItem *item,
					 double x1, double y1,
					 double x2, double y2,
					 double x3, double y3)
#define FUNC_NAME "gnome-canvas-item-affine-relative"
{
  double affine[6];
  gnome_canvas_item_affine_relative (item,
				     fill_affine (affine,
						  x1, y1, x2, y2, x3, y3));
}
#undef FUNC_NAME
