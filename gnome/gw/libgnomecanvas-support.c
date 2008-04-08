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

/*
typedef struct {
	double *coords;
	int num_points;
	int ref_count;
} GnomeCanvasPoints;
*/

#if SCM_MINOR_VERSION < 7
/* guile-1.6.x compatibility */
#define SCM_VECTOR_REF(v, i) (SCM_VELTS ((v))[(i)])
#define scm_is_number(x) (scm_number_p (x) ==SCM_BOOL_T)
#define scm_to_double(x) (scm_num2dbl (x, "scm_to_double"))
#define scm_from_int(x) SCM_MAKINUM (x)
#define scm_from_double(x) (scm_make_real (x))
#endif

GnomeCanvasPoints *
guile_gnome_scm_to_canvas_points (SCM scm)
#define FUNC_NAME "guile-gnome-scm-to-canvas-points"
{
  GnomeCanvasPoints *points = NULL;
  if (scm_vector_p (scm) == SCM_BOOL_T)
    {
      int length = SCM_VECTOR_LENGTH (scm);
      points = gnome_canvas_points_new (length);
      for (int i = 0; i < length; i++)
	{
	  SCM s = SCM_VECTOR_REF (scm, i);
	  /* Just return NULL? */
	  SCM_ASSERT_TYPE (scm_is_number (s), s, SCM_ARG1, FUNC_NAME, "points");
	  (points->coords)[i] = scm_to_double (s);
	}
    }
  else if (SCM_GVALUEP (scm))
    {
      /* We have to cater for GValue, because the code that takes a
	 marshalled GValue doesn't know how to convert that into a
	 scm */
      GValue *value;
      SCM_VALIDATE_GVALUE_TYPE_COPY (1, scm, GNOME_TYPE_CANVAS_POINTS, value);
      points = g_value_dup_boxed (value);
    }
  return points;
}
#undef FUNC_NAME

SCM
guile_gnome_canvas_points_to_scm (GnomeCanvasPoints *points)
{
  int i;
  SCM scm = scm_make_vector (scm_from_int (points->num_points),
			     scm_from_int (0));
  for (i = 0; i < points->num_points; i++)
    scm_vector_set_x (scm, scm_from_int (i),
		    scm_from_double ((points->coords)[i]));
  return scm;
}

#ifndef SCM_GNOME_CANVAS_POINTS
// arg -Werror breaks this nice trick
//#message POINTS-POINTER
GnomeCanvasPoints *
_wrap_gnome_canvas_points_new (SCM scm)
#define FUNC_NAME "gnome-canvas-points-new"
{
  SCM_ASSERT_TYPE (scm_vector_p (scm) == SCM_BOOL_T, scm, SCM_ARG1, FUNC_NAME,
		   "points");
  return guile_gnome_scm_to_canvas_points (scm);
}
#undef FUNC_NAME
#else
//#warning POINTS-SCM
SCM
_wrap_gnome_canvas_points_new (SCM scm)
#define FUNC_NAME "gnome-canvas-points-new"
{
  GnomeCanvasPoints *points;
  SCM spoints;
  SCM_ASSERT_TYPE (scm_vector_p (scm) == SCM_BOOL_T, scm, SCM_ARG1, FUNC_NAME,
		   "points");
  points = guile_gnome_scm_to_canvas_points (scm);
  return scm_c_gvalue_new_take_boxed (GNOME_TYPE_CANVAS_POINTS, points);
}
#endif

GnomeCanvasPoints *
guile_gnome_canvas_points_copy (GnomeCanvasPoints *points)
{
  SCM scm = guile_gnome_canvas_points_to_scm (points);
  return guile_gnome_scm_to_canvas_points (scm);
}
#undef FUNC_NAME
