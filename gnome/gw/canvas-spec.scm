;; guile-gnome
;; Copyright (C) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;                    Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;g-wrap specification for libgnomecanvas.
;;
;;; Code:

(define-module (gnome gw canvas-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw glib-spec)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw gtk-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <canvas-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-canvas
  #:dependencies '(standard gnome-glib gnome-gobject gnome-gdk gnome-gtk))

(define-method (global-declarations-cg (self <canvas-wrapset>))
  (list
   (next-method)
	 "#include <libgnomecanvas/libgnomecanvas.h>\n"
	 "#include <libgnomecanvas/gnome-canvas.h>\n"
	 "#include <libgnomecanvas/gnome-canvas-rect-ellipse.h>\n"
	 "#include <libgnomecanvas/gnome-canvas-clipgroup.h>\n"
	 "#include \"libgnomecanvas-support.h\"\n"))

(if #f
    (custom-wrap-decls
     "GnomeCanvasPoints"
     ;; unwrap
     (unwrap-null-checked
      value status-var
      (list c-var " = guile_gnome_scm_to_canvas_points (" scm-var ");\n"))
     ;; wrap
     (list scm-var " = guile_gnome_canvas_points_to_scm (" c-var ");\n"
	   "gnome_canvas_points_free (" c-var ");\n")))
    

(define-method (initialize (ws <canvas-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw canvas) initargs)))
  (if #f
      (wrap-custom-pointer! "GnomeCanvasPoints"))
  (load-defs-with-overrides ws "gnome/defs/libgnomecanvas.defs"))
