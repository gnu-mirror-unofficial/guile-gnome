;; guile-gnome
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

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
;;Guile wrappers for libgnomecanvas.
;;
;;; Code:

(define-module (gnome canvas)
  #:use-module (gnome gw canvas)
  #:use-module (gnome gw support modules))

(use-modules (gnome gw canvas))

(define-class <gnome-canvas-path-def> (<gobject>)
  ;;(closure #:init-value (gnome-canvas-path-def-new)
  (closure #:init-value (gnome-canvas-path-def-new)
	   #:init-keyword #:path-def
	   #:getter get-def #:setter set-def))

(define-method (moveto (this <gnome-canvas-path-def>) x y)
  (gnome-canvas-path-def-moveto (get-def this) x y))
(define-method (curveto (this <gnome-canvas-path-def>) x1 y1 x2 y2 x3 y3)
  (gnome-canvas-path-def-curveto (get-def this)  x1 y1 x2 y2 x3 y3))
(define-method (lineto (this <gnome-canvas-path-def>) x y)
  (gnome-canvas-path-def-lineto (get-def this) x y))
(define-method (closepath (this <gnome-canvas-path-def>))
  (gnome-canvas-path-def-closepath (get-def this)))
(define-method (reset (this <gnome-canvas-path-def>))
  (gnome-canvas-path-def-reset (get-def this)))

(define -set-path-def set-path-def)
(define -get-path-def get-path-def)

(define-method (set-path-def (this <gnome-canvas-shape>)
			     (def <gnome-canvas-path-def>))
  (-set-path-def this (get-def def)))

(define-method (get-path-def (this <gnome-canvas-shape>))
  (make <gnome-canvas-path-def> #:path-def (-get-path-def this)))

(export <gnome-canvas-path-def>
	get-def set-def
	moveto curveto lineto closepath reset
	set-path-def get-path-def)

(re-export-modules (gnome gw canvas))

