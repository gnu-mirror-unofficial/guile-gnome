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
;;g-wrap specification for Wnck.
;;
;;; Code:

(define-module (gnome gw libgnomecanvas-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw glib-spec)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <canvas-wrapset> (<gobject-wrapset-base>)
  guile #:id 'gnome-libgnomecanvas)

(define-method (global-declarations-cg (self <gobject-wrapset-base>))
  (list
   (next-method)
	 "#include <libgnomecanvas/libgnomecanvas.h>\n"
	 "#include <libgnomecanvas/gnome-canvas-clipgroup.h>\n"))
  
(define-method (initialize (ws <canvas-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw libgnomecanvas) initargs)))

  (depends-on! ws 'standard 'gnome-glib 'gnome-gobject)
  
  (load-defs ws "gnome/defs/libgnomecanvas.defs"))

