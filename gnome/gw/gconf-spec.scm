;; guile-gnome
;; Copyright (C) 2004 Free Software Foundation, Inc.

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
;;g-wrap specification for gconf.
;;
;;; Code:

(define-module (gnome gw gconf-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support defs)
  #:use-module (gnome gw support gobject))

(define-class <gconf-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-gconf
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (initialize (ws <gconf-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw gconf) initargs)))
  
  (load-defs-with-overrides ws "gnome/defs/gconf.defs"))

(define-method (global-declarations-cg (self <gconf-wrapset>))
  (list (next-method)
        "#include <gconf/gconf.h>\n"
        "#include \"gconf-support.h\"\n"))
