;; guile-gnome
;; Copyright (C) 2003,2004,2007 Andy Wingo <wingo at pobox dot com>

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
;;g-wrap specification for pangocairo.
;;
;;; Code:

(define-module (gnome gw pangocairo-spec)
  #:use-module (oop goops)
  #:use-module (gnome gw support g-wrap)
  #:use-module (gnome gw pango-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <pangocairo-wrapset> (<gobject-wrapset-base>)
  guile
  #:id 'gnome-pangocairo
  #:dependencies '(standard gnome-glib gnome-gobject gnome-pango))

(define-method (global-declarations-cg (self <pangocairo-wrapset>))
  (list
   (next-method)
   "#include <pango/pangocairo.h>\n"))
  
(define-method (initialize (ws <pangocairo-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw pangocairo) initargs)))

  (load-defs-with-overrides ws "gnome/defs/pangocairo.defs"))
