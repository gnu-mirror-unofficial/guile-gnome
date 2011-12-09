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
;;g-wrap specification for ATK.
;;
;;; Code:

(define-module (gnome gw atk-spec)
  #:use-module (oop goops)
  #:use-module (gnome gw support g-wrap)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <atk-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-atk
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (global-declarations-cg (self <atk-wrapset>))
  (list
   (next-method)
   "#include <atk/atk.h>\n"
   "#include <atk/atk-enum-types.h>\n"))
  
(define-method (initialize (ws <atk-wrapset>) initargs)

  (next-method ws (append '(#:module (gnome gw atk)) initargs))

  (add-type-alias! ws "AtkState" 'unsigned-int64)
  
  (load-defs-with-overrides ws "gnome/defs/atk.defs"))
