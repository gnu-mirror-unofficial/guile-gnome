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
;;g-wrap specification for libgnome.
;;
;;; Code:

(define-module (gnome gw libgnome-spec)
  :use-module (oop goops)
  :use-module (g-wrap)
  :use-module (gnome gw gobject-spec)
  :use-module (gnome gobject gw-spec-utils)
  :use-module (gnome gobject defs-support))

(define-class <libgnome-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-libgnome
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (initialize (ws <libgnome-wrapset>) initargs)
  (next-method ws (append '(#:module (gnome gw libgnome)) initargs))

  (load-defs ws "gnome/defs/gnome.defs"))

(define-method (global-declarations-cg (self <libgnome-wrapset>))
  (list (next-method)
        "#include <libgnome/libgnome.h>\n"
        "#include <libgnome/libgnometypebuiltins.h>\n"
        "#include \"gnome-support.h\"\n"))

(define-method (client-global-declarations-cg (self <libgnome-wrapset>))
  (list (next-method)
        "#include <libgnome/libgnome.h>\n"
        "#include <libgnome/libgnometypebuiltins.h>\n"
        "#include \"gnome-support.h\"\n"))
