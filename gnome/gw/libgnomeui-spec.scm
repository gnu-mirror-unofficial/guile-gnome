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
;;g-wrap specification for libgnomeui.
;;
;;; Code:

(define-module (gnome gw libgnomeui-spec)
  :use-module (oop goops)
  :use-module (g-wrap)
  :use-module (g-wrap guile)
  :use-module (gnome gw gobject-spec)
  :use-module (gnome gw gtk-spec)
  :use-module (gnome gobject gw-spec-utils)
  :use-module (gnome gobject defs-support))

(define-class <gnome-ui-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-libgnomeui
  #:dependencies '(standard gnome-glib gnome-gobject
                   gnome-atk gnome-gdk gnome-pango gnome-gtk))

(define-method (initialize (ws <gnome-ui-wrapset>) initargs)
  (next-method ws (append '(#:module (gnome gw libgnomeui)) initargs))

  (for-each
   (lambda (elt) (add-type-alias! ws (car elt) (cadr elt)))
   '(("pid_t" int)
     ("time_t" int)
     ;; Until we wrap bonobo, hack in these definitions
     ("BonoboDockItemBehavior" int)
     ("BonoboDockPlacement" int)))
  
  (load-defs ws "gnome/defs/ui.defs"))

(define-method (global-declarations-cg (self <gnome-ui-wrapset>))
  (list (next-method)
        "#include <libgnomeui/libgnomeui.h>\n"))

(define-method (client-global-declarations-cg (self <gnome-ui-wrapset>))
  (list (next-method)
        "#include <libgnomeui/libgnomeui.h>\n"))

