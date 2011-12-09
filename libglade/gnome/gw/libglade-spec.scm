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
;;g-wrap specification for libglade.
;;
;;; Code:

(define-module (gnome gw libglade-spec)
  #:use-module (oop goops)
  #:use-module (gnome gw support g-wrap)
  #:use-module (gnome gw gtk-spec)
  #:use-module (gnome gw support defs)
  #:use-module (gnome gw support gobject))

(define-class <glade-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-libglade
  #:dependencies '(standard gnome-glib gnome-gobject gnome-gtk))

(define-method (initialize (ws <glade-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw libglade) initargs)))
  
  (load-defs-with-overrides ws "gnome/defs/libglade.defs"))

(define-method (global-declarations-cg (self <glade-wrapset>))
  (list (next-method)
        "#include <glade/glade.h>\n"
        "#include \"glade-support.h\"\n"
        "SCM scm_glade_module = SCM_BOOL_F;\n"))

(define-method (initializations-cg (self <glade-wrapset>) err)
   (list (next-method)
         "scm_glade_module = scm_current_module ();\n"
         "glade_set_custom_handler (guile_glade_custom_handler, NULL);\n"))

