;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
;;               2003,2004 Andy Wingo <wingo at pobox dot com>

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
;;A CORBA wrapper for Guile.
;;
;;; Code:

(define-module (gnome corba gw-corba-spec)
  :use-module (g-wrap)
  :use-module (gnome gobject gw-standard-spec)
  :use-module (g-wrap simple-type))

;; gw-corba: an internal glue binding, probably not useful to other people...

(let ((ws (gw:new-wrapset "guile-gnome-gw-corba")))

  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")

  (gw:wrapset-set-guile-module! ws '(gnome corba gw-corba))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <guile-gnome-corba.h>\n")))

  (gw:wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         (list "scm_pre_init_gnome_corba_generic ();\n"
               "scm_pre_init_gnome_corba_types ();\n"
               "scm_pre_init_gnome_corba_primitives ();\n")
         '())))

  ;; Here we wrap some functions to bootstrap the core library.

  (gw:wrap-function
   ws
   '%init-gnome-corba
   '<gw:void>
   "scm_init_gnome_corba"
   '()
   "Export a number of fundamental gtypes and functions to operate on objects.")

  (gw:wrap-function
   ws
   '%init-gnome-corba-primitives
   '<gw:void>
   "scm_init_gnome_corba_primitives"
   '()
   "Export a number of fundamental gtypes and functions to operate on objects.")

  (gw:wrap-function
   ws
   '%init-gnome-corba-types
   '<gw:void>
   "scm_init_gnome_corba_types"
   '()
   "Export a number of fundamental gtypes and functions to operate on objects."))
