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

(define-module (gnome gw corba-spec)
  #:use-module (oop goops)
  #:use-module (gnome gw support g-wrap)
  #:use-module (g-wrap guile ws standard)
  #:use-module (gnome gw support gobject))

;; gw-corba: an internal glue binding, probably not useful to other people...
(define-class <corba-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-corba #:dependencies '(standard))

(define-method (global-declarations-cg (ws <corba-wrapset>)
                                       (item <gw-item>))
  (list "#include <guile-gnome-corba.h>\n"))

(define-method (initializations-cg (ws <corba-wrapset>) err)
  (list (next-method)
        "scm_pre_init_gnome_corba_generic ();\n"
        "scm_pre_init_gnome_corba_types ();\n"
        "scm_pre_init_gnome_corba_primitives ();\n"))

(define-method (initialize (ws <corba-wrapset>) initargs)
  (next-method ws (append '(#:module (gnome gw corba)) initargs))
  
  ;; Here we wrap some functions to bootstrap the core library.

  (wrap-function! ws
                  #:name '%init-gnome-corba
                  #:returns 'void
                  #:c-name "scm_init_gnome_corba"
                  #:arguments '()
                  #:description "Export a number of fundamental gtypes and functions to operate on objects.")

  (wrap-function! ws
                 #:name '%init-gnome-corba-primitives
                 #:returns 'void
                 #:c-name "scm_init_gnome_corba_primitives"
                 #:arguments '()
                 #:description "Export a number of fundamental gtypes and functions to operate on objects.")

  (wrap-function! ws
                  #:name '%init-gnome-corba-types
                  #:returns 'void
                  #:c-name "scm_init_gnome_corba_types"
                  #:arguments '()
                  #:description "Export a number of fundamental gtypes and functions to operate on objects."))
