;; guile-gnome
;; Copyright (C) 2003,2004 Andreas Rottmann

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

(define-module (gw-test-glib-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (gnome gw glib-spec)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <test-glib-wrapset> (<gobject-wrapset-base>)
  #:id 'test-glib #:dependencies '(standard gnome-glib))

(define-method (global-declarations-cg (ws <test-glib-wrapset>))
  (list (next-method)
        "#include \"test-glib.h\"\n"))

(define-method (initialize (ws <test-glib-wrapset>) initargs)
  (next-method ws (append '(#:module (test-suite gw-test-glib)) initargs))
  
  (load-defs ws "test-suite/test-glib.defs"))

