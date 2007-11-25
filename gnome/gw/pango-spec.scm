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
;;g-wrap specification for Pango.
;;
;;; Code:

(define-module (gnome gw pango-spec)
  #:use-module (oop goops)
  #:use-module (gnome gw support g-wrap)
  #:use-module (gnome gw glib-spec)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <pango-wrapset> (<gobject-wrapset-base>)
  guile
  #:id 'gnome-pango
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (global-declarations-cg (self <gobject-wrapset-base>))
  (list
   (next-method)
   "#include <pango/pango.h>\n"
   "#include \"pango-support.h\"\n"))
  
(define-method (initialize (ws <pango-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw pango) initargs)))

  (add-type-alias! ws "PangoGlyph" 'unsigned-int32)
  (wrap-refcounted-pointer! ws "PangoCoverage"
                            "pango_coverage_ref" "pango_coverage_unref")

  (wrap-structure! ws "PangoRectangle"
                   "scm_pango_rectangle_to_scm"
                   "scm_scm_to_pango_rectangle")

  (wrap-freeable-pointer! ws "PangoAttrIterator"
                          "pango_attr_iterator_destroy")
  (wrap-freeable-pointer! ws "PangoScriptIter"
                          "pango_script_iter_free")
  (wrap-freeable-pointer! ws "PangoAttribute"
                          "pango_attribute_destroy")

  (load-defs-with-overrides ws "gnome/defs/pango.defs"))

