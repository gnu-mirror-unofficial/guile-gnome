;; guile-gnome
;; Copyright (C) 2007 Free Software Foundation, Inc.

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
;;g-wrap specification for cairo.
;;
;;; Code:

(define-module (gnome gw cairo-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support defs)
  #:use-module (gnome gw support gobject))

(define-class <cairo-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-cairo
  #:dependencies '(standard))

;; not an RTI type because we want to avoid generating yet another
;; microlibrary
(define-class <cairo-refcounted-type> ()
  (wrap #:init-keyword #:wrap)
  (unwrap #:init-keyword #:unwrap)
  (take #:init-keyword #:take))

(define (unwrap-null-checked value status-var code)
  (if-typespec-option
   value 'null-ok
   (list "if (SCM_FALSEP (" (scm-var value) "))\n"
         "  " (var value) " = NULL;\n"
         "else {\n"
         code
         "}\n")
   code))

(define-method (unwrap-value-cg (type <cairo-refcounted-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     (unwrap-null-checked
      value status-var
      (list c-var " = " (slot-ref type 'unwrap) " (" scm-var ");")))))

(define-method (wrap-value-cg (type <cairo-refcounted-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else {\n"
     (if-typespec-option value 'caller-owned
         (list scm-var " = " (slot-ref type 'take) " (" c-var ");")
         (list scm-var " = " (slot-ref type 'wrap) " (" c-var ");"))
     "}\n")))

(define-class <cairo-opaque-type> ()
  (unwrap #:init-keyword #:unwrap)
  (take #:init-keyword #:take))

(define-method (unwrap-value-cg (type <cairo-opaque-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     (unwrap-null-checked
      value status-var
      (list c-var " = " (slot-ref type 'unwrap) " (" scm-var ");")))))

(define-method (wrap-value-cg (type <cairo-opaque-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else\n"
     "  " scm-var " = " (slot-ref type 'take) " (" c-var ");")))

(define-class <client-actions> (<gw-item>))
(define-method (global-declarations-cg (ws <glib-wrapset>) (a <client-actions>))
  '("#include <guile-cairo.h>\n"))
(define-method (initializations-cg (wrapset <gw-guile-wrapset>) (a <client-actions>) err)
  (list "scm_init_cairo ();\n"))

(define-method (initialize (ws <cairo-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw cairo) initargs)))
  
  (add-client-item! ws (make <client-actions>))

  (add-type! ws (make <cairo-refcounted-type>
                  #:name 'cairo-t
                  #:c-type-name "cairo_t*"
                  #:wrap "scm_from_cairo"
                  #:unwrap "scm_to_cairo"
                  #:take "scm_take_cairo"))
  (add-type-alias! ws "cairo_t*" 'cairo-t)

  (add-type! ws (make <cairo-opaque-type>
                  #:name 'cairo-font-options-t
                  #:c-type-name "cairo_font_options_t*"
                  #:unwrap "scm_to_cairo_font_options"
                  #:take "scm_take_cairo_font_options"))
  (add-type-alias! ws "cairo_font_options_t*" 'cairo-font-options-t))

;; pango-cairo
;; cairo_t, cairo_font_options_t

;; gdk/gtk
;; cairo_t, cairo_surface_t, cairo_font_options_t