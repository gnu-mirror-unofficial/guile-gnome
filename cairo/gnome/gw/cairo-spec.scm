;; guile-gnome
;; Copyright (C) 2007, 2012 Free Software Foundation, Inc.

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
  #:use-module (gnome gw support g-wrap)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support gobject))

;; G-Wrap inelegance
(define-class <cairo-type> (<gw-type>)
  (c-type-name #:getter c-type-name #:init-keyword #:c-type-name))
(define-method (check-typespec-options (type <cairo-type>) (options <list>))
  ;; accept all options -- hacky but we don't care
  #t)
(define-method (c-type-name (type <cairo-type>) (typespec <gw-typespec>))
  (let ((c-name (slot-ref type 'c-type-name)))
    (if (memq 'const (options typespec))
        (string-append "const " c-name)
        c-name)))

;; not an RTI type because we want to avoid generating yet another
;; microlibrary
(define-class <cairo-refcounted-type> (<cairo-type>)
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

(define-class <cairo-opaque-type> (<cairo-type>)
  (unwrap #:init-keyword #:unwrap)
  (copy #:init-keyword #:copy)
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
     "else { \n"
     (if-typespec-option value 'callee-owned
         (list "  " scm-var " = " (slot-ref type 'take) " (" 
               (slot-ref type 'copy) "(" c-var "));")
         (list "  " scm-var " = " (slot-ref type 'take) " (" c-var ");"))
     "}\n")))

(define-class <client-actions> (<gw-item>))
(define-method (global-declarations-cg (ws <gw-guile-wrapset>) (a <client-actions>))
  '("#include <guile-cairo.h>\n"))

(define-class <cairo-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-cairo
  #:dependencies '(standard gnome-glib))

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
                  #:copy "cairo_font_options_copy"
                  #:take "scm_take_cairo_font_options"))
  (add-type-alias! ws "cairo_font_options_t*" 'cairo-font-options-t)

  (add-type! ws (make <cairo-opaque-type>
                  #:name 'cairo-path-t
                  #:c-type-name "cairo_path_t*"
                  #:unwrap "scm_to_cairo_path"
                  #:copy "cairo_copy_path"
                  #:take "scm_take_cairo_path"))
  (add-type-alias! ws "cairo_path_t*" 'cairo-path-t))

(define-method (global-declarations-cg (ws <cairo-wrapset>))
  (list (next-method)
        "#include <guile-cairo.h>\n"))

(define-method (initializations-cg (wrapset <cairo-wrapset>) err)
  (list (next-method)
        "scm_init_cairo ();\n"))

;; pango-cairo
;; cairo_t, cairo_font_options_t

;; gdk/gtk
;; cairo_t, cairo_surface_t, cairo_font_options_t