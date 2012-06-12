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
  #:use-module (g-wrap)
  #:use-module (gnome gw support g-wrap)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support gobject))


(define-class <cairo-boxed-type> (<gobject-classed-pointer-type>)
  (wrap-func #:init-keyword #:wrap-func #:getter wrap-func)
  (unwrap-func #:init-keyword #:unwrap-func #:getter unwrap-func)

  (wrap #:init-keyword #:wrap #:getter wrap)
  (unwrap #:init-keyword #:unwrap #:getter unwrap)
  (take #:init-keyword #:take #:getter take)
  (copy #:init-keyword #:copy #:getter copy)

  #:allowed-options '(null-ok))


(define gen-c-tmp
  (let ((i -1))
    (lambda (suffix)
      (set! i (1+ i))
      (format #f "gw__~A_~A" i suffix))))

(define-method (global-definitions-cg (wrapset <gobject-wrapset-base>)
                                      (type <cairo-boxed-type>))
  (let ((scm-var (gen-c-tmp "scm_val"))
        (c-var (gen-c-tmp "c_val")))
    (list
     (next-method)
     (let ((ctype (c-type-name type))
           (wrap-func (wrap-func type)))
       (list
        "static SCM " wrap-func " (const GValue* gvalue) {\n"
        "  SCM " scm-var " = SCM_BOOL_F;\n"
        "  " ctype " " c-var " = g_value_get_boxed (gvalue);\n"
        (if (wrap type)
            (list scm-var " = " (wrap type) " (" c-var ");")
            (list scm-var " = " (take type) " (" (copy type) " (" c-var "));"))
        "  return " scm-var ";\n"
        "}\n"))
     (let ((ctype (c-type-name type))
           (unwrap-func (unwrap-func type)))
       (list
        "static void " unwrap-func " (SCM " scm-var ", GValue* gvalue) {\n"
        "  " ctype " " c-var " = " (unwrap type) " (" scm-var ");"
        "  g_value_take_boxed (gvalue, " c-var ");\n"
        "}\n")))))

(define-method (global-declarations-cg (wrapset <gobject-wrapset-base>)
                                       (type <cairo-boxed-type>))
  (list
   (next-method)
   "static SCM " (wrap-func type) " (const GValue *);\n"
   "static void " (unwrap-func type) " (SCM, GValue *);\n"))

(define-method (initializations-cg (wrapset <gobject-wrapset-base>)
                                   (type <cairo-boxed-type>)
                                   status-var)
  (list
   (next-method)
   "scm_c_register_gvalue_wrappers (" (gtype-id type) ", "
   (wrap-func type) ", " (unwrap-func type) ");\n"))

(define (unwrap-null-checked value status-var code)
  (if-typespec-option
   value 'null-ok
   (list "if (SCM_FALSEP (" (scm-var value) "))\n"
         "  " (var value) " = NULL;\n"
         "else {\n"
         code
         "}\n")
   code))

(define-method (unwrap-value-cg (type <cairo-boxed-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     (unwrap-null-checked
      value status-var
      (list c-var " = " (unwrap type) " (" scm-var ");")))))

(define-method (wrap-value-cg (type <cairo-boxed-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else {\n"
     (if-typespec-option value 'caller-owned
         (list scm-var " = " (take type) " (" c-var ");")
         (if (wrap type)
             (list scm-var " = " (wrap type) " (" c-var ");")
             (list scm-var " = " (take type) " (" (copy type) " (" c-var "));")))
     "}\n")))

(define (wrap-cairo-boxed! ws ctype gtype wrap unwrap take copy)
  (let* ((pname (string-append ctype "*"))
         (wrap-func (string-append "gw__gvalue_" ctype "_wrap"))
         (unwrap-func (string-append "gw__gvalue_" ctype "_unwrap")))
    (let ((t (make <cairo-boxed-type>
               #:ctype ctype
               #:gtype-id gtype
               #:c-type-name pname
               #:wrapped "Custom"
               #:wrap-func wrap-func
               #:unwrap-func unwrap-func
               #:wrap wrap
               #:unwrap unwrap
               #:take take
               #:copy copy)))
      (add-type! ws t)
      (add-type-alias! ws pname (name t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cairo_path_t doesn't have a GObject type.
(define-class <cairo-opaque-type> (<gw-type>)
  (c-type-name #:getter c-type-name #:init-keyword #:c-type-name)
  (unwrap #:init-keyword #:unwrap)
  (copy #:init-keyword #:copy)
  (take #:init-keyword #:take))
(define-method (check-typespec-options (type <cairo-opaque-type>) (options <list>))
  ;; accept all options -- hacky but we don't care
  #t)
(define-method (c-type-name (type <cairo-opaque-type>) (typespec <gw-typespec>))
  (let ((c-name (slot-ref type 'c-type-name)))
    (if (memq 'const (options typespec))
        (string-append "const " c-name)
        c-name)))
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
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (initialize (ws <cairo-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw cairo) initargs)))
  
  (add-client-item! ws (make <client-actions>))

  (wrap-cairo-boxed! ws "cairo_t" "cairo_gobject_context_get_type ()"
                     "scm_from_cairo"
                     "scm_to_cairo"
                     "scm_take_cairo"
                     #f)

  (wrap-cairo-boxed! ws "cairo_font_options_t" "cairo_gobject_font_options_get_type ()"
                     #f
                     "scm_to_cairo_font_options"
                     "scm_take_cairo_font_options"
                     "cairo_font_options_copy")

  (add-type! ws (make <cairo-opaque-type>
                  #:name 'cairo-path-t
                  #:c-type-name "cairo_path_t*"
                  #:unwrap "scm_to_cairo_path"
                  #:copy "cairo_copy_path"
                  #:take "scm_take_cairo_path"))
  (add-type-alias! ws "cairo_path_t*" 'cairo-path-t))

(define-method (global-declarations-cg (ws <cairo-wrapset>))
  (list (next-method)
        "#include <guile-cairo.h>\n"
        "#include <cairo/cairo-gobject.h>\n"))

(define-method (initializations-cg (wrapset <cairo-wrapset>) err)
  (list (next-method)
        "scm_init_cairo ();\n"))

;; pango-cairo
;; cairo_t, cairo_font_options_t

;; gdk/gtk
;; cairo_t, cairo_surface_t, cairo_font_options_t