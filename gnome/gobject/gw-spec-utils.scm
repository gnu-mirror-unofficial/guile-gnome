;; guile-gnome
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2004 Andreas Rottmann <rotty at debian dot org>

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
;;Routines useful to *-spec.scm g-wrap files.
;;
;;; Code:

(define-module (gnome gobject gw-spec-utils)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap c-types)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 slib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  
  #:export (unwrap-null-check
            
            <gobject-wrapset-base>
            add-type-alias!
            add-type-rule! find-type-rule
            construct-argument-list
            
            <gobject-type-base>
            
            
            <gobject-classed-type>
            gtype-id
            
            <gobject-classed-pointer-type>
            
            glib:func-cname->symbol

            wrap-object!
            wrap-boxed!
            wrap-pointer!
            wrap-opaque-pointer!
            wrap-interface!
            wrap-flags!))

(require 'printf)

(define-class <gobject-wrapset-base> (<gw-guile-wrapset>)
  (type-aliases #:init-form (make-hash-table 31))
  (type-rules #:init-form (make-hash-table 7)))

(define-method (add-type-alias! (wrapset <gobject-wrapset-base>)
                                (alias <string>)
                                (name <symbol>))
  (let ((type (lookup-type wrapset name)))
    (if (not type)
        (error "tried to alias unknown type" name))
    (hash-set! (slot-ref wrapset 'type-aliases) alias type)))

(define-method (lookup-type (wrapset <gobject-wrapset-base>)
                            (name <string>))
  
  (define (lookup wrapset cont)
    ;;(format #t "looking for ~S in ~S\n" name wrapset)
    (let ((ret (hash-ref (slot-ref wrapset 'type-aliases) name)))
      (cond (ret
             (cont ret))
            (else
             (for-each
              (lambda (ws)
                (if (is-a? ws <gobject-wrapset-base>)
                    (lookup ws cont)))
              (wrapsets-depended-on wrapset))
             #f))))

  (call-with-current-continuation
   (lambda (exit)
     (lookup wrapset exit))))

(define-method (add-type-rule! (self <gobject-wrapset-base>) (pattern <list>)
                               typespec)
  (if (not (and (not (null? pattern))
                (every (lambda (elt)
                           (and (list? elt) (<= 1 (length elt) 2)))
                       pattern)))
      (error "invalid type rule pattern"))
  (hash-set! (slot-ref self 'type-rules) (caar pattern)
             (cons pattern typespec)))

(define-method (find-type-rule (self <gobject-wrapset-base>) (params <list>))
  (let ((match (hash-ref (slot-ref self 'type-rules) (caar params))))
    (if match
        (values 1 (cdr match))
        (values 0 #f))))

;; Based on code from slib's strcase.scm, written 1992 by Dirk
;; Lutzebaeck (lutzeb@cs.tu-berlin.de). Public domain.
;;
;; We're tring to mimic the naming conventions of the C functions here.
;; Normally a lower case letter preceded by one capital letter forces a
;; hyphenation before the capital letter: GSource => g-source,
;; GtkIMContext => gtk-im-context. However, a lower case letter preceded
;; by exactly two capital letters that is not at the beginning of the
;; string is treated specially: GtkHBox => gtk-hbox (as the C api does).
(define (GStudlyCapsExpand nstr)
  (do ((idx (+ -1 (string-length nstr)) (+ -1 idx)))
      ((> 1 idx) (string-downcase nstr))
    (cond ((and (> idx 2)
                (char-lower-case? (string-ref nstr (+ -3 idx)))
                (char-upper-case? (string-ref nstr (+ -2 idx)))
                (char-upper-case? (string-ref nstr (+ -1 idx)))
                (char-lower-case? (string-ref nstr idx)))
           (set! idx (1- idx))
           (set! nstr
                 (string-append (substring nstr 0 (+ -1 idx))
                                "-"
                                (substring nstr (+ -1 idx)
                                           (string-length nstr)))))
          ((and (> idx 1)
                (char-upper-case? (string-ref nstr (+ -1 idx)))
                (char-lower-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 (+ -1 idx))
                                "-"
                                (substring nstr (+ -1 idx)
                                           (string-length nstr)))))
          ((and (char-lower-case? (string-ref nstr (+ -1 idx)))
                (char-upper-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 idx)
                                "-"
                                (substring nstr idx
                                           (string-length nstr))))))))

(define glib:type-cname->symbol-alist '())

;; (glib:type-cname->symbol "GtkAccelGroup") => <gtk-accel-group>
;; (glib:type-cname->symbol "GSource*") => <g-source*>
(define (glib:type-cname->symbol cname)
  (or (assoc-ref glib:type-cname->symbol-alist cname)
      (string->symbol
       (string-append
        "<"
        (string-trim-right
         (GStudlyCapsExpand
          ;; only change _ to -, other characters are not valid c names
          (string-map (lambda (c) (if (eq? c #\_) #\- c)) cname))
         #\-)
        ">"))))

;; (glib:type-cname->symbol "gtk_accel_group") => gtk-accel-group
(define (glib:func-cname->symbol cname)
  ;; only change _ to -, other characters are not valid c names
  (string->symbol (string-map (lambda (c) (if (eq? c #\_) #\- c)) cname)))

(define (print-info how-wrapped c-name scm-name ws)
  (printf "%-8.8s|%-18.18s|%-25.25s|%-25.25s\n"
          how-wrapped c-name scm-name (name ws)))

(define-class <gobject-type-base> (<gw-guile-rti-type>)
  (ctype #:init-keyword #:ctype)
  (how-wrapped #:init-keyword #:wrapped #:init-value #f))

(define-method (initialize (type <gobject-type-base>) initargs)
  (let-keywords
   initargs #t (class-name ctype name)
   (let ((name-sym (glib:type-cname->symbol ctype)))
     (next-method
      type
      (append!
       (if class-name '() (list #:class-name name-sym))
       (if name '() (list #:name name-sym))
       initargs)))))

(define-method (add-type! (ws <gobject-wrapset-base>)
                          (type <gobject-type-base>))
  (next-method)
  (let ((how-wrapped (slot-ref type 'how-wrapped)))
    (if how-wrapped
        (print-info how-wrapped (slot-ref type 'ctype) (name type) ws))))

(define-class <gobject-classed-type> (<gobject-type-base>)
  (gtype-id #:init-keyword #:gtype-id #:getter gtype-id)
  (define-class? #:init-keyword #:define-class? #:init-value #t))

(define-method (initialize (type <gobject-classed-type>) initargs)
  (let-keywords
   initargs #t (c-type-name class-name ctype name)
   (let ((name-sym (glib:type-cname->symbol ctype)))
     (next-method
      type
      (append!
       (if c-type-name '() (list #:c-type-name ctype))
       (if class-name '() (list #:class-name name-sym))
       (if name '() (list #:name name-sym))
       initargs)))))

;; Perhaps make this one also use a loop over an array?
(define-method (initializations-cg (wrapset <gobject-wrapset-base>)
                                   (type <gobject-classed-type>)
                                   status-var)
  (list
   (next-method)
   (if (slot-ref type 'define-class?)
       (list
        "scm_c_define (\"" (symbol->string (class-name type)) "\",\n"
        "              scm_call_1 (scm_sym_gtype_to_class,\n"
        "                          scm_c_register_gtype (" (gtype-id type) ")));\n")
       '())))

(define-method (add-type! (ws <gobject-wrapset-base>)
                          (type <gobject-classed-type>))
  (next-method)
  (add-module-export! ws (class-name type)))

(define-class <gobject-classed-pointer-type> (<gobject-classed-type>))

(define-method (initialize (type <gobject-classed-pointer-type>) initargs)
  (let-keywords
   initargs #t (ctype)
   (next-method type
                (append!
                 (list #:c-type-name (string-append ctype "*")
                       #:ffspec 'pointer)
                 initargs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap objects.

(define-class <gobject-object-type> (<gobject-classed-pointer-type>))

(define-method (unwrap-null-check (value <gw-value>)
                                  status-var)
  (if-typespec-option
   value 'null-ok
   (list "if (SCM_FALSEP (" (scm-var value) "))\n"
         "  " (var value) " = NULL;\n")
   (list "if (SCM_FALSEP (" (scm-var value) "))\n"
         `(gw:error ,status-var type ,(wrapped-var value)))))

(define-method (wrap-object! (ws <gobject-wrapset-base>) . args)
  (let ((type (apply make <gobject-object-type> args)))
    (set! (class-name type) (name type))
    (slot-set! type 'how-wrapped "GObject")
    (add-type! ws type)
    type))

(define-method (unwrap-value-cg (type <gobject-object-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     (unwrap-null-check value status-var)
     
     "if (!(" c-var " = (" (c-type-name type) ") "
     "scm_c_scm_to_gtype_instance (" scm-var ", " (gtype-id type) ")))\n"
     `(gw:error ,status-var type ,(wrapped-var value)))))

(define-method (wrap-value-cg (type <gobject-object-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else\n"
     "  " scm-var " = scm_c_gtype_instance_to_scm ((GTypeInstance *)" c-var ");\n"
     (if-typespec-option value 'caller-owned
         ;; the _to_scm will ref the object; if the function is a
         ;; constructor, we don't need that ref
          (list "if (" c-var ") g_object_ref ((GObject*)" c-var ");\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap boxed types, represented on the scheme side by GValues.

(define-class <gobject-boxed-type> (<gobject-classed-pointer-type>))

(define-method (initialize (type <gobject-boxed-type>) initargs)
  (let-keywords
   initargs #t (ctype)
   (next-method
    type
    (append! (list #:c-type-name (string-append ctype "*")) initargs))))

(define-method (wrap-boxed! (ws <gobject-wrapset-base>) . args)
  (let ((type (apply make <gobject-boxed-type> args)))
    (slot-set! type 'how-wrapped "GBoxed")
    (add-type! ws type)
    type))

;; fixme: how to deal with consts?
(define-method (unwrap-value-cg (type <gobject-boxed-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (ctype (c-type-name type)))
     (list
      (unwrap-null-check value status-var)
      "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
      "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " (gtype-id type) ")) {\n"
      (if-typespec-option
       value 'callee-owned
       (list
        "  " c-var " = (" ctype ") g_value_dup_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n")
       (list
        "  " c-var " = (" ctype ") g_value_get_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"))
      " } else {\n"
      "  " c-var " = NULL;\n"
      `(gw:error ,status-var type ,scm-var)
      "}\n")))

(define-method (wrap-value-cg (type <gobject-boxed-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     "  " scm-var " = scm_c_make_gvalue (" (gtype-id type) ");\n"
     "  g_value_set_boxed ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"
     "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap pointers. This is an opaque value type; scheme doesn't know what
;; to do with it.
(define-class <gobject-pointer-type> (<gobject-classed-pointer-type>))

(define-method (wrap-pointer! (ws <gobject-wrapset-base>) . args)
  (let ((type (apply make <gobject-pointer-type> args)))
    (slot-set! type 'how-wrapped "GPointer")
    (add-type! ws type)
    type))

(define-method (unwrap-value-cg (type <gobject-pointer-type>)
                                (value <gw-value>)
                                status-var)
  ;; fixme: how to deal with consts?
  (list
   (unwrap-null-check value status-var)
   "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
   "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " (gtype-id type) "))\n"
   "  " c-var " = (" ctype ") g_value_get_pointer ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
   "else {\n"
   "  " c-var " = NULL;\n"
   `(gw:error ,status-var type ,scm-var)
   "}\n"))

(define-method (wrap-value-cg (type <gobject-pointer-type>)
                              (value <gw-value>)
                              status-var)
  (list
   "if (" c-var " == NULL) {\n"
   "  " scm-var " = SCM_BOOL_F;\n"
   "} else {\n"
   "  " scm-var " = scm_c_make_gvalue (" (gtype-id type) ");\n"
   "  g_value_set_pointer ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"
   "}\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap interfaces. We only understand interfaces implemented by objects.

(define-class <gobject-interface-type> (<gobject-classed-pointer-type>))

(define-method (wrap-interface! (ws <gobject-wrapset-base>) . args)
  (let ((type (apply make <gobject-interface-type> args)))
    (slot-set! type 'how-wrapped "GInterface")
    (add-type! ws type)
    type))

(define-method (unwrap-value-cg (type <gobject-interface-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     (unwrap-null-check value status-var)
     c-var " = (" (c-type-name type) ") scm_c_scm_to_gtype_instance (" scm-var ", G_TYPE_OBJECT);\n"
     
     "if (!" c-var " || !g_type_is_a (G_TYPE_FROM_INSTANCE (" c-var "), " (gtype-id type) "))\n"
      `(gw:error ,status-var type ,(wrapped-var value)))
    ))

(define-method (wrap-value-cg (type <gobject-interface-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else {\n"
     "  " scm-var " = scm_c_gtype_instance_to_scm ((GTypeInstance *)" c-var ");\n"
     (if-typespec-option
      value 'caller-owned
      (list "if (" c-var ") g_object_ref ((GObject*)" c-var ");\n"))
     "}\n")))


;;;
;;; Enums
;;;

(define-class <gobject-enum-type> (<gobject-classed-type>))

(define-method (initialize (type <gobject-enum-type>) initargs)
  (let-keywords
   initargs #t (ctype)
   (next-method type
                (append!
                 (cons #:ffspec (cons 'uint  initargs))))))

(define-method (make-typespec (type <gobject-enum-type>) (options <list>))
  (next-method type (cons 'caller-owned options)))

(define-method (wrap-enum! (ws <gobject-wrapset-base>) . args)
  (let-keywords
   args #t (gtype-id ctype)
   (cond
    (gtype-id
     (let ((type (apply make <gobject-enum-type> args)))
       (slot-set! type 'how-wrapped "GEnum")
       (add-type! ws type)
       type))
   (else
      (print-info "C Enum" ctype ctype ws)
      (apply next-method ws (append!
                             (list #:name (glib:type-cname->symbol ctype)
                                   #:c-type-name ctype)
                             args))))))

;; enums are just guints...
(define-method (unwrap-value-cg (type <gobject-enum-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (gtype-id (gtype-id type)))
    (list
     "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
     "  " c-var " = g_value_get_enum ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
     "else {\n" ;; we can't use scm_make because we need the special allocate-instance
     "  SCM newval = scm_apply_3 (SCM_VARIABLE_REF (scm_c_lookup (\"make\")),\n"
     "                            scm_c_gtype_lookup_class (" gtype-id "),\n"
     "                            scm_c_make_keyword (\"value\"),\n"
     "                            " scm-var ", SCM_EOL);\n"
     ;; should throw an exception if the eval fails
     "  " c-var " = g_value_get_enum ((GValue*)SCM_SMOB_DATA (newval));\n"
     "}\n")))

(define-method (wrap-value-cg (type <gobject-enum-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     scm-var " = scm_c_make_gvalue (" (gtype-id type)");\n"
     "g_value_set_enum ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n")))


;;;
;;; Flags, represented on the scheme side as GValues.
;;;

(define-class <gobject-flags-type> (<gobject-enum-type>))

(define-method (make-typespec (type <gobject-flags-type>) (options <list>))
  (next-method type (cons 'caller-owned options)))

;; (wrap-flags! wrapset #:gtype-id foo [#:values '((a 1) ...)])
;; (wrap-flags! wrapset #:values '((a 1) ...))
(define-method (wrap-flags! (ws <gobject-wrapset-base>) . args)
  (let-keywords
   args #t (gtype-id ctype)
   (cond
    (gtype-id
     (let ((type (apply make <gobject-flags-type> args)))
       (slot-set! type 'how-wrapped "GFlags")
       (add-type! ws type)
       type))
    (else
     (let ((type (apply wrap-enum! ws args)))
       (print-info "C Flags" ctype ctype ws)
       type)))))

;; flags are just guints...
(define-method (unwrap-value-cg (type <gobject-flags-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (gtype-id (gtype-id type)))
    (list
     "if (SCM_FALSEP (" scm-var "))\n"
     "  " c-var " = 0;\n"
     "else if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "         && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
     "  " c-var " = g_value_get_flags ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
     "else {\n" ;; we can't use scm_make because we need the special allocate-instance
     "  SCM newval = scm_apply_3 (SCM_VARIABLE_REF (scm_c_lookup (\"make\")),\n"
     "                            scm_c_gtype_lookup_class (" gtype-id "),\n"
     "                            scm_c_make_keyword (\"value\"),\n"
     "                            " scm-var ", SCM_EOL);\n"
     ;; should throw an exception if the eval fails
     "  " c-var " = g_value_get_flags ((GValue*)SCM_SMOB_DATA (newval));\n"
     "}\n")))

(define-method (wrap-value-cg (type <gobject-flags-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value)) (scm-var (scm-var value)))
    (list
     scm-var " = scm_c_make_gvalue (" (gtype-id type) ");\n"
     "g_value_set_flags ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n")))


(define (wrap-opaque-pointer! ws ctype)
  ;;(print-info "Opaque" ctype ctype ws) ; FIXME: Write to log file
  (let ((type (wrap-as-wct!
               ws
               #:name (glib:type-cname->symbol ctype)
               #:c-type-name ctype
               #:c-const-type-name (string-append "const " ctype))))
    (add-type-alias! ws ctype (name type))))

(for-each (lambda (null-ok-class)
            (class-slot-set! null-ok-class 'allowed-options '(null-ok)))
          (list <gobject-object-type> <gobject-boxed-type>
                <gobject-interface-type>))
            

;; Not used?

; (define (gobject:gwrap-class ws ctype gtype-id)
;   (let ((c-ptr-type (string-append ctype "*")))
;     (gobject:gwrap-helper
;      ws ctype
;      c-ptr-type c-ptr-type
;      (lambda (c-var scm-var typespec status-var)
;        (list "if (g_type_is_a (SCM_SMOB_DATA (scm_slot_ref (" scm-var ", scm_sym_gtype)), " gtype-id "))\n"
;              "  " c-var " = (" ctype "*) SCM_SMOB_DATA (scm_slot_ref (" scm-var ", scm_sym_gtype_class));\n"
;              "else " `(gw:error ,status-var type ,scm-var)))
;      (lambda (scm-var c-var typespec status-var) ; not ideal but ok
;        (list scm-var " = scm_c_gtype_lookup_class (G_TYPE_FROM_CLASS (" c-var "));\n"))
;      (lambda (c-var typespec status-var force?)
;        (list))
;      "GObjectClass")))
