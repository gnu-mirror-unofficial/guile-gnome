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
;;Routines useful to *-spec.scm g-wrap files.
;;
;;; Code:

(define-module (gnome gobject gw-spec-utils)
  :use-module (g-wrap)
  :use-module (g-wrap gw-wct-spec)
  :use-module (ice-9 optargs)
  :use-module (ice-9 slib)
  :use-module (srfi srfi-13)
  :use-module (gnome gobject utils)
  :export (gobject:gwrap-set-all-types-used
           gobject:gwrap-helper
           gobject:gwrap-helper-with-class
           gobject:gwrap-object
           gobject:gwrap-boxed
           gobject:gwrap-pointer
           gobject:gwrap-opaque-pointer
           gobject:gwrap-interface
           gobject:gwrap-flags
           gobject:gwrap-enum
           gobject:gwrap-class))

(require 'printf)

(define (wrapset-get-types-used ws)
  (let ((rtd (record-type-descriptor ws)))
    ((record-accessor rtd 'types-used) ws)))
(define (wrapset-get-wrapped-types ws)
  (let ((rtd (record-type-descriptor ws)))
    ((record-accessor rtd 'wrapped-types) ws)))

;; g-wrap will only output type initialization code (ie, gtype->class
;; stuff) for types that are actually used in the api. some types,
;; however, do not show up in the api -- <gtk-hbox>, for instance. Of
;; course we want to be able to (make <gtk-hbox>), this function exists
;; to say that all of the types are used by the wrapset.
(define (gobject:gwrap-set-all-types-used ws)
  (let ((gw-types-used (wrapset-get-types-used ws)))
    (for-each (lambda (pair)
                (hashq-set! gw-types-used
                            (cdr pair)
                            (cdr pair)))
              (wrapset-get-wrapped-types ws))))

;; "gtk_accel_group" => gtk-accel-group
(define (glib-function-name->scheme-name cname)
  ;; only change _ to -, other characters are not valid c names
  (string->symbol (gtype-name->scheme-name cname)))

(define (print-info how-wrapped c-name scm-name ws)
  (printf "%-8.8s|%-18.18s|%-25.25s|%-25.25s\n"
          how-wrapped c-name scm-name (gw:wrapset-get-name ws)))

(define (gwrap-helper ws ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor
                       how-wrapped)
  (define wrapped-type (gw:wrap-type ws (gtype-name->class-name ctype)))
  
  (define (typespec-options-parser options-form wrapset)
    (let ((remainder options-form))
      (set! remainder (delq 'const remainder))
      (if (and (memq 'caller-owned remainder)
               (memq 'callee-owned remainder))
          (throw 'gw:bad-typespec
                 "Bad gobject-based options form (caller and callee owned!)."
                 options-form))
      (if (not (or (memq 'caller-owned remainder)
                   (memq 'callee-owned remainder)))
          (set! options-form (cons 'caller-owned options-form)))
      (set! remainder (delq 'caller-owned remainder))
      (set! remainder (delq 'callee-owned remainder))
      (set! remainder (delq 'null-ok remainder))
      (if (null? remainder)
          options-form
          (throw 'gw:bad-typespec
                 "Bad gobject-based options form - spurious options: "
                 remainder))))
  
  (define (pre-call-arg-ccg param status-var)
    (let* ((scm-name (gw:param-get-scm-name param))
           (c-name (gw:param-get-c-name param))
           (typespec (gw:param-get-typespec param)))
      (list
       (scm->c-ccg c-name scm-name typespec status-var)
       "if(" `(gw:error? ,status-var type) ")"
       `(gw:error ,status-var arg-type)
       "else if(" `(gw:error? ,status-var range) ")"
       `(gw:error ,status-var arg-range))))
  
  (define (call-ccg result func-call-code status-var)
    (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
  
  (define (post-call-arg-ccg param status-var)
    (let* ((c-name (gw:param-get-c-name param))
           (typespec (gw:param-get-typespec param)))
      (c-destructor c-name typespec status-var #f)))
  
  (define (post-call-result-ccg result status-var)
    (let* ((scm-name (gw:result-get-scm-name result))
           (c-name (gw:result-get-c-name result))
           (typespec (gw:result-get-typespec result)))
      (list
       (c->scm-ccg scm-name c-name typespec status-var)
       (c-destructor c-name typespec status-var #f))))
  
  (gw:type-set-c-type-name-func! wrapped-type c-type-name-func)
  (gw:type-set-typespec-options-parser! wrapped-type typespec-options-parser)
  
  (gw:type-set-scm->c-ccg! wrapped-type scm->c-ccg)
  (gw:type-set-c->scm-ccg! wrapped-type c->scm-ccg)
  (gw:type-set-c-destructor! wrapped-type c-destructor)  
  
  (gw:type-set-pre-call-arg-ccg! wrapped-type pre-call-arg-ccg)
  (gw:type-set-call-ccg! wrapped-type call-ccg)
  (gw:type-set-post-call-arg-ccg! wrapped-type post-call-arg-ccg)
  (gw:type-set-post-call-result-ccg! wrapped-type post-call-result-ccg)
  
  (if how-wrapped
      (print-info how-wrapped ctype (gtype-name->class-name ctype) ws))

  wrapped-type)
(define gobject:gwrap-helper gwrap-helper)

(define (gwrap-helper-with-class ws gtype-id ctype c-type-name-func
                                  scm->c-ccg c->scm-ccg c-destructor how-wrapped)
  (if (not (and (string? gtype-id)
                (string? ctype)
                (closure? c-type-name-func)
                (closure? scm->c-ccg)
                (closure? c->scm-ccg)
                (closure? c-destructor)))
      (error "Bad arguments to gwrap-helper-with-class."))
  (let ((t (gwrap-helper ws ctype c-type-name-func scm->c-ccg c->scm-ccg
                         c-destructor how-wrapped))
        (type-string (symbol->string (gtype-name->class-name ctype))))
    (gw:type-set-global-initializations-ccg!
     t
     (lambda (type client-wrapset status-var)
       (if client-wrapset
           (list "scm_c_define (\"" type-string "\",\n"
                 "              scm_c_gtype_to_class (" gtype-id "));\n"
                 "scm_c_export (\"" type-string "\", NULL);\n")
           '())))
    t))
(define gobject:gwrap-helper-with-class gwrap-helper-with-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap objects.
(define (gobject:gwrap-object ws ctype gtype-id)
  (define (c-type-name-func typespec)
    (if (memq 'const (gw:typespec-get-options typespec))
        (string-append "const " ctype " *")
        (string-append ctype " *")))

  (define (scm->c-ccg c-var scm-var typespec status-var)
    (list
     (if (memq 'null-ok (gw:typespec-get-options typespec))
         (list
          "if (SCM_FALSEP (" scm-var "))\n"
          "  " c-var " = NULL;\n"
          "else ")
         '())
     "if (!(" c-var " = (" (c-type-name-func typespec) ") scm_c_scm_to_gtype_instance (" scm-var ", " gtype-id ")))\n"
     `(gw:error ,status-var type ,scm-var)))
  
  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else\n"
     "  " scm-var " = scm_c_gtype_instance_to_scm ((GTypeInstance *)" c-var ");\n"
     (if (memq 'caller-owned (gw:typespec-get-options typespec))
         ;; the _to_scm will ref the object; if the function is a
         ;; constructor, we don't need that ref
         (list "if (" c-var ") g_object_unref ((GObject*)" c-var ");\n")
         '())))
  
  (define (c-destructor c-var typespec status-var force?)
    ;; our temp vars are just pointers, there's nothing to clean up
    '())

  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg
                           c->scm-ccg c-destructor "GObject"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap boxed types, represented on the scheme side by GValues.
(define (gobject:gwrap-boxed ws ctype gtype-id)
  ;; fixme: how to deal with consts?
  (define (c-type-name-func typespec)
    (if (memq 'const (gw:typespec-get-options typespec))
        (string-append "const " ctype " *")
        (string-append ctype " *")))

  (define (scm->c-ccg c-var scm-var typespec status-var)
    (list
     (if (memq 'null-ok (gw:typespec-get-options typespec))
         (list
          "if (SCM_FALSEP (" scm-var "))\n"
          "  " c-var " = NULL;\n"
          "else ")
         '())
     "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
     "  " c-var " = (" (c-type-name-func typespec) ") "
     "g_value_" (if (memq 'callee-owned (gw:typespec-get-options typespec))
                    "dup"
                    "get")
     "_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
     "else {\n"
     "  " c-var " = NULL;\n"
     `(gw:error ,status-var type ,scm-var)
     "}\n"))

  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     "  " scm-var " = scm_c_make_gvalue (" gtype-id ");\n"
     "  g_value_set_boxed ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"
     "}\n"))
  
  (define (c-destructor c-var typespec status-var force?)
    ;; our temp vars are just pointers, there's nothing to clean up
    '())
  
  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg
                           c->scm-ccg c-destructor "GBoxed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap pointers. This is an opaque value type; scheme doesn't know what
;; to do with it.
(define (gobject:gwrap-pointer ws ctype gtype-id)
  ;; fixme: how to deal with consts?
  (define (c-type-name-func typespec)
    (if (memq 'const (gw:typespec-get-options typespec))
        (string-append "const " ctype)
        ctype))

  (define (scm->c-ccg c-var scm-var typespec status-var)
    (list
     (if (memq 'null-ok (gw:typespec-get-options typespec))
         (list
          "if (SCM_FALSEP (" scm-var "))\n"
          "  " c-var " = NULL;\n"
          "else ")
         '())
     "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
     "  " c-var " = (" (c-type-name-func typespec) ") g_value_get_pointer ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
     "else {\n"
     "  " c-var " = NULL;\n"
     `(gw:error ,status-var type ,scm-var)
     "}\n"))
  
  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     "  " scm-var " = scm_c_make_gvalue (" gtype-id ");\n"
     "  g_value_set_pointer ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"
     "}\n"))
  
  (define (c-destructor c-var typespec status-var force?)
    '())
  
  (gwrap-helper ws ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor
                "GPointer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap interfaces. We only understand interfaces implemented by objects.
(define (gobject:gwrap-interface ws ctype gtype-id)
  (define (c-type-name-func typespec)
    (if (memq 'const (gw:typespec-get-options typespec))
        (string-append "const " ctype " *")
        (string-append ctype " *")))

  (define (scm->c-ccg c-var scm-var typespec status-var)
    (list
     (if (memq 'null-ok (gw:typespec-get-options typespec))
         (list
          "if (SCM_FALSEP (" scm-var "))\n"
          "  " c-var " = NULL;\n"
          "else ")
         '())
     "if (!(" c-var " = (" (c-type-name-func typespec) ") scm_c_scm_to_gtype_instance (" scm-var ", G_TYPE_OBJECT)))\n"
     `(gw:error ,status-var type ,scm-var)
     "if (!g_type_is_a (G_TYPE_FROM_INSTANCE (" c-var "), " gtype-id "))\n"
     `(gw:error ,status-var type ,scm-var)
     ))
  
  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else\n"
     "  " scm-var " = scm_c_gtype_instance_to_scm ((GTypeInstance *)" c-var ");\n"
     (if (memq 'caller-owned (gw:typespec-get-options typespec))
         ;; the _to_scm will ref the object; if the function is a
         ;; constructor, we don't need that ref
         (list "if (" c-var ") g_object_unref ((GObject*)" c-var ");\n")
         '())))
  
  (define (c-destructor c-var typespec status-var force?)
    ;; our temp vars are just pointers, there's nothing to clean up
    '())

  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg
                           c->scm-ccg c-destructor "GInterface"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap flags, represented on the scheme side as GValues.
(define (gobject-wrap-flags ws ctype gtype-id)
  ;; flags are just guints...
  (define (c-type-name-func typespec) ctype)

  (define (scm->c-ccg c-var scm-var typespec status-var)
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
     "}\n"))
  
  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     scm-var " = scm_c_make_gvalue (" gtype-id ");\n"
     "g_value_set_flags ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"))
  
  (define (c-destructor c-var typespec status-var force?)
    '())
  
  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg
                           c->scm-ccg c-destructor "GFlags"))

(define (gw-wrap-flags ws ctype values)
  (let* ((enum (gw:wrap-enumeration ws (string->symbol ctype)
                                    ctype))
         (enum-c-sym
          (gw:any-str->c-sym-str (symbol->string (gw:type-get-name enum))))
         (val-alist (map (lambda (l)
                           (cons (string->symbol (caadr l)) 
                                 (cadr (cadr l))))
                         values)))
    (print-info "C Flags" ctype ctype ws)
    enum))


(define (gobject:gwrap-flags ws ctype gtype-id . args)
  (if gtype-id
      (gobject-wrap-flags ws ctype gtype-id)
      (gw-wrap-flags ws ctype (car args))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap enums, just like flags.
(define (gobject:gwrap-enum ws ctype gtype-id . args)
  ;; enums are just guints...
  (define (c-type-name-func typespec) ctype)
  
  (define (scm->c-ccg c-var scm-var typespec status-var)
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
     "}\n"))
  
  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     scm-var " = scm_c_make_gvalue (" gtype-id ");\n"
     "g_value_set_enum ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"))
  
  (define (c-destructor c-var typespec status-var force?)
    '())
  
  (cond
   (gtype-id
    (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg
                             c->scm-ccg c-destructor "GEnum"))
   (else
    ;; Wrap enum without GType
    (print-info "C Enum" ctype ctype ws)
    (let ((values (car args))
          (enum (gw:wrap-enumeration ws (string->symbol ctype) ctype)))
      (for-each 
       (lambda (l)
         (gw:enum-add-value! enum (cadr (cadr l)) (string->symbol (caadr l))))
       values)
      enum))))
  

(define (gobject:gwrap-opaque-pointer ws ctype)
  ;; don't print, defs-support writes a list of these to the log file
  (gw:wrap-as-wct ws (gtype-name->class-name ctype)
                  ctype (string-append "const " ctype)))

(define (gobject:gwrap-class ws ctype gtype-id)
  (gobject:gwrap-helper
   ws ctype
   (lambda (typespec) (list ctype "*"))
   (lambda (c-var scm-var typespec status-var)
     (list "if (g_type_is_a (SCM_SMOB_DATA (scm_slot_ref (" scm-var ", scm_sym_gtype)), " gtype-id "))\n"
           "  " c-var " = (" ctype "*) SCM_SMOB_DATA (scm_slot_ref (" scm-var ", scm_sym_gtype_class));\n"
           "else " `(gw:error ,status-var type ,scm-var)))
   (lambda (scm-var c-var typespec status-var) ; not ideal but ok
     (list scm-var " = scm_c_gtype_lookup_class (G_TYPE_FROM_CLASS (" c-var "));\n"))
   (lambda (c-var typespec status-var force?)
     (list))
   "GObjectClass"))
