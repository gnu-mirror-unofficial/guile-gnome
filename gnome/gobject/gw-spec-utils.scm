;; things that are useful to *-spec.scm g-wrap files.

(define-module (gnome gobject gw-spec-utils)
  :use-module (g-wrap)
  :use-module (g-wrap gw-wct-spec)
  :use-module (ice-9 optargs)
  :export (glib:type-cname->symbol
           glib:func-cname->symbol
           gobject:gwrap-helper
           gobject:gwrap-object
           gobject:gwrap-boxed
           gobject:gwrap-pointer
           gobject:gwrap-opaque-pointer
           gobject:gwrap-interface
           gobject:gwrap-flags
           gobject:gwrap-enum))

;; (c-name->scm-name "GtkAccelGroup") => "gtk-accel-group"
;; (c-name->scm-name "gtk_accel_group") => "gtk-accel-group"
(define (c-name->scm-name cname)
  (letrec ((schemify (lambda (lst last-lower)
                       (if (null? lst)
                           lst
                           (if (char-upper-case? (car lst))
                               (if last-lower
                                   (cons #\- (cons (char-downcase (car lst))
                                                   (schemify (cdr lst) #f)))
                                   (cons (char-downcase (car lst))
                                         (schemify (cdr lst) #f)))
                               (case (car lst)
                                 ((#\_) (cons #\- (schemify (cdr lst)
                                                            #f)))
                                 (else (cons (car lst) (schemify (cdr lst) #t)))))))))
    (list->string (schemify (string->list cname) #f))))

;; (glib:type-cname->symbol "GtkAccelGroup") => <gtk-accel-group>
(define (glib:type-cname->symbol cname)
  (string->symbol (string-append "<" (c-name->scm-name cname) ">")))

;; (glib:type-cname->symbol "GtkAccelGroup") => gtk-accel-group
(define (glib:func-cname->symbol cname)
  (string->symbol (c-name->scm-name cname)))

(define (gwrap-helper-with-class ws gtype-id ctype c-type-name-func
                                 scm->c-ccg c->scm-ccg c-destructor)
  (if (not (and (string? gtype-id) (string? ctype) (closure? c-type-name-func)
                (closure? scm->c-ccg) (closure? c->scm-ccg) (closure? c-destructor)))
      (error "Bad arguments to gwrap-helper-with-class."))
  (let ((t (gwrap-helper ws ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor)))
    (gw:type-set-global-initializations-ccg!
     t
     (lambda (type client-wrapset status-var)
       (if client-wrapset
           (list "scm_c_define (\"" (symbol->string (glib:type-cname->symbol ctype)) "\",\n"
                 "              scm_call_1 (scm_sym_gtype_to_class,\n"
                 "                          scm_c_register_gtype (" gtype-id ")));\n"
                 "scm_c_export (\"" (symbol->string (glib:type-cname->symbol ctype)) "\", NULL);\n")
           '())))
    t))

(define* (gwrap-helper ws ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor
                       #:key (type-sym #f))
  (let ((type-sym (or type-sym (glib:type-cname->symbol ctype))))
    (define wrapped-type (gw:wrap-type ws type-sym))

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

    wrapped-type))

(define gobject:gwrap-helper gwrap-helper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap objects.
(define (gobject:gwrap-object ws ctype gtype-id)
  (define (c-type-name-func typespec)
    (if (memq 'const (gw:typespec-get-options typespec))
        (string-append "const " ctype " *")
        (string-append ctype " *")))

  (define (scm->c-ccg c-var scm-var typespec status-var)
    (list
     "if (SCM_FALSEP (" scm-var ")) {\n"
     "  " c-var " = NULL;\n"
     "} else {\n"
     "  " c-var " = (" (c-type-name-func typespec) ") scm_c_scm_to_gtype_instance (" scm-var ", " gtype-id ");\n"
     "  if (" c-var " == NULL)\n"
     `(gw:error ,status-var type ,scm-var)
     "}\n"))
  
  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else\n"
     "  " scm-var " = scm_c_gtype_instance_to_scm ((GTypeInstance *)" c-var ");\n"
     (if (memq 'caller-owned (gw:typespec-get-options typespec))
         '()
         (list "g_object_ref ((GObject*)" c-var ");\n"))))
  
  (define (c-destructor c-var typespec status-var force?)
    ;; our temp vars are just pointers, there's nothing to clean up
    '())

  (format #f "Wrapping type ~A as a GObject...\n" ctype)
  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor))

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
     "if (SCM_FALSEP (" scm-var "))\n"
     "  " c-var " = NULL;\n"
     "else if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "         && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
     "  " c-var " = (" (c-type-name-func typespec) ") g_value_get_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
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
  
  (format #f "Wrapping type ~A as a GBoxed...\n" ctype)
  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor))

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
     "if (SCM_FALSEP (" scm-var "))\n"
     "  " c-var " = NULL;\n"
     "else if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "         && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
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
  
  (format #f "Wrapping type ~A as a gpointer...\n" ctype)

  ;; don't create the gtype class? dunno. this function is often called
  ;; with gtype-id == G_TYPE_POINTER.
  (gwrap-helper ws ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap interfaces, for now just as opaque pointers...
(define (gobject:gwrap-interface ws ctype gtype-id)
  (define (c-type-name-func typespec)
    (if (memq 'const (gw:typespec-get-options typespec))
        (string-append "const " ctype "*")
        (string-append ctype "*")))

  (define (scm->c-ccg c-var scm-var typespec status-var)
    (list
     "if (SCM_FALSEP (" scm-var "))\n"
     "  " c-var " = NULL;\n"
     "else if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "         && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
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
  
  (format #f "Wrapping type ~A as a gpointer...\n" ctype)
  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap flags, represented on the scheme side as GValues.
(define (gobject:gwrap-flags ws ctype gtype-id)
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
  
  (format #f "Wrapping type ~A as a GFlags...\n" ctype)
  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap enums, just like flags.
(define (gobject:gwrap-enum ws ctype gtype-id)
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
  
  (format #f "Wrapping type ~A as a GEnum...\n" ctype)
  (gwrap-helper-with-class ws gtype-id ctype c-type-name-func scm->c-ccg c->scm-ccg c-destructor))

(define (gobject:gwrap-opaque-pointer ws ctype)
  (gw:wrap-as-wct ws (glib:type-cname->symbol ctype)
                  ctype (string-append "const " ctype)))
