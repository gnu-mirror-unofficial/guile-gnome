(define-module (gnome gda gw-gda-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gobject defs-support)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject gw-gobject-spec))


(define-class <gnome-gda-wrapset> (<gobject-wrapset-base>)
  #:language guile #:id 'gnome-gda)

(define-method (initialize (ws <gnome-gda-wrapset>) initargs)
  (next-method ws (append '(#:module (gnome gda gw-gda)) initargs))

  
  (depends-on! ws 'standard 'gnome-gobject 'gnome-glib)

  (add-cs-global-declarator!
      ws (lambda (lang)
           (list "#include \"gda-support.h\"\n")))
  
  (add-cs-initializer!
   ws (lambda (lang error-var)
        (list
         "scm_init_gnome_gda_support ();\n"
         "scm_c_eval_string (\"(use-modules (oop goops))"
         " (define-class <gda-primitive-class> (<class>))"
         " (define-method (compute-slots (class <gda-primitive-class>))"
         "   (cons '(gtype-primitive) (next-method)))"
         " (export <gda-primitive-class>)"
         "\");\n")))
  
  (add-type! ws (make <gda-classed-type>
                  #:ctype "GdaValue"
                  #:gtype-id "GDA_TYPE_VALUE"))
  (add-type! ws (make <gda-classed-type>
                  #:ctype "GdaCommand"
                  #:gtype-id "GDA_TYPE_COMMAND"))
  
  (for-each
   (lambda (elt)
     (add-type-alias! ws (car elt) (cadr elt)))
   '(("GdaValue*" <gda-value>)
     ("GdaCommand*" <gda-command>)))
  
  (load-defs ws "gnome/defs/gda.defs"))

(define-class <gda-classed-type> (<gobject-classed-pointer-type>))

(define-method (initialize (type <gda-classed-type>))
  (next-method type (append '(#:define-class? #f #:wrapped  "Custom")
                            initargs)))

(define-method (initializations-cg (lang <gw-guile>)
                                   (wrapset <gnome-gda-wrapset>)
                                   (type <gda-classed-type>)
                                   status-var)
  (let ((class (symbol->string (class-name type))))
    (list
     (next-method)
     (inline-scheme
      wrapset
      `(define-class ,class ()
         #:gtype ,(string->symbol
                   (string-append
                    "gtype:" (substring class 1 (- (string-length class) 1))))
         #:metaclass <gda-primitive-class>)
      `(export ,(class-name type))))))

(define-method (unwrap-value-cg (lang <gw-guile>)
                                (type <gda-classed-type>)
                                (value <gw-value>)
                                status-var)
  (let ((scm-var (scm-var value))
        (c-var (var value))
        (class (class-name type)))
    (list
     "if (SCM_NFALSEP (scm_call_2 (SCM_VARIABLE_REF (scm_c_lookup (\"is-a?\")),\n"
     "                                  " scm-var ",\n"
     "                                  SCM_VARIABLE_REF (scm_c_lookup (\"" class "\")))))\n"
     "  " c-var " = (" (c-type-name type) ") g_value_get_boxed ((GValue*)SCM_SMOB_DATA (scm_slot_ref (" scm-var ", scm_str2symbol (\"gtype-primitive\"))));\n"
     "else " `(gw:error ,status-var type ,scm-var))))

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <gda-classed-type>)
                              (value <gw-value>)
                              status-var)
  (let ((scm-var (scm-var value))
        (c-var (var value)))
    (list
     scm-var " = scm_c_make_gtype_primitive_instance (\""
     (class-name type) "\", " (gtype-id type) ", (void *)" c-var ");\n")))

