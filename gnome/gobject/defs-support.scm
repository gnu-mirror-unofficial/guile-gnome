;; Support for reading in Gtk .defs files as g-wrap instructions

(define-module (gnome gobject defs-support)
  :use-module (g-wrap)
  :use-module (gnome gobject gw-spec-utils)
  :use-module (srfi srfi-13)
  :use-module (ice-9 slib)
  :use-module (ice-9 optargs)
  :export (load-defs register-type))

(require 'glob)

;; wrapset-name <-> hash of (cname . gwrap-type-name)
(define types-hash-hash (make-hash-table 31))

(define opaque-types '())

(define (register-type ws-name cname type . args)
  (let* ((types-hash (hash-ref types-hash-hash ws-name)))
    (if (not types-hash)
        (begin (set! types-hash (make-hash-table 31))
               (hash-create-handle! types-hash-hash ws-name types-hash)))
    (if (eq? args '())
        (format #t "New type ~A/~A (~A)\n" cname type ws-name))
    (hash-create-handle! types-hash cname type))
  *unspecified*)

;; g-wrap doesn't export these for us. oh well..
(define (wrapset-get-wrapsets-depended-on ws)
  (let ((rtd (record-type-descriptor ws)))
    ((record-accessor rtd 'wrapsets-depended-on) ws)))
(define (wrapset-get-types-used ws)
  (let ((rtd (record-type-descriptor ws)))
    ((record-accessor rtd 'types-used) ws)))
(define (wrapset-get-wrapped-types ws)
  (let ((rtd (record-type-descriptor ws)))
    ((record-accessor rtd 'wrapped-types) ws)))

;; the first time i've ever used call/cc :-)
(define (recursive-type-find ws type)
  (let* ((ws-name (gw:wrapset-get-name ws))
         (types-hash (hash-ref types-hash-hash ws-name)))
    (if types-hash
        (let ((ret (hash-ref types-hash type)))
          (if ret
              ret
              (call-with-current-continuation
               (lambda (exit)
                 (for-each
                  (lambda (ws)
                    (let ((ret (recursive-type-find ws type)))
                      (if ret
                          (exit ret))))
                  (wrapset-get-wrapsets-depended-on ws))
                 #f))))
        #f)))

;; find the gwrap type name for a given type name in a defs file, or
;; wrap the type as an opaque gpointer -- this thing is getting nasty!
;; i could really use some keyword args here, ugh
(define* (type-lookup ws type return? #:key (ownership #f))
  (let ((ws-name (gw:wrapset-get-name ws))
        (options (list))
        (const? #f)
        (gwrap-type-name #f))
    (cond
     ((string-prefix? "const-" type)
      (set! options (cons (or ownership (if return? 'callee-owned 'caller-owned))
                          options))
      (set! options (cons 'const options))
      (set! const? #t)
      (set! type (substring type (string-length "const-"))))
     ((string-contains type "char*")
      ;; without an explicit value for ownership, we only assume that we
      ;; own non-const strings
      (set! options (cons (or ownership (if return? 'caller-owned 'callee-owned))
                          options)))
     ((string-index type #\*)
      ;; other things represented by pointers are treated conservatively
      (set! options (cons (or ownership (if return? 'callee-owned 'caller-owned))
                          options))))

    ;; support GList*-of-GtkWindow*
    (if (string-contains type "-of-")
        (begin (set! options (cons
                              (type-lookup
                               ws
                               (substring type (+ (string-contains type "-of-") 4))
                               return?)
                              options))
               (set! type (substring type 0 (string-contains type "-of-")))))

    (set! gwrap-type-name (recursive-type-find ws type))
    (if (not gwrap-type-name)
        (let ((wrapped-type #f))
          (set! wrapped-type (gobject:gwrap-opaque-pointer ws type))
          (set! gwrap-type-name (gw:type-get-name wrapped-type))
          (if (eq? (length opaque-types) 0)
              (gw:wrapset-depends-on ws "gw-wct"))
          (set! opaque-types (cons (list type gwrap-type-name) opaque-types))
          (register-type ws-name type gwrap-type-name #f)))

    ;; gw:wct does not take caller/callee owned type options
    (if (null? options)
        gwrap-type-name
        (if (or-map (lambda (pair) (equal? (car pair) type)) opaque-types)
            (if const? (cons gwrap-type-name '(const)) gwrap-type-name)
            (cons gwrap-type-name options)))))

(define (load-defs ws file . already-included)
  (let* ((old-load-path %load-path)
         (log-file-name (string-append (gw:wrapset-get-name ws)
                                                ".log"))
         (log-file (open-output-file log-file-name))
         (overrides '())
         (bad-methods '())
         (num-types 0)
         (num-functions 0)
         (ignore-matchers '())
         (methods-used? #f))

    (format #f "Loading defs file \"~S\"..." file)

    ;; hm, should use dynamic-wind here...
    (set! %load-path (list (dirname (%search-load-path file))))
    (set! file (basename file))

    (let* ((scan-type!
            (lambda (gwrap-function args)
              (let* ((ctype #f)
                     (gtype-id #f)
                     (wrapped-type #f))
                (set! num-types (1+ num-types))
                (for-each
                 (lambda (arg)
                   (case (car arg) 
                     ;; The gtype system has enough
                     ;; introspection power that we can
                     ;; disregard a lot of the information in
                     ;; the defs files and just look at the
                     ;; ctype and gtype-id.
                     ((gtype-id) (set! gtype-id (cadr arg)))
                     ((c-name) (set! ctype (cadr arg)))))
                 args)

                (if (or (not gtype-id) (not ctype))
                    (error "Type lacks a c-name or gtype-id:\n\n" args))

                (set! wrapped-type (gwrap-function ws ctype gtype-id))
                (register-type (gw:wrapset-get-name ws)
                               (if (memv gwrap-function (list
                                                         gobject:gwrap-flags
                                                         gobject:gwrap-enum))
                                   ctype
                                   (string-append ctype "*"))
                               (gw:type-get-name wrapped-type))
                wrapped-type)))

           (add-method-property-with-generic-name
            (lambda (func-name generic-name of-object)
              (if (not methods-used?)
                  (begin
                    (gw:wrapset-add-cs-wrapper-declarations!
                     ws
                     (lambda (wrapset client-wrapset)
                       (if (not client-wrapset)
                           "SCM sym_of_object, sym_generic_name;\n"
                           '())))
                    (gw:wrapset-add-cs-wrapper-initializers!
                     ws
                     (lambda (wrapset client-wrapset status-var)
                       (if (not client-wrapset)
                           (list "sym_of_object = scm_permanent_object (scm_str2symbol (\"of-object\"));\n"
                                 "sym_generic_name = scm_permanent_object (scm_str2symbol (\"generic-name\"));\n")
                           '())))
                    (set! methods-used? #t)))

              (gw:wrapset-add-cs-wrapper-initializers!
               ws
               (lambda (wrapset client-wrapset status-var)
                 (if (not client-wrapset)
                     (list "scm_set_procedure_property_x (SCM_VARIABLE_REF (scm_c_lookup (\""
                           func-name "\")), sym_of_object, scm_str2symbol(\"" of-object "\"));\n"
                           "scm_set_procedure_property_x (SCM_VARIABLE_REF (scm_c_lookup (\""
                           func-name "\")), sym_generic_name, scm_str2symbol(\"" generic-name "\"));\n")
                     '())))))

           (add-method-property
            (lambda (func-name of-object)
              (let ((generic-name #f)
                    (sanitized-of-obj (substring of-object 1
                                                 (or (string-index of-object #\*)
                                                     (1- (string-length of-object))))))
                (if (string-prefix? (string-append sanitized-of-obj "-") func-name)
                    (add-method-property-with-generic-name
                     func-name
                     (string->symbol
                      (substring func-name (1+ (string-length sanitized-of-obj))))
                     of-object)
                    (set! bad-methods (cons (list func-name of-object)
                                                   bad-methods))))))

           (ignored?
            (lambda (c-name)
              (or-map (lambda (matcher) (matcher c-name))
                      ignore-matchers)))

           (scan-function!
            (lambda (is-method? name args)
              (call-with-current-continuation
               (lambda (exit)
                 (let ((c-name #f)
                       (scm-name #f)
                       (of-object #f)
                       (return-type "none")
                       (caller-owns-return #f)
                       (parameters (list)))
                   (for-each
                    (lambda (arg)
                      (case (car arg)
                        ((return-type)
                         (set! return-type (cadr arg)))
                        ((caller-owns-return)
                         (set! caller-owns-return (cadr arg)))
                        ((is-constructor-of)
                         ;; so, if the caller really doesn't own it (as
                         ;; in gtkwindow), put (caller-owns-return #f)
                         ;; after the (is-constructor-of)
                         (set! caller-owns-return #t))
                        ((parameters)
                         (if (memq #t 
                                   (map (lambda (name)
                                          (eq? (string-ref name 0) #\())
                                        (map cadadr (cdr arg))))
                             (begin
                               (format #t "\nWarning, not binding function ~A because I can't deal with function pointers\n"
                                       name)
                               (exit #f)))
                         (set! parameters (map cadr (cdr arg))))
                        ((of-object)
                         (set! of-object (string-append (cadr arg) "*")))
                        ((overrides)
                         (set! scm-name (glib:func-cname->symbol (cadr arg)))
                         (if (member (cadr arg) overrides)
                             (error "Function ~S already overridden" (cadr arg))
                             (set! overrides (cons (cadr arg) overrides))))
                        ((c-name)
                         (set! c-name (cadr arg))
                         (if (ignored? c-name)
                             (begin (display "x") (exit #f)))
                         (if (member c-name overrides)
                             (exit #f)))))
                    args)
                   
                   (set! num-functions (1+ num-functions))

                   (if (not scm-name)
                       (set! scm-name (glib:func-cname->symbol c-name)))

                   (if is-method?
                       (if (not of-object)
                           (error "Method name lacks an of-object!" c-name)
                           (set! parameters (cons (list of-object "self")
                                                    parameters))))

                   (display ".")
                   (gw:wrap-function
                    ws
                    scm-name
                    (type-lookup ws return-type #t
                                 #:ownership (if caller-owns-return
                                                 'caller-owned
                                                 'callee-owned))
                    c-name
                    (let ((parse-restargs
                           (lambda (restargs)
                             (let ((options (list)))
                               (for-each
                                (lambda (restarg)
                                  (case (car restarg)
                                    ((null-ok)
                                     (set! options (cons 'null-ok options)))))
                                restargs)
                               options))))
                      (map (lambda (defs-parameter-spec)
                             (let ((looked-up (type-lookup
                                               ws
                                               (car defs-parameter-spec)
                                               #f))
                                   (parsed (parse-restargs
                                            (cddr defs-parameter-spec)))
                                   (arg-name (string->symbol
                                              (cadr defs-parameter-spec))))
                               (if (list? looked-up)
                                   (list (append looked-up parsed) arg-name)
                                   (list looked-up arg-name))))
                           parameters)))
                   (if is-method?
                       (add-method-property
                        (symbol->string scm-name)
                        (symbol->string (recursive-type-find ws of-object))))))))))

      (letrec ((define-enum (procedure->syntax
                             (lambda (x env)
                               (scan-type! gobject:gwrap-enum (cddr x)))))
               (define-flags (procedure->syntax
                              (lambda (x env)
                                (scan-type! gobject:gwrap-flags (cddr x)))))
               (define-object (procedure->syntax
                               (lambda (x env)
                                 (scan-type! gobject:gwrap-object (cddr x)))))
               (define-interface (procedure->syntax
                                  (lambda (x env)
                                    (scan-type! gobject:gwrap-interface (cddr x)))))
               (define-pointer (procedure->syntax
                                (lambda (x env)
                                  (scan-type! gobject:gwrap-pointer (cddr x)))))
               (define-boxed (procedure->syntax
                              (lambda (x env)
                                (scan-type! gobject:gwrap-boxed (cddr x)))))
               
               (define-function (procedure->syntax
                                 (lambda (x env)
                                   (scan-function! #f (cadr x) (cddr x)))))
               (define-method (procedure->syntax
                               (lambda (x env)
                                 (scan-function! #t (cadr x) (cddr x)))))

               (ignore (lambda args
                         (for-each
                          (lambda (c-name)
                            (if (member c-name overrides)
                                (error "Function ~S already overridden" c-name)
                                (set! overrides (cons c-name overrides))))
                          args)))

               (ignore-glob (lambda args
                              (for-each
                               (lambda (glob)
                                 (set! ignore-matchers
                                       (cons (glob:match?? glob) ignore-matchers)))
                               args)))

               (include (lambda (file)
                          (if (not (member file already-included))
                              (let* ((filename (%search-load-path file)))
                                (if (not filename)
                                    (error "Could not find file" file)
                                    (let* ((port (open-input-file filename))
                                           (sexp (read port)))
                                      (set! already-included (cons file already-included))
                                      (while (not (eof-object? sexp))
                                             (local-eval sexp (the-environment))
                                             (set! sexp (read port))))))))))

        
        ;; we're now in an environment to interpret the defs files natively.
        ;; nice :-)

        (include file)

        ;; g-wrap will only output type initialization code (ie,
        ;; gtype->class stuff) for types that are actually used in the
        ;; api. some types, however, do not show up in the api --
        ;; <gtk-hbox>, for instance. Of course we want to be able to
        ;; (make <gtk-hbox>), so we just say that all of the types are
        ;; used by the wrapset.

        (let ((gw-types-used (wrapset-get-types-used ws)))
          (for-each (lambda (pair)
                      (hashq-set! gw-types-used
                                  (cdr pair)
                                  (cdr pair)))
                    (wrapset-get-wrapped-types ws)))))

    (format log-file "Opaque types in the ~A wrapset: c-name scm-name\n\n"
            (gw:wrapset-get-name ws))
    (for-each
     (lambda (pair)
       (format log-file "~A ~A\n" (car pair) (cadr pair)))
     opaque-types)

    (format log-file "\n\nBad method names in the ~A wrapset: c-name of-object\n\n"
            (gw:wrapset-get-name ws))
    (for-each
     (lambda (pair)
       (format log-file "~A ~A\n" (car pair) (cadr pair)))
     bad-methods)

    (close log-file)

    (format #t "\n\nWrapped ~A types (~A opaque) and ~A functions.\n"
            (+ num-types (length opaque-types)) (length opaque-types) num-functions)
    (format #t "A list of opaque types and bad method names has been written to ~A.\n\n"
            log-file-name)

    (set! %load-path old-load-path)))
