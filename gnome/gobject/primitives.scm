;;; Commentary:
;;
;; This is the GObject wrapper for Guile.
;;
;;; Code:

(define-module (gnome gobject primitives)
  :use-module (oop goops)
  :use-module (ice-9 documentation)
  :use-module (gnome gobject gw-gobject)
  :re-export (%init-gnome-gobject
              %post-init-gnome-gobject
              gtype-name
              gtype-from-name
              gtype-parent
              gtype-is-a?
              g-source-set-closure))


(define %gruntime-debug #t)

(define (gruntime-error format-string . args)
  (save-stack)
  (scm-error 'gruntime-error #f format-string args '()))

(define (gruntime-debug format-string . args)
  (if %gruntime-debug
      (apply format (append (list #t (string-append "gruntime: "
                                                    format-string))
                            args)))
  *unspecified*)

(defmacro define-public-with-docs args
  (define (syntax)
    (error "bad syntax" (list 'define-public args)))
  (define (get-name n)
    (cond
      ((symbol? n) n)
      ((pair? n) (get-name (car n)))
      (else (syntax))))
  (define (get-documentation n)
    (cond
      ((and (pair? n) (string? (car n))) (car n))
      (else (syntax))))
  (cond
    ((null? args)
     (syntax))
    (#t
     (let ((name (get-name (car args)))
	   (object-documentation (get-documentation (cdr args))))
       `(begin
	  (define-private ,(car args) ,@(cddr args))
	  (set-object-property! ,name 'documentation ,object-documentation)
	  (eval-case ((load-toplevel) (export ,name))))))))

(define-macro (define-generic-with-docs name documentation)
  `(define-public-with-docs ,name ,documentation
     (make-generic ',name)))

(define-class <gtype-class-meta> (<class>))

(define (create-set-once-g-n-s class s class-slot?)
  (let* ((already-allocated (slot-ref class 'nfields))
         (name (slot-definition-name s))
         (get (lambda (x) (%get-struct-slot (if class-slot? class x)
                                            already-allocated)))
         (set (lambda (x o) (if (not (get x))
                                (%set-struct-slot! (if class-slot? class x)
                                                   already-allocated
                                                   o)
                                (gruntime-error "set-once slot already set: ~S"
                                                name)))))
    (slot-set! class 'nfields (+ already-allocated 1))
    (list get set)))

(define-method (compute-get-n-set (class <gtype-class-meta>) s)
  (case (slot-definition-allocation s)
    ((#:set-once)
     (create-set-once-g-n-s class s #f))

    ((#:each-subclass)
     (if (eq? (slot-definition-name s) 'gtype)
        (create-set-once-g-n-s class s #t)
        (next-method)))

    ;; Chain up for the default allocation methods...
    (else (next-method))))

(define-class <gtype-class> (<gtype-class-meta>)
  (gtype #:allocation #:each-subclass)
  (gtype-instance #:allocation #:set-once)
  #:metaclass <gtype-class-meta>)

(define-generic gtype-instance:write)

(%init-gnome-gobject-primitives)

(define (find-enum vtable func index)
  (let loop ((l (vector->list vtable)))
    (if (null? l)
	#f
	(begin
	  (if (equal? (func (car l)) index)
	      (car l)
	      (loop (cdr l)))))))

(define (enum-by-index type index)
  (find-enum (genum-primitive-get-values type) (lambda (l) (caddr l)) index))

(define (enum-by-name type name)
  (find-enum (genum-primitive-get-values type) (lambda (l) (cadr l)) name))

(define (enum-by-symbol type symbol)
  (find-enum (genum-primitive-get-values type) (lambda (l) (car l)) symbol))

(define (flags-by-index type index)
  (find-enum (gflags-primitive-get-values type) (lambda (l) (caddr l)) index))

(define (flags-by-name type name)
  (find-enum (gflags-primitive-get-values type) (lambda (l) (cadr l)) name))

(define (flags-by-symbol type symbol)
  (find-enum (gflags-primitive-get-values type) (lambda (l) (car l)) symbol))

(define (genum->value-table obj)
  (if (gvalue? obj)
    (genum-primitive-get-values (gvalue->type obj))
    (genum-primitive-get-values obj)))

(define (genum->symbol obj)
  (let* ((type (gvalue->type obj))
	 (enum-values (genum-primitive-get-values type))
	 (value (gvalue-primitive-get obj))
	 (the-value (enum-by-index type value)))
    (car the-value)))

(define (genum->name obj)
  (let* ((type (gvalue->type obj))
	 (enum-values (genum-primitive-get-values type))
	 (value (gvalue-primitive-get obj))
	 (the-value (enum-by-index type value)))
    (cadr the-value)))

(define (gflags->element-list obj)
  (let* ((type (gvalue->type obj))
	 (flags-values (gflags-primitive-get-values type))
	 (value (gvalue-primitive-get obj))
	 (element-list '()))
    (for-each (lambda (x)
		(let ((f (caddr x)))
		  (if (gflags-primitive-bit-set? value f)
		    (set! element-list (append! element-list (list x))))))
	      (vector->list flags-values))
    element-list))

(define (gflags->symbol-list obj)
  (let* ((element-list (gflags->element-list obj)))
    (map (lambda (x)
	   (car x))
	 element-list)))

(define (gflags->name-list obj)
  (let* ((element-list (gflags->element-list obj)))
    (map (lambda (x)
	   (cadr x))
	 element-list)))

(define (gflags->value-list obj)
  (let* ((element-list (gflags->element-list obj)))
    (map (lambda (x)
	   (caddr x))
	 element-list)))

(define (gsignal:id signal)
  (struct-ref signal gsignal-id))

(define (gsignal:name signal)
  (struct-ref signal gsignal-name))

(define (gsignal:interface-type signal)
  (struct-ref signal gsignal-interface-type))

(define (gsignal:return-type signal)
  (struct-ref signal gsignal-return-type))

(define (gsignal:param-types signal)
  (struct-ref signal gsignal-param-types))

(define (gparam-spec:name pspec)
  (struct-ref pspec gparam-spec-name))

(define (gparam-spec:nick pspec)
  (struct-ref pspec gparam-spec-nick))

(define (gparam-spec:blurb pspec)
  (struct-ref pspec gparam-spec-blurb))

(define (gparam-spec:flags pspec)
  (struct-ref pspec gparam-spec-flags))

(define (gparam-spec:param-type pspec)
  (struct-ref pspec gparam-spec-param-type))

(define (gparam-spec:value-type pspec)
  (struct-ref pspec gparam-spec-value-type))

(define (gparam-spec:owner-type pspec)
  (struct-ref pspec gparam-spec-owner-type))

(define (gparam-spec:args pspec)
  (struct-ref pspec gparam-spec-args))

(export <gtype-class> <gtype-class-meta>
        %gruntime-debug gruntime-error gruntime-debug
	gtype-instance:write gtype-instance-primitive
	define-public-with-docs define-generic-with-docs
	enum-by-index enum-by-symbol enum-by-name 
	flags-by-index flags-by-symbol flags-by-name 
	genum->value-table genum->symbol genum->name
	gflags->element-list gflags->symbol-list gflags->name-list
	gflags->value-list
	%gtype-bind-to-class %gtype-lookup-class
	gsignal:id gsignal:name gsignal:interface-type gsignal:return-type
	gsignal:param-types gparam-spec:name gparam-spec:nick gparam-spec:blurb
	gparam-spec:flags gparam-spec:param-type gparam-spec:value-type
	gparam-spec:owner-type gparam-spec:args
	)
