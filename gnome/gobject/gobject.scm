;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
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
;; This is the GObject wrapper for Guile.
;;
;; See the guile-gnome tutorial for more details.
;;
;;; Code:

(define-module (gnome gobject gobject)
  :use-module (oop goops)
  :use-module (ice-9 documentation) ;; for define-with-docs
  :use-module (gnome gobject utils)
  :use-module (gnome gobject gtype)
  :use-module (gnome gobject gvalue)
  :use-module (gnome gobject gparameter)
  :use-module (gnome gobject gsignal)

  :export     (;; Classes
               <gobject> <ginterface>
               ;; Low-level subclassing
               gtype-register-static
               ;; Methods to override
               gobject:get-property gobject:set-property
               make-gobject-instance
               ;; Properties
               gobject-class-get-properties gobject-class-find-property
               gobject-class-get-property-names
               gobject-interface-get-properties
               gobject-interface-find-property
               gobject-interface-get-property-names
               gobject-get-property gobject-set-property))

(dynamic-call "scm_init_gnome_gobject"
              (dynamic-link "libguile-gnome-gobject"))

;;;
;;; {Class Initialization}
;;;

(define (vector-map proc vector)
  (let* ((length (vector-length vector))
	 (result-vector (make-vector length)))
    (do ((index 0 (+ index 1)))
	((>= index length) result-vector)
      (vector-set! result-vector index (proc (vector-ref vector index))))))

(define (gtype-class-get-vector-slot class name root-class)
  (define (class-slot-ref-default class name default)
    (if (class-slot-definition class name)
        (class-slot-ref class name)
        default))
  (list->vector
   (let loop ((ancestry (class-precedence-list class)) (ret '()))
     (if (or (null? ancestry) (eq? (car ancestry) root-class))
         ret
         (loop (cdr ancestry)
               (append ret (vector->list (class-slot-ref-default (car ancestry) name #()))))))))

;; Needed by C.
(define (gobject-class-set-properties! class)
  ;; should also work for interfaces.
  (let* ((type (gtype-class->type class))
         (properties-vector (gobject-type-get-properties type))
         (properties
          (vector-map
           (lambda (x)
             (let* ((param-struct (gparam->param-struct x))
                    (param-type (gparam-struct:param-type param-struct))
                    (param-class (gtype->class param-type)))
               (make param-class #:param-struct param-struct)))
           properties-vector)))
    (class-slot-set! class 'gobject-properties properties)))
    
(define-class <gobject-class> (<gtype-instance-class>))
(define-method (initialize (class <gobject-class>) initargs)
  (define (init-gobject-class type)
    (class-slot-set! class 'gsignals (gtype-get-signals type))
    (gobject-class-set-properties! class)
    
    ;; To expose slots as gobject properties, <gobject> will process a
    ;; #:gparam slot option to create a new gobject property.
    (let loop ((slots (class-direct-slots class)))
      (if (not (null? slots))
          (let ((pspec (get-keyword #:gparam
                                    (slot-definition-options (car slots))
                                    #f)))
            (if pspec
                (let* ((name (slot-definition-name (car slots)))
                       (property (apply make (append pspec (list #:name name)))))
                  (gobject-class-install-property class property)))
            (loop (cdr slots)))))
    
    ;; We parse a #:gsignal initialization argument to install signals.
    (let loop ((args initargs))
      (if (not (null? args))
          (if (eq? (car args) #:gsignal)
              (let ((signal (cadr args)))
                (if (not (and (list? signal) (>= (length signal) 2)))
                    (gruntime-error "Invalid signal specification: ~A" signal))
                (let* ((name (car signal))
                       (return-type (or (cadr signal) gtype:void))
                       (param-types (cddr signal))
                       (generic (gtype-class-create-signal class name return-type param-types)))
                  ;; Some magic to define the generic
                  (module-define!
                   (current-module)
                   (gtype-name->method-name (gtype-name type) name) generic)))
              (loop (cddr args))))))

  (define (first-gobject-class cpl)
    (cond
     ((null? cpl) #f)
     ((memq <gobject> (class-precedence-list (car cpl))) (car cpl))
     (else (first-gobject-class (cdr cpl)))))

  ;; real work starts here
  (let ((gtype (cond
                ((get-keyword #:gtype initargs #f)
                 => noop)
                ((get-keyword #:gtype-name initargs #f)
                 => gtype-from-name)
                (else
                 (gtype-register-static
                  (or (get-keyword #:gtype-name initargs #f)
                      (class-name->gtype-name (get-keyword #:name initargs #f)))
                  (gtype-class->type
                   (first-gobject-class
                    (apply append
                           (map class-precedence-list
                                (get-keyword #:dsupers initargs '()))))))))))
    (if (%gtype-lookup-class gtype)
        (gruntime-error "<gtype> ~A already has a GOOPS class, use gtype->class" gtype))

    (%gtype-bind-to-class class gtype)
    (next-method)
    (init-gobject-class gtype)))

;; <gobject> offers a form of slot allocation where properties are set
;; on the GObject directly, not the GOOPS object.
(define-method (compute-get-n-set (class <gobject-class>) s)
  (case (slot-definition-allocation s)
    ((#:instance)
     (let ((sym (slot-definition-name s)))
       (list
        (lambda (o) (gobject-get-data o sym))
        (lambda (o v) (gobject-set-data! o sym v)))))
    (else
     (next-method))))

(define-class <gobject> (<gtype-instance>)
  (gsignals #:allocation #:each-subclass)
  (gobject-properties #:allocation #:each-subclass)
  #:metaclass <gobject-class>
  #:gtype gtype:gobject)

(define-class <ginterface> (<gtype-instance>)
  (gsignals #:allocation #:each-subclass)
  (gobject-properties #:allocation #:each-subclass)
  #:metaclass <gobject-class>
  #:gtype gtype:ginterface)

;;;
;;; {Instance Allocation and Initialization}
;;;

;; This is a method so that it can be extended by subclasses, e.g. so
;; that <gtk-object> can implement explicit destruction.
(define-method (make-gobject-instance class type object options)
  (define (last l)
    (if (null? (cdr l))
        (car l)
        (last (cdr l))))
  (let* ((class-properties (gobject-class-get-properties class))
	 (init-properties '())
         (init-keywords (map slot-definition-init-keyword (class-slots class)))
         (kwargs '()))
    (let loop ((options options) (res '()))
      (cond ((null? options)
             ;; We want to set the keyword args using the <object>
             ;; initialize. Kindof hacky, but doesn't really break any
             ;; rules...
             (set! kwargs res))
	    ((null? (cdr options))
	     (goops-error "malformed argument list" options))
	    ((not (keyword? (car options)))
	     (goops-error "malformed argument list" options))
            ((memq (car options) init-keywords)
             (loop (cddr options) (cons* (car options) (cadr options) res)))
	    (else
	     (let* ((option-value (cadr options))
		    (param-name (keyword->symbol (car options)))
		    (param (or (find-property class-properties param-name)
			       (gruntime-error "No such property in class ~S: ~S" class param-name)))
		    (pspec (gparam->param-struct param))
		    (pspec-value-type (gparam-struct:value-type pspec))
		    (pspec-value (scm->gvalue pspec-value-type option-value)))
	       (set! init-properties
		     (append
		      init-properties (list (cons param-name pspec-value))))
	       (loop (cddr options) res)))))
    (gobject-primitive-create-instance class type object
				       (list->vector init-properties))
    ((method-procedure (last (generic-function-methods initialize))) object kwargs)))

(define (make-ginterface-instance class type instance initargs)
  (let ((value (get-keyword #:value initargs *unspecified*)))
    (if (unspecified? value)
        (gruntime-error "Missing #:value argument initializing an interface"))
    (if (not (is-a? value <gobject>))
        (gruntime-error "Only interfaces implemented by objects are allowed (~A)" value))
    (if (not (gtype-is-a? (gtype-class->type (class-of value))
                          type))
        (gruntime-error "~A's class (~A) does not implement ~A."
                        value (class-of value) class))
    (slot-set! instance 'gtype-instance (slot-ref value 'gtype-instance))))

;; The MOP specifies that make-instance normally calls
;; `allocate-instance' and then `initialize'. We call
;; `allocate-instance', but then we don't call `initialize' for
;; objects, because it is called by the gobject system.
(define-method (make-instance (class <gobject-class>) . initargs)
  (let ((instance (allocate-instance class initargs)))
    (if (get-keyword #:%real-instance initargs #f)
        (slot-set! instance 'gtype-instance (get-keyword #:%real-instance initargs #f))
        (let* ((type (gtype-class->type class))
               (fundamental (gtype->fundamental type)))
          (cond
           ;; GObject
           ((eq? fundamental gtype:gobject)
            (make-gobject-instance class type instance initargs))
           
           ;; GInterface
           ((eq? fundamental gtype:ginterface)
            (make-ginterface-instance class type instance initargs)
            (initialize instance initargs))

           (else (error "yikes!")))))
    instance))

;;;
;;; {GObject Properties}
;;;

(define (find-property vtable name)
  (let loop ((l (vector->list vtable)))
    (if (null? l)
      #f
      (let* ((param-struct (gparam->param-struct (car l)))
	     (pspec-name (gparam-struct:name param-struct)))
	(if (equal? pspec-name name)
	  (car l)
	  (loop (cdr l)))))))

(define (gobject-class-get-properties class)
  "(gobject-class-get-properties class)

Returns a vector of properties belonging to CLASS and all parent
classes."
  (or (memq <gobject> (class-precedence-list class))
      (gruntime-error "Not a subclass of <gobject>: ~S" class))
  (gtype-class-get-vector-slot class 'gobject-properties <gobject>))

(define (gobject-class-get-property-names class)
  "(gobject-class-get-property-names class)

Returns a vector of property names belonging to CLASS and all
parent classes."
  (let* ((properties (gobject-class-get-properties class)))
    (vector-map (lambda (x) (gparam-struct:name (gparam->param-struct x)))
		properties)))

(define (gobject-class-find-property class name)
  "(gobject-class-find-property class name)

Returns a property named NAME (a symbol), belonging to CLASS or one of
its parent classes, or #f if not found."
  (let* ((properties (gobject-class-get-properties class)))
    (find-property properties name)))

(define (gobject-interface-get-properties class)
  "(ginterface-class-get-properties class)

Returns a vector of properties belonging to CLASS and all parent
classes."
  (or (memq <ginterface> (class-precedence-list class))
      (gruntime-error "Not a subclass of <ginterface>: ~S" class))
  (gtype-class-get-vector-slot class 'gobject-properties <ginterface>))

(define (gobject-interface-get-property-names class)
  "(ginterface-class-get-property-names class)

Returns a vector of property names belonging to CLASS and all
parent classes."
  (let* ((properties (gobject-interface-get-properties class)))
    (vector-map (lambda (x) (gparam-struct:name (gparam->param-struct x)))
		properties)))

(define (gobject-interface-find-property class name)
  "(ginterface-class-find-property class name)

Returns a property named NAME (a symbol), belonging to CLASS or one of
its parent classes, or #f if not found."
  (let* ((properties (gobject-interface-get-properties class)))
    (find-property properties name)))

(define (gobject-get-property object name)
  (if (not (is-a? object <gobject>))
      (gruntime-error "Not a <gobject>: ~A" object))
  (if (not (symbol? name))
      (gruntime-error "Not a symbol: ~A" name))
  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (pspec-vector (gobject-class-get-properties class))
	 (pspec (or (find-property pspec-vector name)
		     (gruntime-error "No such property in class ~S: ~S" class name)))
	 (retval (gobject-primitive-get-property instance name)))
    (gvalue->scm retval)))

(define (gobject-set-property object name init-value)
  (if (not (is-a? object <gobject>))
      (gruntime-error "Not a <gobject>: ~A" object))
  (if (not (symbol? name))
      (gruntime-error "Not a symbol: ~A" name))
  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (pspec-vector (gobject-class-get-properties class))
	 (param (or (find-property pspec-vector name)
		    (gruntime-error "No such property in class ~S: ~S" class name)))
	 (param-struct (gparam->param-struct param))
	 (value-type (gparam-struct:value-type param-struct))
	 (value (scm->gvalue value-type init-value)))
    (gobject-primitive-set-property instance name value)))

(define-generic-with-docs gobject:set-property
  "(gobject:set-property obj name value)

Called to set a gobject property. Only properties belonging to (class-of
obj) will come through this function.

Call (next-method) in your methods to invoke the default handler, which
sets slots on the object.")

(define-method (gobject:set-property (object <gobject>) (name <symbol>) value)
  (if (class-slot-definition (class-of object) name)
      (slot-set! object name value)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

(define-generic-with-docs gobject:get-property
  "(gobject:get-property obj name)

Called to get a gobject property. Only properties belonging to (class-of
obj) will come through this function.

Call (next-method) in your methods to invoke the default handler, which
calls (slot-ref obj name).
")

(define-method (gobject:get-property (object <gobject>) (name <symbol>))
  (if (class-slot-definition (class-of object) name)
      (slot-ref object name)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

(%gnome-gobject-object-post-init)
