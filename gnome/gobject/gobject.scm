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
;; GObject is what is commonly understood as @emph{the} object system
;; for GLib. This is not strictly true. GObject is @emph{one}
;; implementation of an object system, built on the other modules:
;; GType, GValue, GParameter, GClosure, and GSignal.
;;
;; Similarly, this Guile module provides integration with the GObject
;; object system, built on the Guile modules that support GType, GValue,
;; GParameter, GClosure, and GSignal.
;;
;; The main class exported by this module is @code{<gobject>}.
;; @code{<gobject>} classes can be subclassed by the user, which will
;; register new subtypes with the GType runtime type system.
;; @code{<gobject>} classes are are also created as needed when wrapping
;; GObjects that come from C, for example from a function's return
;; value.
;;
;; Besides supporting derivation, and signals like other
;; @code{<gtype-instance>} implementations, @code{<gobject>} has the
;; concept of @dfn{properties}, which are @code{<gvalue>}'s associated
;; with the object. The values are constrained by @code{<gparam>}'s,
;; which are associated with the object's class. This module exports the
;; necessary routines to query, get, and set @code{<gobject>}
;; properties.
;;
;; In addition, this module defines the @code{<ginterface>} base class,
;; whose subclasses may be present as mixins of @code{<gobject>}
;; classes. For example:
;;
;; @lisp
;; (use-modules (gnome gtk) (oop goops))
;; (class-direct-supers <gtk-widget>) @result{}
;;    (#<<gobject-class> <atk-implementor-iface> 3033bad0>
;;     #<<gobject-class> <gtk-object> 3034bc90>)
;; @end lisp
;;
;; In this example, we see that @code{<gtk-widget>} has two
;; superclasses, @code{<gtk-object>} and @code{<atk-implementor-iface>}.
;; The second is an interface implemented by the @code{<gtk-widget>}
;; class. See @code{gtype-interfaces} for more details.
;;
;;; Code:

(define-module (gnome gobject gobject)
  #:use-module (oop goops)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject config)
  #:use-module (gnome gobject gtype)
  #:use-module (gnome gobject gvalue)
  #:use-module (gnome gobject gparameter)
  #:use-module (gnome gobject gsignal)

  #:export     (;; Classes
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
              (dynamic-link *guile-gnome-gobject-lib-path*))

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

(define-class-with-docs <gobject> (<gtype-instance>)
  "The base class for GLib's default object system.

@code{<gobject>}'s metaclass understands a new slot option,
@code{#:gparam}, which will export a slot as a @code{<gobject>}
property. The default implementation will set and access the value from
the slot, but you can customize this by writing your own methods for
@code{gobject:set-property} and @code{gobject:get-property}.

In addition, the metaclass also understands @code{#:gsignal} arguments,
which define signals on the class, and define the generics for the
default signal handler. See @code{gtype-class-define-signal} for more
information.

For example:
@lisp
 ;; deriving from <gobject>
 (define-class <test> (<gobject>)
  ;; a normal object slot
  my-data

  ;; an object slot exported as a gobject property
  (pub-data #:gparam (list <gparam-long> #:name 'test))

  ;; likewise, using non-default parameter settings
  (foo-data #:gparam (list <gparam-long> #:name 'foo
                           #:minimum -3 #:maximum 1000
                           #:default-value 42))

  ;; a signal with no arguments and no return value
  #:gsignal '(frobate #f)

  ;; a signal with arguments and a return value
  #:gsignal (list 'frobate <gboolean> <gint> <glong>))

 ;; deriving from <test> -- also inherits properties and signals
 (define-class <hungry> (<test>))
@end lisp
"
  (gsignals #:allocation #:each-subclass)
  (gobject-properties #:allocation #:each-subclass)
  #:metaclass <gobject-class>
  #:gtype gtype:gobject)

(define-class-with-docs <ginterface> (<gtype-instance>)
  "The base class for GLib's interface types. Not derivable in Scheme."
  (gsignals #:allocation #:each-subclass)
  (gobject-properties #:allocation #:each-subclass)
  #:metaclass <gobject-class>
  #:gtype gtype:ginterface)

;;;
;;; {Instance Allocation and Initialization}
;;;

;; By the time this function exits, the object should be initialized.
(define-generic-with-docs make-gobject-instance
  "A generic defined to initialize a newly created @code{<gobject>}
instance. @code{make-gobject-instance} takes four arguments: the class
of the object, its @code{<gtype>}, the object itself, and the options,
which is a list of keyword arguments.

This operation is a generic function so that subclasses can override it,
e.g. so that @code{<gtk-object>} can implement explicit destruction.")
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
			       (gruntime-error
                                "No such property or init-keyword in class ~S: ~S"
                                class param-name)))
		    (pspec (gparam->param-struct param))
		    (pspec-value-type (gparam-struct:value-type pspec))
		    (pspec-value (scm->gvalue pspec-value-type option-value)))
	       (set! init-properties
		     (append
		      init-properties (list (cons param-name pspec-value))))
	       (loop (cddr options) res)))))
    (with-fluids ((%gobject-initargs kwargs))
       (gobject-primitive-create-instance class type object
                                          (list->vector init-properties))
       ;; If the object was a scheme-derived type, `initialize' was
       ;; already called on the object by the callback from
       ;; scm_c_gtype_instance_instance_init. Otherwise, although I know
       ;; from looking at the source that all it does is process keyword
       ;; arguments (which we know to be null from the above
       ;; option-filtering code), we should (and do) call the <object>
       ;; initializer on the instance.
       (if (not (scheme-gclass? class))
           (initialize object kwargs)))))

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
  "Returns a vector of properties belonging to @var{class} and all
parent classes."
  (or (memq <gobject> (class-precedence-list class))
      (gruntime-error "Not a subclass of <gobject>: ~S" class))
  (gtype-class-get-vector-slot class 'gobject-properties <gobject>))

(define (gobject-class-get-property-names class)
  "Returns a vector of property names belonging to @var{class} and all
parent classes."
  (let* ((properties (gobject-class-get-properties class)))
    (vector-map (lambda (x) (gparam-struct:name (gparam->param-struct x)))
		properties)))

(define (gobject-class-find-property class name)
  "Returns a property named @var{name} (a symbol), belonging to
@var{class} or one of its parent classes, or @code{#f} if not found."
  (let* ((properties (gobject-class-get-properties class)))
    (find-property properties name)))

(define (gobject-interface-get-properties class)
  "Returns a vector of properties belonging to @var{class} and all
parent classes."
  (or (memq <ginterface> (class-precedence-list class))
      (gruntime-error "Not a subclass of <ginterface>: ~S" class))
  (gtype-class-get-vector-slot class 'gobject-properties <ginterface>))

(define (gobject-interface-get-property-names class)
  "Returns a vector of property names belonging to @var{class} and all
parent classes."
  (let* ((properties (gobject-interface-get-properties class)))
    (vector-map (lambda (x) (gparam-struct:name (gparam->param-struct x)))
		properties)))

(define (gobject-interface-find-property class name)
  "Returns a property named @var{name} (a symbol), belonging to
@var{class} or one of its parent classes, or @code{#f} if not found."
  (let* ((properties (gobject-interface-get-properties class)))
    (find-property properties name)))

(define (gobject-get-property object name)
  "Gets a the property named @var{name} (a symbol) from @var{object}."
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
  "Sets the property named @var{name} (a symbol) on @var{object} to
@var{init-value}."
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
  "Called to set a gobject property. Only properties directly belonging
to the object's class will come through this function; superclasses
handle their own properties.

Takes three arguments: the object, the property name, and the value.

Call @code{(next-method)} in your methods to invoke the default handler,
which sets slots on the object.")

(define-method (gobject:set-property (object <gobject>) (name <symbol>) value)
  (if (class-slot-definition (class-of object) name)
      (slot-set! object name value)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

(define-generic-with-docs gobject:get-property
  "Called to get a gobject property. Only properties directly belonging
to the object's class will come through this function; superclasses
handle their own properties.

Takes two arguments: the object and the property name.

Call @code{(next-method)} in your methods to invoke the default handler,
calls @code{(slot-ref obj name)}.")

(define-method (gobject:get-property (object <gobject>) (name <symbol>))
  (if (class-slot-definition (class-of object) name)
      (slot-ref object name)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

(%gnome-gobject-object-post-init)
