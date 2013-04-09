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
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:use-module (oop goops)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject config)
  #:use-module (gnome gobject gtype)
  #:use-module (gnome gobject gvalue)
  #:use-module (gnome gobject gparameter)
  #:use-module (gnome gobject gsignal)

  #:export     (;; Classes
                <gobject> <ginterface> <gparam-object>
                ;; Low-level subclassing
                gtype-register-static
                ;; Methods to override
                gobject:get-property gobject:set-property
                ;; Properties
                gobject-class-get-properties gobject-class-find-property
                gobject-class-get-property-names
                gobject-get-property gobject-set-property))

(dynamic-call "scm_init_gnome_gobject"
              (dynamic-link *guile-gnome-gobject-lib-path*))

;;;
;;; {Class Initialization}
;;;

(define-class <gobject-class> (<gtype-class>))

(define-method (compute-slots (class <gobject-class>))
  (define (compute-extra-slots props slots)
    (filter-map (lambda (prop)
                  (and (not (assq prop slots))
                       `(,prop #:allocation #:gproperty)))
                props))
  (let* ((slots (next-method))
         (extra (compute-extra-slots
                 (gobject-class-get-property-names class) slots)))
    (with-accessors (direct-slots)
      (set! (direct-slots class) (append (direct-slots class) extra)))
    (append slots extra)))

(define-method (compute-get-n-set (class <gobject-class>) slotdef)
  (let ((name (slot-definition-name slotdef)))
    (case (slot-definition-allocation slotdef)
      ((#:gproperty) (list (lambda (o) (gobject-get-property o name))
                           (lambda (o v) (gobject-set-property o name v))))
      (else (next-method)))))

(define-method (initialize (class <gobject-class>) initargs)
  (define (install-properties!)
    ;; To expose slots as gobject properties, <gobject> will process a
    ;; #:gparam slot option to create a new gobject property.
    (for-each
     (lambda (slot)
       (let ((pspec (get-keyword #:gparam (slot-definition-options slot) #f)))
         (if pspec
             (gobject-class-install-property
              class
              (apply make
                     (car pspec) #:name (slot-definition-name slot)
                     (cdr pspec))))))
     (class-direct-slots class)))
    
  (define (install-signals!)
    ;; We parse a #:gsignal initialization argument to install signals.
    (let loop ((args initargs))
      (if (not (null? args))
          (begin
            (if (eq? (car args) #:gsignal)
                (let ((signal (cadr args)))
                  (if (not (and (list? signal) (>= (length signal) 2)))
                      (gruntime-error "Invalid signal specification: ~A" signal))
                  (let* ((name (car signal))
                         (return-type (cadr signal))
                         (param-types (cddr signal))
                         (generic (gtype-class-create-signal class name return-type param-types)))
                    ;; Some magic to define the generic
                    (module-define! (current-module)
                                    (generic-function-name generic) generic))))
            (loop (cddr args))))))

  (define (first pred list)
    (cond ((null? list) #f)
          ((pred (car list)) (car list))
          (else (first pred (cdr list)))))
  (define (gobject-class? c)
    (memq <gobject> (class-precedence-list c)))

  ;; real work starts here

  (next-method
   class
   (cons*
    #:gtype-name
    (or (get-keyword #:gtype-name initargs #f)
        (gtype-register-static
         (class-name->gtype-name (get-keyword #:name initargs #f))
         (first gobject-class?
                (apply append
                       (map class-precedence-list
                            (get-keyword #:dsupers initargs '()))))))
    initargs))
  (install-properties!)
  (install-signals!))

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

@code{<gobject>} classes also expose a slot for each GObject property
defined on the class, if such a slot is not already defined.
"
  ;; add a slot for signal generics instead of module-define! ?
  #:metaclass <gobject-class>
  #:gtype-name "GObject")

(define-class-with-docs <ginterface> (<gtype-instance>)
  "The base class for GLib's interface types. Not derivable in Scheme."
  #:metaclass <gobject-class>
  #:gtype-name "GInterface")

(define (class-is-a? x is-a)
  (memq is-a (class-precedence-list x)))

(define-class-with-docs <gparam-object> (<gparam>)
  "Parameter for @code{<gobject>} values."
  (object-type
   #:init-keyword #:object-type #:allocation #:checked
   #:pred (lambda (x) (is-a? x <gobject-class>)))
  #:value-type <gobject>
  #:gtype-name "GParamObject")

;;;
;;; {GObject Properties}
;;;

(define (gobject-class-find-property class name)
  "Returns a property named @var{name} (a symbol), belonging to
@var{class} or one of its parent classes, or @code{#f} if not found."
  (let ((propname name))
    (with-accessors (name)
      (let lp ((props (gobject-class-get-properties class)))
        (cond ((null? props) #f)
              ((eq? (name (car props)) propname) (car props))
              (else (lp (cdr props))))))))

(define-generic-with-docs gobject:set-property
  "Called to set a gobject property. Only properties directly belonging
to the object's class will come through this function; superclasses
handle their own properties.

Takes three arguments: the object, the property name, and the value.

Call @code{(next-method)} in your methods to invoke the default handler.")

(define-method (gobject:set-property (object <gobject>) (name <symbol>) value)
  "The default implementation of @code{gobject:set-property}, which sets
slots on the object."
  (if (class-slot-definition (class-of object) name)
      (slot-set! object name value)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

(define-generic-with-docs gobject:get-property
  "Called to get a gobject property. Only properties directly belonging
to the object's class will come through this function; superclasses
handle their own properties.

Takes two arguments: the object and the property name.

Call @code{(next-method)} in your methods to invoke the default handler")

(define-method (gobject:get-property (object <gobject>) (name <symbol>))
  "The default implementation of @code{gobject:get-property}, which
calls @code{(slot-ref obj name)}."
  (if (class-slot-definition (class-of object) name)
      (slot-ref object name)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

(%gnome-gobject-object-post-init)
