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
;; Support for GSignal.
;;
;; See the guile-gnome tutorial for more details.
;;
;;; Code:

(define-module (gnome gobject gsignal)
  :use-module (oop goops)
  :use-module (ice-9 documentation)
  :use-module (gnome gobject utils)
  :use-module (gnome gobject gtype)
  :use-module (gnome gobject gclosure)
  :use-module (gnome gobject gvalue)

  :export     (;; The signal struct class and its accessors
               <gsignal>
               gsignal:id gsignal:name gsignal:interface-type
               gsignal:return-type gsignal:param-types
               ;; Introspection
               gtype-get-signals
               gtype-class-get-signals gtype-class-get-signal-names
               ;; Emission
               gtype-instance-signal-emit
               ;; Connection, Disconnection, Blocking, Unblocking
               gtype-instance-signal-connect-data
               gtype-instance-signal-connect
               gtype-instance-signal-connect-after
               gsignal-handler-block gsignal-handler-unblock
               gsignal-handler-disconnect gsignal-handler-connected?
               ;; Creation and Definition
               gtype-class-create-signal gtype-class-define-signal))


(dynamic-call "scm_init_gnome_gobject_signals"
              (dynamic-link "libguile-gnome-gobject"))

;;;
;;; {Signal Field Accessors}
;;;

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

;;;
;;; {Introspection}
;;;

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

(define (gtype-class-get-signals class)
  "(gtype-class-get-signals class)

Returns a vector of signals belonging to CLASS and all parent classes."
  (if (not (is-a? class <gtype-class>))
      (gruntime-error "Not a <gtype-class>: ~S" class))
  (gtype-class-get-vector-slot class 'gsignals <gtype-instance>))

(define (gtype-class-get-signal-names class)
  "(gtype-class-get-signal-names class)

Returns a vector of signal names belonging to CLASS and all parent
classes."
  (vector-map gsignal:name (gtype-class-get-signals class)))

(define (signal-by-name signals symbol)
  (let loop ((l (vector->list signals)))
    (if (null? l)
	#f
	(let* ((this (car l)) (name (gsignal:name this)))
	  (if (eq? symbol (string->symbol name))
              this
              (loop (cdr l)))))))

;;;
;;; {Emission}
;;;

(define (gtype-instance-signal-emit object name . args)
  "(gtype-instance-signal-emit object name . args)

Emits signal `name' with arguments `args' on the GTypeInstance `object':

   object            - instance of <gtype-instance> or a subclass of it.

   name              - symbol identifying the signal

"
  (or (is-a? object <gtype-instance>)
      (gruntime-error "Not a <gtype-instance>: ~S" object))
  (or (symbol? name)
      (gruntime-error "Not a symbol: ~S" name))

  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (signal-vector (gtype-class-get-signals class))
	 (signal (or (signal-by-name signal-vector name)
		     (gruntime-error "No such signal in class ~S: ~S" class name)))
	 (id (gsignal:id signal))
	 (params (gsignal:param-types signal))
	 (num-params (vector-length params)))
    
    (or (eq? (length args) num-params)
	(gruntime-error "Wrong number of arguments: ~S" args))

    (let* ((values (do ((index 0 (+ index 1))
			(values (make-vector num-params #f)))
		       ((>= index num-params) values)
		     (let* ((value-type (vector-ref params index))
			    (init-value (list-ref args index))
			    (value (scm->gvalue value-type init-value)))
		       
		       (vector-set! values index value))))
	   (retval (gtype-instance-primitive-signal-emit instance id values)))
 
      (if (unspecified? retval) retval (gvalue->scm retval)))))

;;;
;;; {Connecting, Disconnecting, Blocking, Unblocking}
;;;

(define (gtype-instance-signal-connect-data object name func after)
  "(gtype-instance-signal-connect-data object name func after)

Connects `func' as handler for the GTypeInstance `object's signal `name':

   object            - instance of <gtype-instance> or a subclass.

   name              - symbol identifying the signal

   func              - procedure which is installed as signal handler.

   after             - boolean specifying whether the handler is run before (#f)
                       or after (#t) the signal's default handler.

Returns an integer number which can be used as arugment of gsignal-handler-block,
gsignal-handler-unblock, gsignal-handler-disconnect and gsignal-handler-connected?.

"
  (or (is-a? object <gtype-instance>)
      (gruntime-error "Not a <gtype-instance>: ~S" object))
  (or (symbol? name)
      (gruntime-error "Not a symbol: ~S" name))
  (or (procedure? func)
      (gruntime-error "Not a procedure: ~S" func))
  (or (boolean? after)
      (gruntime-error "Not a boolean: ~S" after))

  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (signal-vector (gtype-class-get-signals class))
	 (signal (or (signal-by-name signal-vector name)
		     (gruntime-error "No such signal in class ~S: ~S" class name)))
	 (id (gsignal:id signal))
	 (rtype (gsignal:return-type signal))
	 (params (vector->list (gsignal:param-types signal)))
	 (closure (make <gclosure> #:func func #:return-type rtype #:param-types params))
	 (pclosure (slot-ref closure 'closure)))
    (gtype-instance-primitive-signal-connect instance id pclosure after)))

(define (gtype-instance-signal-connect object name func)
  "Convenience function for `(gtype-instance-signal-connect-data object name func #f)'."
  (gtype-instance-signal-connect-data object name func #f))

(define (gtype-instance-signal-connect-after object name func)
  "Convenience function for `(gtype-instance-signal-connect-data object name func #t)'."
  (gtype-instance-signal-connect-data object name func #t))

;; FIXME: just make the C functions take gtype-instance GOOPS objects.
(define (gtype-instance-primitive obj)
  (slot-ref obj 'gtype-instance))

(define (gsignal-handler-block obj id)
  (gsignal-primitive-handler-block (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-unblock obj id)
  (gsignal-primitive-handler-unblock (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-disconnect obj id)
  (gsignal-primitive-handler-disconnect (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-connected? obj id)
  (gsignal-primitive-handler-connected? (gtype-instance-primitive obj) id))

;;;
;;; {Creation and Definition}
;;;

(define (gtype-class-create-signal class name return-type param-types)
  (let* ((type (gtype-class->type class))
	 (signal-vector (gtype-class-get-signals class))
	 (signal (make-struct gsignal-struct-vtable 0 #f #f
			      0 name type return-type #f
                              (list->vector
                               (map
                                (lambda (t)
                                  (cond 
                                   ((is-a? t <gtype>) t)
                                   ((is-a? t <gtype-class>) (gtype-class->type t))
                                   (else
                                    (gruntime-error "Invalid parameter type: ~A" t))))
                                param-types))))
	 (method-name (gtype-name->method-name (gtype-name type) name))
	 (default-func (lambda args *unspecified*))
	 (generic (ensure-generic default-func method-name))
	 (func (lambda args
		 (apply generic args)))
	 (closure (make <gclosure> #:func func #:return-type return-type))
	 (closure-primitive (slot-ref closure 'closure)))
    (and (signal-by-name signal-vector name)
	 (gruntime-error "Class ~S already has a signal with this name: ~S"
			 class name))
    (gsignal-primitive-create signal closure-primitive)
    (class-slot-set! class 'gsignals (gtype-get-signals type))
    generic))

(define (top-level-env? env)
  (or (null? env)
      (procedure? (car env))))

(define-macro (make-param-vector . args)
  `(list->vector
    (map (lambda (x)
	   (or (is-a? x <gtype>)
	       (gruntime-error "Wrong parameter type: ~S" x))
	   x) (list ,@args))))

(define-with-docs gtype-class-define-signal
  "(gtype-class-define-signal class name return-type . param-types)

Creates and adds a new signal `name' to the GTypeClass `class':

  class         - this must be a <gtype-class>.

  name          - this is a symbol which identifies the signal. There must be
                  no signal with this name in the `class'es class ancestry.

  return-type   - is either a <gtype> specifying the signal's return type or #f
                  if the return type is void (#f is the same than gtype:void).

  param-types   - a list of <gtype>s specifying the signal's arguments.

This is implemented as a macro which must be called at the top-level.
If it does not already exist, it'll define a new generic function for the signal.

The name of this GF is the concatenation of the type name, a colon and the signal
name - it's calculated by `(gtype->method-name (gtype-class->type class) name)'.

NOTE: Even if this is not strictly a bug, it is highly recommended not to add any
      signals to existing classes which you did not create.

      Create a subclass using gtype-register-object-static and then add your signals
      to this subclass.

Examples:

  (gtype-class-define-signal <foo> 'roswell #f)
  (define-method (foo:roswell (obj <foo>))
     *unspecified*)

  (gtype-class-define-signal <foo> 'berlin  gtype:glong gtype:int)
  (define-method (foo:berlin (obj <foo>) (x (<number>)))
     85)

"
  (procedure->macro
   (lambda (exp env)
     (cond ((not (top-level-env? env))
	    (gruntime-error "gtype-class-define-signal: Only allowed at top level"))
	   ((not (and (list? exp) (>= (length exp) 3)))
	    (gruntime-error "missing or extra expression"))

	   (else
	    (let ((class (cadr exp)) (name (caddr exp))
		  (return-type (or (cadddr exp) gtype:void))
		  (param-types `(map (lambda (x)
                                       (or (is-a? x <gtype>)
                                           (gruntime-error "Wrong parameter type: ~S" x))
                                       x) (list ,@(cddddr exp))))

		  (method-name `(gtype-name->method-name
				 (gtype-name (gtype-class->type ,(cadr exp)))
				 ,(caddr exp)))
		  )

	      `(cond
		 ((not (is-a? ,class <gtype-class>))
		  (gruntime-error "Bad object class: ~S" ,class))
		 
		 ((not (gtype-classed? (gtype-class->type ,class)))
		  (gruntime-error "Non-classed GType: ~S" ,class))
		 
		 ((not (symbol? ,name))
		  (gruntime-error "Bad signal name: ~S" ,name))

		 ((not (is-a? ,return-type <gtype>))
		  (gruntime-error "Bad return type: ~S" ,return-type))

		 ((,signal-by-name (gtype-class-get-signals ,class)
				   ,name)
		  (gruntime-error
		   "Class ~S already has a signal with this name: ~S"
		   ,class ,name))

		 (else
		  ,(if (not (defined? (local-eval method-name env) env))
		     `(define ,(local-eval method-name env)
			(gtype-class-create-signal ,class ,name
						     ,return-type
						     ,param-types)))))))))))
