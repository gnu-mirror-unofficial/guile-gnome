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
;; GSignal is a mechanism by which code, normally written in C, may
;; expose extension points to which closures can be connected, much like
;; Guile's hooks. Instantiatable types can have signals associated with
;; them; for example, @code{<gtk-widget>} has an @code{expose} signal
;; that will be ``fired'' at certain well-documented points.
;;
;; Signals are typed. They specify the types of their return value, and
;; the types of their arguments.
;;
;; This module defines routines for instrospecting, emitting, connecting
;; to, disconnecting from, blocking, and unblocking signals.
;; Additionally it defines routines to define new signal types on
;; instantiatable types.
;;
;;; Code:

(define-module (gnome gobject gsignal)
  #:use-module (oop goops)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject config)
  #:use-module (gnome gobject gtype)
  #:use-module (gnome gobject gclosure)
  #:use-module (gnome gobject gvalue)

  #:export     ( ;; The signal struct class and its accessors
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
              (dynamic-link *guile-gnome-gobject-lib-path*))

(set-object-property! <gsignal> 'documentation
                      "The structure vtable for @code{<gsignal>} instances.")

;;;
;;; {Signal Field Accessors}
;;;

(define (gsignal:id signal)
  "Access the ``id'' of a @code{<gsignal>} structure, an integer."
  (struct-ref signal gsignal-id))

(define (gsignal:name signal)
  "Access the name of a @code{<gsignal>} structure, a string."
  (struct-ref signal gsignal-name))

(define (gsignal:interface-type signal)
  "Access the type to which a @code{<gsignal>} structure is associated,
a @code{<gtype>}."
  (struct-ref signal gsignal-interface-type))

(define (gsignal:return-type signal)
  "Access the return type from a a @code{<gsignal>} structure,
a @code{<gtype>}."
  (struct-ref signal gsignal-return-type))

(define (gsignal:param-types signal)
  "Access the parameter types from a a @code{<gsignal>} structure,
a list of @code{<gtype>}."
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
  "Returns a vector of signals belonging to @var{class} and all parent classes."
  (if (not (is-a? class <gtype-class>))
      (gruntime-error "Not a <gtype-class>: ~S" class))
  (gtype-class-get-vector-slot class 'gsignals <gtype-instance>))

(define (vector-map proc vector)
  (let* ((length (vector-length vector))
	 (result-vector (make-vector length)))
    (do ((index 0 (+ index 1)))
	((>= index length) result-vector)
      (vector-set! result-vector index (proc (vector-ref vector index))))))

(define (gtype-class-get-signal-names class)
  "Returns a vector of signal names belonging to @var{class} and all
parent classes."
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
  "Emits signal @var{name} with arguments @var{args} on the
@code{<gtype-instance>} @var{object}. @var{name} should be a symbol."
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
  "Connects @var{func} as handler for the @code{<gtype-instance>}
@var{object}'s signal @var{name}.

@var{name} should be a symbol. @var{after} is boolean specifying whether
the handler is run before (@code{#f}) or after (@code{#t}) the signal's
default handler.

Returns an integer number which can be used as arugment of
@code{gsignal-handler-block}, @code{gsignal-handler-unblock},
@code{gsignal-handler-disconnect} and
@code{gsignal-handler-connected?}."
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
  "Convenience function for calling
@code{gtype-instance-signal-connect-data} with @var{after} = @code{#f}."
  (gtype-instance-signal-connect-data object name func #f))

(define (gtype-instance-signal-connect-after object name func)
  "Convenience function for calling
@code{gtype-instance-signal-connect-data} with @var{after} = @code{#t}."
  (gtype-instance-signal-connect-data object name func #t))

;; FIXME: just make the C functions take gtype-instance GOOPS objects.
(define (gtype-instance-primitive obj)
  (slot-ref obj 'gtype-instance))

(define (gsignal-handler-block obj id)
  "Block invocation of the signal handler identified by @var{id} from
high-level GOOPS object @var{object}."
  (gsignal-primitive-handler-block (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-unblock obj id)
  "Unblock invocation of the signal handler identified by @var{id} from
high-level GOOPS object @var{object}."
  (gsignal-primitive-handler-unblock (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-disconnect obj id)
  "Disconnect the signal handler identified by @var{id} from high-level
GOOPS object @var{object}."
  (gsignal-primitive-handler-disconnect (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-connected? obj id)
  "Returns @code{#t} if the signal handler identified by @var{id} is
connected on the high-level GOOPS object @var{object}, or @code{#f}
otherwise."
  (gsignal-primitive-handler-connected? (gtype-instance-primitive obj) id))

;;;
;;; {Creation and Definition}
;;;

(define (gtype-class-create-signal class name return-type param-types)
  "Create a new signal associated with the @code{<gtype-class>}
@var{class}.

@var{name} should be a symbol, the name of the signal. @var{return-type}
should be either a @code{<gtype>} or a @code{<gtype-class>} object.
Similarly, @var{param-types} should be a list of either @code{<gtype>}
or @code{<gtype-class>} objects.

In a bit of an odd interface, this function will return a new generic
function, which will be run as the signal's default handler, whose
default method will silently return an unspecified value. The user may
define new methods on this generic to provide alternative default
handler implementations."
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
  "A macro invoked as:
@lisp
 (gtype-class-define-signal @var{class} @var{name} @var{return-type}
                            . @var{param-types})
@end lisp

All arguments will be passed to @code{gtype-class-create-signal}.

This form is a macro because it will actually take the generic returned
from @code{gtype-class-create-signal} and bind it to a name in the
toplevel environment.

The name of the new generic function is the concatenation of the type
name, a colon, and the signal name.

For example:
@lisp
 (gtype-class-define-signal <foo> 'roswell #f)
 (define-method (foo:roswell (obj <foo>))
   *unspecified*)

 (gtype-class-define-signal <foo> 'berlin <glong> <gint>)
 (define-method (foo:berlin (obj <foo>) (x <number>))
   85)
@end lisp
"
  (procedure->memoizing-macro
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
