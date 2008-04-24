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
;; This module defines routines for introspecting, emitting, connecting
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

  #:export     (;; The signal class
                <gsignal>
                ;; Introspection
                gtype-class-get-signals gtype-class-get-signal-names
                ;; Emission
                gtype-instance-signal-emit
                ;; Connection, Disconnection, Blocking, Unblocking
                gtype-instance-signal-connect
                gtype-instance-signal-connect-after
                gsignal-handler-block gsignal-handler-unblock
                gsignal-handler-disconnect gsignal-handler-connected?
                ;; Creation
                gtype-class-create-signal))


(define-class-with-docs <gsignal> ()
  "A @code{<gsignal>} describes a signal on a @code{<gtype-instance>}:
its name, and how it should be called."
  (id #:init-keyword #:id #:init-value #f)
  (name #:init-keyword #:name)
  (interface-type #:init-keyword #:interface-type)
  (return-type #:init-keyword #:return-type)
  (param-types #:init-keyword #:param-types)
  (class-generic #:init-keyword #:class-generic #:init-value #f))

(define-method (initialize (instance <gsignal>) initargs)
  (next-method)
  (with-accessors
   (id name interface-type return-type param-types class-generic) instance
   (unless (id instance)
     (unless (class-generic instance)
       (set! (class-generic instance)
             (ensure-generic (lambda args (if #f #f))
                             (gtype-class-name->method-name
                              (name (interface-type instance))
                              (name instance)))))
     (set! (id instance)
           (gsignal-create
            instance
            (make <gclosure> #:func (class-generic instance)
                  #:return-type (return-type instance)))))))

(define-method (write (obj <gsignal>) port)
  (with-accessors (name interface-type return-type param-types)
    (format port "#<~a ~a ~a - ~a>"
            (class-name (class-of obj)) (name obj) (return-type obj)
            (cons (interface-type obj) (param-types obj)))))

(dynamic-call "scm_init_gnome_gobject_signals"
              (dynamic-link *guile-gnome-gobject-lib-path*))

(define (gtype-class-get-signal-names class)
  "Returns a vector of signal names belonging to @var{class} and all
parent classes."
  (with-accessors (name)
    (map name (gtype-class-get-signals class))))

;;;
;;; {Emission}
;;;

;;;
;;; {Connecting, Disconnecting, Blocking, Unblocking}
;;;

(define (gtype-instance-signal-connect object name func . after?)
  "Connects @var{func} as handler for the @code{<gtype-instance>}
@var{object}'s signal @var{name}.

@var{name} should be a symbol. @var{after} is boolean specifying whether
the handler is run before (@code{#f}) or after (@code{#t}) the signal's
default handler.

Returns an integer number which can be used as arugment of
@code{gsignal-handler-block}, @code{gsignal-handler-unblock},
@code{gsignal-handler-disconnect} and
@code{gsignal-handler-connected?}."
  (let ((signal (or (gsignal-query (class-of object) name)
                    (gruntime-error "No such signal in class ~S: ~S"
                                    class name))))
    (with-accessors (id return-type param-types)
      (gtype-instance-signal-connect-closure
       object (id signal)
       (make <gclosure> #:func func #:return-type (return-type signal)
             #:param-types (param-types signal))
       (and (pair? after?) (car after?) #t)))))

(define (gtype-instance-signal-connect-after object name func)
  "Convenience function for calling
@code{gtype-instance-signal-connect} with @var{after} = @code{#t}."
  (gtype-instance-signal-connect object name func #t))

;;;
;;; {Creation and Definition}
;;;

;; fixme: unnecessary?
(define (gtype-class-create-signal class name return-type param-types)
  "Create a new signal associated with the @code{<gtype-class>}
@var{class}.

@var{name} should be a symbol, the name of the signal. @var{return-type}
should be a @code{<gtype-class>} object. @var{param-types} should be a
list of @code{<gtype-class>} objects.

In a bit of an odd interface, this function will return a new generic
function, which will be run as the signal's default handler, whose
default method will silently return an unspecified value. The user may
define new methods on this generic to provide alternative default
handler implementations."
  (with-accessors (class-generic)
    (class-generic
     (make <gsignal> #:name name #:interface-type class
           #:return-type return-type #:param-types param-types))))
