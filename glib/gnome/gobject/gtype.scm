;; guile-gnome
;; Copyright (C) 2003,2004,2015 Andy Wingo <wingo at pobox dot com>

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
;; Base support for the GLib type system.
;;
;; The GLib runtime type system is broken into a number of modules, of
;; which GType is the base. A GType is a simply a named type. Some types
;; are fundamental and cannot be subclassed, such as integers. Others
;; can form the root of complicated object hierarchies, such as
;; @code{<gobject>}.
;;
;; One can obtain the class for a type if you know its name. For
;; example,
;;
;; @lisp
;;  (gtype-name->class "guint64") @result{} #<<gvalue-class> <guint64>>
;; @end lisp
;;
;; A more detailed reference on the GLib type system may be had at
;; @uref{http://library.gnome.org/devel/gobject/stable/}.
;;
;;; Code:

(define-module (gnome gobject gtype)
  #:use-module (oop goops)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject config)
  #:export     (<gtype-class> <gtype-instance>
                gtype-name->class class-name->gtype-name
                gruntime-error
                gtype-instance-destroy!))

(dynamic-call "scm_init_gnome_gobject_gc"
              (dynamic-link *guile-gnome-gobject-lib-path*))
(dynamic-call "scm_init_gnome_gobject_types"
              (dynamic-link *guile-gnome-gobject-lib-path*))

(define (gruntime-error format-string . args)
  "Signal a runtime error. The error will be thrown to the key
@code{gruntime-error}."
  (scm-error 'gruntime-error #f format-string args '()))


;;;
;;; {Base Class Hierarchy]
;;;


(define-class-with-docs <gtype-class> (<class>)
  "The metaclass of all GType classes. Ensures that GType classes have a
@code{gtype} slot, which records the primitive GType information for
this class."
  (gtype #:class <foreign-slot>))

(define-method (initialize (class <gtype-class>) initargs)
  (let ((gtype-name (or (get-keyword #:gtype-name initargs #f)
                        (gruntime-error "Need #:gtype-name initarg: ~a"
                                        (pk initargs)))))
    ;; allow gtype-name of #t for base classes without gtypes (e.g.
    ;; <gtype-instance>)
    (if (not (eq? gtype-name #t))
        (%gtype-class-bind class gtype-name))
    (next-method)
    (%gtype-class-inherit-magic class)))

(define-method (write (class <gtype-class>) file)
  (format file "#<~a ~a>" (class-name (class-of class)) (class-name class)))

(dynamic-call "scm_init_gnome_gobject_types_gtype_class"
              (dynamic-link *guile-gnome-gobject-lib-path*))

(define-class-with-docs <gtype-instance> ()
  "The root class of all instantiatable GType classes. Adds a slot,
@code{gtype-instance}, to instances, which holds a pointer to the C
value."
  (gtype-instance #:class <read-only-slot>)
  #:gtype-name #t
  #:metaclass <gtype-class>)

(define-method (initialize (instance <gtype-instance>) initargs)
  (next-method)
  (%gtype-instance-construct instance initargs))

(dynamic-call "scm_init_gnome_gobject_types_gtype_instance"
              (dynamic-link *guile-gnome-gobject-lib-path*))

;;;
;;; {Misc]
;;;


(define (class-name->gtype-name class-name)
  "Convert the name of a class into a suitable name for a GType. For
example:

@lisp
 (class-name->gtype-name '<foo-bar>) @result{} \"FooBar\"
@end lisp"
  ;; By convention, GTypes are named with StudlyCaps.
  (list->string
   (reverse!
    (let loop ((to-process (string->list (symbol->string class-name))) (ret '()) (caps? #t))
      (cond
       ((null? to-process)
        ret)
       ((char-alphabetic? (car to-process))
        (loop (cdr to-process)
              (cons (if caps? (char-upcase (car to-process)) (car to-process)) ret)
              #f))
       ((char-numeric? (car to-process))
        (loop (cdr to-process)
              (cons (car to-process) ret)
              #f))
       (else
        (loop (cdr to-process) ret #t)))))))
