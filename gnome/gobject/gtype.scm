;; guile-gnome
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
;; Base support for the GLib type system.
;;
;;; Code:

(define-module (gnome gobject gtype)
  :use-module (oop goops)
  :use-module (gnome gobject utils)
  :use-module (gnome gobject config)
  :export     (;; From C:

               ;; GType
               gtype? gtype-is-a? gtype-basic? gtype-classed?
               gtype-instantiatable? gtype-fundamental?
               gtype->fundamental gtype-parent gtype-children
               gtype-interfaces
               gtype-name gtype-from-name gtype-from-instance
               ;; GTypeInstance
               %gtype-instance-primitive-destroy!
               gtype-instance-primitive->type
               ;; Misc
               %function->method-public especify-metaclass!

               ;; From Scheme:

               ;; Base Classes
               <gtype-class> <gtype-instance-class> <gtype-instance>
               ;; GType <-> GTypeClass
               gtype->class gtype-class->type
               %gtype-lookup-class %gtype-bind-to-class
               ;; Misc
               gtype-instance:write gruntime-error class-name->gtype-name))

(define (gruntime-error format-string . args)
  (save-stack)
  (scm-error 'gruntime-error #f format-string args '()))

;;;
;;; {Base Class Hierarchy]
;;;

(define (create-set-once-g-n-s class s class-slot?)
  (let* ((already-allocated (slot-ref class 'nfields))
         (name (slot-definition-name s))
         (get (lambda (x) (%get-struct-slot (if class-slot? class x)
                                            already-allocated)))
         (set (lambda (x o) (if (not (get x))
                                (%set-struct-slot! (if class-slot? class x)
                                                   already-allocated
                                                   o)
                                (gruntime-error "set-once slot already set: ~S=~A"
                                                name (get x))))))
    (slot-set! class 'nfields (1+ already-allocated))
    (list get set)))

(define-class <set-once-class> (<class>))
(define-method (compute-get-n-set (class <set-once-class>) s)
  (case (slot-definition-allocation s)
    ((#:set-once)
     (create-set-once-g-n-s class s #f))

    ((#:set-once-each-subclass)
     (create-set-once-g-n-s class s #t))

    ;; Chain up for the default allocation methods...
    (else (next-method))))

;; We have to inherit from class because we're a metaclass. We do that
;; via <set-once-class>. We have #:set-once slots, so we also need to
;; have <set-once-class> as our metaclass.
(define-class <gtype-class> (<set-once-class>)
  (gtype #:allocation #:set-once)
  (gtype-class #:allocation #:set-once)
  #:metaclass <set-once-class>)

(define-class <gtype-instance-class> (<gtype-class>))
(define-class <gtype-instance> ()
  (gtype-instance #:allocation #:set-once)
  #:metaclass <gtype-instance-class>)

(dynamic-call "scm_init_gnome_gobject_types"
              (dynamic-link *guile-gnome-gobject-lib-path*))

;;;
;;; {Class Allocation and Initialization}
;;;

(define (class-name->gtype-name class-name)
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

(define-method (initialize (class <gtype-class>) initargs)
  (next-method)
  (cond
   ((slot-ref class 'gtype)
    ;; A superclass already did the work, nothing to do
    )
   ((get-keyword #:gtype initargs #f)
    => (lambda (gtype)
         (if (%gtype-lookup-class gtype)
             (gruntime-error "~A already has a GOOPS class, use gtype->class" gtype))
         (%gtype-bind-to-class class gtype)))
   (else
    (gruntime-error "Don't know how to make subclass ~A" class))))

(define (get-direct-supers type)
  (if (not (gtype-parent type))
      (if (gtype-instantiatable? type)
          (list <gtype-instance>)
          '())
      (let* ((direct-super (gtype->class (gtype-parent type)))
             (cpl (class-precedence-list direct-super)))
        (let loop ((supers (list direct-super))
                   (interfaces (map gtype->class (gtype-interfaces type))))
          (if (null? interfaces)
              supers
              (loop
               (if (memq (car interfaces) cpl) supers (cons (car interfaces) supers))
               (cdr interfaces)))))))

(define (gtype->class type)
  "If there is already a GOOPS class associated with the GType `type',
return this class.

Otherwise, create a new GOOPS class and bind it to this type. The
created class is an immortal, persistent object which is bound in some
magic way to its GType.
"
  (or (%gtype-lookup-class type)
      (let* ((class-name (gtype-name->class-name (gtype-name type)))
	     (direct-supers (get-direct-supers type)))
        (if (null? direct-supers)
            ;; Need to set the metaclass on base classes.
            (make-class '() '()
                        #:gtype type
                        #:name class-name
                        #:metaclass <gtype-class>)
            (make-class direct-supers '()
                        #:gtype type
                        #:name class-name)))))

(define (gtype-class->type class)
  "Returns the <gtype> associated with a <gtype-class>."
  (if (slot-bound? class 'gtype)
      (slot-ref class 'gtype)
      (gruntime-error "Can't get type of unknown class: ~S" class)))

;;;
;;; {Methods for Writing}
;;;

(define-generic-with-docs gtype-instance:write
  "Hacky function so we can write smob types in scheme.")

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (gtype-instance:write (class <gtype-class>) (obj <%gtype-instance>) file)
  (display "#<%gtype-instance " file)
  (display (class-name class) file)
  (display #\space file)
  (display-address obj file)
  (let* ((type (gtype-instance-primitive->type obj))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (eq? type fundamental)))
    (cond
     ((eq? fundamental gtype:gparam)
      (display #\space file)
      (display (gparam-primitive->pspec-struct obj) file))))

  (display #\> file))

(%gnome-gobject-types-post-init)
