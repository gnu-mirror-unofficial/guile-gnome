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
;; Support for GValue-based types.
;;
;;; Code:

(define-module (gnome gobject gvalue)
  :use-module (oop goops)
  :use-module (gnome gobject utils)
  :use-module (gnome gobject gtype)

  :export     (;; Primitive GValue support
               ;; <gvalue> is exported from (oop goops) by the smob system
               gvalue? gvalue->type
               ;; Simple classes
               <gboolean> <gchar> <guchar> <gint> <guint> <glong>
               <gulong> <gint64> <guint64> <gfloat> <gdouble>
               <gchararray> <gboxed> <gboxed-scm> <gvalue-array>
               ;; Enums and Flags Classes
               <genum> <gflags>
               genum-register-static gflags-register-static
               genum-class->value-table gflags-class->value-table
               genum-type-get-values gflags-type-get-values
               ;; Conversion (from C)
               scm->gvalue gvalue->scm
               ;; Enums and Flags
               genum->symbol genum->name genum->value
               gflags->symbol-list gflags->name-list gflags->value-list))

(dynamic-call "scm_init_gnome_gobject_values"
              (dynamic-link "libguile-gnome-gobject"))

;;;
;;; {Simple Classes}
;;;

(define-class <gvalue-class> (<gtype-class>))
(define (gvalued-type->class type)
  ;; doesn't check to see if there's already a class
  (make-class '() '() ;; no supers or subs
              #:gtype type
              #:name (gtype-name->class-name (gtype-name type))
              #:metaclass <gvalue-class>))

(define <gchar>        (gvalued-type->class gtype:gchar))
(define <guchar>       (gvalued-type->class gtype:guchar))
(define <gboolean>     (gvalued-type->class gtype:gboolean))
(define <gint>         (gvalued-type->class gtype:gint))
(define <guint>        (gvalued-type->class gtype:guint))
(define <glong>        (gvalued-type->class gtype:glong))
(define <gulong>       (gvalued-type->class gtype:gulong))
(define <gint64>       (gvalued-type->class gtype:gint64))
(define <guint64>      (gvalued-type->class gtype:guint64))
(define <gfloat>       (gvalued-type->class gtype:gfloat))
(define <gdouble>      (gvalued-type->class gtype:gdouble))
(define <gchararray>   (gvalued-type->class gtype:gchararray))
(define <gboxed>       (gvalued-type->class gtype:gboxed))
(define <gvalue-array> (gvalued-type->class gtype:gvalue-array))
(define <gboxed-scm>   (gvalued-type->class gtype:gboxed-scm))

;;;
;;; {Enums and Flags Classes}
;;;

(define-class <genum-class> (<gvalue-class>))
(define-method (initialize (class <genum-class>) initargs)
  (let ((gtype (cond
                ((get-keyword #:gtype initargs #f)
                 => noop)
                ((get-keyword #:gtype-name initargs #f)
                 => gtype-from-name)
                (else
                 (genum-register-static
                  (or (get-keyword #:gtype-name initargs #f)
                      (class-name->gtype-name (get-keyword #:name initargs '???)))
                  (or (kw-arg-ref initargs #:vtable)
                      (error
                       "You need to specify the #:vtable when subclassing <genum>.")))))))
    (if (%gtype-lookup-class gtype)
        (gruntime-error "<gtype> ~A already has a GOOPS class, use gtype->class" gtype))

    (%gtype-bind-to-class class gtype)
    (next-method)

    (if (not (gtype-fundamental? gtype))
        (class-slot-set! class 'genum-values
                         (genum-type-get-values gtype)))))

(define-class <gflags-class> (<gvalue-class>))
(define-method (initialize (class <gflags-class>) initargs)
  (let ((gtype (cond
                ((get-keyword #:gtype initargs #f)
                 => noop)
                ((get-keyword #:gtype-name initargs #f)
                 => gtype-from-name)
                (else
                 (gflags-register-static
                  (or (get-keyword #:gtype-name initargs #f)
                      (class-name->gtype-name (get-keyword #:name initargs '???)))
                  (or (kw-arg-ref initargs #:vtable)
                      (error
                       "You need to specify the #:vtable when subclassing <gflags>.")))))))
    (if (%gtype-lookup-class gtype)
        (gruntime-error "<gtype> ~A already has a GOOPS class, use gtype->class" gtype))

    (%gtype-bind-to-class class gtype)
    (next-method)

    (if (not (gtype-fundamental? gtype))
        (class-slot-set! class 'genum-values
                         (gflags-type-get-values gtype)))))

;; Enums and flags have special slots, so we define the classes
;; manually.
(define-class <genum> ()
  (genum-values #:allocation #:each-subclass)
  #:gtype gtype:genum
  #:metaclass <genum-class>)
(define (genum-class->value-table class)
  (class-slot-ref class 'genum-values))

(define-class <gflags> ()
  (genum-values #:allocation #:each-subclass) ;; FIXME
  #:gtype gtype:gflags
  #:metaclass <gflags-class>)
(define (gflags-class->value-table class)
  (class-slot-ref class 'genum-values))

;;;
;;; {Instance Allocation and Initialization}
;;;

(define-method (allocate-instance (class <gvalue-class>) initargs)
  ;; Actually returns a <gvalue> instead of an instance of the class. It
  ;; is, after all, a generic value.
  (gvalue-primitive-new (slot-ref class 'gtype)))

;; Thus the specializer on initialize is <gvalue>, not <gint> or whatever.
(define-method (initialize (instance <gvalue>) initargs)
  (let* ((type (gvalue->type instance))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (eq? type fundamental)))
    (cond
     ;; Basic types have only one possible Scheme representation.
     ((gtype-basic? type)
      (or (memq #:value initargs)
          (gruntime-error "Missing #:value argument"))
      (gvalue-primitive-set instance (get-keyword #:value initargs 'foo)))

     ;; Fundamental type - but not a basic one.
     ((eq? type fundamental)
      (gruntime-error "<gvalue> can't initialize for fundamental type: ~S" type))

     ;; GEnum
     ((eq? fundamental gtype:genum)
      (let* ((init-value (get-keyword #:value initargs *unspecified*))
	     (enum (cond
		    ((unspecified? init-value)
		     (gruntime-error "Missing #:value argument"))
		    ((integer? init-value)
		     (enum-by-index type init-value))
		    ((symbol? init-value)
		     (enum-by-symbol type init-value))
		    ((string? init-value)
		     (enum-by-name type init-value))
		    (else
		     (gruntime-error "Wrong type argument: ~S" init-value)))))
	(gvalue-primitive-set instance (caddr enum))))

     ;; GFlags
     ((eq? fundamental gtype:gflags)
      (let* ((init-values (get-keyword #:value initargs *unspecified*))
	     (real-init-values init-values)
	     (flags-value 0))
	(if (unspecified? init-values)
	    (gruntime-error "Missing #:value argument"))

	(cond
         ((list? init-values)
          #t) ;; noop, this is what we want
         ((vector? init-values)
          (set! init-values (vector->list init-values)))
         ((integer? init-values)
          (set! init-values
                (let loop ((ret '()) (and-value 1))
                  (if (> and-value init-values)
                      ret
                      (if (zero? (logand init-values and-value))
                          (loop ret (* and-value 2))
                          (loop (cons and-value ret) (* and-value 2)))))))
         ((symbol? init-values)
          (set! init-values (list init-values)))
         (else
          (gruntime-error "Wrong type argument: ~S" real-init-values)))
        
        (set! init-values (map (lambda (x)
                                 (cond
                                  ((integer? x)
                                   (flags-by-index type x))
                                  ((symbol? x)
                                   (flags-by-symbol type x))
                                  ((string? x)
                                   (flags-by-name type x))
                                  (else
                                   (gruntime-error "Wrong type argument: ~S" real-init-values))))
                               init-values))
        (set! init-values (map (lambda (x) (caddr x)) init-values))
        (set! flags-value (apply logior init-values))
	(gvalue-primitive-set instance flags-value)))

     (else
      (noop)))))

;;;
;;; {Methods for Writing}
;;;

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

;; These gtype-instance:write methods are necessary for some reason.
;; Just defining a write method specialized to <gvalue> doesn't seem to
;; work, probably because <gvalue> and <%gtype-instance> are smob
;; types.
(define-method (gtype-instance:write (class <gtype-class>) (obj <gvalue>) file)
  (display "#<gvalue " file)
  (display (class-name class) file)
  (display #\space file)
  (display-address obj file)
  (let* ((type (gtype-class->type class))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (eq? type fundamental)))
    (cond
     ;; Basic types, with one possible Scheme representation.
     ((gtype-basic? type)
      (display #\space file)
      (display (gvalue-primitive-get obj) file))

     ;; GEnum
     ((eq? fundamental gtype:genum)
      (let* ((enum-values (genum-type-get-values type))
	     (value (gvalue-primitive-get obj))
	     (value-text (enum-by-index type value)))
	(display #\space file)
	(display value-text file)))

     ;; GFlags
     ((eq? fundamental gtype:gflags)
      (let* ((flags-values (gflags-type-get-values type))
	     (value (gvalue-primitive-get obj))
	     (value-text '()))
	(for-each (lambda (x)
		    (let ((f (caddr x)))
		      (if (positive? (logior value f))
			  (set! value-text (append! value-text (list x))))))
		  (vector->list flags-values))
	(display #\space file)
	(display value-text file)))))

  (display #\> file))

;; If we get a GValue from somewhere else, the "primitive" code doesn't
;; know how to make the class. So we have this hacky function.
(define-method (gtype-instance:write (type <gtype>) (obj <gvalue>) file)
  (gtype-instance:write (gtype->class type) obj file))

;;;
;;; {Enums and Flags}
;;;

(define (find-enum vtable func index)
  (let loop ((l (vector->list vtable)))
    (if (null? l)
	(gruntime-error "No such value in ~A: ~A" vtable index)
	(begin
	  (if (equal? (func (car l)) index)
	      (car l)
	      (loop (cdr l)))))))

(define (enum-by-index type index)
  (find-enum (genum-type-get-values type) (lambda (l) (caddr l)) index))

(define (enum-by-name type name)
  (find-enum (genum-type-get-values type) (lambda (l) (cadr l)) name))

(define (enum-by-symbol type symbol)
  (find-enum (genum-type-get-values type) (lambda (l) (car l)) symbol))

(define (flags-by-index type index)
  (find-enum (gflags-type-get-values type) (lambda (l) (caddr l)) index))

(define (flags-by-name type name)
  (find-enum (gflags-type-get-values type) (lambda (l) (cadr l)) name))

(define (flags-by-symbol type symbol)
  (find-enum (gflags-type-get-values type) (lambda (l) (car l)) symbol))

(define (genum->symbol obj)
  (let* ((type (gvalue->type obj))
	 (enum-values (genum-type-get-values type))
	 (value (gvalue-primitive-get obj))
	 (the-value (enum-by-index type value)))
    (car the-value)))

(define (genum->name obj)
  (let* ((type (gvalue->type obj))
	 (enum-values (genum-type-get-values type))
	 (value (gvalue-primitive-get obj))
	 (the-value (enum-by-index type value)))
    (cadr the-value)))

(define (genum->value obj)
  (let* ((type (gvalue->type obj))
	 (enum-values (genum-type-get-values type))
	 (value (gvalue-primitive-get obj))
	 (the-value (enum-by-index type value)))
    (caddr the-value)))

(define (gflags->element-list obj)
  (let* ((type (gvalue->type obj))
	 (flags-values (gflags-type-get-values type))
	 (value (gvalue-primitive-get obj))
	 (element-list '()))
    (for-each (lambda (x)
		(let ((f (caddr x)))
		  (if (positive? (logand value f))
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
