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
;; GLib supports generic typed values via its GValue module. These
;; values are wrapped in Scheme as instances of @code{<gvalue-class>}
;; classes, such as @code{<gint>}, @code{<gfloat>}, etc.
;;
;; In most cases, use of @code{<gvalue>} are transparent to the Scheme
;; user. Values which can be represented directly as Scheme values are
;; normally given to the user in their Scheme form, e.g. @code{#\a}
;; instead of @code{#<gvalue <gchar> 3020c708 a>}. However, when dealing
;; with low-level routines it is sometimes necessary to have values in
;; @code{<gvalue>} form. The conversion between the two is performed via
;; the @code{scm->gvalue} and @code{gvalue->scm} functions.
;;
;; The other set of useful procedures exported by this module are those
;; dealing with enumerated values and flags. These objects are normally
;; represented on the C side with integers, but they have symbolic
;; representations registered in the GLib type system.
;;
;; On the Scheme side, enumerated and flags values are canonically
;; expressed as @code{<gvalue>} objects. They can be converted to
;; integers or symbols using the conversion procedures exported by this
;; module. It is conventional for Scheme procedures that take enumerated
;; values to accept any form for the values, which can be canonicalized
;; using @code{(make <your-enum-type> #:value @var{value})}, where
;; @var{value} can be an integer, a symbol (or symbol list in the case
;; of flags), or the string ``nickname'' (or string list) of the
;; enumerated/flags value.
;;
;;; Code:

(define-module (gnome gobject gvalue)
  #:use-module (oop goops)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject config)
  #:use-module (gnome gobject gtype)

  ;; FIXME: the c code exports a bunch of gtypes behind our back
  #:export     (;; Primitive GValue support
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
              (dynamic-link *guile-gnome-gobject-lib-path*))

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

(define-with-docs <gchar>
  "A @code{<gvalue>} class for signed 8-bit values."
  (gvalued-type->class gtype:gchar))
(define-with-docs <guchar>
  "A @code{<gvalue>} class for unsigned 8-bit values."
  (gvalued-type->class gtype:guchar))
(define-with-docs <gboolean>
  "A @code{<gvalue>} class for boolean values."
  (gvalued-type->class gtype:gboolean))
(define-with-docs <gint>
  "A @code{<gvalue>} class for signed 32-bit values."
  (gvalued-type->class gtype:gint))
(define-with-docs <guint>
  "A @code{<gvalue>} class for unsigned 32-bit values."
  (gvalued-type->class gtype:guint))
(define-with-docs <glong>
  "A @code{<gvalue>} class for signed ``long'' (32- or 64-bit)
values."
  (gvalued-type->class gtype:glong))
(define-with-docs <gulong>
  "A @code{<gvalue>} class for unsigned ``long'' (32- or 64-bit)
values."
  (gvalued-type->class gtype:gulong))
(define-with-docs <gint64>
  "A @code{<gvalue>} class for signed 64-bit values."
  (gvalued-type->class gtype:gint64))
(define-with-docs <guint64>
  "A @code{<gvalue>} class for unsigned 64-bit values."
  (gvalued-type->class gtype:guint64))
(define-with-docs <gfloat>
  "A @code{<gvalue>} class for 32-bit floating-point values."
  (gvalued-type->class gtype:gfloat))
(define-with-docs <gdouble>
  "A @code{<gvalue>} class for 64-bit floating-point values."
  (gvalued-type->class gtype:gdouble))
(define-with-docs <gchararray>
  "A @code{<gvalue>} class for arrays of 8-bit values (C strings)."
  (gvalued-type->class gtype:gchararray))
(define-with-docs <gboxed>
  "A @code{<gvalue>} class for ``boxed'' types, a way of wrapping
generic C structures. Use @code{gvalue->type} on an instance of this
class to determine what type it holds."
  (gvalued-type->class gtype:gboxed))
(define-with-docs <gvalue-array>
  "A @code{<gvalue>} class for arrays of @code{<gvalue>}."
  (gvalued-type->class gtype:gvalue-array))
(define-with-docs <gboxed-scm>
  "A @code{<gboxed>} class for holding arbitrary Scheme objects."
  (gvalued-type->class gtype:gboxed-scm))

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
(define-class-with-docs <genum> ()
  "A @code{<gvalue>} base class for enumerated values. Users may define
new enumerated value types via subclssing from @code{<genum>}, passing
@code{#:vtable @var{table}} as an initarg, where @var{table} should be
in a format suitable for passing to @code{genum-register-static}."
  (genum-values #:allocation #:each-subclass)
  #:gtype gtype:genum
  #:metaclass <genum-class>)
(define (genum-class->value-table class)
  "Return the vtable of possible values for @var{class}. The same as
@code{genum-type-get-values}, but operates on classes."
  (class-slot-ref class 'genum-values))

(define-class-with-docs <gflags> ()
  "A @code{<gvalue>} base class for flag values. Users may define new
flag value types via subclssing from @code{<gflags>}, passing
@code{#:vtable @var{table}} as an initarg, where @var{table} should be
in a format suitable for passing to @code{gflags-register-static}."
  (genum-values #:allocation #:each-subclass) ;; FIXME
  #:gtype gtype:gflags
  #:metaclass <gflags-class>)
(define (gflags-class->value-table class)
  "Return the vtable of possible values for @var{class}. The same as
@code{gflags-type-get-values}, but operates on classes."
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
      (display #\space file)
      (display (gflags->symbol-list obj)))))

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
  "Convert the enumerated value @var{obj} from a @code{<gvalue>} to its
symbol representation (its ``nickname'')."
  (let* ((type (gvalue->type obj))
         (enum-values (genum-type-get-values type))
         (value (gvalue-primitive-get obj))
         (the-value (enum-by-index type value)))
    (car the-value)))

(define (genum->name obj)
  "Convert the enumerated value @var{obj} from a @code{<gvalue>} to its
representation as a string (its ``name'')."
  (let* ((type (gvalue->type obj))
         (enum-values (genum-type-get-values type))
         (value (gvalue-primitive-get obj))
         (the-value (enum-by-index type value)))
    (cadr the-value)))

(define (genum->value obj)
  "Convert the enumerated value @var{obj} from a @code{<gvalue>} to its
representation as an integer."
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
                  (if (= (logand value f) f)
                      (set! element-list (append! element-list (list x))))))
              (vector->list flags-values))
    element-list))

(define (gflags->symbol-list obj)
  "Convert the flags value @var{obj} from a @code{<gvalue>} to a list of
the symbols that it represents."
  (let* ((element-list (gflags->element-list obj)))
    (map (lambda (x)
           (car x))
         element-list)))

(define (gflags->name-list obj)
  "Convert the flags value @var{obj} from a @code{<gvalue>} to a list of
strings, the names of the values it represents."
  (let* ((element-list (gflags->element-list obj)))
    (map (lambda (x)
           (cadr x))
         element-list)))

(define (gflags->value-list obj)
  "Convert the flags value @var{obj} from a @code{<gvalue>} to a list of
integers, which when @code{logand}'d together yield the flags' value."
  (let* ((element-list (gflags->element-list obj)))
    (map (lambda (x)
           (caddr x))
         element-list)))
