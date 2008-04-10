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
;; Parameters are constraints for values, both in type and in range.
;; This module wraps the parameters code of the GLib type system,
;; defining C classes such that parameters may be manipulated and
;; created from Scheme.
;;
;; There is a parameter class for each type of parameter:
;; @code{<gparam-int>}, @code{<gparam-object>}, etc.

;;
;;; Code:

(define-module (gnome gobject gparameter)
  #:use-module (oop goops)
  #:use-module (gnome gobject config)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject gtype)
  #:use-module (gnome gobject gvalue)

  #:export     ( ;; GOOPS parameter base class
                <gparam>
                ;; Parameter classes
                <gparam-char> <gparam-uchar> <gparam-boolean> <gparam-int>
                <gparam-uint> <gparam-long> <gparam-ulong> <gparam-int64>
                <gparam-uint64> <gparam-float> <gparam-double>
                <gparam-pointer> <gparam-string>
                <gparam-boxed> <gparam-enum> <gparam-flags>
                ;; Helper class
                <gparam-spec-flags>
                ;; Limits
                gparameter:uint-max gparameter:int-min gparameter:int-max
                gparameter:ulong-max gparameter:long-min
                gparameter:long-max gparameter:uint64-max
                gparameter:int64-min gparameter:int64-max
                gparameter:float-max gparameter:float-min
                gparameter:double-max gparameter:double-min
                gparameter:byte-order
                ))

(define-class-with-docs <gparam-spec-flags> (<gflags>)
  "A @code{<gflags>} type for the flags allowable on a @code{<gparam>}:
@code{read}, @code{write}, @code{construct}, @code{construct-only}, and
@code{lax-validation}."
  #:vtable
  #((read "Readable" 1)
    (write "Writable" 2)
    (construct "Set on object construction" 4)
    (construct-only "Only set on object construction" 8)
    (lax-validation "Don't require strict validation on parameter conversion" 16)))

;; The C code needs to reference <gparam> for use in its predicates.
;; Define it now before loading the library.
(define-class <gparam-class> (<gtype-class>)
  (value-type #:init-keyword #:value-type))

(define-method (compute-get-n-set (class <gparam-class>) s)
  (case (slot-definition-allocation s)
    ((#:checked)
     (let ((already-allocated (slot-ref class 'nfields))
           (pred (get-keyword #:pred (slot-definition-options s) #f))
           (trans (get-keyword #:trans (slot-definition-options s) #f)))
       (or pred (gruntime-error "Missing #:pred for #:checked slot"))
       ;; allocate a field in the struct
       (slot-set! class 'nfields (+ already-allocated 1))
       ;; struct-ref and struct-set! don't work on the structs that back
       ;; GOOPS objects, because they are "light structs", without the
       ;; hidden word that says how many fields are in the struct.
       ;; Patches submitted to guile-devel on 10 April 2008. Until then,
       ;; use our own struct accessors.
       (list (lambda (instance)
               (%hacky-struct-ref instance already-allocated))
             (lambda (instance value)
               (let ((value (if trans (trans value) value)))
                 (if (pred value)
                     (%hacky-struct-set! instance already-allocated value)
                     (gruntime-error
                      "Bad value for slot ~A on instance ~A: ~A"
                      (slot-definition-name s) instance value)))))))

    (else (next-method))))

(define-class-with-docs <gparam> (<gtype-instance>)
  "The base class for GLib parameter objects. (Doc slots)"
  (name #:init-keyword #:name #:allocation #:checked #:pred symbol?)
  (nick #:init-keyword #:nick #:allocation #:checked #:pred string?)
  (blurb #:init-keyword #:blurb #:allocation #:checked #:pred string?)
  (flags #:init-keyword #:flags #:init-value '(read write)
         #:allocation #:checked #:pred number?
         #:trans (lambda (x)
                   (apply + (gflags->value-list
                             (make <gparam-spec-flags> #:value x)))))
  #:gtype-name "GParam"
  #:metaclass <gparam-class>)

(dynamic-call "scm_init_gnome_gobject_parameters"
              (dynamic-link *guile-gnome-gobject-lib-path*))

(define-class-with-docs <gparam-char> (<gparam>)
  "Parameter for @code{<gchar>} values."
  (minimum
   #:init-keyword #:minimum #:init-value (integer->char 0)
   #:allocation #:checked #:pred char?)
  (maximum
   #:init-keyword #:maximum #:init-value (integer->char 127)
   #:allocation #:checked #:pred char?)
  (default-value
   #:init-keyword #:default-value #:init-value (integer->char 127)
   #:allocation #:checked #:pred char?)
  #:value-type <gchar>
  #:gtype-name "GParamChar")

(define-class-with-docs <gparam-uchar> (<gparam>)
  "Parameter for @code{<guchar>} values."
  (minimum
   #:init-keyword #:minimum #:init-value (integer->char 0)
   #:allocation #:checked #:pred char?)
  (maximum
   #:init-keyword #:maximum #:init-value (integer->char 255)
   #:allocation #:checked #:pred char?)
  (default-value
   #:init-keyword #:default-value #:init-value (integer->char 255)
   #:allocation #:checked #:pred char?)
  #:value-type <guchar>
  #:gtype-name "GParamUChar")

(define-class-with-docs <gparam-boolean> (<gparam>)
  "Parameter for @code{<gboolean>} values."
  (default-value
   #:init-keyword #:default-value #:init-value #f
   #:allocation #:checked #:pred boolean?)
  #:value-type <gboolean>
  #:gtype-name "GParamBoolean")

(define-class-with-docs <gparam-int> (<gparam>)
  "Parameter for @code{<gint>} values."
  (minimum
   #:init-keyword #:minimum #:init-value gparameter:int-min
   #:allocation #:checked #:pred integer?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:int-max
   #:allocation #:checked #:pred integer?)
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred integer?)
  #:value-type <gint>
  #:gtype-name "GParamInt")

(define-class-with-docs <gparam-uint> (<gparam>)
  "Parameter for @code{<guint>} values."
  (minimum
   #:init-keyword #:minimum #:init-value 0
   #:allocation #:checked #:pred integer?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:uint-max
   #:allocation #:checked #:pred integer?)
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred integer?)
  #:value-type <guint>
  #:gtype-name "GParamUInt")

(define-class-with-docs <gparam-long> (<gparam>)
  "Parameter for @code{<glong>} values."
  (minimum
   #:init-keyword #:minimum #:init-value gparameter:long-min
   #:allocation #:checked #:pred integer?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:long-max
   #:allocation #:checked #:pred integer?)
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred integer?)
  #:value-type <glong>
  #:gtype-name "GParamLong")

(define-class-with-docs <gparam-ulong> (<gparam>)
  "Parameter for @code{<gulong>} values."
  (minimum
   #:init-keyword #:minimum #:init-value 0
   #:allocation #:checked #:pred integer?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:ulong-max
   #:allocation #:checked #:pred integer?)
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred integer?)
  #:value-type <gulong>
  #:gtype-name "GParamULong")

(define-class-with-docs <gparam-int64> (<gparam>)
  "Parameter for @code{<gint64>} values."
  (minimum
   #:init-keyword #:minimum #:init-value gparameter:int64-min
   #:allocation #:checked #:pred integer?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:int64-max
   #:allocation #:checked #:pred integer?)
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred integer?)
  #:value-type <gint64>
  #:gtype-name "GParamInt64")

(define-class-with-docs <gparam-uint64> (<gparam>)
  "Parameter for @code{<guint64>} values."
  (minimum
   #:init-keyword #:minimum #:init-value 0
   #:allocation #:checked #:pred integer?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:uint64-max
   #:allocation #:checked #:pred integer?)
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred integer?)
  #:value-type <guint64>
  #:gtype-name "GParamUInt64")

(define-class-with-docs <gparam-float> (<gparam>)
  "Parameter for @code{<gfloat>} values."
  (minimum
   #:init-keyword #:minimum #:init-value (- gparameter:float-max)
   #:allocation #:checked #:pred real?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:float-max
   #:allocation #:checked #:pred real?)
  (default-value
   #:init-keyword #:default-value #:init-value 0.0
   #:allocation #:checked #:pred real?)
  #:value-type <gfloat>
  #:gtype-name "GParamFloat")

(define-class-with-docs <gparam-double> (<gparam>)
  "Parameter for @code{<gdouble>} values."
  (minimum
   #:init-keyword #:minimum #:init-value (- gparameter:double-max)
   #:allocation #:checked #:pred real?)
  (maximum
   #:init-keyword #:maximum #:init-value gparameter:double-max
   #:allocation #:checked #:pred real?)
  (default-value
   #:init-keyword #:default-value #:init-value 0.0
   #:allocation #:checked #:pred real?)
  #:value-type <gdouble>
  #:gtype-name "GParamDouble")

(define-class-with-docs <gparam-pointer> (<gparam>)
  "Parameter for @code{<gpointer>} values."
  #:value-type <gpointer>
  #:gtype-name "GParamPointer")

(define-class-with-docs <gparam-string> (<gparam>)
  "Parameter for @code{<gchararray>} values."
  (default-value
   #:init-keyword #:default-value #:init-value ""
   #:allocation #:checked #:pred (lambda (x) (or (not x) (string? x))))
  #:value-type <gchararray>
  #:gtype-name "GParamString")

(define (class-is-a? x is-a)
  (memq is-a (class-precedence-list x)))

(define-class-with-docs <gparam-boxed> (<gparam>)
  "Parameter for @code{<gboxed>} values."
  (boxed-type
   #:init-keyword #:boxed-type #:allocation #:checked
   #:pred (lambda (x) (class-is-a? x <gboxed>)))
  #:value-type <gboxed>
  #:gtype-name "GParamBoxed")

(define-class-with-docs <gparam-enum> (<gparam>)
  "Parameter for @code{<genum>} values."
  (enum-type
   #:init-keyword #:enum-type #:allocation #:checked
   #:pred (lambda (x) (class-is-a? x <genum>)))
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred number?)
  #:value-type <genum>
  #:gtype-name "GParamEnum")

(define-class-with-docs <gparam-flags> (<gparam>)
  "Parameter for @code{<gflags>} values."
  (flags-type
   #:init-keyword #:flags-type #:allocation #:checked
   #:pred (lambda (x) (class-is-a? x <gflags>)))
  (default-value
   #:init-keyword #:default-value #:init-value 0
   #:allocation #:checked #:pred number?)
  #:value-type <gflags>
  #:gtype-name "GParamFlags")

(define-class-with-docs <gparam-value-array> (<gparam>)
  "Parameter for @code{<gvalue-array>} values."
  (element-spec
   #:init-keyword #:element-spec #:allocation #:checked
   #:pred (lambda (x) (is-a? x <gparam>)))
  #:value-type <gvalue-array>
  #:gtype-name "GParamValueArray")

;;;
;;; {Instance Initialization}
;;;

;; fixme, make me more useful
(define-method (write (param <gparam>) file)
  (let ((class (class-of param))
        (loc (number->string (object-address param) 16)))
    (if (slot-bound? class 'name)
        (with-accessors (name)
          (format file "<~a ~a ~a>" (class-name class) (name param) loc))
        (format file "<~a (uninitialized) ~a>" (class-name class) loc))))
