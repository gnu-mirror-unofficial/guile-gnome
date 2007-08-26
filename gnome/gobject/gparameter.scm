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
;; As a technical detail, the C structure @code{GParamSpec} is wrapped
;; at two levels. One is a mapping of the C structure to a Guile
;; structure. The other is a GOOPS representation. The low level is
;; called @code{gparam-struct}, and the high level is called
;; @code{<gparam>}. @code{gparam-struct} is a generic container of any
;; type. @code{<gparam>} has subclasses for the various kinds of
;; parameter types: @code{<gparam-int>}, @code{<gparam-object>}, etc.

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
                <gparam-pointer> <gparam-string> <gparam-object>
                <gparam-boxed> <gparam-enum> <gparam-flags>
                ;; Helper class
                <gparam-spec-flags>
                ;; Param structs
                gparam-struct:name gparam-struct:nick gparam-struct:blurb
                gparam-struct:flags gparam-struct:param-type
                gparam-struct:value-type gparam-struct:owner-type
                gparam-struct:args
                ;; From C:
                gparam->param-struct gparam->value-type
                ;; Limits
                gparameter:uint-max gparameter:int-min gparameter:int-max
                gparameter:ulong-max gparameter:long-min
                gparameter:long-max gparameter:uint64-max
                gparameter:int64-min gparameter:int64-max
                gparameter:float-max gparameter:float-min
                gparameter:double-max gparameter:double-min
                gparameter:byte-order

                ;; FIXME: there are also gtypes being exported by the C
                ;; code.
                ))

(use-modules ((srfi srfi-1) #:select (zip)))

;; The C code needs to reference <gparam> for use in its predicates.
;; Define it now before loading the library.
(define-class <gparam-class> (<gtype-instance-class>))
(define-class-with-docs <gparam> (<gtype-instance>)
  "The base class for GLib parameter objects."
  #:gtype gtype:gparam
  #:metaclass <gparam-class>)

(dynamic-call "scm_init_gnome_gobject_parameters"
              (dynamic-link *guile-gnome-gobject-lib-path*))

(define parameters-module (current-module))
(define gparam-struct-arg-info
  '(("GParamChar"    . (gtype:gchar
                        (#:minimum char? (integer->char 0))
                        (#:maximum char? (integer->char 127))
                        (#:default-value char? (integer->char 127))))
    ("GParamUChar"   . (gtype:guchar
                        (#:minimum char? (integer->char 0))
                        (#:maximum char? (integer->char 255))
                        (#:default-value char? (integer->char 255))))
    ("GParamBoolean" . (gtype:gboolean
                        (#:default-value boolean? #f)))
    ("GParamInt"     . (gtype:gint
                        (#:minimum integer? gparameter:int-min)
                        (#:maximum integer? gparameter:int-max)
                        (#:default-value integer? 0)))
    ("GParamUInt"    . (gtype:guint
                        (#:minimum integer? 0)
                        (#:maximum integer? gparameter:uint-max)
                        (#:default-value integer? 0)))
    ("GParamLong"    . (gtype:glong
                        (#:minimum integer? gparameter:long-min)
                        (#:maximum integer? gparameter:long-max)
                        (#:default-value integer? 0)))
    ("GParamULong"   . (gtype:gulong
                        (#:minimum integer? 0)
                        (#:maximum integer? gparameter:ulong-max)
                        (#:default-value integer? 0)))
    ("GParamInt64"   . (gtype:gint
                        (#:minimum integer? gparameter:int64-min)
                        (#:maximum integer? gparameter:int64-max)
                        (#:default-value integer? 0)))
    ("GParamUInt64"  . (gtype:guint
                        (#:minimum integer? 0)
                        (#:maximum integer? gparameter:uint64-max)
                        (#:default-value integer? 0)))
    ("GParamFloat"   . (gtype:gfloat
                        (#:minimum real? (- 0 gparameter:float-max))
                        (#:maximum real? gparameter:float-max)
                        (#:default-value real? 0.0)))
    ("GParamDouble"  . (gtype:gdouble
                        (#:minimum real? (- 0 gparameter:double-max))
                        (#:maximum real? gparameter:double-max)
                        (#:default-value real? 0.0)))
    ("GParamPointer" . (gtype:gpointer))
    ("GParamString"  . (gtype:gchararray
                        (#:default-value string? "")))
    ("GParamObject"  . (gtype:gobject
                        (#:object-type gtype? *unspecified*)))
    ("GParamBoxed"   . (gtype:gboxed
                        (#:boxed-type gtype? *unspecified*)))
    ("GParamEnum"    . (gtype:genum
                        (#:enum-type gtype? *unspecified*)
                        (#:default-value number? *unspecified*)))
    ("GParamFlags"   . (gtype:gflags
                        (#:flags-type gtype? *unspecified*)
                        (#:default-value number? *unspecified*)))
    ("GParamValueArray"   . (gtype:gvalue-array
                             (#:flags-type gtype? *unspecified*)
                             (#:element-param gparam-struct? *unspecified*)))
    ))

(define (gparam-struct:name param-struct)
  "Retrieve the name from a @code{gparam-struct}."
  (struct-ref param-struct gparam-struct-name))

(define (gparam-struct:nick param-struct)
  "Retrieve the `nickname' from a @code{gparam-struct}."
  (struct-ref param-struct gparam-struct-nick))

(define (gparam-struct:blurb param-struct)
  "Retrieve the `blurb', a short descriptive string, from a
@code{gparam-struct}."
  (struct-ref param-struct gparam-struct-blurb))

(define (gparam-struct:flags param-struct)
  "Retrieve the flags from a @code{gparam-struct}."
  (struct-ref param-struct gparam-struct-flags))

(define (gparam-struct:param-type param-struct)
  "Retrieve the GParam type from a @code{gparam-struct}, for example
@code{gtype:gparam-uint64}."
  (struct-ref param-struct gparam-struct-param-type))

(define (gparam-struct:value-type param-struct)
  "Retrieve the value type from a @code{gparam-struct}, for example
@code{gtype:guint64}."
  (struct-ref param-struct gparam-struct-value-type))

(define (gparam-struct:owner-type param-struct)
  "Retrieve the `owner type' from a @code{gparam-struct}. Appears to be
stored into GLib param specs, but never used."
  (struct-ref param-struct gparam-struct-owner-type))

(define (gparam-struct:args param-struct)
  "Retrieve the arguments from a @code{gparam-struct}, as a list. The
length and composition of the arguments depends on the parameter type."
  (let ((n-args (struct-ref param-struct gparam-struct-n-args))
        (offset gparam-struct-args)
        (param-type (gparam-struct:param-type param-struct)))
    (zip
     (map car (cdr (assoc-ref gparam-struct-arg-info (gtype-name param-type))))
     (let loop ((arg (+ offset (1- n-args))) (ret '()))
       (if (>= arg offset)
           (loop (1- arg) (cons (struct-ref param-struct arg) ret))
           ret)))))

(define-with-docs <gparam-char>
  "Parameter for @code{<gchar>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-char))
(define-with-docs <gparam-uchar>
  "Parameter for @code{<guchar>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-uchar))
(define-with-docs <gparam-boolean>
  "Parameter for @code{<gboolean>} values. 1 argument: default value."
  (gtype->class gtype:gparam-boolean))
(define-with-docs <gparam-int>
  "Parameter for @code{<gint>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-int))
(define-with-docs <gparam-uint>
  "Parameter for @code{<guint>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-uint))
(define-with-docs <gparam-long>
  "Parameter for @code{<glong>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-long))
(define-with-docs <gparam-ulong>
  "Parameter for @code{<gulong>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-ulong))
(define-with-docs <gparam-int64>
  "Parameter for @code{<gint64>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-int64))
(define-with-docs <gparam-uint64>
  "Parameter for @code{<guint64>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-uint64))
(define-with-docs <gparam-float>
  "Parameter for @code{<gfloat>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-float))
(define-with-docs <gparam-double>
  "Parameter for @code{<gdouble>} values. 3 arguments: minimum, maximum,
and default values."
  (gtype->class gtype:gparam-double))
(define-with-docs <gparam-pointer>
  "Parameter for @code{<gpointer>} values. No arguments."
  (gtype->class gtype:gparam-pointer))
(define-with-docs <gparam-string>
  "Parameter for @code{<gchararray>} values. 1 argument: the default
value, which may be @code{#f}."
  (gtype->class gtype:gparam-string))
(define-with-docs <gparam-object>
  "Parameter for @code{<gobject>} values. 1 argument: the @code{<gtype>}
of the value."
  (gtype->class gtype:gparam-object))
(define-with-docs <gparam-boxed>
  "Parameter for @code{<gboxed>} values. 1 argument: the @code{<gtype>}
of the value."
  (gtype->class gtype:gparam-boxed))
(define-with-docs <gparam-enum>
  "Parameter for @code{<genum>} values. 2 arguments: the @code{<gtype>}
of the value, and the default value."
  (gtype->class gtype:gparam-enum))
(define-with-docs <gparam-flags>
  "Parameter for @code{<gflags>} values. 2 arguments: the @code{<gtype>}
of the value, and the default value."
  (gtype->class gtype:gparam-flags))

;;;
;;; {Instance Initialization}
;;;

(define (make-param-struct-args type initargs)
  (let* ((args (or (assoc-ref gparam-struct-arg-info (gtype-name type))
		   (gruntime-error "Unknown type: ~A" type))))
    (map (lambda (argdesc)
	   (let* ((value (get-keyword (car argdesc) initargs
				      (eval (caddr argdesc) parameters-module))))
	     (if (unspecified? value)
                 (gruntime-error "Missing init keyword: ~A " (car argdesc)))
	     (or (eval (list (cadr argdesc) value) parameters-module)
                 ;; Accept gtype-classes where we can take gtypes
                 (if (and (eq? (eval (cadr argdesc) parameters-module) gtype?)
                          (is-a? value <gtype-class>))
                     (begin (set! value (gtype-class->type value)) #t)
                     (gruntime-error "Wrong init keyword ~A: ~A" (car argdesc) value)))
	     value))
	 (cdr args))))

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

(define-method (initialize (instance <gparam>) initargs)
  (cond
   ((get-keyword #:%real-instance initargs #f)
    => (lambda (gtype-instance)
         (slot-set! instance 'gtype-instance gtype-instance)))
   (else
    ;; nothing for next-method to do
    (let* ((class (class-of instance))
           (type (gtype-class->type class))
           (struct (or (get-keyword #:param-struct initargs #f)
                       (let* ((args (make-param-struct-args type initargs))
                              (name (or (get-keyword #:name initargs #f)
                                        (gruntime-error "Missing #:name keyword")))
                              (nick (get-keyword #:nick initargs #f))
                              (blurb (get-keyword #:blurb initargs #f))
                              (arg-info (or (assoc-ref gparam-struct-arg-info (gtype-name type))
                                            (gruntime-error "Unknown type: ~A" type)))
                              (value-type (eval (car arg-info) parameters-module))
                              (flags (apply
                                      +
                                      (gflags->value-list
                                       (make <gparam-spec-flags> #:value
                                             (get-keyword #:flags initargs '(read write))))))
                              (owner-type type))
                         (or (symbol? name)
                             (gruntime-error "Wrong #:name keyword"))
                         (or (or (eq? nick #f) (string? nick))
                             (gruntime-error "Wrong #:nick keyword"))
                         (or (or (eq? blurb #f) (string? blurb))
                             (gruntime-error "Wrong #:blurb keyword"))
                         (apply make-struct gparam-struct-vtable (length args) #f #f
                                name nick blurb flags type value-type owner-type args)))))
      (gparam-primitive-create class type instance struct)))))

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (write (param <gparam>) file)
  (let ((class (class-of param)))
    (if (slot-bound? class 'name)
        (begin
          (display "#<" file)
          (display (class-name class) file)
          (display #\space file)
          (display-address param file)
          (display #\space file)
          (if (slot-ref param 'gtype-instance)
              (write (gparam->param-struct param) file)
              (display "(uninitialized)" file))
          (display #\> file))
        (next-method))))
