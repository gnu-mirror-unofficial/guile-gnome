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
;; This is the GObject wrapper for Guile.
;;
;; See the guile-gnome tutorial for more details.
;;
;;; Code:

(define-module (gnome gobject gparameter)
  :use-module (oop goops)
  :use-module (gnome gobject gtype)
  :use-module (gnome gobject gvalue)

  :export     (;; GOOPS parameter base class
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
               gparam-struct:args gparam-struct-arg-info
               
               ;; From C:
               gparam->param-struct gparam->value-type))

(use-modules ((srfi srfi-1) #:select (zip)))

;; GParamSpec is wrapped at two levels. One is a mapping of the C
;; structure to a guile structure. The other is a GOOPS representation.
;; The low level is called gparam-struct, and the high level is called
;; <gparam>. gparam-struct is a generic container of any type. <gparam>
;; has subclasses for the various kinds of parameter types:
;; <gparam-int>, <gparam-object>, etc.

;; The C code needs to reference <gparam> for use in its predicates.
;; Define it now before loading the library.
(define-class <gparam-class> (<gtype-instance-class>))
(define-class <gparam> (<gtype-instance>)
  #:gtype gtype:gparam
  #:metaclass <gparam-class>)

(dynamic-call "scm_init_gnome_gobject_parameters"
              (dynamic-link "libguile-gnome-gobject"))

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
                        (#:minimum integer? gruntime:int-min)
                        (#:maximum integer? gruntime:int-max)
                        (#:default-value integer? 0)))
    ("GParamUInt"    . (gtype:guint
                        (#:minimum integer? 0)
                        (#:maximum integer? gruntime:uint-max)
                        (#:default-value integer? 0)))
    ("GParamLong"    . (gtype:glong
                        (#:minimum integer? gruntime:long-min)
                        (#:maximum integer? gruntime:long-max)
                        (#:default-value integer? 0)))
    ("GParamULong"   . (gtype:gulong
                        (#:minimum integer? 0)
                        (#:maximum integer? gruntime:ulong-max)
                        (#:default-value integer? 0)))
    ("GParamInt64"   . (gtype:gint
                        (#:minimum integer? gruntime:int64-min)
                        (#:maximum integer? gruntime:int64-max)
                        (#:default-value integer? 0)))
    ("GParamUInt64"  . (gtype:guint
                        (#:minimum integer? 0)
                        (#:maximum integer? gruntime:uint64-max)
                        (#:default-value integer? 0)))
    ("GParamFloat"   . (gtype:gfloat
                        (#:minimum real? (- 0 gruntime:float-max))
                        (#:maximum real? gruntime:float-max)
                        (#:default-value real? 0.0)))
    ("GParamDouble"  . (gtype:gdouble
                        (#:minimum real? (- 0 gruntime:double-max))
                        (#:maximum real? gruntime:double-max)
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
  (struct-ref param-struct gparam-struct-name))

(define (gparam-struct:nick param-struct)
  (struct-ref param-struct gparam-struct-nick))

(define (gparam-struct:blurb param-struct)
  (struct-ref param-struct gparam-struct-blurb))

(define (gparam-struct:flags param-struct)
  (struct-ref param-struct gparam-struct-flags))

(define (gparam-struct:param-type param-struct)
  (struct-ref param-struct gparam-struct-param-type))

(define (gparam-struct:value-type param-struct)
  (struct-ref param-struct gparam-struct-value-type))

(define (gparam-struct:owner-type param-struct)
  (struct-ref param-struct gparam-struct-owner-type))

(define (gparam-struct:args param-struct)
  (let ((n-args (struct-ref param-struct gparam-struct-n-args))
        (offset gparam-struct-args)
        (param-type (gparam-struct:param-type param-struct)))
    (zip
     (map car (cdr (assoc-ref gparam-struct-arg-info (gtype-name param-type))))
     (let loop ((arg (+ offset (1- n-args))) (ret '()))
       (if (>= arg offset)
           (loop (1- arg) (cons (struct-ref param-struct arg) ret))
           ret)))))

(define <gparam-char>    (gtype->class gtype:gparam-char))
(define <gparam-uchar>   (gtype->class gtype:gparam-uchar))
(define <gparam-boolean> (gtype->class gtype:gparam-boolean))
(define <gparam-int>     (gtype->class gtype:gparam-int))
(define <gparam-uint>    (gtype->class gtype:gparam-uint))
(define <gparam-long>    (gtype->class gtype:gparam-long))
(define <gparam-ulong>   (gtype->class gtype:gparam-ulong))
(define <gparam-int64>   (gtype->class gtype:gparam-int64))
(define <gparam-uint64>  (gtype->class gtype:gparam-uint64))
(define <gparam-float>   (gtype->class gtype:gparam-float))
(define <gparam-double>  (gtype->class gtype:gparam-double))
(define <gparam-pointer> (gtype->class gtype:gparam-pointer))
(define <gparam-string>  (gtype->class gtype:gparam-string))
(define <gparam-object>  (gtype->class gtype:gparam-object))
(define <gparam-boxed>   (gtype->class gtype:gparam-boxed))
(define <gparam-enum>    (gtype->class gtype:gparam-enum))
(define <gparam-flags>   (gtype->class gtype:gparam-flags))

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

(define-class <gparam-spec-flags> (<gflags>)
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
