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
;;The GObject wrapper for Guile (primitive routines).
;;
;;; Code:

(define-module (gnome gobject primitives)
  :use-module (oop goops)
  :use-module (ice-9 documentation)
  :use-module (gnome gobject gw-gobject)
  :use-module (srfi srfi-1) ; zip
  :re-export  (%init-gnome-gobject
               %post-init-gnome-gobject
               gtype-name
               gtype-from-name
               gtype-from-instance
               gtype-parent
               gtype-is-a?
               gtype-is-classed?
               gtype-is-instantiatable?
               g-source-set-closure)
  :export     (;; Classes
               <gtype-class> <gtype-instance-class> <gtype-instance>
               ;; Misc
               gruntime-error define-with-docs define-generic-with-docs
               gtype-instance:write
               ;; Enums
               enum-by-index enum-by-symbol enum-by-name 
               genum->value-table genum->symbol genum->name
               ;; Flags
               flags-by-index flags-by-symbol flags-by-name 
               gflags->element-list gflags->symbol-list gflags->name-list
               gflags->value-list
               ;; Signals
               gsignal:id gsignal:name gsignal:interface-type
               gsignal:return-type gsignal:param-types
               ;; Paramspecs
               gparam-spec:name gparam-spec:nick gparam-spec:blurb
               gparam-spec:flags gparam-spec:param-type
               gparam-spec:value-type gparam-spec:owner-type
               gparam-spec:args
               ;; The C code also exports bindings
               ))

(define (gruntime-error format-string . args)
  (save-stack)
  (scm-error 'gruntime-error #f format-string args '()))

(defmacro define-with-docs args
  (define (syntax)
    (error "bad syntax" (list 'define-public args)))
  (define (get-name n)
    (cond
      ((symbol? n) n)
      ((pair? n) (get-name (car n)))
      (else (syntax))))
  (define (get-documentation n)
    (cond
      ((and (pair? n) (string? (car n))) (car n))
      (else (syntax))))
  (cond
    ((null? args)
     (syntax))
    (#t
     (let ((name (get-name (car args)))
	   (object-documentation (get-documentation (cdr args))))
       `(begin
	  (define ,(car args) ,@(cddr args))
	  (set-object-property! ,name 'documentation ,object-documentation)
          *unspecified*)))))

(define-macro (define-generic-with-docs name documentation)
  `(define-with-docs ,name ,documentation
     (make-generic ',name)))

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

(define-generic gtype-instance:write)

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

(%init-gnome-gobject-primitives)

(define (find-enum vtable func index)
  (let loop ((l (vector->list vtable)))
    (if (null? l)
	(gruntime-error "No such value in ~A: ~A" vtable index)
	(begin
	  (if (equal? (func (car l)) index)
	      (car l)
	      (loop (cdr l)))))))

(define (enum-by-index type index)
  (find-enum (genum-primitive-get-values type) (lambda (l) (caddr l)) index))

(define (enum-by-name type name)
  (find-enum (genum-primitive-get-values type) (lambda (l) (cadr l)) name))

(define (enum-by-symbol type symbol)
  (find-enum (genum-primitive-get-values type) (lambda (l) (car l)) symbol))

(define (flags-by-index type index)
  (find-enum (gflags-primitive-get-values type) (lambda (l) (caddr l)) index))

(define (flags-by-name type name)
  (find-enum (gflags-primitive-get-values type) (lambda (l) (cadr l)) name))

(define (flags-by-symbol type symbol)
  (find-enum (gflags-primitive-get-values type) (lambda (l) (car l)) symbol))

(define (genum->value-table obj)
  (if (gvalue? obj)
    (genum-primitive-get-values (gvalue->type obj))
    (genum-primitive-get-values obj)))

(define (genum->symbol obj)
  (let* ((type (gvalue->type obj))
	 (enum-values (genum-primitive-get-values type))
	 (value (gvalue-primitive-get obj))
	 (the-value (enum-by-index type value)))
    (car the-value)))

(define (genum->name obj)
  (let* ((type (gvalue->type obj))
	 (enum-values (genum-primitive-get-values type))
	 (value (gvalue-primitive-get obj))
	 (the-value (enum-by-index type value)))
    (cadr the-value)))

(define (gflags->element-list obj)
  (let* ((type (gvalue->type obj))
	 (flags-values (gflags-primitive-get-values type))
	 (value (gvalue-primitive-get obj))
	 (element-list '()))
    (for-each (lambda (x)
		(let ((f (caddr x)))
		  (if (gflags-primitive-bit-set? value f)
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

(define-public gparam-spec-type-args
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
    ))

(define (gparam-spec:name pspec)
  (struct-ref pspec gparam-spec-name))

(define (gparam-spec:nick pspec)
  (struct-ref pspec gparam-spec-nick))

(define (gparam-spec:blurb pspec)
  (struct-ref pspec gparam-spec-blurb))

(define (gparam-spec:flags pspec)
  (struct-ref pspec gparam-spec-flags))

(define (gparam-spec:param-type pspec)
  (struct-ref pspec gparam-spec-param-type))

(define (gparam-spec:value-type pspec)
  (struct-ref pspec gparam-spec-value-type))

(define (gparam-spec:owner-type pspec)
  (struct-ref pspec gparam-spec-owner-type))

(define (gparam-spec:args pspec)
  (let ((n-args (struct-ref pspec gparam-spec-n-args))
        (offset gparam-spec-args)
        (param-type (gparam-spec:param-type pspec)))
    (zip
     (map car (cdr (assoc-ref gparam-spec-type-args (gtype-name param-type))))
     (let loop ((arg (+ offset (1- n-args))) (ret '()))
       (if (>= arg offset)
           (loop (1- arg) (cons (struct-ref pspec arg) ret))
           ret)))))
