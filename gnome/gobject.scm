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
;; In the GType runtime type system (also known as `gruntime'), each
;; type is associated with unique ID (a number). This is represented in
;; scheme with the GOOPS class, <gtype>. The following "simple" types
;; are exported:
;;
;;   gtype:gchar gtype:guchar gtype:gboolean gtype:gint gtype:guint
;;   gtype:glong gtype:gulong gtype:gint64 gtype:guint64 gtype:gfloat
;;   gtype:gdouble gtype:gchararray
;;
;; Each of these types is associated with a GOOPS class:
;;
;;   <gchar> <guchar> <gboolean> <gint> <guint> <glong> <gulong>
;;   <gint64> <guint64> <gfloat> <gdouble> <gchararray>
;;
;; These GOOPS classes can be used to create instances of their
;; respective types. You pass a value via the #:value initarg:
;;
;;   (make <gchar> #:value #\A)
;;   (make <gdouble> #:value 3.14159)
;;
;; All of the GType classes are instances of a metaclass, <gtype-class>.
;; It has the following slots:
;;
;;      gtype       - The corresponding <gtype> for the class. You can
;;                    slot-ref this directly, or use the
;;                    gtype-class->type function.
;;
;;      gtype-class - The corresponding primitive, opaque type class.
;;                    Private.
;;
;; Other types exported by (guile gobject) include:
;;
;;   gtype:genum gtype:gflags gtype:gboxed gtype:gclosure gtype:gparam
;;   gtype:gobject gtype:ginterface gtype:gboxed-scm
;;
;; Of course there are <gtype-class> instances for these as well. These
;; types are abstract base classes, and so are not directly
;; instantiatable. Read the reference manual for more information.
;;
;; There are also a number of typed parameter GTypes and classes,
;; for instance <gparam-int> and gtype:gparam-int.
;;
;; <gobject> has a parent class, <gtype-instance>, which corresponds to
;; GTypeInstance in C. <gtype-instance> has the following slot:
;;
;;      gtype-instance - The corresponding primitive, opaque type
;;                       instance, of type <%gtype-instance>.
;;
;; While there is a many-to-one relationship between <gtype-instance>
;; objects and their corresponding C objects, there is just one
;; <%gtype-instance> per C object. That means you should set scheme
;; object properties on the <%gtype-instance>, not the <gtype-instance>.
;;
;; Note that a <gtype-class> is a persistent, immortal object - it can
;; never be freed by GC. When you call `gtype->class' on a GType the
;; first time, this creates a new GOOPS class and "binds" it to that
;; type - so if you call `gtype->class' a second time on a type, you'll
;; get the same class back. The same applies for <gtype>s.
;;
;; If you create any instances of this or any of its child classes,
;; it will also create the corresponding C instance.
;;
;; So, to summarize, a <gobject> is a GObjectClass in C and an instance
;; of a <gobject> is a GObject in C.
;;
;;; Code:

(define-module (gnome gobject)
  :use-module (oop goops)
  :use-module (ice-9 documentation)
  :use-module (gnome gobject primitives)

  :re-export  (;; Classes
               <gtype> <gtype-class> <gtype-instance>
               ;; GType
               gtype-name gtype-from-name gtype-parent gtype-is-a?
               ;; GValue
               gvalue? gvalue->type
               ;; GEnum
               genum->value-table genum->symbol genum->name
               ;; GFlags
               gflags->symbol-list gflags->name-list gflags->value-list
               ;; GSignal
               gsignal:id gsignal:name gsignal:interface-type
               gsignal:return-type gsignal:param-types
               ;; GParamSpec
               gparam-spec:name gparam-spec:nick gparam-spec:blurb
               gparam-spec:flags gparam-spec:param-type
               gparam-spec:value-type gparam-spec:owner-type
               gparam-spec:args
               ;; Misc
               gruntime-error g-source-set-closure)

  :export     (;; Simple classes
               <gboolean> <gchar> <guchar> <gint> <guint> <glong>
               <gulong> <gint64> <guint64> <gfloat> <gdouble>
               <gchararray> <gboxed> <gboxed-scm>
               ;; Other classes
               <gclosure> <genum> <gflags> <gobject> <gparam> <gsignal>
               ;; Parameter classes
               <gparam-char> <gparam-uchar> <gparam-boolean> <gparam-int>
               <gparam-uint> <gparam-long> <gparam-ulong> <gparam-float>
               <gparam-double>  <gparam-pointer> <gparam-string> <gparam-object>
               <gparam-boxed> <gparam-enum> <gparam-flags> <ginterface>
               ;; Methods to override
               gtype-instance:initialize gobject:get-property
               gobject:set-property make-gobject-instance
               ;; GType <-> GTypeClass
               gtype->class gtype-class->type
               ;; GValue
               gvalue->scm scm->gvalue
               ;; Properties
               gobject-class-get-properties gobject-class-find-property
               gobject-class-get-property-names
               gobject-get-property gobject-set-property
               ;; Signals
               gtype-class-get-signals gtype-class-get-signal-names
               gtype-instance-signal-emit
               gtype-instance-signal-connect-data
               gtype-instance-signal-connect
               gtype-instance-signal-connect-after
               gsignal-handler-block gsignal-handler-unblock
               gsignal-handler-disconnect gsignal-handler-connected?
               gtype-class-create-signal gtype-class-define-signal
               ;; Misc
               gclosure-invoke gparam->pspec-struct))

(%init-gnome-gobject)

;;;
;;;
;;; {The Class Hierarchy}
;;;

(define (vector-map proc vector)
  (let* ((length (vector-length vector))
	 (result-vector (make-vector length)))
    (do ((index 0 (+ index 1)))
	((>= index length) result-vector)
      (vector-set! result-vector index (proc (vector-ref vector index))))))

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

(define (gobject-class-set-properties! class)
  (let* ((type (slot-ref class 'gtype))
 	 (properties-vector (gobject-primitive-get-properties type))
 	 (properties (vector-map (lambda (x)
 				   (let* ((pspec-struct (gparam-primitive->pspec-struct x))
 					  (param-type (gparam-spec:param-type pspec-struct))
 					  (param-class (gtype->class param-type)))
 				     (make param-class #:pspec-struct pspec-struct)))
 				 properties-vector)))
    (class-slot-set! class 'gobject-properties properties)))

(define (init-gobject-class type class initargs)
  (class-slot-set! class 'gsignals (gtype-primitive-get-signals type))
  (gobject-class-set-properties! class)

  ;; To expose slots as gobject properties, <gobject> will process a
  ;; #:param-spec slot option to create a new gobject property.
  (let loop ((slots (class-direct-slots class)))
    (if (not (null? slots))
        (let ((pspec (get-keyword #:param-spec
                                  (slot-definition-options (car slots))
                                  #f)))
          (if pspec
              (let* ((name (slot-definition-name (car slots)))
                     (property (apply make (append pspec (list #:name name)))))
                (gobject-class-install-property class property)))
          (loop (cdr slots)))))

  ;; We parse a #:signal initialization argument to install signals.
  (let loop ((args initargs))
    (if (not (null? args))
        (if (eq? (car args) #:signal)
            (let ((signal (cadr args)))
              (if (not (and (list? signal) (>= (length signal) 2)))
                  (gruntime-error "Invalid signal specification: ~A" signal))
              (let* ((name (car signal))
                     (return-type (or (cadr signal) gtype:void))
                     (param-types (cddr signal))
                     (generic (gtype-class-create-signal class name return-type param-types)))
                ;; Some magic to define the generic
                (module-define! (current-module) (gtype->method-name type name) generic)))
            (loop (cddr args))))))

(define (first-gobject-class cpl)
  (cond
   ((null? cpl)
    #f)
   ((memq <gobject> (class-precedence-list (car cpl)))
    (car cpl))
   (else
    (first-gobject-class (cdr cpl)))))

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
       (else
        (loop (cdr to-process) ret #t)))))))

(define-method (initialize (class <gtype-class>) initargs)
  (next-method)
  (let ((gtype (get-keyword #:gtype initargs #f))
        (gtype-name (get-keyword #:gtype-name initargs #f)))
    (if (and (not gtype) gtype-name (gtype-from-name gtype-name))
        (set! gtype (gtype-from-name gtype-name)))

    (if (not gtype)
        (let ((parent-class (first-gobject-class (cdr (class-precedence-list class)))))
          (if (not parent-class)
              (gruntime-error "initialize ~A: You can only derive from GObject classes"
                              class))
          ;; Subclass an existing GObject class.
          (set! gtype (gtype-register-static
                       (or gtype-name (class-name->gtype-name (slot-ref class 'name)))
                       (gtype-class->type parent-class))))

        (if (%gtype-lookup-class gtype)
            (gruntime-error "<gtype> ~A already has a GOOPS class, use gtype->class" gtype)))

    (%gtype-bind-to-class class gtype)

    (let* ((fundamental (gtype->fundamental gtype))
           (is-fundamental (eq? gtype fundamental)))
	(cond
	  ((eq? fundamental gtype:genum) 
	   (if (not is-fundamental)
	     (class-slot-set! class 'genum-values
			      (genum-primitive-get-values gtype))))

	  ((eq? fundamental gtype:gflags) 
	   (if (not is-fundamental)
	     (class-slot-set! class 'genum-values
			      (gflags-primitive-get-values gtype))))

	  ((eq? fundamental gtype:gobject)
	   (init-gobject-class gtype class initargs))

	  ((eq? fundamental gtype:ginterface)
           ;; It seems the signals aren't yet initialized until an
           ;; implementor class is initialized. So these signals will be
           ;; set again when an implementor is run through
           ;; %gtype-bind-to-class.
           (if is-fundamental
               (class-slot-set! class 'gsignals #())
               (class-slot-set! class 'gsignals (gtype-primitive-get-signals gtype))))))))

(define (get-direct-supers type)
  (if (not (gtype-parent type))
      (if (gtype-is-instantiatable? type)
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
      (let* ((class-name (gtype->class-name type))
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

(define <gchar>      (gtype->class gtype:gchar))
(define <guchar>     (gtype->class gtype:guchar))
(define <gboolean>   (gtype->class gtype:gboolean))
(define <gint>       (gtype->class gtype:gint))
(define <guint>      (gtype->class gtype:guint))
(define <glong>      (gtype->class gtype:glong))
(define <gulong>     (gtype->class gtype:gulong))
(define <gint64>     (gtype->class gtype:gint64))
(define <guint64>    (gtype->class gtype:guint64))
(define <gfloat>     (gtype->class gtype:gfloat))
(define <gdouble>    (gtype->class gtype:gdouble))
(define <gchararray> (gtype->class gtype:gchararray))

(define <gboxed>     (gtype->class gtype:gboxed))
(define <gboxed-scm> (gtype->class gtype:gboxed-scm))
(define <gparam>     (gtype->class gtype:gparam))

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

;; Enums, flags, closures, interfaces, and objects all have special
;; slots, so we define the classes manually.
(define-class <genum> ()
  (genum-values #:allocation #:each-subclass)
  #:gtype gtype:genum
  #:metaclass <gtype-class>)

(define-class <gflags> ()
  (genum-values #:allocation #:each-subclass) ;; FIXME
  #:gtype gtype:gflags
  #:metaclass <gtype-class>)

(define-class <gclosure> ()
  closure
  return-type
  param-types
  #:gtype gtype:gclosure
  #:metaclass <gtype-class>)

(define-class <ginterface> (<gtype-instance>)
  (gsignals #:allocation #:each-subclass)
  #:gtype gtype:ginterface)

;; <gobject> offers a form of slot allocation where properties are set
;; on the GObject directly, not the GOOPS object. To implement this, we
;; make a new metaclass.
(define-class <gobject-class> (<gtype-instance-class>))
(define-method (compute-get-n-set (class <gobject-class>) s)
  (case (slot-definition-allocation s)
    ((#:instance)
     (let ((sym (slot-definition-name s)))
       (list
        (lambda (o) (gobject-get-data o sym))
        (lambda (o v) (gobject-set-data! o sym v)))))
    (else
     (next-method))))

(define-class <gobject> (<gtype-instance>)
  (gsignals #:allocation #:each-subclass)
  (gobject-properties #:allocation #:each-subclass)
  #:metaclass <gobject-class>
  #:gtype gtype:gobject)

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

;;;
;;; {Instance Allocation and Initialization}
;;;

(define-method (allocate-instance (class <gtype-class>) initargs)
  ;; Depending on what this method returns, the object might actually be
  ;; of a different type (like those with <gvalue> storage).
  (if (get-keyword #:%real-instance initargs #f)
      (next-method)
      (let* ((type (gtype-class->type class))
             (fundamental (gtype->fundamental type)))
        (cond
         ;; Basic types - implemented as GValues.
         ((gtype-primitive-basic? type)
          (gtype-primitive-create-basic-instance type))

         ;; Fundamental type - but not a basic one.
         ((eq? type fundamental)
          (gruntime-error "Can't make instances of fundamental type: ~S" type))

         ;; GEnum
         ((eq? fundamental gtype:genum)
          (gtype-primitive-create-basic-instance type))

         ;; GFlags
         ((eq? fundamental gtype:gflags)
          (gtype-primitive-create-basic-instance type))

         ;; GInterface
         ((eq? fundamental gtype:ginterface)
          (next-method))

         ;; GObject
         ((eq? fundamental gtype:gobject)
          (next-method))

         ;; GParam
         ((eq? fundamental gtype:gparam)
          (next-method))

         ;; GClosure
         ((eq? type gtype:gclosure)
          (next-method))

         ;; GBoxedScm
         ((eq? type gtype:gboxed-scm)
          (let* ((init-value (get-keyword #:value initargs *unspecified*)))
            (if (unspecified? init-value)
                (gruntime-error "Missing #:value argument"))
            (gboxed-scm-primitive-new init-value)))

         ;; Oooops. Unknown or non-instantiable type.
         (else
          (gruntime-error "Don't know how to make instances of this type: ~S" type))))))

;; This is a method so that it can be extended by subclasses, e.g. so
;; that <gtk-object> can implement explicit destruction.
(define-method (make-gobject-instance class type object options)
  (let* ((class-properties (gobject-class-get-properties class))
	 (init-properties '()))
    (let loop ((options options) (res '()))
      (cond ((null? options)
	     (reverse res))
	    ((null? (cdr options))
	     (goops-error "malformed argument list"))
	    ((not (keyword? (car options)))
	     (goops-error "malformed argument list"))
	    (else
	     (let* ((option-value (cadr options))
		    (param-name (keyword->symbol (car options)))
		    (param (or (find-property class-properties param-name)
			       (gruntime-error "No such property in class ~S: ~S" class param-name)))
		    (pspec (gparam->pspec-struct param))
		    (pspec-value-type (gparam-spec:value-type pspec))
		    (pspec-value (scm->gvalue pspec-value-type option-value)))
	       (set! init-properties
		     (append
		      init-properties (list (cons param-name pspec-value))))
	       (loop (cddr options)
		     (cons (cadr options)
			   (cons (car options)
				 res)))))))
    (gobject-primitive-create-instance class type object
				       (list->vector init-properties))))

(define (make-ginterface-instance class type instance initargs)
  (let ((value (get-keyword #:value initargs *unspecified*)))
    (if (unspecified? value)
        (gruntime-error "Missing #:value argument initializing an interface"))
    (if (not (is-a? value <gobject>))
        (gruntime-error "Only interfaces implemented by objects are allowed (~A)" value))
    (if (not (gtype-is-a? (gtype-class->type (class-of value))
                          type))
        (gruntime-error "~A's class (~A) does not implement ~A."
                        value (class-of value) class))
    (slot-set! instance 'gtype-instance (slot-ref value 'gtype-instance))))

(define (make-pspec-args type initargs)
  (let* ((args (or (assoc-ref gparam-spec-type-args (gtype-name type))
		   (gruntime-error "Unknown type: ~A" type))))
    (map (lambda (argdesc)
	   (let* ((value (get-keyword (car argdesc) initargs
				      (eval (caddr argdesc) (current-module)))))
	     (if (unspecified? value)
	       (gruntime-error "Missing init keyword: ~A " (car argdesc)))
	     (or (eval (list (cadr argdesc) value) (current-module))
		 (gruntime-error "Wrong init keyword ~A: ~A" (car argdesc) value))
	     value))
	 (cdr args))))

;; no need to export this, it's really just for private usage
(define <guile-param-spec-flags>
  (gtype->class
   (gflags-register-static
    "GuileParamSpecFlags"
    #((read "Readable" 1)
      (write "Writable" 2)
      (construct "Set on object construction" 4)
      (construct-only "Only set on object construction" 8)
      (lax-validation "Don't require strict validation on parameter conversion" 16)))))

(define (make-gparam-instance class type object initargs)
  (let* ((type (gtype-class->type class))
	 (pspec (or (get-keyword #:pspec-struct initargs #f)
		    (let* ((args (make-pspec-args type initargs))
			   (name (or (get-keyword #:name initargs #f)
				     (gruntime-error "Missing #:name keyword")))
			   (nick (get-keyword #:nick initargs #f))
			   (blurb (get-keyword #:blurb initargs #f))
			   (pspec-descr (or (assoc-ref gparam-spec-type-args (gtype-name type))
					    (gruntime-error "Unknown type: ~A" type)))
			   (value-type (eval (car pspec-descr) (current-module)))
			   (flags (apply + (gflags->value-list
                                            (make <guile-param-spec-flags> #:value
                                                  (get-keyword #:flags initargs '())))))
			   (owner-type type))
		      (or (symbol? name)
			  (gruntime-error "Wrong #:name keyword"))
		      (or (or (eq? nick #f) (string? nick))
			  (gruntime-error "Wrong #:nick keyword"))
		      (or (or (eq? blurb #f) (string? blurb))
			  (gruntime-error "Wrong #:blurb keyword"))
		      (apply make-struct gparam-spec-struct-vtable (length args) #f #f
			     name nick blurb flags type value-type owner-type args)))))
    (gparam-primitive-create class type object pspec)))

;; The MOP specifies that make-instance normally calls
;; `allocate-instance' and then `initialize'. We call
;; `allocate-instance', but then we don't call `initialize' for
;; objects, because it is called by the gobject system.
(define-method (make-instance (class <gtype-instance-class>) . initargs)
  (let ((instance (allocate-instance class initargs)))
    (if (get-keyword #:%real-instance initargs #f)
        (slot-set! instance 'gtype-instance (get-keyword #:%real-instance initargs #f))
        (let* ((class (class-of instance))
               (type (gtype-class->type class))
               (fundamental (gtype->fundamental type))
               (is-fundamental (eq? type fundamental)))
          (cond
           ;; GObject
           ((eq? fundamental gtype:gobject)
            (make-gobject-instance class type instance initargs))
           
           ;; GInterface
           ((eq? fundamental gtype:ginterface)
            (make-ginterface-instance class type instance initargs)
            (initialize instance initargs))
           
           ;; GParam
           ((eq? fundamental gtype:gparam)
            (make-gparam-instance class type instance initargs)
            (initialize instance initargs))
           
           (else
            (noop)))))
    instance))

;; Noop so that you can always call (next-method) without problems
(define-method (initialize (instance <gtype-instance>) initargs)
  *unspecified*)

(define-method (initialize (closure <gclosure>) initargs)
  (let* ((func (get-keyword #:func initargs *unspecified*))
	 (rettype (get-keyword #:return-type initargs *unspecified*))
	 (paramtypes (get-keyword #:param-types initargs *unspecified*)))
    (if (unspecified? func)
      (gruntime-error "Missing #:func argument"))
    (or (procedure? func)
	(gruntime-error "Wrong type argument: ~S" func))
    (next-method)
    (let* ((newfunc (lambda (. args)
		      (let* ((newargs (map (lambda (x) (gvalue->scm x)) args))
			     (retval (apply func newargs)))
			(scm->gvalue rettype retval)))))
      (slot-set! closure 'closure (gclosure-primitive-new newfunc)))
    (slot-set! closure 'return-type (if (unspecified? rettype) #f rettype))
    (if (unspecified? paramtypes)
      (slot-set! closure 'param-types (make-vector 0))
      (begin
	(or (list? paramtypes)
	    (gruntime-error "Wrong type argument: ~S" paramtypes))
	(for-each (lambda (x)
		    (or (is-a? x <gtype>)
			(gruntime-error "Wrong type argument: ~S" x)))
		  paramtypes)
	(slot-set! closure 'param-types (list->vector paramtypes))))))

(define-method (initialize (instance <gvalue>) initargs)
  (let* ((type (gvalue->type instance))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (eq? type fundamental)))
    (cond
     ;; Basic types - implemented as GValues.
     ((gtype-primitive-basic? type)
      (let* ((init-value (get-keyword #:value initargs *unspecified*)))
	(and (unspecified? init-value) (gruntime-error "Missing #:value argument"))
	(gvalue-primitive-set instance init-value)))

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
	(gvalue-primitive-set-enum instance (caddr enum))))

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
	(gvalue-primitive-set-flags instance flags-value)))

     (else
      (noop)))))

;;;
;;; {Methods for Writing}
;;;

;; These gtype-instance:write methods are necessary for some reason.
;; Just defining a write method specialized to <gvalue> doesn't seem to
;; work, probably because <gvalue> and <%gtype-instance> are smobs
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
     ;; Basic types
     ((gtype-primitive-basic? type)
      (display #\space file)
      (display (gvalue-primitive-get obj) file))

     ;; GEnum
     ((eq? fundamental gtype:genum)
      (let* ((enum-values (genum-primitive-get-values type))
	     (value (gvalue-primitive-get obj))
	     (value-text (enum-by-index type value)))
	(display #\space file)
	(display value-text file)))

     ;; GFlags
     ((eq? fundamental gtype:gflags)
      (let* ((flags-values (gflags-primitive-get-values type))
	     (value (gvalue-primitive-get obj))
	     (value-text '()))
	(for-each (lambda (x)
		    (let ((f (caddr x)))
		      (if (gflags-primitive-bit-set? value f)
			  (set! value-text (append! value-text (list x))))))
		  (vector->list flags-values))
	(display #\space file)
	(display value-text file)))

     ;; GBoxedScm
     ((eq? type gtype:gboxed-scm)
      (display #\space file)
      (display (gboxed-scm-primitive->scm obj) file))))

  (display #\> file))

(define-method (gtype-instance:write (class <gtype-class>) (obj <%gtype-instance>) file)
  (display "#<%gtype-instance " file)
  (display (class-name class) file)
  (display #\space file)
  (display-address obj file)
  (let* ((type (gtype-instance-primitive->type obj))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (eq? type fundamental)))
    (cond
      ;; Basic types
      ((eq? fundamental gtype:gparam)
       (display #\space file)
       (display (gparam-primitive->pspec-struct obj) file))))

  (display #\> file))

(define-method (write (param <gparam>) file)
  (let ((class (class-of param))
        (primitive (slot-ref param 'gtype-instance)))
    (if (slot-bound? class 'name)
      (begin
	(display "#<" file)
	(display (class-name class) file)
	(display #\space file)
	(display-address param file)
	(display #\space file)
        (if primitive
            (write (gparam->pspec-struct param) file)
            (display "(uninitialized)" file))
	(display #\> file))
      (next-method))))

(define-method (write (closure <gclosure>) file)
  (let* ((class (class-of closure)))
    (display "#<gclosure " file)
    (display (class-name class) file)
    (display #\space file)
    (display (slot-ref closure 'return-type) file)
    (display " - " file)
    (display (slot-ref closure 'param-types) file)
    (display #\> file)))

;;;
;;; {Generic Typed Values}
;;;

(define (scm->gvalue type init-value)
  (cond
    ((or (unspecified? type) (eq? type gtype:void))
     *unspecified*)
    ((gtype-primitive-basic? type)
     (if (unspecified? init-value)
       (gruntime-error "Function returned no value, but expected ~S" type))
     (make (gtype->class type) #:value init-value))
    ((eq? (gtype->fundamental type) gtype:gobject)
     (gtype-instance-primitive->value (slot-ref init-value 'gtype-instance)))
    ((eq? (gtype->fundamental type) gtype:ginterface)
     (gtype-instance-primitive->value (slot-ref init-value 'gtype-instance)))
    ((eq? (gtype->fundamental type) gtype:genum)
     (if (is-a? init-value <gvalue>)
         init-value
         (make (gtype->class type) #:value init-value)))
    ((eq? type gtype:gvalue-array)
     (if (is-a? init-value <gvalue>)
         init-value
         (let ((gvarray (gvalue-array-primitive-new)))
           (for-each
            (lambda (v)
              (let ((gvalue
                     (gvalue-primitive-new
                      (cond
                       ((string? v) gtype:gchararray)
                       ((boolean? v) gtype:gboolean)
                       ((integer? v) gtype:glong)
                       ((real? v) gtype:gdouble)
                       ((char? v) gtype:gchar)
                       (else
                        (gruntime-error "Cannot convert ~A to a GValue" v))))))
                (gvalue-primitive-set gvalue v)
                (gvalue-array-primitive-append gvarray gvalue)))
            init-value)
           gvarray)))
    ((eq? type gtype:gboxed-scm)
     (if (is-a? init-value <gvalue>)
         init-value
         (gboxed-scm-primitive-new init-value)))
    (else
     init-value)))

(define (gvalue->scm value)
  "Converts a <gvalue> into a scheme object."
  (let* ((value-type (gvalue->type value))
	 (fundamental-value-type (gtype->fundamental value-type)))
    (cond
      ((gtype-primitive-basic? value-type)
       (gvalue-primitive-get value))

      ((eq? value-type gtype:gboxed-scm)
       (gboxed-scm-primitive->scm value))

      ((eq? fundamental-value-type gtype:gobject)
       ;; An optimization to avoid excessive allocation and reffing --
       ;; we could make with :real-instance instead.
       (%gtype-instance-value->scm value))
      
      ((eq? fundamental-value-type gtype:gparam)
       (let ((instance (gvalue-primitive-get value)))
         (make (gtype->class (gtype-from-instance instance))
           #:%real-instance instance)))
      (else
       value))))

;;;
;;; {Miscellaneous}
;;;

(define (gparam->pspec-struct param)
  (if (is-a? param <%gtype-instance>)
      (gparam-primitive->pspec-struct param)
      (gparam-primitive->pspec-struct (slot-ref param 'gtype-instance))))

(define-method (gclosure-invoke (closure <gclosure>) . args)
  (let* ((primitive-closure (slot-ref closure 'closure))
	 (return-type (slot-ref closure 'return-type))
	 (param-types (slot-ref closure 'param-types))
	 (num-params (vector-length param-types)))
    (or (eq? (length args) (vector-length param-types))
	(gruntime-error "Wrong number of arguments"))
    (let* ((params (do ((index 0 (+ index 1))
			(params (make-vector num-params #f)))
		       ((>= index num-params) params)
		     (let* ((value-type (vector-ref param-types index))
			    (init-value (list-ref args index))
			    (value (scm->gvalue value-type init-value)))
		       (vector-set! params index value))))
	   (retval (gclosure-primitive-invoke primitive-closure
                                              return-type params)))
      (if return-type
          (gvalue->scm retval)))))

;;;
;;; {GObject Properties}
;;;

(define (find-property vtable name)
  (let loop ((l (vector->list vtable)))
    (if (null? l)
      #f
      (let* ((pspec-struct (gparam->pspec-struct (car l)))
	     (pspec-name (gparam-spec:name pspec-struct)))
	(if (equal? pspec-name name)
	  (car l)
	  (loop (cdr l)))))))

(define (gobject-class-get-properties class)
  "(gobject-class-get-properties class)

Returns a vector of properties belonging to CLASS and all parent
classes."
  (or (memq <gobject> (class-precedence-list class))
      (gruntime-error "Not a subclass of <gobject>: ~S" class))
  (gtype-class-get-vector-slot class 'gobject-properties <gobject>))

(define (gobject-class-get-property-names class)
  "(gobject-class-get-property-names class)

Returns a vector of property names belonging to CLASS and all
parent classes."
  (let* ((properties (gobject-class-get-properties class)))
    (vector-map (lambda (x) (gparam-spec:name (gparam->pspec-struct x)))
		properties)))

(define (gobject-class-find-property class name)
  "(gobject-class-find-property class name)

Returns a property named NAME (a symbol), belonging to CLASS or one of
its parent classes, or #f if not found."
  (let* ((properties (gobject-class-get-properties class)))
    (find-property properties name)))

(define (gobject-get-property object name)
  (if (not (is-a? object <gobject>))
      (gruntime-error "Not a <gobject>: ~A" object))
  (if (not (symbol? name))
      (gruntime-error "Not a symbol: ~A" name))
  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (pspec-vector (gobject-class-get-properties class))
	 (pspec (or (find-property pspec-vector name)
		     (gruntime-error "No such property in class ~S: ~S" class name)))
	 (retval (gobject-primitive-get-property instance name)))
    (gvalue->scm retval)))

(define (gobject-set-property object name init-value)
  (if (not (is-a? object <gobject>))
      (gruntime-error "Not a <gobject>: ~A" object))
  (if (not (symbol? name))
      (gruntime-error "Not a symbol: ~A" name))
  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (pspec-vector (gobject-class-get-properties class))
	 (param (or (find-property pspec-vector name)
		    (gruntime-error "No such property in class ~S: ~S" class name)))
	 (pspec-struct (gparam->pspec-struct param))
	 (value-type (gparam-spec:value-type pspec-struct))
	 (value (scm->gvalue value-type init-value)))
    (gobject-primitive-set-property instance name value)))

(define-generic-with-docs gobject:set-property
  "(gobject:set-property obj name value)

Called to set a gobject property. Only properties belonging to (class-of
obj) will come through this function.

Call (next-method) in your methods to invoke the default handler, which
sets slots on the object.")

(define-method (gobject:set-property (object <gobject>) (name <symbol>) value)
  (if (class-slot-definition (class-of object) name)
      (slot-set! object name value)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

(define-generic-with-docs gobject:get-property
  "(gobject:get-property obj name)

Called to get a gobject property. Only properties belonging to (class-of
obj) will come through this function.

Call (next-method) in your methods to invoke the default handler, which
calls (slot-ref obj name).
")

(define-method (gobject:get-property (object <gobject>) (name <symbol>))
  (if (class-slot-definition (class-of object) name)
      (slot-ref object name)
      (gruntime-error "Properties added after object definition must be accessed via custom property methods: ~A" name)))

;;;
;;; {Signals}
;;;

(define (gtype-class-get-signals class)
  "(gtype-class-get-signals class)

Returns a vector of signals belonging to CLASS and all parent classes."
  (if (not (is-a? class <gtype-class>))
      (gruntime-error "Not a <gtype-class>: ~S" class))
  (gtype-class-get-vector-slot class 'gsignals <gtype-instance>))

(define (gtype-class-get-signal-names class)
  "(gtype-class-get-signal-names class)

Returns a vector of signal names belonging to CLASS and all parent
classes."
  (vector-map gsignal:name (gtype-class-get-signals class)))

(define (signal-by-name signals symbol)
  (let loop ((l (vector->list signals)))
    (if (null? l)
	#f
	(let* ((this (car l)) (name (gsignal:name this)))
	  (if (eq? symbol (string->symbol name))
              this
              (loop (cdr l)))))))

(define (gtype-instance-signal-emit object name . args)
  "(gtype-instance-signal-emit object name . args)

Emits signal `name' with arguments `args' on the GObject instance `object':

   object            - instance of <gobject> or a subclass of it.

   name              - symbol identifying the signal

"
  (or (is-a? object <gobject>)
      (gruntime-error "Not a <gobject> instance: ~S" object))
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

(define (gtype-instance-signal-connect-data object name func after)
  "(gtype-instance-signal-connect-data object name func after)

Connects `func' as handler for the GObject instance `object's signal `name':

   object            - instance of <gobject> or a subclass of it.

   name              - symbol identifying the signal

   func              - procedure which is installed as signal handler.

   after             - boolean specifying whether the handler is run before (#f)
                       or after (#t) the signal's default handler.

Returns an integer number which can be used as arugment of gsignal-handler-block,
gsignal-handler-unblock, gsignal-handler-disconnect and gsignal-handler-connected?.

"
  (or (is-a? object <gobject>)
      (gruntime-error "Not a <gobject> instance: ~S" object))
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
  "Convenience function for `(gtype-instance-signal-connect-data object name func #f)'."
  (gtype-instance-signal-connect-data object name func #f))

(define (gtype-instance-signal-connect-after object name func)
  "Convenience function for `(gtype-instance-signal-connect-data object name func #t)'."
  (gtype-instance-signal-connect-data object name func #t))

(define (gsignal-handler-block obj id)
  (gsignal-primitive-handler-block (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-unblock obj id)
  (gsignal-primitive-handler-unblock (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-disconnect obj id)
  (gsignal-primitive-handler-disconnect (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-connected? obj id)
  (gsignal-primitive-handler-connected? (gtype-instance-primitive obj) id))

(define (gtype-class-create-signal class name return-type param-types)
  (let* ((type (gtype-class->type class))
	 (signal-vector (gtype-class-get-signals class))
	 (signal (make-struct gsignal-struct-vtable 0 #f #f
			      0 name type return-type #f (list->vector param-types)))
	 (method-name (gtype->method-name type name))
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
    (class-slot-set! class 'gsignals (gtype-primitive-get-signals type))
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
  "(gtype-class-define-signal class name return-type . param-types)

Creates and adds a new signal `name' to the GTypeClass `class':

  class         - this must be a <gtype-class>.

  name          - this is a symbol which identifies the signal. There must be
                  no signal with this name in the `class'es class ancestry.

  return-type   - is either a <gtype> specifying the signal's return type or #f
                  if the return type is void (#f is the same than gtype:void).

  param-types   - a list of <gtype>s specifying the signal's arguments.

This is implemented as a macro which must be called at the top-level.
If it does not already exist, it'll define a new generic function for the signal.

The name of this GF is the concatenation of the type name, a colon and the signal
name - it's calculated by `(gtype->method-name (gtype-class->type class) name)'.

NOTE: Even if this is not strictly a bug, it is highly recommended not to add any
      signals to existing classes which you did not create.

      Create a subclass using gtype-register-object-static and then add your signals
      to this subclass.

Examples:

  (gtype-class-define-signal <foo> 'roswell #f)
  (define-method (foo:roswell (obj <foo>))
     *unspecified*)

  (gtype-class-define-signal <foo> 'berlin  gtype:glong gtype:int)
  (define-method (foo:berlin (obj <foo>) (x (<number>)))
     85)

"
  (procedure->macro
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

		  (method-name `(gtype->method-name
				 (gtype-class->type ,(cadr exp))
				 ,(caddr exp)))
		  )

	      `(cond
		 ((not (is-a? ,class <gtype-class>))
		  (gruntime-error "Bad object class: ~S" ,class))
		 
		 ((not (gtype-is-classed? (gtype-class->type ,class)))
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

(%post-init-gnome-gobject)

(let* ((doc-dir (gobject-scheme-dir))
       (doc-file (in-vicinity doc-dir "guile-gnome-gobject-procedures.txt")))
  (set! documentation-files (append! documentation-files (list doc-file))))
