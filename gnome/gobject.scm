;; Commentary:
;;
;; This is the GObject wrapper for Guile.
;;
;; Exports the following classes:
;;
;;  <gtype> - This corresponds to the C type `GType', it's an abstract
;;            type ID (a number) to uniquely identify a type in the
;;            gruntime system.
;;
;;  Pre-defined types are
;;
;;    gtype:gchar gtype:guchar gtype:gboolean gtype:gint gtype:guint
;;    gtype:glong gtype:gulong gtype:gfloat gtype:gdouble gtype:gchararray
;;    gtype:genum gtype:gflags gtype:gboxed gtype:gclosure gtype:gparam
;;    gtype:gobject
;;
;;  For each of these types, there's a corresponding <gtype> GOOPS class:
;;
;;    <gchar> <guchar> <gboolean> <gint> <guint> <glong> <gulong>
;;    <gfloat> <gdouble> <gchararray> <gclosure> <genum> <gflags>
;;    <gboolean> <gobject> <gboxed> <gparam>
;;
;;  <gtype-class> - This is a GOOPS class corresponding to the GTypeClass
;;                  in C, it is used as parent class for all the classes
;;                  listed above and for all user-defined types.
;;
;;    It has the following slots:
;;
;;       gtype          - an `#:allocation #:each-subclass' class slot
;;                        containing the corresponding <gtype>.
;;
;;                        This is somewhat private, please use the public
;;                        `gtype-class->type' accessor function.
;;
;;       gtype-instance - this is a private instance slot which holds the
;;                        corresponding C instance GTypeInstance as a
;;                        <gtype-instance> smob.
;;
;;   For each GType there is a corresponding <gtype-class> subclass which
;;   can be created using the `gtype->class' function. Please do not create
;;   <gtype-class> subclasses manually, this won't work as expected.
;;
;;   Note that a <gtype-class> is a persistent, immortal object - it can
;;   never be freed by GC. When you call `gtype->class' on a GType the first
;;   time, this creates a new GOOPS class and "binds" it to that type - so
;;   if you call `gtype->class' a second time on a type, you'll get the same
;;   class back. The same applies for <gtype>s.
;;
;;   If you create any instances of this or any of its child classes, it'll
;;   also create the corresponding C instance.
;;
;;   So, to summarize, a <gobject> is a GObjectClass in C and an instance
;;   of a <gobject> is a GObject in C.
;;
;;; Code:

(define-module (gnome gobject)
  :use-module (oop goops)
  :use-module (ice-9 documentation)
  :use-module (gnome gobject primitives))

(define-generic-with-docs gtype-instance:class-init
  "")
(define-generic-with-docs gtype-instance:instance-init
  "")
(define-generic-with-docs gobject:class-init
  "")
(define-generic-with-docs gobject:instance-init
  "")
(define-generic-with-docs gobject-class:install-property
  "")
(define-generic-with-docs gobject:set-property
  "")
(define-generic-with-docs gobject:get-property
  "")

(%init-gnome-gobject)

(define (vector-map proc vector)
  (let* ((length (vector-length vector))
	 (result-vector (make-vector length)))
    (do ((index 0 (+ index 1)))
	((>= index length) result-vector)
      (vector-set! result-vector index (proc (vector-ref vector index))))))

(define (get-parent-types type)
  (let ((parent-types '()))
    (do ((t (gtype-parent type) (gtype-parent t)))
	((and (boolean? t) (not t)) parent-types)
      (let* ((class-name (gtype->class-name t))
	     (class (gtype->class t)))
	(set! parent-types (append! parent-types (list class)))))
    (set! parent-types (append! parent-types (list <gtype-class>)))
    parent-types))

(define (get-metaclass type)
  (if (not (gtype-eq? type (gtype->fundamental type)))
    (gtype->class (gtype-parent type))
    <gtype-class>))

(define (make-class-slots type)
  (let* ((fundamental (gtype->fundamental type))
	 (is-fundamental (gtype-eq? type fundamental)))
    (cond
     ((gtype-eq? type gtype:genum)
      (list '(genum-values #:allocation #:each-subclass)))

     ((gtype-eq? type gtype:gflags)
      (list '(genum-values #:allocation #:each-subclass)))

     ((gtype-eq? fundamental gtype:gobject)
      (list '(gobject-signals #:allocation #:each-subclass)
            '(gobject-properties #:allocation #:each-subclass)
            '(gobject-servant-properties)))

     ((gtype-eq? type gtype:gclosure)
      (list '(closure) '(return-type) '(param-types)))

     (else '()))))

(define (make-value-from-scm type init-value)
  (cond
    ((or (unspecified? type) (gtype-eq? type gtype:void))
     *unspecified*)
    ((gtype-primitive-basic? type)
     (if (unspecified? init-value)
       (gruntime-error "Function returned no value, but expected ~S" type))
     (make (gtype->class type) #:value init-value))
    ((gtype-eq? (gtype->fundamental type) gtype:gobject)
     (gtype-instance-primitive->value (slot-ref init-value 'gtype-instance)))
    (else
     init-value)))

(define-public-with-docs (gtype-ancestry type)
  "Returns the \"type ancestry\" of the GType `type':

The \"type ancestry\" is a list consisting of the type itself and
its parent type's ancestry.

Example:

    (gtype-ancestry gtype:gobject)  => (list gtype:gobject)

    (gtype-ancestry gtype:gclosure) => (list gtype:gclosure gtype:gboxed)
"
  (cond
    ((gtype-fundamental? type) (list type))
    (else
     (append (list type) (gtype-ancestry (gtype-parent type))))))

(define-public-with-docs (gtype-class-ancestry class)
  "Returns the \"class ancestry\" of the GTypeClass `class'.
This is the same than gtype-ancestry, but operates on GOOPS classes
and not GTypes.

This is just a convenience function, all it does it
(map (lambda (x) (gtype->class x)) (gtype-ancestry (gtype-class->type class)))).

Example:

    (gtype-class-ancestry <gobject>)  => (list <gobject>)

    (gtype-class-ancestry <gclosure>) => (list <gclosure> <gboxed>)
"
  (map (lambda (x) (gtype->class x)) (gtype-ancestry (gtype-class->type class))))

(define (gtype-class-get-vector-slot class name)
  (let* ((class-ancestry (gtype-class-ancestry class)) (slot-list '()))
    (for-each (lambda (x)
		(and (slot-bound-using-class? x x name)
		     (set! slot-list (append slot-list (vector->list (class-slot-ref x name))))))
	      class-ancestry)
    (list->vector slot-list)))

(define (build-signals-vector type)
  ;; we know type has gtype:gobject as a parent somewhere up the line
  (let loop ((type type) (ret '()))
    (let ((newret (append ret (vector->list (gobject-primitive-get-signals type)))))
      (if (gtype-eq? type gtype:gobject)
          (list->vector newret)
          (loop (gtype-parent type) newret)))))

(define (init-gobject-class type class)
  (let* ((signals (build-signals-vector type))
 	 (properties-vector (gobject-primitive-get-properties type))
 	 (properties (vector-map (lambda (x)
 				   (let* ((pspec-struct (gparam-primitive->pspec-struct x))
 					  (param-type (gparam-spec:param-type pspec-struct))
 					  (param-class (gtype->class param-type)))
 				     (make param-class #:pspec-struct pspec-struct)))
 				 properties-vector)))
     
     (class-slot-set! class 'gobject-signals signals)
     (class-slot-set! class 'gobject-properties properties)))

(define-public-with-docs (gobject-class-get-signals class)
  ""
  (or (or (eq? class <gobject>) (is-a? class <gobject>))
      (gruntime-error "Not a subclass of <gobject>: ~S" class))
  (gtype-class-get-vector-slot class 'gobject-signals))

(define-public-with-docs (gobject-class-get-properties class)
  ""
  (or (or (eq? class <gobject>) (is-a? class <gobject>))
      (gruntime-error "Not a subclass of <gobject>: ~S" class))
  (gtype-class-get-vector-slot class 'gobject-properties))

(define-public-with-docs (gobject-class-get-property-names class)
  ""
  (let* ((properties (gobject-class-get-properties class)))
    (vector-map (lambda (x) (gparam-spec:name (gparam->pspec-struct x)))
		properties)))

(define-public-with-docs (gobject-class-find-property class name)
  ""
  (let* ((properties (gobject-class-get-properties class)))
    (find-property properties name)))

(define-public-with-docs (gtype-class->type class)
  "Returns the GType (a <gtype>) which is associated with a GTypeClass `class',
which needs to be a GOOPS subclass of <gtype-class>."
   (cond
    ((slot-bound-using-class? class class 'gtype)
     (class-slot-ref class 'gtype))
    
;    ((is-a? class <gtype-class>)
;     (gtype-class->type (class-of class)))
    
    (else
     (gruntime-error "Can't get type of unknown class: ~S" class))))

(define-public-with-docs (gtype->class type)
  "If there is already a GOOPS class associated with the GType `type', return this class.

Otherwise, create a new GOOPS class and bind it to this type. The created class is an immortal,
persistent object which is bound in some magic way to its GType.

In the gruntime type system, each GType has a parent type (which can be identical to the type
itself, in which case the type is called a fundamental type). When a GOOPS class for a non-
fundamental type is created, it's put into the parent type's metaclass.
"
  (or (%gtype-lookup-class type)
      (let* ((class-name (gtype->class-name type))
	     (parent-types (get-parent-types type))
	     (slots (make-class-slots type))
	     (class (make-class parent-types slots
				#:metaclass (get-metaclass type)
				#:%real-instance *unspecified*
				#:name class-name))
	     (fundamental (gtype->fundamental type))
	     (is-fundamental (gtype-eq? type fundamental)))
	
        (%gtype-bind-to-class class type)

        ;; do some initialization if it's an enum, flags, or object.
	(cond
	  ((gtype-eq? fundamental gtype:genum) 
	   (if (not is-fundamental)
	     (class-slot-set! class 'genum-values
			      (genum-primitive-get-values type))))

	  ((gtype-eq? fundamental gtype:gflags) 
	   (if (not is-fundamental)
	     (class-slot-set! class 'genum-values
			      (gflags-primitive-get-values type))))

	  ((and (gtype-eq? fundamental gtype:gobject) (not is-fundamental))
	   (init-gobject-class type class)))

	class)))

(define <gchar>      (gtype->class gtype:gchar))
(define <guchar>     (gtype->class gtype:guchar))
(define <gboolean>   (gtype->class gtype:gboolean))
(define <gint>       (gtype->class gtype:gint))
(define <guint>      (gtype->class gtype:guint))
(define <glong>      (gtype->class gtype:glong))
(define <gulong>     (gtype->class gtype:gulong))
(define <gfloat>     (gtype->class gtype:gfloat))
(define <gdouble>    (gtype->class gtype:gdouble))
(define <gchararray> (gtype->class gtype:gchararray))

(define <genum>      (gtype->class gtype:genum))
(define <gflags>     (gtype->class gtype:gflags))
(define <gboxed>     (gtype->class gtype:gboxed))
(define <gclosure>   (gtype->class gtype:gclosure))
(define <gparam>     (gtype->class gtype:gparam))
;; <gobject> defined below

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

(define-public-with-docs <gobject>
  "This GOOPS class corresponds to the GObjectClass in the gruntime type system.
It acts as base class for all types which are derived from gtype:gobject.

When you create instances of this class, the corresponding gruntime object instances
are created.

The following class slots are public:

    gobject-signals		- a vector describing all direct signals of this
                                  GObjectClass

    gobject-properties          - a vector describing all direct properties of this
                                  GObjectClass

Instead of accessing these slots directly, you should ask the accessor functions
`gobject-class-get-signals' and `gobject-class-get-properties' - they'll return the
signals/properties of the class and all its parent classes.

"
  (gtype->class gtype:gobject))

(define <gboxed-scm> (gtype->class gtype:gboxed-scm))

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (gtype-instance:write (class <gtype-class>) (obj <gvalue>) file)
  (display "#<gvalue " file)
  (display (class-name class) file)
  (display #\space file)
  (display-address obj file)
  (let* ((type (gtype-class->type class))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (gtype-eq? type fundamental)))
    (cond
     ;; Basic types
     ((gtype-primitive-basic? type)
      (display #\space file)
      (display (gvalue-primitive-get obj) file))

     ;; GEnum
     ((gtype-eq? fundamental gtype:genum)
      (let* ((enum-values (genum-primitive-get-values type))
	     (value (gvalue-primitive-get obj))
	     (value-text (enum-by-index type value)))
	(display #\space file)
	(display value-text file)))

     ;; GFlags
     ((gtype-eq? fundamental gtype:gflags)
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
     ((gtype-eq? type gtype:gboxed-scm)
      (display #\space file)
      (display (gboxed-scm-primitive->scm obj) file))))

  (display #\> file))

(define-method (gtype-instance:write (class <gtype-class>) (obj <gtype-instance>) file)
  (display "#<gtype-instance " file)
  (display (class-name class) file)
  (display #\space file)
  (display-address obj file)
  (let* ((type (gtype-instance-primitive->type obj))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (gtype-eq? type fundamental)))
    (cond
      ;; Basic types
      ((gtype-eq? fundamental gtype:gparam)
       (display #\space file)
       (display (gparam-primitive->pspec-struct obj) file))))

  (display #\> file))


(define-method (gtype-instance:write (type <gtype>) (obj <gvalue>) file)
  (let* ((class (gtype->class type)))
    (gtype-instance:write class obj file)))

(define-method (gtype-instance:write (type <gtype>) (obj <gtype-instance>) file)
  (let* ((class (gtype->class type)))
    (gtype-instance:write class obj file)))

(define-method (write (param <gparam>) file)
  (let ((class (class-of param)))
    (if (slot-bound? class 'name)
      (begin
	(display "#<" file)
	(display (class-name class) file)
	(display #\space file)
	(display-address param file)
	(display #\space file)
	(write (gparam->pspec-struct param) file)
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

(define-method (initialize (instance <gvalue>) initargs)
  (let* ((type (gvalue->type instance))
	 (fundamental (gtype->fundamental type))
	 (is-fundamental (gtype-eq? type fundamental)))
    (cond
     ;; Basic types - implemented as GValues.
     ((gtype-primitive-basic? type)
      (let* ((init-value (get-keyword #:value initargs *unspecified*)))
	(and (unspecified? init-value) (gruntime-error "Missing #:value argument"))
	(gvalue-primitive-set instance init-value)))

     ;; Fundamental type - but not a basic one.
     ((gtype-eq? type fundamental)
      (gruntime-error "Can't make instances of fundamental type: ~S" type))

     ;; GEnum
     ((gtype-eq? fundamental gtype:genum)
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
     ((gtype-eq? fundamental gtype:gflags)
      (let* ((init-values (get-keyword #:value initargs *unspecified*))
	     (real-init-values init-values)
	     (flags-value 0))
	(if (unspecified? init-values)
	    (gruntime-error "Missing #:value argument"))
	(cond
	 ((or (list? init-values) (vector? init-values))
	  (if (vector? init-values)
	      (set! init-values (vector->list init-values)))
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
	  (if (memq #f init-values)
	      (gruntime-error "Wrong type argument: ~S" real-init-values))
	  (set! init-values (map (lambda (x) (caddr x)) init-values))
	  (for-each (lambda (x)
		      (if (memq (caddr x) init-values)
			  (set! flags-value (+ flags-value (caddr x)))))
		    (vector->list (gflags-primitive-get-values type))))
         ((or (integer? init-values) (symbol? init-values) (string? init-values))
	  (set! flags-value (caddr ((lambda (x)
                                      (cond
                                       ((integer? x)
                                        (flags-by-index type x))
                                       ((symbol? x)
                                        (flags-by-symbol type x))
                                       ((string? x)
                                        (flags-by-name type x))))
                                    init-values)))
          (if (not flags-value) 
              (gruntime-error "Bad value for ~A: ~S" type real-init-values)))
	 (else
	  (gruntime-error "Wrong type argument: ~S" real-init-values)))
	(gvalue-primitive-set-flags instance flags-value)))

     ;; GClosure
     ((gtype-eq? type gtype:gclosure)
      (let* ((func (get-keyword #:func initargs *unspecified*))
	     (rettype (get-keyword #:return-type initargs *unspecified*))
	     (paramtypes (get-keyword #:param-types initargs *unspecified*)))
	(if (unspecified? func)
	  (gruntime-error "Missing #:func argument"))
	(or (procedure? func)
	    (gruntime-error "Wrong type argument: ~S" func))
	(let* ((return-type (if (unspecified? rettype) #f rettype))
	       (closure (gclosure-primitive-new func return-type)))
	  (gvalue-primitive-set-closure instance closure))))

     (else
      (noop)))))

(define (gvalue->scm value)
  "Converts a <gvalue> into a scheme object."
  (let* ((value-type (gvalue->type value))
	 (fundamental-value-type (gtype->fundamental value-type)))
    (cond
      ((gtype-primitive-basic? value-type)
       (gvalue-primitive-get value))

      ((gtype-eq? fundamental-value-type gtype:gobject)
       (make (gtype->class value-type)
	 #:%real-instance (gvalue-primitive-get value)))

      ((gtype-eq? value-type gtype:gboxed-scm)
       (gboxed-scm-primitive->scm value))

      (else
       value))))

(define (gparam->pspec-struct param)
  (if (is-a? param <gtype-instance>)
      (gparam-primitive->pspec-struct param)
      (gparam-primitive->pspec-struct (slot-ref param 'gtype-instance))))

(define pspec-args '(("GParamChar"    . (gtype:gchar
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

(define (make-pspec-args type initargs)
  (let* ((args (or (assoc-ref pspec-args (gtype-name type))
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

(define (make-gparam-instance class type object initargs)
  (let* ((type (gtype-class->type class))
	 (pspec (or (get-keyword #:pspec-struct initargs #f)
		    (let* ((args (make-pspec-args type initargs))
			   (name (or (get-keyword #:name initargs #f)
				     (gruntime-error "Missing #:name keyword")))
			   (nick (get-keyword #:nick initargs #f))
			   (blurb (get-keyword #:blurb initargs #f))
			   (pspec-descr (or (assoc-ref pspec-args (gtype-name type))
					    (gruntime-error "Unknown type: ~A" type)))
			   (value-type (eval (car pspec-descr) (current-module)))
			   (flags #f)
			   (owner-type type))
		      (or (symbol? name)
			  (gruntime-error "Wrong #:name keyword"))
		      (or (or (eq? nick #f) (string? name))
			  (gruntime-error "Wrong #:nick keyword"))
		      (or (or (eq? blurb #f) (string? blurb))
			  (gruntime-error "Wrong #:blurb keyword"))
		      (apply make-struct gparam-spec-struct-vtable (length args) #f #f
			     name nick blurb flags type value-type owner-type args)))))
    (gparam-primitive-create class type object pspec)))

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
			(make-value-from-scm rettype retval)))))
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

(define-method (allocate-instance (class <gboxed-scm>) initargs)
  (let* ((init-value (get-keyword #:value initargs *unspecified*)))
    (if (unspecified? init-value)
        (gruntime-error "Missing #:value argument"))
    (gboxed-scm-primitive-new init-value)))

(define (find-property vtable name)
  (let loop ((l (vector->list vtable)))
    (if (null? l)
      #f
      (let* ((pspec-struct (gparam->pspec-struct (car l)))
	     (pspec-name (gparam-spec:name pspec-struct)))
	(if (equal? pspec-name name)
	  (car l)
	  (loop (cdr l)))))))

(define (make-gobject-instance class type object options)
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
		    (pspec-value-class (gtype->class pspec-value-type))
		    (pspec-value (make pspec-value-class #:value option-value)))
	       (set! init-properties
		     (append
		      init-properties (list (cons param-name pspec-value))))
	       (loop (cddr options)
		     (cons (cadr options)
			   (cons (car options)
				 res)))))))
    (gobject-primitive-create-instance class type object
				       (list->vector init-properties))))

(define-method (initialize (object <gtype-class>)  initargs)
  (next-method)
  (if (get-keyword #:%real-instance initargs #f)
      (slot-set! object 'gtype-instance (get-keyword #:%real-instance initargs #f))
      (let* ((class (class-of object))
	     (type (gtype-class->type class))
	     (fundamental (gtype->fundamental type))
	     (is-fundamental (gtype-eq? type fundamental)))
	(cond
	  ;; GObject
	  ((gtype-eq? fundamental gtype:gobject)
	   (make-gobject-instance class type object initargs))

	  ;; GParam
	  ((gtype-eq? fundamental gtype:gparam)
	   (make-gparam-instance class type object initargs))

	  (else
	   (noop))))))

(define-method (allocate-instance (class <gtype-class>) initargs)
  (if (or (get-keyword #:%real-instance initargs #f))
    (next-method)
    (let* ((type (gtype-class->type class))
	   (fundamental (gtype->fundamental type))
	   (is-fundamental (gtype-eq? type fundamental)))
      (cond
	;; Basic types - implemented as GValues.
	((gtype-primitive-basic? type)
	 (gtype-primitive-create-basic-instance type))

	;; Fundamental type - but not a basic one.
	((gtype-eq? type fundamental)
	 (gruntime-error "Can't make instances of fundamental type: ~S" type))

	;; GEnum
	((gtype-eq? fundamental gtype:genum)
	 (gtype-primitive-create-basic-instance type))

	;; GFlags
	((gtype-eq? fundamental gtype:gflags)
	 (gtype-primitive-create-basic-instance type))

	;; GObject
	((gtype-eq? fundamental gtype:gobject)
	 (next-method))

	;; GParam
	((gtype-eq? fundamental gtype:gparam)
	 (next-method))

	;; GClosure
	((gtype-eq? type gtype:gclosure)
	 (next-method))

	;; Oooops. Unknown or non-instantiable type.
	(else
	 (gruntime-error "Don't know how to make instances of this type: ~S" type))))))

(define (signal-by-name signal symbol)
  (let loop ((l (vector->list signal)))
    (if (null? l)
	#f
	(let* ((this (car l)) (name (gsignal:name this)))
	  (if (equal? symbol (string->symbol name))
              this
              (loop (cdr l)))))))

(define-public-with-docs (gobject-signal-emit object name . args)
  "(gobject-signal-emit object name . args)

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
	 (signal-vector (gobject-class-get-signals class))
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
			    (value (make-value-from-scm value-type init-value)))
		       
		       (vector-set! values index value))))
	   (retval (gobject-primitive-signal-emit instance id values)))
 
      (if (unspecified? retval) retval (gvalue->scm retval)))))

(define-public-with-docs (gobject-signal-connect-data object name func after)
  "(gobject-signal-connect-data object name func after)

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
	 (signal-vector (gobject-class-get-signals class))
	 (signal (or (signal-by-name signal-vector name)
		     (gruntime-error "No such signal in class ~S: ~S" class name)))
	 (id (gsignal:id signal))
	 (rtype (gsignal:return-type signal))
	 (params (vector->list (gsignal:param-types signal)))
	 (closure (make <gclosure> #:func func #:return-type rtype #:param-types params))
	 (pclosure (slot-ref closure 'closure)))
    (gobject-primitive-signal-connect instance id pclosure after)))

(define-public-with-docs (gobject-signal-connect object name func)
  "Convenience function for `(gobject-signal-connect-data object name func #f)'."
  (gobject-signal-connect-data object name func #f))

(define-public-with-docs (gobject-signal-connect-after object name func)
  "Convenience function for `(gobject-signal-connect-data object name func #t)'."
  (gobject-signal-connect-data object name func #t))

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
			    (value (make-value-from-scm value-type init-value)))
		       (vector-set! params index value))))
	   (retval (gclosure-primitive-invoke primitive-closure return-type params)))
      (gvalue->scm retval))))

(define (gsignal-handler-block obj id)
  (gsignal-primitive-handler-block (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-unblock obj id)
  (gsignal-primitive-handler-unblock (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-disconnect obj id)
  (gsignal-primitive-handler-disconnect (gtype-instance-primitive obj) id))
	
(define (gsignal-handler-connected? obj id)
  (gsignal-primitive-handler-connected? (gtype-instance-primitive obj) id))

(define-method (gobject-get-property (object <gobject>) (name <symbol>))
  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (pspec-vector (gobject-class-get-properties class))
	 (pspec (or (find-property pspec-vector name)
		     (gruntime-error "No such property in class ~S: ~S" class name)))
	 (retval (gobject-primitive-get-property instance name)))
    (if (gtype-primitive-basic? (gvalue->type retval))
      (gvalue-primitive-get retval)
      retval)))

(define-method (gobject-set-property (object <gobject>) (name <symbol>) init-value)
  (let* ((class (class-of object))
         (instance (slot-ref object 'gtype-instance))
	 (pspec-vector (gobject-class-get-properties class))
	 (param (or (find-property pspec-vector name)
		    (gruntime-error "No such property in class ~S: ~S" class name)))
	 (pspec-struct (gparam->pspec-struct param))
	 (value-type (gparam-spec:value-type pspec-struct))
	 (value (make-value-from-scm value-type init-value)))
    (gobject-primitive-set-property instance name value)))

(define-method (gobject:class-init (class <gtype-class>))
  *unspecified*)

(define-method (gobject:instance-init (class <gobject>) (object <gobject>))
  (let* ((properties (class-slot-ref class 'gobject-properties))
	 (property-hash (make-hash-table (vector-length properties))))
    (for-each (lambda (param)
		(let* ((pspec (gparam->pspec-struct param))
		       (pspec-name (gparam-spec:name pspec))
		       (pspec-value-type (gparam-spec:value-type pspec))
		       (value (gvalue-primitive-new pspec-value-type)))
		  (hashq-set! property-hash pspec-name value)))
	      (vector->list properties))
    (slot-set! object 'gobject-servant-properties property-hash)
    *unspecified*))

(define-method (gobject:get-property (object <gobject>) (param <gparam>))
  (let* ((property-hash (slot-ref object 'gobject-servant-properties))
	 (pspec-struct (gparam->pspec-struct param))
	 (pspec-name (gparam-spec:name pspec-struct)))
    (hashq-ref property-hash pspec-name)))

(define-method (gobject:set-property (object <gobject>) (value <gvalue>) (param <gparam>))
  (let* ((property-hash (slot-ref object 'gobject-servant-properties))
	 (pspec-struct (gparam->pspec-struct param))
	 (pspec-name (gparam-spec:name pspec-struct)))
    (hashq-set! property-hash pspec-name value)))

(define-method (gobject-class:install-property (class <gobject>) (param <gparam>))
  (init-gobject-class (gtype-class->type class) class))

(define-method (gtype-instance:class-init (class <gtype-class>))
  *unspecified*)

(define-method (gtype-instance:instance-init (class <gtype-class>) (instance <gtype-instance>))
  *unspecified*)

(define (gobject-class-create-signal class name return-type param-types)
  (let* ((type (gtype-class->type class))
	 (signal-vector (gobject-class-get-signals class))
	 (signal (make-struct gsignal-struct-vtable 0 #f #f
			      0 name type return-type #f param-types))
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
    (class-slot-set! class 'gobject-signals (gobject-primitive-get-signals type))
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

(define-public-with-docs gobject-class-define-signal
  "(gobject-class-define-signal class name return-type . param-types)

Creates and adds a new signal `name' to the GObjectClass `class':

  class         - this must be a GOOPS subclass of <gobject>.

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
      signals to existing GObjectClasses which you did not create.

      Create a subclass using gtype-register-object-static and then add your signals
      to this subclass.

Examples:

  (gobject-class-define-signal <foo> 'roswell #f)
  (define-method (foo:roswell (obj <foo>))
     *unspecified*)

  (gobject-class-define-signal <foo> 'berlin  gtype:glong gtype:int)
  (define-method (foo:berlin (obj <foo>) (x (<number>)))
     85)

"
  (procedure->macro
   (lambda (exp env)
     (cond ((not (top-level-env? env))
	    (gruntime-error "gobject-class-define-signal: Only allowed at top level"))
	   ((not (and (list? exp) (>= (length exp) 3)))
	    (gruntime-error "missing or extra expression"))

	   (else
	    (let ((class (cadr exp)) (name (caddr exp))
		  (return-type (or (cadddr exp) gtype:void))
		  (param-types `(list->vector
				 (map (lambda (x)
					(or (is-a? x <gtype>)
					    (gruntime-error "Wrong parameter type: ~S" x))
					x) (list ,@(cddddr exp)))))

		  (method-name `(gtype->method-name
				 (gtype-class->type ,(cadr exp))
				 ,(caddr exp)))
		  )

	      `(cond
		 ((not (is-a? ,class <gobject>))
		  (gruntime-error "Bad object class: ~S" ,class))
		 
		 ((not (symbol? ,name))
		  (gruntime-error "Bad signal name: ~S" ,name))

		 ((not (is-a? ,return-type <gtype>))
		  (gruntime-error "Bad return type: ~S" ,return-type))

		 ((,signal-by-name (gobject-class-get-signals ,class)
				   ,name)
		  (gruntime-error
		   "Class ~S already has a signal with this name: ~S"
		   ,class ,name))

		 (else
		  ,(if (not (defined? (local-eval method-name env) env))
		     `(define ,(local-eval method-name env)
			(gobject-class-create-signal ,class ,name
						     ,return-type
						     ,param-types)))))))))))

(define-public-with-docs (gobject-type-register-static parent-type name)
  "Registers a new GObject type with parent `parent-type'.

This function is used to derive existing GObject classes:

   (define gtype:roswell (gobject-type-register-static gtype:gobject \"Roswell\"))
   (define <roswell> (gtype->class gtype:roswell))

"
  (or (gtype-is-a? parent-type gtype:gobject)
      (gruntime-error "Parent type is not a GObject: ~S" parent-type))
  (or (string? name)
      (gruntime-error "Type name is not a string: ~S" name))
  (gtype-register-static name parent-type))

(%post-init-gnome-gobject)

(let* ((doc-dir (gobject-scheme-dir))
       (doc-file (in-vicinity doc-dir "guile-gnome-gobject-procedures.txt")))
  (set! documentation-files (append! documentation-files (list doc-file))))

;; from (gnome gobject primitives)
(re-export <gtype> <gtype-class> <gtype-class-meta> gruntime-error
	   gtype-name gtype-from-name gtype-parent gtype-is-a?
           g-source-set-closure
	   gtype-instance:write
	   gvalue? gvalue->type genum->value-table genum->symbol genum->name
	   gflags->symbol-list gflags->name-list gflags->value-list
	   gsignal:id gsignal:name gsignal:interface-type gsignal:return-type
	   gsignal:param-types gparam-spec:name gparam-spec:nick gparam-spec:blurb
	   gparam-spec:flags gparam-spec:param-type gparam-spec:value-type
	   gparam-spec:owner-type gparam-spec:args)

(export <gchar> <guchar> <gboolean> <gint> <guint> <glong> <gulong>
	<gfloat> <gdouble> <gchararray> <gclosure> <genum> <gflags>
	<gboolean> <gobject> <gboxed> <gparam> <gboxed-scm> <gsignal>
	<gparam-char> <gparam-uchar> <gparam-boolean> <gparam-int>
	<gparam-uint> <gparam-long> <gparam-ulong> <gparam-float>
	<gparam-double>  <gparam-pointer> <gparam-string> <gparam-object>
	<gparam-boxed> <gparam-enum> <gparam-flags>
	gtype-instance:class-init gtype-instance:instance-init
	gobject:class-init gobject:instance-init gobject:get-property gobject:set-property
	gobject-class:install-property gtype->class gtype-class->type gvalue->scm
	gobject-signal-emit gobject-signal-connect gobject-signal-connect-after
	gsignal-handler-block gsignal-handler-unblock gsignal-handler-disconnect
	gsignal-handler-connected? gobject-get-property gobject-set-property
	gclosure-invoke gobject-class-create-signal gobject-class-define-signal
	gparam->pspec-struct)
