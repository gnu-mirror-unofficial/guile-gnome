(define-module (gnome corba primitives)
  :use-module (gnome corba gw-corba)
  :use-module (gnome corba types)
  :use-module (gnome gobject)
  :use-module (oop goops))

(define-class <PortableServer-ServantBase> (<class>)
  (%orbit-iinterface #:allocation #:each-subclass)
  (servant))

(define-class <CORBA:Object> (<class>)
  (corba-typecode #:allocation #:each-subclass)
  (corba-objref))

(%init-gnome-corba-primitives)

(define-method (allocate-instance (class <PortableServer-ServantBase>) initargs)
  (corba-primitive-make-poa-instance class))

(define-method (allocate-instance (class <CORBA:Object>) initargs)
  (if (or (get-keyword #:dsupers initargs #f))
    (next-method)
    (let* ((object (next-method))
	   (servant (get-keyword #:servant initargs *unspecified*))
	   (ior (get-keyword #:ior initargs *unspecified*)))
      (gnome-corba-error "Can't make instances of this type: ~A" type))))

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (write (o <CORBA:Object>) file)
  (let ((class (class-of o)))
    (if (slot-bound? class 'name)
	(begin
	  (display "#<" file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))

(export <PortableServer-ServantBase> <CORBA:Object>)

