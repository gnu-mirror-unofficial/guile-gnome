;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
;;               2003,2004,2011 Andy Wingo <wingo at pobox dot com>

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
;; A CORBA wrapper for Guile.
;;
;; @section Opening CORBA modules
;;
;; @code{(gnome corba)} allows full integration between Scheme and
;; remote CORBA objects. However, the problem is how to get type
;; information about these remote objects -- it's not very useful to
;; have an opaque @code{<CORBA:Object>} in Scheme. It's also not very
;; useful if you can't write your own CORBA servants in Scheme.
;;
;; Basically, there are two ways to solve this problem. You can parse
;; the type's interface description language (IDL) at runtime, or you
;; can get the necessary information from some other source.
;; @code{(gnome corba)} does the latter, via so-called "imodules".
;;
;; Imodules are a feature of ORBit2, the ORB used in GNOME. ORBit2 is a
;; CORBA 2.4-compliant Object Request Broker (ORB), which is of course
;; interoperable with other ORB implementations. An imodule is a shared
;; library, installed as @code{$(libdir)/<modulename>_imodule.la}. To
;; create such a library for your own IDL, you need to run ORBit2's IDL
;; compiler, @code{orbit-idl}, with the @option{--imodule} argument. See
;; the @code{demos/corba/} directory in this distribution for an
;; example.
;;
;; As an example, the rest of this section will refer to the sample IDL
;; which can be found in @code{demos/corba/Foo.idl} in this
;; distribution.
;;
;; Once you have installed the @code{Foo} CORBA library (including its
;; imodule), you can load its type information into Scheme by calling:
;;
;; @example
;; (corba-primitive-open-module "Foo")
;; @end example
;;
;; As a side effect, this call will define all of the GOOPS classes and
;; methods associated with the @code{Foo} module.
;;
;; If there is a CORBA interface @code{Foo::Hello},
;; @code{corba-primitive-open-module} will create a GOOPS class
;; @code{<Foo:Hello>} which serves as stub class and another GOOPS class
;; @code{<POA:Foo:Hello>} which serves as skeleton class.
;;
;; All stub classes are derived from @code{<CORBA:Object>} and their
;; CORBA class hierarchy is preserved in Scheme.
;;
;; All skeleton classes are derived from
;; @code{<PortableServer-ServantBase>} and their CORBA class hierarchy
;; is preserved as well.
;;
;; @section Calling CORBA Methods
;;
;; To call a CORBA method, all you need to do is to invoke the
;; corresponding method in the stub class. Let's assume @code{hello} is
;; an instance of the @code{<Foo:Hello>} class. We may invoke the
;; @code{Foo::Hello::doHello} method directly, in a most Schemely
;; fashion:
;;
;; @example
;; (Foo:Hello:doHello hello)
;; @end example
;;
;; So to call CORBA methods, you don't even need to know that it's
;; CORBA.
;;
;; If a CORBA exception is signalled, a Scheme error will be thrown to
;; the key @code{corba-system-exception} or @code{corba-user-exception},
;; as appropriate.
;;
;; @section Implementing CORBA servants
;;
;; The interesting part is to implement CORBA servants in Scheme. Let's
;; assume you want to write a servant for the @code{Foo::Hello}
;; interface.
;;
;; The first thing you need to do is to derive its POA class
;;
;; @example
;; (define-class <hello> (<POA:Foo:Hello>))
;; @end example
;;
;; Then, you define methods:
;;
;; @example
;; (define-method (Foo:Hello:doHello (hello <hello>))
;;   (display (list "Hello World!" hello)) (newline))
;; @end example
;;
;; If you call @code{(next-method)}, the POA class' method will be run,
;; which by default will throw a @code{CORBA::NO_IMPLEMENT} system
;; exception.
;;
;; However, you can override this:
;;
;; @example
;; (define-method (Foo:Bar:Baz:haveFun (object <POA:Foo:Bar:Baz>) a b)
;;   (display (list "Default Foo:Bar:Baz:haveFun handler!" a b))
;;   (newline))
;; @end example
;;
;; If you created all the methods, you can create servants and call
;; @code{corba-servant->reference} to get a @code{CORBA::Object}
;; reference:
;;
;; @example
;; (define servant (make <hello>))
;; (define hello (corba-servant->reference servant))
;; @end example
;;
;; Now you have a CORBA Object @code{hello}, and can invoke methods on
;; it:
;;
;; @example
;; (Foo:Hello:doHello hello)
;; @end example
;;
;; Although this looks like a normal Scheme procedural application, this
;; is a "real" CORBA call: @code{hello} is a "normal" CORBA Object.
;;
;; Note of course that any CORBA Objects which you create in Guile are
;; "owned" by Guile's garbage collector, so make sure to
;; @code{CORBA_Object_duplicate()} in a C function before you store it
;; somewhere.
;;
;; @section Multiple inheritance
;;
;; Like in C, you can also create servants for CORBA interfaces which are
;; derived from other interfaces:
;;
;; @example
;; (define-class <maximum> (<hello> <POA:Foo:MaximumHello>))
;; (define-method (Foo:Hello:doHello (hello <maximum>))
;;   (display (list "Hello Maximum World!" hello))
;;   (newline)
;;   (next-method))
;;
;; (define maximum-servant (make <maximum>))
;; (define maximum (corba-servant->reference maximum-servant))
;; @end example
;;
;; This creates a new servant for the CORBA interface
;; @code{Foo::MaximumHello} which is derived from @code{Foo::Hello} and
;; @code{Foo::Bar::Baz}. This inheritance is reflected in Scheme.
;;
;; @example
;; ;; Calls method `Foo:Hello:doHello' in class <maximum> and then
;; ;; in <hello> because of the (next-method).
;; (Foo:Hello:doHello maximum)
;;
;; ;; Calls method `Foo:Bar:Baz:haveFun' in class <POA:Foo:Bar:Baz>,
;; ;; the default handler.
;; (Foo:Bar:Baz:haveFun maximum 1 2)
;; @end example
;;
;; Since we're using real CORBA calls, all of this also works for calls
;; which are coming "from the outside", i.e. from C or from a remote
;; process.
;;
;; @section An important limitation
;;
;; CORBA servants can be implemented either in C or in Scheme, but you
;; cannot mix them.
;;
;; For example, in the example above, you learned how to create a CORBA
;; servant for the @code{Foo::MaximumHello} CORBA interface in Scheme.
;; Now let's assume you already have an implementation for the
;; @code{Foo::Hello} interface in C.
;;
;; Even if @code{Foo::MaximumHello} is derived from @code{Foo::Hello},
;; you cannot use the @code{Foo::Hello} C implementation in Scheme.
;;
;; This limitation may sound obvious, but it's not so obvious at all if
;; you're a bit familiar with CORBA. In C, you would normally expect to
;; have a @code{vepv} and a @code{epv} vector in a CORBA servant, and to be
;; able to poke around in the vepv to override methods.
;;
;; As an ORBit2 specific implementation detail, servants which you
;; create from Scheme don't have a @code{vepv} at all and the @code{epv}
;; is not what you'd expect -- the @code{epv} entries are Scheme vectors
;; and not pointers to C functions.
;;
;; @section CORBA structs / sequences
;;
;; There is also support to access CORBA structs / sequences from
;; Scheme, including a special record type for structs. See the source
;; code for details.
;;
;;; Code:

(define-module (gnome corba)
  :use-module (gnome gw corba)
  :use-module (gnome corba types)
  :use-module (gnome corba primitives)
  :use-module (gnome gobject)
  :use-module (oop goops))

(re-export <PortableServer-ServantBase> <CORBA:Object>
	   gnome-corba-error)

(%init-gnome-corba)

(or (corba-primitive-open-module "Bonobo")
    (gnome-corba-error "Can't open `Bonobo' module"))


;;; {Records}
;;;

;; 0: type-name, 1: fields
(define corba-record-type-vtable
  (make-vtable-vtable "prpr" 0
		      (lambda (s p)
			(cond ((eq? s corba-record-type-vtable)
			       (display "#<corba-record-type-vtable>" p))
			      (else
			       (display "#<corba-record-type " p)
			       (display (corba-record-typecode s) p)
			       (display ">" p))))))

(define (make-corba-record-type typecode . opt)
  (let ((printer-fn (and (pair? opt) (car opt))))
    (let* ((corba-fields (corba-struct-fields typecode))
	   (type-name (corba-typecode-primitive->name typecode))
	   (struct (make-struct corba-record-type-vtable 0
				(make-struct-layout
				 (apply string-append
					(map (lambda (f) "pw") corba-fields)))
				(or printer-fn
				    (lambda (s p)
				      (display "#<" p)
				      (display type-name p)
				      (let loop ((fields corba-fields)
						 (off 0))
					(cond
					 ((not (null? fields))
					  (display " " p)
					  (display (car fields) p)
					  (display ": (" p)
					  (display (corba-struct-ref s off) p)
					  (display ")" p)
					  (loop (cdr fields) (+ 1 off)))))
				      (display ">" p)))
				typecode
				(copy-tree corba-fields))))
      ;; Temporary solution: Associate a name to the corba-record type descriptor
      ;; so that the object system can create a wrapper class for it.
      (set-struct-vtable-name! struct type-name)
      struct)))

(define the-corba-environment (current-module))

(define (corba-record-type? obj)
  (and (struct? obj) (eq? corba-record-type-vtable (struct-vtable obj))))

(define (corba-record-typecode obj)
  (if (corba-record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-corba-record-type obj)))

(define (corba-record-type-fields obj)
  (if (corba-record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-corba-record-type obj)))

(define (corba-record-constructor rtd . opt)
  (let ((field-names (if (pair? opt) (car opt) (corba-record-type-fields rtd))))
    (eval `(lambda ,field-names
             (let ((struct (make-corba-struct (corba-record-typecode ',rtd) 1)))
               (struct-set! struct %corba-struct-vtable-offset-printer
                            (struct-ref ',rtd %corba-struct-vtable-offset-printer))
               (struct-set! struct (+ 1 %corba-struct-vtable-offset-user) ',rtd)
               (let loop ((fields (list ,@(map (lambda (f)
                                                 (if (memq f field-names)
                                                     f
                                                     #f))
                                               (corba-record-type-fields `,rtd))))
                          (off 0))
                 (cond
                  ((not (null? fields))
                   (corba-struct-set! struct off (car fields))
                   (loop (cdr fields) (+ 1 off)))))
               struct))
          the-corba-environment)))

(define (corba-record-constructor-from-struct rtd)
  (let ((field-names (corba-record-type-fields rtd)))
    (eval `(lambda (corba-struct)
             (let ((struct (make-corba-struct (corba-record-typecode ',rtd) 1 corba-struct)))
               (struct-set! struct %corba-struct-vtable-offset-printer
                            (struct-ref ',rtd %corba-struct-vtable-offset-printer))
               (struct-set! struct (+ 1 %corba-struct-vtable-offset-user) ',rtd)
               struct))
          the-corba-environment)))

(define (corba-record-predicate rtd)
  (lambda (obj) (and (corba-struct? obj) (eq? rtd (corba-record-type-descriptor obj)))))

(define (corba-record-accessor rtd field-name)
  (let* ((pos (list-index (corba-record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj)
             (and (eq? ',rtd (corba-record-type-descriptor obj))
                  (corba-struct-ref obj ,pos)))
          the-corba-environment)))

(define (corba-record-modifier rtd field-name)
  (let* ((pos (list-index (corba-record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj val)
             (and (eq? ',rtd (corba-record-type-descriptor obj))
                  (corba-struct-set! obj ,pos val)))
          the-corba-environment)))

(define (corba-record? obj)
  (and (corba-struct? obj) (corba-record-type? (corba-record-type-descriptor obj))))

(define (corba-record-type-descriptor obj)
  (if (corba-struct? obj)
      (struct-ref obj (+ 1 %corba-struct-vtable-offset-user))
      (error 'not-a-corba-record obj)))

(define (corba-struct->record struct)
  (let* ((typecode (corba-struct-type struct))
	 (record-type (make-corba-record-type typecode))
	 (constructor (corba-record-constructor-from-struct record-type)))
    (constructor struct)))

(define (corba-sequence->list sequence)
  (let* ((length (corba-sequence-length sequence))
	 (thelist (list)))
    (do ((i 0 (+ i 1)))
	((>= i length) thelist)
      (let ((this (corba-sequence-ref sequence i)))
	(and (corba-struct? this) (set! this (corba-struct->record this)))
	(set! thelist (append! thelist (list this)))))))

(provide 'corba-record)

(export corba-record-type-vtable
	corba-record-type? make-corba-record-type corba-record-typecode
	corba-record-type-fields corba-record-constructor corba-record-predicate
	corba-record-accessor corba-record-modifier corba-record?
	corba-record-type-descriptor corba-record-constructor-from-struct
	corba-struct->record corba-sequence->list)
