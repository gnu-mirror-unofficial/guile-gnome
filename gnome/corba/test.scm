(use-modules (gnome corba) (gnome corba primitives) (gnome gobject) (oop goops))

(corba-primitive-open-module "Foo")

(define-class <hello> (<POA:Foo:Hello>))
(define-method (Foo:Hello:doHello (hello <hello>))
  (display (list "Hello World!" hello)) (newline))
(define servant (make <hello>))
(define hello (corba-servant->reference servant))

(define-method (Foo:Bar:Baz:haveFun (object <POA:Foo:Bar:Baz>) a b)
  (display (list "Default Foo:Bar:Baz:haveFun handler" a b))
  (newline))

(define-class <maximum> (<hello> <POA:Foo:MaximumHello>))
(define-method (Foo:Hello:doHello (hello <maximum>))
  (display (list "Hello Maximum World!" hello)) (newline)
  (next-method))

(define maximum-servant (make <maximum>))
(define maximum (corba-servant->reference maximum-servant))

