#! /usr/bin/guile -s
!#

(use-modules (gnome gobject)
             (gnome gobject primitives)
             (oop goops))

(debug-enable 'backtrace)

(define (p . args) (for-each display args))

(p "\n============\nGObject test\n============\n\n")

(p "Let's make a closure...")

(define closure
  (make <gclosure>
    #:return-type gtype:gulong
    #:param-types (list gtype:gulong gtype:gchar)
    #:func (lambda (x y) (* x 5))))

(p " and invoke it to get the answer "
   (gclosure-invoke closure 6 #\a)
   ".\n\n")

(p "We can also use the primitive closure construction interface,\n"
   "without using objects.\n"
   "We get the answer "
   (gclosure-primitive-invoke 
    (gclosure-primitive-new (lambda (x y) (make <gulong> #:value 8)))
    gtype:gulong
    (vector (make <gulong> #:value 5) (make <gchar> #:value #\a)))
   " from this one.\n\n")

(p "We can derive new object types, like ")
(define gtype:test
  (gobject-type-register-static gtype:gobject "Test"))
(p gtype:test ".\n\n")

(p "Of course, we need to override some class methods to make\n"
   "this type interesting to us. Overriding the base GObject\n"
   "initializer can allow us to see new types as they come in\n"
   "to existence.\n\n")
(define-method (gobject:class-init (class <gobject>))
  (p "custom gobject:class-init: New GObject class! (" class ")\n")
  (next-method))

(p "Let's get the class from " gtype:test ".\n")
(define <test> (gtype->class gtype:test))

(p "\nNow that we have the class, we can add signals and properties...")

(gobject-class-define-signal <test> 'touch-me gtype:void)

(define-method (test:touch-me (obj <test>))
  (p "(In the default test::touch-me signal handler)\n"))

(gobject-class-install-property <test> (make <gparam-long> #:name 'my-property))

(define-method (gobject-set-property (obj <test>) (name <symbol>) init-value)
  (p "(In the test::get-property handler, chaining up to actually set the\n"
     " value to " init-value ")\n")
  (next-method))

(p " done.\n\n")

(p "We have instantiated the class " <test> ", but we have not\n"
   "made any instances of this class. Let's do that.\n")

(define test-instance (make <test>))

(p "\nWe can now emit the touch-me signal on our new object, " test-instance
   ":\n")
(gobject-signal-emit test-instance 'touch-me)

(p "\nAnd set test::my-property on the new object as well.\n")
(gobject-set-property test-instance 'my-property 2112)

(p "\nOf course, our objects are all first-class GOOPS objects, so they\n"
   "can have some nice things as object methods and such:\n")

(define-method (my-method (obj <object>))
  (p "my-method for object of type <gobject>.\n"))
(define-method (my-method (obj <test>))
  (p "my-method for object of type <test>, chaining up...\n")
  (next-method))

(my-method test-instance)

(p "\nShould you ever need to use GValues directly, you can just instantiate\n"
   "them as objects, e.g. " (make <gboolean> #:value #f) ", \n"
   (make <gchararray> #:value "Hello World!") ", etc.\n")
