#! /usr/bin/guile -s
!#

(use-modules (oop goops)
             (gnome gobject))

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

(p "We can derive new object types, like ")
(define-class <test> (<gobject>)
  (my-property #:gparam (list <gparam-long>))
  #:gsignal (list 'touch-me gtype:void))
(p <test> ".\n\n")

(define-method (test:touch-me (obj <test>))
  (p "(In the default test::touch-me signal handler)\n"))

(define-method (gobject:set-property (obj <test>) (name <symbol>) init-value)
  (p "(In the test::set-property handler. You can implement your own storage\n"
     " mechanism, or call (next-method) for a default implementation, as we\n"
     " are doing now (value " init-value ").)\n")
  (next-method))

(p "We have instantiated the class " <test> ", but we have not\n"
   "made any instances of this class. Let's do that.\n")

(define test-instance (make <test>))

(p "\nWe can now emit the touch-me signal on our new object, " test-instance
   ":\n")
(gtype-instance-signal-emit test-instance 'touch-me)

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
