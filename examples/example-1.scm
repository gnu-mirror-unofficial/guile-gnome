#! /bin/sh
exec guile-gnome-0 -s $0
!#
;; guile-gnome
;; Copyright (C) 2003,2004 Free Software Foundation, Inc.

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
