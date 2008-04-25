#! /bin/sh
# -*- scheme -*-
exec guile-gnome-2 -s $0
!#

(use-modules (gnome gobject)
             (gnome glib)
             (oop goops))

(debug-enable 'backtrace)

(define main-context (g-main-context-default))
(define main-loop (g-main-loop-new #f #f))

(define closure-true (make <gclosure>
                       #:return-type <gboolean>
                       #:func (lambda ()
                                (display "Called all the time, because I return #t\n")
                                #t)))
(define closure-false (make <gclosure>
                        #:return-type <gboolean>
                        #:func (lambda ()
                                 (display "Called only once, because I return #f\n")
                                 #f)))
(define closure-countdown (make <gclosure>
                            #:return-type <gboolean>
                            #:func (let ((c 5))
                                     (lambda ()
                                       (set! c (1- c))
                                       (format #t "~A more times to go...\n" c)
                                       (if (> c 0)
                                           #t
                                           (begin (g-main-loop-quit main-loop)
                                                  #f))))))


(define sources '())

(for-each
 (lambda (closure)
   (let ((s (g-idle-source-new)))
     (g-source-set-closure s closure)
     (set! sources (cons (g-source-attach s main-context) sources))))
 (list closure-true closure-false closure-countdown))

(g-main-loop-run main-loop)

(display "\n\n(clearing all sources)\n\n")

(for-each g-source-remove sources)

;; let's try the new g-idle-add and g-timeout-add...

(g-idle-add (let ((c 5))
              (lambda ()
                (set! c (1- c))
                (format #t "Idle counts go quickly! ~A more times to go...\n" c)
                (if (> c 0)
                    #t
                    #f))))

(g-timeout-add 500
               (let ((c 5))
                 (lambda ()
                   (set! c (1- c))
                   (format #t "Timeout counts come every 500 ms. ~A more times to go...\n" c)
                   (if (> c 0)
                       #t
                       (begin (g-main-loop-quit main-loop)
                              #f)))))

(g-main-loop-run main-loop)
