;; This is an example of how to use continuations with the Gtk event
;; loop.  It implements a dialog box that looks like to the programmer
;; like it was modal, and to the user like it was non-modal.  The
;; function `yes-or-no?' that implements this dialog box only returns
;; to the caller when the user has aswered the dialog.  The user
;; however can pop up any number of these dialog boxes and answer them
;; in any order he likes.  The main application stays alive as well.

(use-modules (gnome gtk))

;; The callbacks that have been delayed

(define callbacks '())

;; Our own event-loop.  We remove the callbacks before invoking them
;; so that we don't get confused when the callback reenters the
;; event-loop.

(define (event-loop)
  (cond
   ((not (null? callbacks))
    (let ((c (car callbacks)))
      (set! callbacks (cdr callbacks))
	 (c #f)
	 (event-loop)))
   ((gtk-main-iteration)
    (event-loop))))

;; Connect to a signal and arrange for PROC to be consed onto
;; CALLBACKS when the signal is emitted.

(define (connect-delayed obj sig proc)
  (connect obj sig (lambda (o) (set! callbacks (cons proc callbacks)))))

;; Now for the continuation part.  To implement the non-modal dialog box
;; that can be used from your code like a modal one, we save the
;; continuation of the YES-OR-NO? invokation and reenter the event-loop
;; (after popping up the window).  When a button has been clicked, we
;; destroy the window and invoke the saved continuation with the
;; appropriate return value.

(define (yes-or-no? title)
  (call-with-current-continuation
   (lambda (cont)
     ;; Now CONT is the part of the program that receives our
     ;; return value.

     (let* ((d (make <gtk-window> #:type 'toplevel))
	    (v (make <gtk-vbox>))
	    (h (make <gtk-hbox>))
	    (l (make <gtk-label> #:label title))
            (s (make <gtk-hseparator>))
            (y (make <gtk-button> #:label "Yes"))
            (n (make <gtk-button> #:label "No"))

	    (answer (lambda (val)
		      (destroy d)

		      ;; Here we return to our caller after the
		      ;; dialog has been destroyed.
		      (cont val))))

       (add d v)
       (pack-start v l #f #f 0)
       (pack-start v s #f #f 0)
       (pack-start v h #f #f 0)
       (pack-start h y #f #f 0)
       (pack-start h n #f #f 0)
       (show-all d)

       ;; Bind ANSWER to the "clicked" signals of the action
       ;; buttons.
       (connect-delayed y 'clicked (lambda (y) (answer #t)))
       (connect-delayed n 'clicked (lambda (n) (answer #f)))

       ;; Reenter the event-loop.  You can think of this as a goto.
       (event-loop)))))

(define w (make <gtk-window> #:type 'toplevel))
(define b (make <gtk-button> #:label "Ok!"))


(set-default-size w 150 100)
(add w b)

(connect-delayed 
 b 'clicked
 (let ((i 0))
   (lambda (f) 
     (set! i (1+ i))
     ;; capture I in a local environment so that nobody can alter
     ;; it while YES-OR-NO? does its thing.
     (let ((i i))
       ;; Use YES-OR-NO? as if it were a modal dialog.
       (pk i (yes-or-no? (string-append (number->string i)
					": Really?")))))))

(connect w 'destroy (lambda (w) (quit)))

(show b)
(show w)

(event-loop)
