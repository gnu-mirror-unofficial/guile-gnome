#! /usr/bin/guile -s
!#

(use-modules (gnome gtk))

(define (hello)
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (button (make <gtk-button> #:label "Hello, World!")))

    ;; up to here, everything is the same as hello.scm. however, now we
    ;; can make use of generic functions:

    ;; since window is a container, this generic maps onto the function
    ;; gtk-container-set-border-width
    (set-border-width window 10)

    ;; note that we can set the border width with a gobject property as
    ;; well:
    (gobject-set-property window 'border-width 15)

    ;; (gnome gobject generics), re-exported by (gnome gtk), defines a
    ;; generic `set' method for gobject-set-property, se we can also do
    ;; it like this:
    (set window 'border-width 20)

    ;; this is much less typing :-)
    (add window button)
    
    ;; see (gnome gobject generics) for a full list of gobject generic
    ;; functions
    (connect button 'clicked (lambda (b) (gtk-main-quit)))

    ;; generic functions for .defs apis are defined in the .defs files,
    ;; not manually
    (show-all window)

    (gtk-main)))

(hello)
