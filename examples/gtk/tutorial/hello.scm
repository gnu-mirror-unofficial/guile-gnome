(use-modules (gnome gtk))

(let ((window (make <gtk-window> #:type 'toplevel))
      (button (make <gtk-button> #:label "Hello World")))

  (connect window 'delete-event (lambda (window event)
                                  (display "delete-event occurred\n")
                                  ;; returning #t to prevent further
                                  ;; propogation of this signal...
                                  #t))

  (connect window 'destroy (lambda (window) (gtk-main-quit)))

  ;; we could also call (set-border-width window 10), but i like the property
  ;; interface...
  (set window 'border-width 10)

  ;; with closures being so easy to make, there's not too much of a need
  ;; for connect-swapped, although i suppose we could add it
  (connect button 'clicked (lambda (button) (destroy window)))

  (add window button)

  (show-all window)

  (gtk-main))
