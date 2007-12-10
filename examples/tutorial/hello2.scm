(use-modules (oop goops) (gnome gobject) (gnome gtk))

(let ((window (make <gtk-window> #:type 'toplevel #:title "Hello Buttons!"))
      (box1 (make <gtk-hbox>))
      (button1 (make <gtk-button> #:label "Button 1"))
      (button2 (make <gtk-button> #:label "Button 2"))
      (signal-handler (lambda (w message)
                        (format #t "Hello again -- ~A was pressed\n" message))))

  (connect window 'delete-event (lambda (window event) (gtk-main-quit) #f))

  (set window 'border-width 10)

  (add window box1)

  (pack-start box1 button1 #t #t 0)
  (pack-start box1 button2 #t #t 0)

  ;; there are many ways to use the same lambda with different data --
  ;; wrapping like this is one way...
  (connect button1 'clicked (lambda (b) (signal-handler b "button 1")))
  (connect button2 'clicked (lambda (b) (signal-handler b "button 2")))

  (show-all window)

  (gtk-main))
