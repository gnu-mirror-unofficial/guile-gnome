(use-modules (gnome gobject)
             (gnome gobject generics)
             (gnome gtk)
             (gnome gtk generics)
             (oop goops))

;; "Ongiini, evi" is Oshindonga, a language spoken in northern namibia

(define (app)
  (let* ((window  (make <gtk-window> #:type 'toplevel))
	 (hbox    (make <gtk-hbox>))
	 (vbox    (make <gtk-vbox>))
	 (button  (make <gtk-button> #:label "Ca va, monde?"))
	 (qbutton (make <gtk-button> #:label "Quit"))
	 (label   (make <gtk-label>  #:label "Ongiini, evi?")))
    
    (add window hbox)

    (pack-start-defaults vbox button)
    (pack-start-defaults vbox qbutton)
    (pack-start-defaults hbox vbox)
    (pack-start-defaults hbox label)
    
    (connect button 'clicked (lambda (button) (display "Hola, Mundo!!")))
    (connect qbutton 'clicked (lambda (button) (gtk-main-quit)))
    
    (emit button 'clicked)
    
    (show-all window)
    (gtk-main)))

(app)
