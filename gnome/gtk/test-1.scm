(use-modules (gnome gtk)
             (gnome gobject)
	     (oop goops))

;; "Ongiini, evi" is Oshindonga, a language spoken in northern namibia

(define (app)
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (hbox   (make <gtk-hbox>))
	 (vbox (make <gtk-vbox>))
	 (button (make <gtk-button> #:label "Ca va, monde?"))
	 (qbutton (make <gtk-button> #:label "Quit"))
	 (label (make <gtk-label> #:label "Ongiini, evi?")))
    
    (gtk-container-add window hbox)

    (gtk-box-pack-start-defaults vbox button)
    (gtk-box-pack-start-defaults vbox qbutton)
    (gtk-box-pack-start-defaults hbox vbox)
    (gtk-box-pack-start-defaults hbox label)
    
    (gobject-signal-connect button 'clicked (lambda (button) (display "Hola, Mundo!!")))
    (gobject-signal-connect qbutton 'clicked (lambda (button) (gtk-main-quit)))
    
    (gobject-signal-emit button 'clicked)
    
    (gtk-widget-show-all window)
    (gtk-main)))

(app)


