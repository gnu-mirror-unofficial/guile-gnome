(read-set! keywords 'prefix)

(define-module (demos button-box)
  :use-module (gnome gtk))


(define (create-bbox horizontal title spacing layout)
  (let (
	(frame (make <gtk-frame> :label title))
	(bbox  (make (if horizontal <gtk-hbutton-box> <gtk-vbutton-box>) 
		 :layout-style layout :spacing spacing))
	)
    (set-border-width bbox 5)
    (add frame bbox)

    (for-each (lambda (s)
		(add bbox (gtk-button-new-from-stock (gtk-stock-id s))))
	      '(ok cancel help))

    frame
    )
  )

(define (main)
  (let (
	 (window    (make <gtk-window> :type 'toplevel))
	 (mainvbox  (gtk-vbox-new #f 0))
	 (framehorz (make <gtk-frame> :label "Horizontal Button Boxes"))
	 (vbox      (gtk-vbox-new #f 0))
	 (framevert (make <gtk-frame> :label "Vertical Button Boxes"))
	 (hbox      (gtk-hbox-new #f 0))
	 )
    (set-title window "Button Boxes")

    (set-border-width window 10)
    
    (add window mainvbox)

    (pack-start mainvbox framehorz #t #t 10)

    (set-border-width vbox 10)
    (add framehorz vbox)

    (pack-start vbox
		(create-bbox #t "Spread" 40 'spread)
		#t #t 0)
    (pack-start vbox
		(create-bbox #t "Edge" 40 'edge)
		#t #t 5)
    (pack-start vbox
		(create-bbox #t "Start" 40 'start)
		#t #t 5)
    (pack-start vbox
		(create-bbox #t "End" 40 'end)
		#t #t 5)

    (pack-start mainvbox framevert #t #t 10)

    (set-border-width hbox 10)
    (add framevert hbox)

    (pack-start hbox
		(create-bbox #f "Spread" 30 'spread)
		#t #t 0)
    (pack-start hbox
		(create-bbox #f "Edge" 30 'edge)
		#t #t 5)
    (pack-start hbox
		(create-bbox #f "Start" 30 'start)
		#t #t 5)
    (pack-start hbox
		(create-bbox #f "End" 30 'end)
		#t #t 5)

    (show-all window)
    )
  )


(define name "Button Boxes")
(define description
  (string-append
   "The Button Box widgets are used to arrange buttons with padding."))
