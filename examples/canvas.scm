#! /usr/bin/guile -s
!#
(use-modules (gnome gtk)
	     (gnome gtk gdk-event)
	     (gnome canvas))

(debug-enable 'backtrace)

(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

(define canvas-width 300)
(define canvas-height canvas-width)
(define output-scale 1.8)

(define (main)

  (define (item-event item event . data)
    (case (gdk-event:type event)
      ((enter-notify) (gobject-set-property item 'fill-color "white"))
      ((leave-notify) (gobject-set-property item 'fill-color "black"))
      ((2button-press) (gobject-set-property item 'fill-color "red")))
    #t)
    
  (define (key-press-event item event . data)
    (let ((keyval (gdk-event-key:keyval event))
	  (mods (gdk-event-key:modifiers event)))
      (if (and (or (eq? keyval gdk:q)
		   (eq? keyval gdk:w))
	       (equal? mods '(control-mask modifier-mask)))
	  (gtk-main-quit))
      #f))
    
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (button (make <gtk-button> #:label "Exit"))
	 (canvas (make <gnome-canvas>))
	 (vbox (make <gtk-vbox>))
	 (canvas-root (root canvas)))

    (gtk-container-add window vbox)
    (add vbox canvas)

    (let* ((rect-type (gtype-class->type <gnome-canvas-rect>))
	   (line (gnome-canvas-item-new canvas-root rect-type))
    	   (text (make <gnome-canvas-text> #:parent canvas-root
		       #:font "new century schoolbook, i bold 20"
		       #:text "Guile GNOME"
		       #:x 0.0 #:y 0.0
		       #:size-points 18
		       #:size-set #t
		       #:fill-color "black"
		       #:anchor 'west))
	   (line-2 (make <gnome-canvas-rect> #:parent canvas-root)))
      
      (set line 'x1 0.0)
      (set line 'y1 0.0)
      (set line 'x2 100.0)
      (set line 'y2 9.0)
      (set line 'fill-color "black")
      
      (map (lambda (x) (apply gobject-set-property (cons line-2 x)))
	   '((x1 0.0) (y1 30.0) (x2 100.0) (y2 39.0) (fill-color "black")))

      ;;(gtk-signal-connect text 'event item-event))
      (move text -40 55)
      (gtype-instance-signal-connect text 'event item-event)
    
      (for-each (lambda (item)
		  (move item -40 20)
		  (affine-relative item output-scale 0 0 output-scale 0 0))
       (list line line-2)))

    (add vbox button)
    (gtype-instance-signal-connect button 'clicked
				   (lambda (b) (gtk-main-quit)))

    (gtype-instance-signal-connect window 'key-press-event key-press-event)
    
    (set-size-request button canvas-width 20)
    (set-child-packing vbox button #f #f 0 'end)
    (set-size-request canvas canvas-width canvas-height)
    
    (show-all window)
    (gtk-main)))

(main)
