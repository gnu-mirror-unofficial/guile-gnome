#! /bin/sh
# -*- scheme -*-
exec guile-gnome-0 -s $0 "$@"
!#
;;;A translation of the GTK scribble example
;;;It features a resizable canvas that can be drawn on with the mouse
;;;by Marcello Mathias Herreshoff
;;;translated to the guile-gnome API by Andy Wingo
;;;(C) 2004 GNU GPL

(use-modules (oop goops) (gnome glib) (gnome gtk) (gnome gtk gdk-event))

;Variables holding the size of the pixmap & drawing area.
(define width 200)
(define height 200)
(define brush-size 4)

;;The widgets:
(define toplevel-window (make <gtk-window> #:type 'toplevel))
(define layout-box (make <gtk-vbox>))
(define drawing-area (make <gtk-drawing-area>))
(define close-button (make <gtk-button> #:label (gtk-stock-id 'close)
                           #:use-stock #t))
(define pixmap #f) ;to be created by recreate-pixmap

; We can't generate these until the window is visble,
; because we need the style object
(define fore-gc #f)
(define back-gc #f)



;Note: configure is GTKese for resize
(define (configure-handler x ev)
  (recreate-pixmap)
  #t)

(define (recreate-pixmap)
  (let ((window (get-window drawing-area))
        (a (get-allocation drawing-area)))
    (set! width (vector-ref a 2))
    (set! height (vector-ref a 3))
    (set! pixmap (gdk-pixmap-new window width height -1))
    (gdk-draw-rectangle pixmap back-gc 1 0 0 width height))
  (update-handler))
;Important note: we can't draw directly on the drawing area.
;the pixmap must use the drawing area's window.


;expose, that is, some other window is no longer covering us
(define (expose-handler w x)
  (update-handler) ;we just redraw everything.
  #t)


;To redraw, we just write our backup pixmap onto the drawing area's window
(define (update-handler) 
  (gdk-draw-drawable (get-window drawing-area)
                     back-gc pixmap 0 0 0 0 width height))

;How to draw on the pixmap:
(define (draw-brush widget x y) 
  (gdk-draw-rectangle pixmap fore-gc 1 x y brush-size brush-size)
  ;;we just make a rectangle
  (update-handler))

; if they click, call draw-brush with the exact position
(define (click-handler w ev)
  (draw-brush drawing-area 
              (inexact->exact (gdk-event-button:x ev))
              (inexact->exact (gdk-event-button:y ev)))
  #t)

;If they drag the mouse over the drawing area and the button is down,
;then we call draw-brush with the exact position
(define (drag-handler w ev)
  (if (memq 'button1-mask (gdk-event-motion:modifiers ev))
      (draw-brush drawing-area
                  (inexact->exact (gdk-event-motion:x ev))
                  (inexact->exact (gdk-event-motion:y ev))))
  #t)

;Set the events that the drawing-area can capture,
; so we can respond to click and drags.
;We need to to this before the widget is visible.
(set-events drawing-area '(button-press-mask pointer-motion-mask))

;Lay out the widgets
(pack-start layout-box drawing-area)
(pack-start layout-box close-button)
(add toplevel-window layout-box)

;Set the size of the drawing area to something reasonable.
(set-size-request drawing-area width height)

;make the window, and its contents visible
(show-all toplevel-window)

;Now we connect the events
;We need to do this things are visible, as some of these events might get
;called and panic.
(connect drawing-area 'configure-event configure-handler)
(connect drawing-area 'expose-event expose-handler)
(connect drawing-area 'motion-notify-event drag-handler)
(connect drawing-area 'button-press-event click-handler)
(connect close-button 'clicked (lambda args (exit 0)))

;This also needs to happen after the window is visible, because
;we need its style
(let ((style (get-style toplevel-window)))
  (set! fore-gc (get-black-gc style))
  (set! back-gc (get-white-gc style)))

;We set up the pixmap
(recreate-pixmap)

(g-main-loop-run (g-main-loop-new))

