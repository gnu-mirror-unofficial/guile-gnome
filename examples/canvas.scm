#! /bin/sh
exec guile-gnome-0 -s $0 "$@"
!#
;; guile-gnome
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(use-modules (srfi srfi-8)
	     (gnome gtk)
	     (gnome gtk gdk-event)
	     (gnome canvas))

(debug-enable 'backtrace)

(define (stderr string . rest)
  (apply format (current-error-port) string rest)
  (force-output (current-error-port)))

(define canvas-width 300)
(define canvas-height canvas-width)
(define output-scale 1.8)

(define (main)

  (define (item-event item event . data)
    (case (gdk-event:type event)
      ((enter-notify) (set item 'fill-color "white"))
      ((leave-notify) (set item 'fill-color "black"))
      ((2button-press) (set item 'fill-color "red")))
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

    (add window vbox)
    (add vbox canvas)

    (let* ((line (make <gnome-canvas-line> #:parent canvas-root
		       #:fill-color "black"
		       #:first-arrowhead #t
		       #:arrow-shape-a 10
		       #:arrow-shape-b 10
		       #:arrow-shape-c 10
		       #:line-style 'on-off-dash
;;;FIXME: how to wrap this properly?
;;;#:points #(0 0 100 100)))
;;;ERROR: In procedure scm->gvalue:
;;;ERROR: Don't know how to make values of type #<gtype GnomeCanvasPoints>
		       #:points (gnome-canvas-points-new #(0 0 100 100))))
	   (rect (make <gnome-canvas-rect> #:parent canvas-root
                       #:x1 0.0 #:y1 0.0 #:x2 100.0 #:y2 9.0
                       #:fill-color "black"))
    	   (text (make <gnome-canvas-text> #:parent canvas-root
		       #:font "new century schoolbook, i bold 20"
		       #:text "Guile GNOME"
		       #:x 0.0 #:y 0.0
		       #:size-points 18
		       #:size-set #t
		       #:fill-color "black"
		       #:anchor 'west))
	   (def (make <gnome-canvas-path-def>))
	   (bezier (make <gnome-canvas-bpath>
		     #:parent canvas-root
		     #:fill-color "black"
		     #:width-pixels 2)))

      (move text -40 55)
      (connect text 'event item-event)
      
      (move bezier 0 -30.0)
      (reset def)
      (moveto def 0.0 0.0)
      (curveto def 40.0 -10.0 60.0 -10.0 100.0 0.0)
      (lineto def 0.0 0.0)
      (closepath def)
      (set-path-def bezier def)
      
      (for-each (lambda (item)
		  (move item -40 70)
		  (affine-relative item output-scale 0 0 output-scale 0 0))
       (list rect bezier)))

    (add vbox button)
    (connect button 'clicked
             (lambda (b) (gtk-main-quit)))

    (connect window 'key-press-event key-press-event)
    (connect window 'delete-event (lambda args (gtk-main-quit) #t))
    
    ;; (set-size-request button canvas-width 20) ?
    (set-child-packing vbox button #f #f 0 'end)
    (set-size-request canvas canvas-width canvas-height)

    (set-pixels-per-unit canvas output-scale)
    (receive (r x y)
	     (world-to-window canvas 1.0 1.0)
	     (stderr "result: ~S (~S, ~S)\n" r x y))
    (set-pixels-per-unit canvas 1.0)
    
    (show-all window)
    (gtk-main)))

(main)
