;; A small calendar application for Guile-gtk
;;
;; This is a simple calendar built using the GtkCalendar Widget
;; It actually does nothing but being a Calendar.

(use-modules (gnome gtk))

(define (calendar-example)
 (let ((window (make <gtk-window> #:type 'toplevel))
       (calendar (make <gtk-calendar>)))
   (connect window 'delete-event (lambda (w e) (gtk-main-quit) #f))
   (add window calendar)
   (show-all window)
   (gtk-main)))

(calendar-example)

;;Ariel Rios, Andy Wingo
