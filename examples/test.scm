(use-modules (gnome gtk)
             (gnome gw libglade))

(define (frobify button)
  (display "Frobbing button...\n")
  (gtk-main-quit))

(let* ((xml (glade-xml-new "test.glade" #f #f))
       (window (get-widget xml "window1")))
  (signal-autoconnect xml (current-module))
  (show window)
  (gtk-main))
