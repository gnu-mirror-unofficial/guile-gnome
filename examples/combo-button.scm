(use-modules (gnome gtk))


(define (demo)
  (let* ((w (make <gtk-window> #:type 'toplevel))
	 (cb (make <gtk-combo>)))

    (set-border-width w 10)

    (add w cb)
    
    (connect w 'delete-event (lambda (w e)
                               (gtk-widget-destroy w)
                               (gtk-main-quit)
                               #t))

    ;; note that we use a scheme list here; it's translated to a glist
    ;; on the c side
    (set-popdown-strings cb '("foo" "bar"))

    (show-all w)

    (gtk-main)))

(demo)
