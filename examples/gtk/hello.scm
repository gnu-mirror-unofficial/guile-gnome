;; load up gtk -- also pulls in goops bindings
(use-modules (gnome gtk))

;; define the app as a function -- there are many other ways to do this,
;; of course...
(define (app)
  ;; we can make new widgets just like we make goops objects -- there is
  ;; a corresponding goops class for every GType we know about. the
  ;; arguments to make, after the class, are interpreted as properties
  ;; to set. in this case we make a toplevel window and a button with
  ;; the label, "Hello, World!".
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (button (make <gtk-button> #:label "Hello, World!")))

    ;; there are scheme functions corresponding to all of the c ones...
    ;; and of course we don't have to cast anything.
    (gtk-container-set-border-width window 10)
    (gtk-container-add window button)
    
    ;; and of course you can attach a lambda to a signal :-)
    (gobject-signal-connect button 'clicked (lambda (b) (gtk-main-quit)))

    (gtk-widget-show-all window)

    ;; this will block until gtk-main-quit is called...
    (gtk-main)))

;; meaning this blocks until the button is clicked.
(app)
