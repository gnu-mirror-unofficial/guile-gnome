(use-modules (gnome gtk) (gnome gtk graphical-repl))

(define (guile-gtk-repl)
  (let* ((w (make <gtk-window> #:title "Guile-Gtk REPL"))
         (repl (make <guile-gtk-repl>))
         (main-loop (g-main-loop-new #f #f))
         (in-port (get repl 'in-port))
         (out-port (get repl 'out-port))
         (old-in-port #f)
         (old-out-port #f)
         (old-error-port #f)
         (old-repl-reader #f))

    (add w repl)
    (set-default-size w 600 400)
    (show-all w)
    (connect w 'delete-event (lambda args (apply throw 'quit args) #f))

    (dynamic-wind
        (lambda ()
          (set! old-in-port (set-current-input-port in-port))
          (set! old-out-port (set-current-output-port out-port))
          (set! old-error-port (set-current-error-port out-port))
          (set! old-repl-reader repl-reader)
          (set! repl-reader
                (lambda (prompt)
                  (display prompt)
                  (force-output)
                  (run-hook before-read-hook)
                  (read))))

        top-repl

        (lambda ()
          (set-current-input-port old-in-port)
          (set-current-output-port old-out-port)
          (set-current-error-port old-error-port)
          (set! repl-reader old-repl-reader)))))

(guile-gtk-repl)
