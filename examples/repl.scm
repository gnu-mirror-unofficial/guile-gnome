(use-modules (gnome gtk) (gnome gtk graphical-repl))

(define (guile-gtk-repl)
  (let* ((w (make <gtk-window> #:title "Guile-Gtk REPL"))
         (repl (make <guile-gtk-repl>))
         (main-loop (g-main-loop-new #f #f))
         (in-port (get repl 'in-port))
         (out-port (get repl 'out-port)))

    (add w repl)
    (set-default-size w 600 400)
    (show-all w)
    (connect w 'delete-event (lambda args (apply throw 'quit args) #f))

    (set-current-input-port in-port)
    (set-current-output-port out-port)
    (set-current-error-port out-port)

    ;; we don't use readline
    (set! (using-readline?) #f)
    (set! repl-reader
          (lambda (prompt)
            (display prompt)
            (force-output)
            (run-hook before-read-hook)
            (read (current-input-port))))

    (top-repl)))

(guile-gtk-repl)
