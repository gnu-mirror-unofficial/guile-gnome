(define-module (gnome glib)
  :use-module (gnome gobject)
  :use-module (gnome gobject gw-glib)
  :use-module (gnome gobject gw-utils)
  :use-module (oop goops))

(re-export-bindings (gnome gobject gw-glib))

(define-public (g-idle-add proc)
  (let ((closure (make <gclosure>
                   #:return-type gtype:gboolean
                   #:func proc))
        (source (g-idle-source-new)))
    (g-source-set-closure source closure)
    (g-source-set-priority source 200) ; G_PRIORITY_DEFAULT_IDLE
    (g-source-attach source #f)))

(define-public (g-timeout-add milliseconds proc)
  (let ((closure (make <gclosure>
                   #:return-type gtype:gboolean
                   #:func proc))
        (source (g-timeout-source-new milliseconds)))
    (g-source-set-closure source closure)
    (g-source-set-priority source 200) ; G_PRIORITY_DEFAULT_IDLE
    (g-source-attach source #f)))

(use-modules (gnome gobject event-repl))

;; taken from gnome-guile 0.10.0

(define-public (g-main-loop-console-repl)
  (define main-loop (g-main-loop-new #f #f))

  ;; for some reason, this doesn't work with readline... patches welcome
  ;; :)
  (define inport (fdes->inport 1))
  (define outport (current-output-port))

  (define unspecified (if #f #f))
  (define (prompt)
    (display "gtk> " outport)
    (force-output outport))
  (define (print val)
    (cond ((not (eq? unspecified val))
	   (write val outport)
	   (newline outport)))
    (prompt))
  (define (report data)
    (repl-display-backtrace data outport)
    (repl-display-error data outport)
    (prompt))
  (define (nonblocking-read port)
    (let loop ((res '()))
      (if (char-ready? port)
          (let ((ch (read-char port)))
            (if (eof-object? ch)
                (if (null? res)
                    ch 
                    (apply string (reverse res)))
                (loop (cons ch res))))
	  (apply string (reverse res)))))

  (let ((repl (make-event-repl read primitive-eval print report)))
    (g-timeout-add 50
                   (lambda ()
                     (let ((str (nonblocking-read inport)))
                       (if (eof-object? str)
                           (g-main-loop-quit main-loop)
                           (repl-input repl str))
                       #t)))
    (prompt)
    (g-main-loop-run main-loop)))
