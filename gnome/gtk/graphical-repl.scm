;; guile-gnome
;; Copyright (C) 2001 Neil Jerram <neil@ossau.uklinux.net>
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

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

;;; Commentary:
;;
;;A class for a graphical REPL. For the moment, see repl.scm in the
;;examples directory for documentation.
;;
;;; Code:

(define-module (gnome gtk graphical-repl)
  #:use-module (ice-9 buffered-input)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 session) ;; for the completer
  #:use-module (ice-9 regex) ;; for the completer
  #:use-module (gnome gtk)
  #:use-module (gnome pango)
  #:use-module (gnome gtk gdk-event)
  #:export (<guile-gtk-repl> <gtk-buffer-output-port> construct-view))

(define-class <repl-paren-matching-style> (<gflags>)
  #:vtable #((move-cursor "Move cursor" 1)
             (highlight-region "Highlight region" 2)))

(define-class <guile-gtk-repl-entry> (<gtk-entry>)
  new-read-hook
  read-complete-hook
  main-loop
  (port
   #:gparam `(,<gparam-boxed> #:boxed-type ,<gboxed-scm> #:flags (read)))
  (paren-matching-style
   #:gparam `(,<gparam-flags> #:flags-type ,<repl-paren-matching-style>
                              #:default-value 1 ;; arrg
                              #:flags (read write construct)))

  #:gsignal `(complete #f ,<gchararray> ,<gboxed-scm>))


(define (find-matching-open str pos)
  (let ((reversed-str (let loop ((pos pos)
                                 (chars '()))
                        (if (< pos 0)
                            (list->string (reverse! chars))
                            (loop (- pos 1)
                                  (let ((c (string-ref str pos)))
                                    (case c
                                      ((#\()
                                       (cons #\) chars))
                                      ((#\))
                                       (cons #\( chars))
                                      ((#\\)
                                       (set-car! chars #\x)
                                       (cons #\x chars))
                                      (else
                                       (cons c chars)))))))))
    (if (char=? (string-ref reversed-str 0) #\()
        (with-input-from-string reversed-str
          (lambda ()
            (let ((x (false-if-exception (read))))
              (cond ((eof-object? x) #f)
                    (x (- (string-length reversed-str)
                          (port-column (current-input-port))))
                    (else #f)))))
        #f)))

(define (install-paren-matching-handlers entry)
  (letrec ((saved-pos #f)
           (restore-text (lambda args
                           (if saved-pos
                               (begin
                                 (select-region entry 0 0)
                                 (set-position entry saved-pos)
                                 (set! saved-pos #f)))
                           #f)))

    ;; This handler runs before insertion and checks whether the
    ;; character to be inserted is a closing parenthesis. If so, it
    ;; moves the cursor and/or highlights the matching region, and
    ;; installs a timeout to restore the entry contents after half a
    ;; second.
    (connect-after entry
                   'insert-text
                   ;; The position passed to this handler is an in-out
                   ;; gint*. That's broken. Why not just a GValue?
                   (lambda (entry text length in-out-new-pos)
                     (let ((style (gflags->symbol-list (slot-ref entry 
                                                                 'paren-matching-style)))
                           (str (get-text entry))
                           (pos (get-position entry))) ;; before insertion
                       (if (and (eq? length 1) (char=? (string-ref text 0) #\)))
                           (let ((open-pos (find-matching-open str pos)))
                             (if open-pos
                                 (begin
                                   (set! saved-pos (1+ pos))
                                   (if (memq 'move-cursor style)
                                       ;; We can't call set-position here
                                       ;; because the in-out position will
                                       ;; obviate our settings. So do it in
                                       ;; an idle callback (grr).
                                       (g-idle-add (lambda () (set-position entry open-pos) #f)))
                                   (if (memq 'highlight-region style)
                                       (select-region entry open-pos (1+ pos)))
                                   (g-timeout-add 500 restore-text))))))))

    ;; This handler restores the entry contents early in the event of
    ;; a key press occurring before the above timer pops.
    (connect entry 'key-press-event restore-text)))

(define (install-history-handlers entry)
  (let ((history '())
        (position -1)
        (non-history-line "--should never see this--")
        (handler #f)
        (new-read-hook (slot-ref entry 'new-read-hook))
        (read-complete-hook (slot-ref entry 'read-complete-hook)))

    (define (history-up)
      (if (< (+ position 1) (length history))
          (begin
            (if (< position 0)
                (set! non-history-line (get entry 'text)))
            (set! position (+ position 1))
            (let ((text (list-ref history position)))
              (set entry 'text text)
              (set-position entry (string-length text))))))

    (define (history-down)
      (if (>= position 0)
          (begin
            (set! position (- position 1))
            (set entry 'text
                 (if (negative? position)
                     non-history-line
                     (list-ref history position))))))

    (connect entry
             'key-press-event
             (lambda (entry event)
               (let ((keyval (gdk-event-key:keyval event)))
                 (cond ((or (eq? keyval gdk:Up)
                            (eq? keyval gdk:KP-Up))
                        (history-up) #t)
                       ((or (eq? keyval gdk:Down)
                            (eq? keyval gdk:KP-Down))
                        (history-down) #t)
                       (else #f)))))
    
    (add-hook! read-complete-hook
               (lambda (str)
                 (if (not (string=? str ""))
                     (set! history (cons str history)))
                 (set! position -1)))

    (catch #t
           (lambda ()
             (let ((history-file (or (getenv "GUILE_HISTORY")
                                     (string-append (passwd:dir (getpwuid (getuid)))
                                                    "/.guile_history"))))
               (with-input-from-file history-file
                 (lambda ()
                   (let lp ((out '()) (line (read-line)))
                     (if (eof-object? line)
                         (set! history out)
                         (lp (cons line out) (read-line))))))

               (let ((old history))
                 (add-hook! exit-hook
                            (lambda ()
                              (with-output-to-port (open-file history-file "a")
                                (lambda ()
                                  (for-each write-line
                                            (let lp ((in history) (out '()))
                                              (if (eq? in old)
                                                  out
                                                  (lp (cdr in) (cons (car in) out))))))))))))
           noop)))

(if (provided? 'regex)
    (define (complete text)
      (map symbol->string
           (apropos-internal
            (string-append "^" (regexp-quote text)))))
    (define (complete text)
      '()))

(define (do-completion entry)
  ;; `extended' is from r5rs
  (let* ((pos (get-position entry))
         (head (get-chars entry 0 pos))
         (text (let ((extended (string->list "!$%&*+-./:<=>?@^_~")))
                 (let loop ((in (reverse (string->list head)))
                            (out '()))
                   (if (or (null? in)
                           (not (or (char-alphabetic? (car in))
                                    (char-numeric? (car in))
                                    (memq (car in) extended))))
                       (list->string out)
                       (loop (cdr in) (cons (car in) out))))))
         (completions (complete text))
         (common (if (null? completions)
                     ""
                     (let loop ((l completions) (index 0) (ch #f))
                       (cond
                        ((null? l)
                         (loop completions (1+ index) #f))
                        ((< index (string-length (car l)))
                         (if ch
                             (if (eq? (string-ref (car l) index) ch)
                                 (loop (cdr l) index ch)
                                 (substring (car completions) 0 index))
                             (loop (cdr l) index (string-ref (car l) index))))
                        (else
                         (substring (car completions) 0 index)))))))
    (if (> (string-length common) (string-length head))
        (set-position entry
                      (insert-text
                       entry
                       (string-append
                        (substring common (string-length text))
                        (if (eq? (length completions) 1)
                            " "
                            ""))
                       pos))
        (emit entry 'complete head (sort-list completions string<?)))))

(define-method (initialize (entry <guile-gtk-repl-entry>) initargs)
  (next-method)
  (slot-set! entry 'new-read-hook (make-hook))
  (slot-set! entry 'read-complete-hook (make-hook 1))
  (slot-set! entry 'main-loop #f)
  (let ((return-from-read #f)
        (new-read-hook (slot-ref entry 'new-read-hook))
        (read-complete-hook (slot-ref entry 'read-complete-hook)))

    (slot-set! entry 'port
               (make-line-buffered-input-port
                (lambda (continuation?)
                  (run-hook new-read-hook)
                  (let ((read-string #f)
                        (main-loop (or (slot-ref entry 'main-loop)
                                       (g-main-loop-new #f #f))))
                    (set! return-from-read
                          (lambda (x)
                            (set! read-string x)
                            (g-main-loop-quit main-loop)))
                    (g-main-loop-run main-loop)
                    read-string))))

    (modify-font entry (pango-font-description-from-string "Monospace"))

    ;; Define a new-read hook procedure that prepares the entry.
    (add-hook! new-read-hook
               (lambda ()
                 (set entry 'sensitive #t)
                 (grab-focus entry)))

    ;; Define a read-complete hook procedure that clears the entry
    ;; field and makes it insensitive.
    (add-hook! read-complete-hook
               (lambda (str)
                 (set entry 'sensitive #f)
                 (set entry 'text "")))

    ;; When the user presses RETURN, run the read-complete hook and
    ;; return the string to the continuation.
    (connect entry
             'activate
             (lambda (entry)
               (if return-from-read
                   (let ((str (get entry 'text)))
                     (run-hook read-complete-hook str)
                     (return-from-read str))
                   (warn "no return-from-read!"))))

    ;; These keypresses can't be caught by the parent window,
    ;; apparently...
    (connect entry 'key-press-event
             (lambda (entry event)
               (let ((keyval (gdk-event-key:keyval event))
                     (modifiers (gdk-event-key:modifiers event)))
                 (cond
                  ;; Turn Ctrl-C into SIGINT
                  ((and (memq 'control-mask modifiers)
                        (eq? keyval gdk:c))
                   (kill (getpid) SIGINT)
                   #f)
                  ((eq? keyval gdk:Tab)
                   (do-completion entry)
                   #t)
                  (else
                   #f)))))

    (install-paren-matching-handlers entry)

    (install-history-handlers entry)))

(define-class <gtk-buffer-output-port> (<gtk-text-buffer>)
  (port
   #:gparam `(,<gparam-boxed> #:boxed-type ,<gboxed-scm> #:flags (read))))

(define-method (initialize (output <gtk-buffer-output-port>) initargs)
  (define (output-string str)
    (let ((end (get-end-iter output)))
      (insert output end str)))

  (next-method)
  (slot-set! output 'port
             (make-soft-port (vector (lambda (char)
                                       (output-string (string char)))
                                     output-string
                                     #f
                                     #f
                                     #f)
                             "w")))

(define-method (construct-view (buffer <gtk-buffer-output-port>))
  (let ((view (make <gtk-text-view>
                #:editable #f #:cursor-visible #f #:can-focus #f
                #:wrap-mode 'char)))
        
    (modify-font view (pango-font-description-from-string "Monospace"))
    (set-buffer view buffer)
    (let ((mark (create-mark buffer "end" (get-end-iter buffer) #f)))
      (connect buffer 'changed
               (lambda (buf)
                 (scroll-to-mark view mark 0.0 #t 0.0 1.0))))
    view))

(define-class <guile-gtk-repl> (<gtk-vbox>)
  entry
  output
  scrolled
  output-view
  
  (in-port
   #:gparam `(,<gparam-boxed> #:boxed-type ,<gboxed-scm>))
  (out-port
   #:gparam `(,<gparam-boxed> #:boxed-type ,<gboxed-scm>)))

(define-method (gobject:get-property (obj <guile-gtk-repl>) (prop <symbol>))
  (case prop
    ((in-port) (get (slot-ref obj 'entry) 'port))
    ((out-port) (get (slot-ref obj 'output) 'port))))

(define (pretty-display-strings scm width)
  (let* ((max-length (1+ (apply max (map string-length scm))))
         (num-cols (inexact->exact (floor (/ width max-length))))
         (col-width (inexact->exact (floor (/ width num-cols)))))
    (let loop ((l scm) (col 0))
      (if (not (null? l))
          (let* ((str (car l))
                 (len (string-length str))
                 (next-col (modulo (1+ col) num-cols)))
            (display str)
            (if (zero? next-col)
                (if (not (null? (cdr l)))
                    (newline))
                (display (make-string (- col-width len) #\space)))
            (loop (cdr l) next-col))))))

(define-method (initialize (obj <guile-gtk-repl>) initargs)
  (slot-set! obj 'entry (make <guile-gtk-repl-entry>))
  (slot-set! obj 'output (make <gtk-buffer-output-port>))
  (slot-set! obj 'scrolled (make <gtk-scrolled-window>
                             #:hscrollbar-policy 'never
                             #:vscrollbar-policy 'always))
  (slot-set! obj 'output-view (construct-view (slot-ref obj 'output)))
  (let ((entry (slot-ref obj 'entry))
        (output (slot-ref obj 'output))
        (scrolled (slot-ref obj 'scrolled))
        (view (slot-ref obj 'output-view)))
    
    ;; Install some nice keybindings
    (connect obj 'key-press-event
             (lambda (obj event)
               (let ((keyval (gdk-event-key:keyval event))
                     (modifiers (gdk-event-key:modifiers event)))
                 (cond
                  ;; Page-Up/Down to move the view -- this is broken,
                  ;; need to wrap GtkAdjustment better
                  ((or (eq? keyval gdk:Page-Up) (eq? keyval gdk:Page-Down))
                   (let* ((adjustment (get scrolled 'vadjustment))
                          (new-value ((if (eq? keyval gdk:Page-Up) + -)
                                      0.1 (get-value adjustment))))
                     ;; 0.1 is a hack, cause adjustments don't have properties
                     (set-value adjustment
                                (if (< new-value 0) 0 (if (> new-value 1) 1 new-value))))
                   #f)
                  (else
                   #f)))))


    (add-hook! (slot-ref entry 'read-complete-hook)
               (lambda (str)
                 (display str (get output 'port))
                 (newline (get output 'port))))

    (connect entry 'complete
             (lambda (entry text completions)
               (with-output-to-port (get output 'port)
                 (lambda ()
                   (with-error-to-port (get output 'port)
                     (lambda ()
                       (display text) (newline)
                       (if (null? completions)
                           (display "[no completions]")
                           (pretty-display-strings completions 70))
                       ;; We don't really know how big the window is,
                       ;; unfortunately
                       (newline)
                       (display scm-repl-prompt)))))))

    (pack-start obj scrolled #t #t 0)
    (add scrolled view)
    (for-each show (list entry scrolled view))
    (pack-start obj entry #f #f 0)))

