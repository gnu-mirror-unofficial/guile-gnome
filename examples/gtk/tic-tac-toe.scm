(read-set! keywords 'prefix)

;; translation unfinished, must sleep...

(use-modules (gnome gtk))

(define gtype:ttt (gobject-type-register-static 
                   (gtype-class->type <gtk-vbox>) "TicTacToe"))
(define <tictactoe> (gtype->class gtype:ttt))

(define gtype:foo (gobject-type-register-static 
                   (gtype-class->type <gtk-label>) "Foo"))
(define <foo> (gtype->class gtype:foo))

(gobject-class-define-signal <tictactoe> 'tictactoe #f)

(define-method (initialize (widget <tictactoe>) initargs)
  (next-method)
  (display "initializing table\n")
  (let* ((table (gtk-table-new 3 3 #t))
         (buttons (make-vector 9)))
    (define (ttt-clear)
      (do ((p 0 (1+ p)))
          ((>= p 9))
        (set (vector-ref buttons p) 'active #f)))
    (define (ttt-toggle unused)
      (let loop ((wins '((0 1 2) (3 4 5) (6 7 8)
                         (0 3 6) (1 4 7) (2 5 8)
                         (0 4 8) (2 4 6))))
        (cond ((not (null? wins))
               (cond ((and-map (lambda (wp) 
                                 (get (vector-ref buttons wp) 'active))
                               (car wins))
                      (emit widget 'tictactoe)
                      (ttt-clear))
                     (else
                      (loop (cdr wins))))))))
    
    (do ((p 0 (1+ p)))
        ((>= p 9))
      (let ((b (make <gtk-toggle-button>))
            (i (quotient p 3))
            (j (remainder p 3)))
        (vector-set! buttons p b)
        (attach-defaults table b i (1+ i) j (1+ j))
        (connect b 'toggled ttt-toggle)
        ;(set-size-usize b 20 20)
        ))
    (add widget table)
    (show-all table)))

(define w (make <gtk-window> :type 'toplevel))
(define ttt (make <tictactoe>))
(add w ttt)
(show-all w)
(connect ttt 'tictactoe (lambda (ttt) (pk 'Yay)))

(gtk-main)
