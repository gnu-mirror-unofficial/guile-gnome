(read-set! keywords 'prefix)
(use-modules (gnome gtk) (gnome gobject primitives))

(define-class <tic-tac-toe> (<gtk-vbox>)
  (board-size ;; this slot is exported as a gobject property
   :param-spec `(,<gparam-int> :minimum 2 :maximum 100 :default-value 3
                               :flags (read write construct)))
  table
  buttons
  winning-combinations

  :signal '(tic-tac-toe #f))

(define (ttt-clear ttt)
  (let ((buttons (slot-ref ttt 'buttons)))
    (do ((p 0 (1+ p)))
        ((>= p (vector-length buttons)))
      (set (vector-ref buttons p) 'active #f))))

(define (ttt-toggle ttt)
  (let ((buttons (slot-ref ttt 'buttons)))
    (let loop ((wins (slot-ref ttt 'winning-combinations)))
      (cond ((not (null? wins))
             (cond ((and-map (lambda (wp) 
                               (get (vector-ref buttons wp) 'active))
                             (car wins))
                    (emit ttt 'tic-tac-toe)
                    (ttt-clear ttt))
                   (else
                    (loop (cdr wins)))))))))

(define (make-sequence len init step)
  (let loop ((i len) (val init))
    (if (eq? i 0)
        '()
        (cons val (loop (1- i) (+ val step))))))

(define-method (gobject:set-property (ttt <tic-tac-toe>) (name <symbol>) value)
  (next-method) ;; actually store the values with the next-method
  (case name
    ((board-size)
     (if (slot-ref ttt 'table) (destroy (slot-ref ttt 'table)))
     (let ((t (gtk-table-new value value #f))
           (bvect (make-vector (* value value))))
       (do ((p 0 (1+ p)))
           ((>= p (vector-length bvect)))
         (let ((b (make <gtk-toggle-button>))
               (i (quotient p value))
               (j (remainder p value)))
           (vector-set! bvect p b)
           (attach-defaults t b i (1+ i) j (1+ j))
           (connect b 'toggled (lambda (unused-arg) (ttt-toggle ttt)))))
       (slot-set! ttt 'winning-combinations
                  (map (lambda (pair) (make-sequence value (car pair) (cadr pair)))
                       (cons*
                        ;; the diagonals
                        (list 0 (1+ value)) 
                        (list (1- value) (1- value))
                        (append
                         ;; the horizontals
                         (let loop ((i 0))
                           (if (eq? i value)
                               '()
                               (cons (list i value) (loop (1+ i)))))
                         ;; the verticals
                         (let loop ((i 0))
                           (if (eq? i (* value value))
                               '()
                               (cons (list i 1) (loop (+ i value)))))))))
       (slot-set! ttt 'table t)
       (slot-set! ttt 'buttons bvect)
       (pack-start-defaults ttt t)
       (show-all t))
     (slot-set! ttt 'board-size value))))

;; setting the 'board-size property on ttt, which is done on
;; construction (due to the 'construct flag in the param's flags), takes
;; care of setting up widget internals. we don't need to explicitly
;; initialize anything -- which is nice, that
;; means the object has a robust interface.

(let* ((w (make <gtk-window> :type 'toplevel :title "Tic tac toe"))
       (vbox (make <gtk-vbox>))
       (ttt (make <tic-tac-toe>))
       (adj (gtk-adjustment-new 3 2 100 1 1 1)) ;; not a gobject yet, argh
       (spin (make <gtk-spin-button>)))
  (set spin 'adjustment adj)
  (connect adj 'value-changed
           (lambda (a) (set ttt 'board-size (inexact->exact (get-value a)))))
  (set-default-size w 250 250)
  (add w vbox)
  (pack-start-defaults vbox ttt)
  (pack-start vbox spin #f #f 0)
  (show-all w)
  (g-timeout-add 100 (lambda () #t))
  (connect ttt 'tic-tac-toe (lambda (ttt) (display "Yay!\n")))
  (connect w 'delete-event (lambda (ttt e) (gtk-main-quit) #f)))

(gtk-main)
