(use-modules (gnome gtk)
             (srfi srfi-13))

(define (build-path . args)
  (string-join args "/"))

(define (get-demo-files)
  (let* ((this-dir (dirname (car (program-arguments))))
         (demo-files '())
         (dir (opendir this-dir)))
    (do ((entry (readdir dir) (readdir dir)))
        ((eof-object? entry))
      (if (eq? 'regular (stat:type (stat (build-path this-dir entry))))
          (set! demo-files (cons (build-path this-dir entry) demo-files))))
    demo-files))

;; unfortunately, we have to use nonstandard function calls to subclass
;; gobject types. maybe we can make this go away sometime and just use
;; define-class somehow, that would be nice.
(define gtype:demo (gobject-type-register-static 
                    (gtype-class->type <gtk-window>) "Demo"))
(define <demo> (gtype->class gtype:demo))

(define-method (initialize (self <demo>) initargs)
  (let ((hbox (make <gtk-hbox>))
        (tree-view (make <gtk-tree-view>))
        (notebook (make <gtk-notebook>))
        (scrolled-factory (lambda () (make <gtk-scrolled-window>)))
        (text-factory (lambda () (make <gtk-text>))))
    (next-method)
    (connect self 'delete-event (lambda (s e) (gtk-main-quit) #f))
    (set-default-size self 600 400)
    
    (add self hbox)
    (pack-start hbox tree-view #t #t 0)
    (pack-start hbox notebook #t #t 0)))

(define d (make <demo>))
(show-all d)
(gtk-main)
