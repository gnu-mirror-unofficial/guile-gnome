(read-set! keywords 'prefix)

(define-module (demos tree-model)
  :use-module (gnome gtk))

(define-class <my-tree-model> (<guile-gtk-tree-model>)
  depth
  siblings)

(define-method (on-get-flags (obj <my-tree-model>))
  #f)

(define-method (on-get-n-columns (obj <my-tree-model>))
  1)

(define-method (on-get-column-type (obj <my-tree-model>) index)
  gtype:gchararray)

(define-method (on-get-iter (obj <my-tree-model>) path)
  path)

(define-method (on-get-path (obj <my-tree-model>) iter)
  iter)

(define-method (on-get-value (obj <my-tree-model>) iter index)
  (format #f "~A" iter))

(define-method (on-iter-next (obj <my-tree-model>) iter)
  (let* ((reversed (reverse iter))
         (next (1+ (car reversed))))
    (if (eq? next (slot-ref obj 'siblings))
        #f
        (reverse (cons next (cdr reversed))))))
    
(define-method (on-iter-children (obj <my-tree-model>) parent)
  (cond
   ((not parent)
    (list 0))
   ((eq? (length parent) (slot-ref obj 'depth))
    #f)
   (else
    (reverse (cons 0 (reverse parent))))))

(define-method (on-iter-has-child (obj <my-tree-model>) iter)
  (not (eq? (length iter) (slot-ref obj 'depth))))

(define-method (on-iter-n-children (obj <my-tree-model>) iter)
  (cond
   ((not iter)
    (slot-ref obj 'siblings))
   ((on-iter-has-child obj iter)
    (slot-ref obj 'siblings))
   (else
    0)))

(define-method (on-iter-nth-child (obj <my-tree-model>) parent n)
  (let ((nchildren (on-iter-n-children obj parent)))
    (if (< n nchildren)
        (reverse (cons n (if parent (reverse parent) '())))
        #f)))

(define-method (on-iter-parent (obj <my-tree-model>) iter)
  (if (zero? (length iter))
      #f
      (reverse (cdr (reverse iter)))))

(define-method (initialize (obj <my-tree-model>) initargs)
  (slot-set! obj 'depth 4)
  (slot-set! obj 'siblings 5))

(define (main)
  (let* ((w (make <gtk-window> :type 'toplevel :title "TreeModel Test"))
         (scroll (make <gtk-scrolled-window>
                   :hscrollbar-policy 'automatic :vscrollbar-policy 'automatic))
         (tmodel (make <my-tree-model>))
         (tview (make <gtk-tree-view> :model tmodel))
         (cell (make <gtk-cell-renderer-text>))
         (column (make <gtk-tree-view-column> :title "Data")))
    
    (pack-start column cell #t)
    (add-attribute column cell "text" 0)
    (append-column tview column)
    
    (set-default-size w 250 250)
    (add w scroll)
    (add scroll tview)
    (show-all w)
    (connect w 'delete-event (lambda (w e) (gtk-widget-destroy w) #f))))

(define name "Tree Model")
(define description
  (string-append
   "This example shows how to implement a tree model in Scheme.\n"
   "Tree paths are natively represented as lists of integers. In this simple "
   "model, iters and values of the model are also the same as the paths. Note "
   "that the data is not stored in the model, only the algorithm of how to "
   "produce the data when it is requested. "))
