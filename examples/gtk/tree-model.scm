(read-set! keywords 'prefix)
(use-modules (gnome gtk))

(define-gobject-class <my-tree-model> <guile-gtk-tree-model> "my-tree-model")

(define *debugging* #f)
(define (debug . args)
  (if *debugging* (apply format #t args)))

(let-params <my-tree-model>
            ((depth #f)
             (siblings #f))
  (define-method (on-get-flags (obj <my-tree-model>))
    (debug "on-get-flags\n")
    #f)

  (define-method (on-get-n-columns (obj <my-tree-model>))
    (debug "on-get-n-columns\n")
    1)

  (define-method (on-get-column-type (obj <my-tree-model>) index)
    (debug "on-get-column-type\n")
    gtype:gchararray)

  (define-method (on-get-iter (obj <my-tree-model>) path)
    (debug "on-get-iter: ~A\n" path)
    path)

  (define-method (on-get-path (obj <my-tree-model>) iter)
    (debug "on-get-path\n")
    iter)

  (define-method (on-get-value (obj <my-tree-model>) iter index)
    (debug "on-get-value\n")
    (format #f "~A" iter))

  (define-method (on-iter-next (obj <my-tree-model>) iter)
    (let* ((reversed (reverse iter))
           (next (1+ (car reversed)))
           (ret (if (eq? next (siblings obj))
                    #f
                    (reverse (cons next (cdr reversed))))))
      (debug "on-iter-next: ~A->~A\n" iter ret)
      ret))
      
  (define-method (on-iter-children (obj <my-tree-model>) parent)
    (debug "on-iter-children: ~A\n" parent)
    (cond
     ((not parent)
      (list 0))
     ((eq? (length parent) (depth obj))
      #f)
     (else
      (reverse (cons 0 (reverse parent))))))
  
  (define-method (on-iter-has-child (obj <my-tree-model>) iter)
    (let ((ret (not (eq? (length iter) (depth obj)))))
      (debug "on-iter-has-child: ~A\n" ret)
      ret))

  (define-method (on-iter-n-children (obj <my-tree-model>) iter)
    (let ((ret (cond
                ((not iter)
                 (siblings obj))
                ((on-iter-has-child obj iter)
                 (siblings obj))
                (else
                 0))))
      (debug "on-iter-n-children: ~A\n")
      ret))

  (define-method (on-iter-nth-child (obj <my-tree-model>) parent n)
    (let* ((nchildren (on-iter-n-children obj parent))
           (ret (if (< n nchildren)
                    (reverse (cons n (if parent (reverse parent) '())))
                    #f)))
      (debug "on-iter-nth-child: ~A[~A]->~A\n" parent n ret)
      ret))

  (define-method (on-iter-parent (obj <my-tree-model>) iter)
    (debug "on-iter-parent\n")
    (if (zero? (length iter))
        #f
        (reverse (cdr (reverse iter)))))

  (define-method (initialize (obj <my-tree-model>) initargs)
    (debug "instance-init\n")
    (set! (depth obj) 4)
    (set! (siblings obj) 4)))

(let* ((w (make <gtk-window> :type 'toplevel :title "TreeModel Test"))
       (scroll (make <gtk-scrolled-window>
                 :hscrollbar-policy 'automatic :vscrollbar-policy 'automatic))
       (tmodel (make <my-tree-model>))
       (tview (make <gtk-tree-view> :model tmodel))
       (cell (make <gtk-cell-renderer-text>))
       (column (make <gtk-tree-view-column> :title "Data"))
       (main-loop (g-main-loop-new #f #f)))

  (pack-start column cell #t)
  (add-attribute column cell "text" 0)
  (append-column tview column)

  (set-default-size w 250 250)
  (add w scroll)
  (add scroll tview)
  (show-all w)
  (connect w 'delete-event (lambda (ttt e) (g-main-loop-quit main-loop) #f))
  (g-main-loop-run main-loop))
