;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos editable-cells)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk))

(define (populate-model model)
  (for-each
   (lambda (x)
     (add-item model x))
   '((3 "bottles of coke" #t)
     (5 "package of noodles" #t)
     (2 "packages of chocolate chip cookies" #t)
     (1 "can vanilla ice cream" #t)
     (6 "eggs" #t))))

(define (add-item model item)
  (let ((iter (append model)))
    (for-each
     (lambda (i x) (set-value model iter i x))
     '(0 1 2)
     item)))

(define (add-new-item model)
  (add-item model '(0 "Description here" #t)))
         
(define (remove-selected-item treeview)
  (call-with-values (lambda () (get-selected (get-selection treeview)))
    (lambda (model iter)
      (if iter (remove model iter)))))

(define (cell-edited model column path newtext)
  (let ((iter (get-iter model path)))
    (cond (;; number column
	   (eqv? column 0)
	   (set-value model iter 0 (string->number newtext)))
	  (;; product column
	   (eq? column 1)
	   (set-value model iter 1 newtext)))))

(define (add-columns treeview)
  (let ((model     (get-model treeview))
	;; number column
	(renderer1 (make <gtk-cell-renderer-text>))
	(column1   (make <gtk-tree-view-column>
		     :title "Number"))
	;; product column
	(renderer2 (make <gtk-cell-renderer-text>))
	(column2   (make <gtk-tree-view-column>
		     :title "Product")))

    (connect renderer1 'edited (lambda (w p d)
				 (cell-edited model 0 p d)))
    (pack-start column1 renderer1 #f)
    (add-attribute column1 renderer1 "text" 0)
    (add-attribute column1 renderer1 "editable" 2)
    (append-column treeview column1)

    (connect renderer2 'edited (lambda (w p d)
				 (cell-edited model 1 p d)))
    (pack-start column2 renderer2 #f)
    (add-attribute column2 renderer2 "text" 1)
    (add-attribute column2 renderer2 "editable" 2)
    (append-column treeview column2)))

(define (main)
  (let* (;; create window, etc
	 (window   (make <gtk-window> 
		     :type 'toplevel :title "Shopping list"))
	 (vbox     (make <gtk-vbox> :homogeneous #f :spacing 5))
	 (sw       (make <gtk-scrolled-window>
		     :hscrollbar-policy 'automatic 
		     :vscrollbar-policy 'automatic
		     :shadow-type 'etched-in))
	 ;; create tree model
         (model (gtk-list-store-new (list gtype:gint
                                          gtype:gchararray
                                          gtype:gboolean)))
	 ;; create tree view
	 (treeview (make <gtk-tree-view> 
		     :model model :rules-hint #t))
	 ;; some buttons
	 (hbox     (make <gtk-hbox> :homogeneous #t :spacing 4))
	 (button1  (make <gtk-button> :label "Add item"))
	 (button2  (make <gtk-button> :label "Remove item")))

    (set-border-width window 5)

    (add window vbox)
    (pack-start vbox
		(make <gtk-label> 
		  :label "Shopping list (you can edit the cells!)")
		#f #f 0)

    (pack-start vbox sw #t #t 0)

    (populate-model model)
    (set-mode (get-selection treeview) 'single)
    (add-columns treeview)
    (add sw treeview)

    (pack-start vbox hbox #f #f 0)

    (connect button1 'clicked (lambda (w)
			       (add-new-item model)))
    (pack-start hbox button1 #t #t 0)

    (connect button2 'clicked (lambda (w)
				(remove-selected-item treeview)))
    (pack-start hbox button2 #t #t 0)

    (set-default-size window 320 200)

    (show-all window)))

(define name "Tree View/Editable Cells")
(define description
  (string-append
   "This demo demonstrates the use of editable cells in a GtkTreeView. If "
   "you're new to the GtkTreeView widgets and associates, look into "
   "the GtkListStore example first."))
