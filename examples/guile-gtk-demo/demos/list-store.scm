;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos list-store)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk))


(define data
  '((#f 60482 "Normal"      "scrollable notebooks and hidden tabs")
    (#f 60620 "Critical"    "gdk_window_clear_area (gdkwindow-win32.c) is not thread-safe")
    (#f 50214 "Major"       "Xft support does not clean up correctly")
    (#t 52877 "Major"       "GtkFileSelection needs a refresh method. ")
    (#f 56070 "Normal"      "Can't click button after setting in sensitive")
    (#t 56355 "Normal"      "GtkLabel - Not all changes propagate correctly")
    (#f 50055 "Normal"      "Rework width/height computations for TreeView")
    (#f 58278 "Normal"      "gtk_dialog_set_response_sensitive () doesn't work")
    (#f 55767 "Normal"      "Getters for all setters")
    (#f 56925 "Normal"      "Gtkcalender size")
    (#f 56221 "Normal"      "Selectable label needs right-click copy menu")
    (#t 50939 "Normal"      "Add shift clicking to GtkTextView")
    (#f 6112  "Enhancement" "netscape-like collapsable toolbars")
    (#f 1     "Normal"      "First bug :=)")))


(define (populate-model store)
  ;; add data to the list store
  (for-each 
   (lambda (b)
     (let ((iter (gtk-list-store-append store)))
       (for-each
	(lambda (col data)
	  (set-value store iter col data))
	'(0 1 2 3) b)))
   data))
		
(define (fixed-toggled treemodel pathstr)
  (let* (
	 ;; get toggled iter
	 (iter  (get-iter treemodel pathstr))
	 ;; get current value and invert
	 (fixed (not (get-value treemodel iter 0))))
    ;; set the new value
    (set-value treemodel iter 0 fixed)))

(define (add-columns treeview)
  (let* ((model    (get-model treeview))
	 ;; column for fixed toggles
	 (renderer1 (make <gtk-cell-renderer-toggle>))
	 (column1   (make <gtk-tree-view-column>
		      :title "Fixed?" 
		      ;; set this column to a fixed sizing (of 50 pixels)
		      :sizing 'fixed
		      :fixed-width 50))
	 ;; column for bug numbers
	 (renderer2 (make <gtk-cell-renderer-text>))
	 (column2   (make <gtk-tree-view-column>
		      :title "Bug number"))
	 ;; column for severities
	 (renderer3 (make <gtk-cell-renderer-text>))
	 (column3   (make <gtk-tree-view-column>
		      :title "Severity"))
	 ;; column for description
	 (renderer4 (make <gtk-cell-renderer-text>))
	 (column4   (make <gtk-tree-view-column>
		      :title "Description"))
	 )
    (connect renderer1 'toggled (lambda (w p)
				  (fixed-toggled model p)))
    (pack-start column1 renderer1 #f)
    (add-attribute column1 renderer1 "active" 0)
    (append-column treeview column1)

    (pack-start column2 renderer2 #f)
    (add-attribute column2 renderer2 "text" 1)
    (set-sort-column-id column2 1)
    (append-column treeview column2)

    (pack-start column3 renderer3 #f)
    (add-attribute column3 renderer3 "text" 2)
    (set-sort-column-id column3 2)
    (append-column treeview column3)

    (pack-start column4 renderer4 #f)
    (add-attribute column4 renderer4 "text" 3)
    (set-sort-column-id column4 4)
    (append-column treeview column4)))

(define (main)
  (let* (
	 ;; create window, etc
	 (window   (make <gtk-window> 
		     :type 'toplevel :title "GtkListStore demo"
		     :default-width 280 :default-height 250 :border-width 8))
	 (vbox     (make <gtk-vbox> :homogeneous #f :spacing 8))
	 (label    (make <gtk-label>
		     :label (string-append 
			     "This is the bug list (note: not based on real "
			     "data, it would be nice to have a nice ODBC "
			     "interface to bugzilla or so, though).")))
	 (sw       (make <gtk-scrolled-window>
		     :hscrollbar-policy 'never :vscrollbar-policy 'automatic
		     :shadow-type 'etched-in))
	 ;; create list store
	 (model    (gtk-list-store-new (list gtype:gboolean
					     gtype:guint
					     gtype:gchararray
					     gtype:gchararray)))
	 ;; create tree view
	 (treeview (make <gtk-tree-view> 
		     :model model :rules-hint #t :search-column 3)))
    (populate-model model)

    (add window vbox)

    (pack-start vbox label #f #f 0)

    (pack-start vbox sw #t #t 0)

    (add sw treeview)

    ;; add columns to the tree view
    (add-columns treeview)

    (show-all window)))


(define name "Tree View/List Store")
(define description
  (string-append
   "The GtkListStore is used to store data in list form, to be used "
   "later on by a GtkTreeView to display it. This demo builds a "
   "simple GtkListStore and displays it. See the Stock Browser "
   "demo for a more advanced example."))
