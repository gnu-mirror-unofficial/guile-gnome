;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos tree-store)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk))


(define january
  '(("New Years Day" #t #t #t #t #f #t)
    ("Presidential Inauguration" #f #t #f #t #f #f)
    ("Martin Luther King Jr. day" #f #t #f #t #f #f)))

(define february
  '(("Presidents' Day" #f #t #f #t #f #f)
    ("Groundhog Day" #f #f #f #f #f #f)
    ("Valentine's Day" #f #f #f #f #t #t)))

(define march
  '(("National Tree Planting Day" #f #f #f #f #f #f)
    ("St Patrick's Day" #f #f #f #f #f #t)))

(define april
  '(("April Fools' Day" #f #f #f #f #f #t)
    ("Army Day" #f #f #f #f #f #f)
    ("Earth Day" #f #f #f #f #f #t)
    ("Administrative Professionals' Day" #f #f #f #f #f #f)))

(define may
  '(("Nurses' Day" #f #f #f #f #f #f)
    ("National Day of Prayer" #f #f #f #f #f #f)
    ("Mothers' Day" #f #f #f #f #f #t)
    ("Armed Forces Day" #f #f #f #f #f #f)
    ("Memorial Day" #t #t #t #t #f #t)))

(define june
  '(("June Fathers' Day" #f #f #f #f #f #t)
    ("Juneteenth (Liberation of Slaves)" #f #f #f #f #f #f)
    ("Flag Day" #f #t #f #t #f #f)))

(define july
  '(("Parents' Day" #f #f #f #f #f #t)
    ("Independence Day" #f #t #f #t #f #f)))

(define august
  '(("Air Force Day" #f #f #f #f #f #f)
    ("Coast Guard Day" #f #f #f #f #f #f)
    ("Friendship Day" #f #f #f #f #f #f)))

(define september
  '(("Grandparents' Day" #f #f #f #f #f #t)
    ("Citizenship Day or Constitution Day" #f #f #f #f #f #f)
    ("Labor Day" #t #t #t #t #f #t)))

(define october
  '(("National Children's Day" #f #f #f #f #f #f)
    ("Bosses' Day" #f #f #f #f #f #f)
    ("Sweetest Day" #f #f #f #f #f #f)
    ("Mother-in-Law's Day" #f #f #f #f #f #f)
    ("Navy Day" #f #f #f #f #f #f)
    ("Columbus Day" #f #t #f #t #f #f)
    ("Halloween" #f #f #f #f #f #t)))

(define november
  '(("Marine Corps Day" #f #f #f #f #f #f)
    ("Veterans' Day" #t #t #t #t #f #t)
    ("Thanksgiving" #f #t #f #t #f #f)))

(define december
  '(("Pearl Harbor Remembrance Day" #f #f #f #f #f #f)
    ("Christmas" #t #t #t #t #f #t)
    ("Kwanzaa" #f #f #f #f #f #f)))

(define toplevel
  `(("January"   ,january)
    ("February"  ,february)
    ("March"     ,march)
    ("April"     ,april)
    ("May"       ,may)
    ("June"      ,june)
    ("July"      ,july)
    ("August"    ,august)
    ("September" ,september)
    ("October"   ,october)
    ("November"  ,november)
    ("December"  ,december)))


(define holiday-name-column 0)
(define alex-column    1)
(define havoc-column   2)
(define tim-column     3)
(define owen-column    4)
(define dave-column    5)
(define visible-column 6)
(define world-column   7)


(define (create-model)
  (let (
	; create tree store
	(model (gtk-tree-store-new (list <gchararray>
					 <gboolean>
					 <gboolean>
					 <gboolean>
					 <gboolean>
					 <gboolean>
					 <gboolean>
					 <gboolean>))))
    (for-each 
     (lambda (m)
       (let ((iter (append model (make <gtk-tree-iter>))))
	 (set-value model iter holiday-name-column (car m))
	 (for-each (lambda (c)
		     (set-value model iter c #f))
		   (list alex-column havoc-column tim-column owen-column 
			 dave-column visible-column world-column))
	 (for-each
	  (lambda (h)
	    (let ((iter (append model iter)))
	      (for-each 
	       (lambda (c i)
		 (set-value model iter i c))
	       h (list holiday-name-column alex-column havoc-column
		       tim-column owen-column dave-column world-column))
	      (set-value model iter visible-column #t)))
	  (cadr m))))
     toplevel)

    model))

(define (item-toggled treemodel pathstr column)
  (let* (
	 ;; get toggled iter
	 (iter   (get-iter treemodel pathstr))
	 ;; get current value and invert
	 (toggle (not (get-value treemodel iter column))))
    ;; set the new value
    (set-value treemodel iter column toggle)))

(define (add-columns treeview)
  (let ((model (get-model treeview))
	;; column for holiday names
	(renderer1 (make <gtk-cell-renderer-text>
		     :xalign 0))
	(column1   (make <gtk-tree-view-column>
		     :title "Holliday" 
		     ;; set this column to a fixed sizing (of 50 pixels)
		     :clickable #t))
	;; alex column
	(renderer2 (make <gtk-cell-renderer-toggle>
		     :xalign 0))
	(column2   (make <gtk-tree-view-column>
		     :title "Alex" 
		     :sizing 'fixed
		     :fixed-width 50
		     :clickable #t))
	;; havoc column
	(renderer3 (make <gtk-cell-renderer-toggle>
		     :xalign 0))
	(column3   (make <gtk-tree-view-column>
		     :title "Havoc" 
		     :sizing 'fixed
		     :fixed-width 50
		     :clickable #t))
	;; tim column
	(renderer4 (make <gtk-cell-renderer-toggle>
		     :xalign 0))
	(column4   (make <gtk-tree-view-column>
		     :title "Tim" 
		     :sizing 'fixed
		     :fixed-width 50
		     :clickable #t))
	;; owen column
	(renderer5 (make <gtk-cell-renderer-toggle>
		     :xalign 0))
	(column5   (make <gtk-tree-view-column>
		     :title "Owen" 
		     :sizing 'fixed
		     :fixed-width 50
		     :clickable #t))
	;; dave column
	(renderer6 (make <gtk-cell-renderer-toggle>
		     :xalign 0))
	(column6   (make <gtk-tree-view-column>
		     :title "Dave" 
		     :sizing 'fixed
		     :fixed-width 50
		     :clickable #t)))
    (pack-start column1 renderer1 #f)
    (add-attribute column1 renderer1 "text" 0)
    (append-column treeview column1)

    (pack-start column2 renderer2 #f)
    (connect renderer2 'toggled (lambda (w p)
				  (item-toggled model p alex-column)))
    (add-attribute column2 renderer2 "active"      alex-column)
    (add-attribute column2 renderer2 "visible"     visible-column)
    (add-attribute column2 renderer2 "activatable" world-column)
    (append-column treeview column2)

    (connect renderer3 'toggled (lambda (w p)
				  (item-toggled model p havoc-column)))
    (pack-start column3 renderer3 #f)
    (add-attribute column3 renderer3 "active"      havoc-column)
    (add-attribute column3 renderer3 "visible"     visible-column)
    (append-column treeview column3)

    (connect renderer4 'toggled (lambda (w p)
				  (item-toggled model p tim-column)))
    (pack-start column4 renderer4 #f)
    (add-attribute column4 renderer4 "active"      tim-column)
    (add-attribute column4 renderer4 "visible"     visible-column)
    (add-attribute column4 renderer4 "activatable" world-column)
    (append-column treeview column4)

    (connect renderer5 'toggled (lambda (w p)
				  (item-toggled model p owen-column)))
    (pack-start column5 renderer5 #f)
    (add-attribute column5 renderer5 "active"      owen-column)
    (add-attribute column5 renderer5 "visible"     visible-column)
    (append-column treeview column5)

    (connect renderer6 'toggled (lambda (w p)
				  (item-toggled model p dave-column)))
    (pack-start column6 renderer6 #f)
    (add-attribute column6 renderer6 "active"      dave-column)
    (add-attribute column6 renderer6 "visible"     visible-column)
    (append-column treeview column6)))

(define (main)
  (let* (
	 ;; create window, etc
	 (window   (make <gtk-window> 
		     :type 'toplevel :title "Card planning sheet"
		     :default-width 650 :default-height 400))
	 (vbox     (make <gtk-vbox> 
		     :homogeneous #f :spacing 8 :border-width 8))
	 (sw       (make <gtk-scrolled-window>
		     :hscrollbar-policy 'automatic 
		     :vscrollbar-policy 'automatic
		     :shadow-type 'etched-in))
	 ;; create tree model
	 (model    (create-model))
	 ;; create tree view
	 (treeview (make <gtk-tree-view> 
		     :model model :rules-hint #t))
	 ;; some buttons
	 (hbox     (make <gtk-hbox> :homogeneous #t :spacing 4))
	 (button1  (make <gtk-button> :label "Add item"))
	 (button2  (make <gtk-button> :label "Remove item")))
    (add window vbox)

    (pack-start vbox
		(make <gtk-label>
		  :label "Jonathan's Holiday Card Planning Sheet")
		#f #f 0)

    (pack-start vbox sw #t #t 0)
    
    (set-mode (get-selection treeview) 'multiple)

    (add-columns treeview)

    (add sw treeview)

    ;; expand all rows after the treeview widget has been realized
    (connect treeview 'realize (lambda (w)
				 (gtk-tree-view-expand-all treeview)))

    (show-all window)))


(define name "Tree View/Tree Store")
(define description
  (string-append
   "The GtkTreeStore is used to store data in tree form, to be "
   "used later on by a GtkTreeView to display it. This demo builds "
   "a simple GtkTreeStore and displays it. If you're new to the "
   "GtkTreeView widgets and associates, look into the GtkListStore "
   "example first."))

