(read-set! keywords 'prefix)

(define-module (demos sizegroup)
  :use-module (gnome gtk))


(define color-options '("Red" "Green" "Blue"))

(define dash-options  '("Solid" "Dashed" "Dotted"))

(define end-options   '("Square" "Round" "Arrow"))


;; convenience function to create a combo box holding a number of strings
(define (create-combo-box strings)
  (let ((combobox (gtk-combo-box-new-text)))
    (for-each (lambda (str)
		(append-text combobox str)) strings)
    (set-active combobox 0)
    combobox
    )
  )

(define (add-row table row sizegroup labeltext options)
  (let ((label    (gtk-label-new-with-mnemonic labeltext))
	(combobox (create-combo-box options))
	)
    (set-alignment label 0 1)
    (attach table label
	    0 1 row (+ row 1)
	    '(fill expand) #f
	    0 0)

    (set-mnemonic-widget label combobox)
    (add-widget sizegroup combobox)
    (attach table combobox
	    1 2 row (+ 1 row)
	    #f #f
	    0 0)
    )
  )

(define (toggle-grouping checkbutton sizegroup)
  (set-mode sizegroup (if (get-active checkbutton)
			  'horizontal
			  'none))
  )

(define (main)
  (let* (
	 ;(w      (make <gtk-dialog> :title "GtkSizeGroup" :parent main-window))
	 (w      (make <gtk-dialog> :title "GtkSizeGroup"))
	 (vbox   (gtk-vbox-new #f 5))
	 (sg     (gtk-size-group-new 'horizontal))
	 (frame1 (make <gtk-frame> :label "Color options"))
	 (table1 (make <gtk-table> :n-columns 2 :n-rows 2 :homogeneous #f))
	 (frame2 (make <gtk-frame> :label "Line options"))
	 (table2 (make <gtk-table> :n-columns 2 :n-rows 2 :homogeneous #f))
	 (cb     (gtk-check-button-new-with-mnemonic "_Enable grouping"))
	 )
    (add-button w 
		(gtk-stock-id 'close)
		(genum->value 
		 (make <gtk-response-type> :value 'none)))
    (set-resizable w #f)

    (connect w 'response (lambda (w a) (gtk-widget-destroy w) #f))

    (pack-start (get-vbox w) vbox #t #t 0)
    ;(add-action-widget w vbox 0)
    (set-border-width vbox 5)

    ;; frame holding color options
    (pack-start vbox frame1 #t #t 0)
    (set-border-width table1 5)
    (set-row-spacings table1 5)
    (set-col-spacings table1 10)
    (add frame1 table1)

    (add-row table1 0 sg "_Foreground" color-options)
    (add-row table1 1 sg "_Background" color-options)

    ;; second frame holding line style options
    (pack-start vbox frame2 #f #f 0)

    (set-border-width table2 5)
    (set-row-spacings table2 5)
    (set-col-spacings table2 10)
    (add frame2 table2)

    (add-row table2 0 sg "_Dashing"   dash-options)
    (add-row table2 1 sg "_Line ends" end-options)

    ;; check button to turn grouping on and off
    (pack-start vbox cb #f #f 0)

    (set-active cb #t)
    (connect cb 'toggled (lambda (b)
			   (toggle-grouping b sg)))

    (show-all w)
    )
  )


(define name "Size Groups")
(define description
  (string-append
   "GtkSizeGroup provides a mechanism for grouping a number of "
   "widgets together so they all request the same amount of space."
   "This is typically useful when you want a column of widgets to "
   "have the same size, but you can't use a GtkTable widget."
   "\n"
   "Note that size groups only affect the amount of space requested,"
   "not the size that the widgets finally receive. If you want the"
   "widgets in a GtkSizeGroup to actually be the same size, you need"
   "to pack them in such a way that they get the size they request"
   "and not more. For example, if you are packing your widgets"
   "into a table, you would not include the GTK_FILL flag."))


; temporary
(define (gtk-stock-id nick)
  (string-append "gtk-" (symbol->string nick)))

