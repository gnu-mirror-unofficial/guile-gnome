;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos sizegroup)
  :use-module (gnome gtk))


(define color-options '("Red" "Green" "Blue"))

(define dash-options  '("Solid" "Dashed" "Dotted"))

(define end-options   '("Square" "Round" "Arrow"))


(define (main)

  ;; convenience function to create a combo box holding a number of strings
  (define (create-combo-box strings)
    (let ((combobox (gtk-combo-box-new-text)))
      (for-each (lambda (str)
		  (append-text combobox str)) strings)
      (set-active combobox 0)
      combobox))
  
  (define (add-row table row sizegroup labeltext options)
    (let* ((combobox (create-combo-box options))
	   (label    (make <gtk-label> 
		       :label labeltext :use-underline #t 
		       :mnemonic-widget combobox :xalign 0 :yalign 1)))
      (attach table label
	      0 1 row (+ row 1)
	      '(fill expand) #f
	      0 0)
      (add-widget sizegroup combobox)
      (attach table combobox
	      1 2 row (+ 1 row)
	      #f #f
	      0 0)))
  
  (define (toggle-grouping checkbutton sizegroup)
    (set sizegroup 'mode (if (get-active checkbutton)
			     'horizontal
			     'none)))

  (let* ((w      (make <gtk-dialog> :title "GtkSizeGroup" :resizable #f))
	 (vbox   (make <gtk-vbox> 
		   :homogeneous #f :spacing 5 :border-width 5))
	 (sg     (make <gtk-size-group> :mode 'horizontal))
	 (frame1 (make <gtk-frame> :label "Color options"))
	 (table1 (make <gtk-table> 
		   :n-columns 2 :n-rows 2 :homogeneous #f
		   :border-width 5 :row-spacing 5 :row-spacing 10))
	 (frame2 (make <gtk-frame> :label "Line options"))
	 (table2 (make <gtk-table> 
		   :n-columns 2 :n-rows 2 :homogeneous #f
		   :border-width 5 :row-spacing 5 :row-spacing 10))
	 (cb     (make <gtk-check-button> 
		   :label "_Enable grouping" :use-underline #t :active #t)))
    (add-button w 
		(gtk-stock-id 'close)
		(genum->value 
		 (make <gtk-response-type> :value 'none)))

    (connect w 'response (lambda (w a) (gtk-widget-destroy w) #f))

    (pack-start (get-vbox w) vbox #t #t 0)

    ;; frame holding color options
    (pack-start vbox frame1 #t #t 0)
    (add frame1 table1)

    (add-row table1 0 sg "_Foreground" color-options)
    (add-row table1 1 sg "_Background" color-options)

    ;; second frame holding line style options
    (pack-start vbox frame2 #f #f 0)
    (add frame2 table2)

    (add-row table2 0 sg "_Dashing"   dash-options)
    (add-row table2 1 sg "_Line ends" end-options)

    ;; check button to turn grouping on and off
    (pack-start vbox cb #f #f 0)

    (connect cb 'toggled (lambda (b)
			   (toggle-grouping b sg)))

    (show-all w)))


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
