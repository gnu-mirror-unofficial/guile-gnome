;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos dialog)
  :use-module (gnome gtk))


(define message-dialog-clicked 
  (let ((i 0))
    (lambda (parent)
      (let ((dialog (make <gtk-message-dialog> 
		      :parent parent
		      :flags '(modal destroy-with-parent)
		      :message-type 'info
		      :buttons 'ok
		      :text (string-append
			     "This message box has been popped up "
			     "the following\n"
			     "number of times:\n\n"
			     (number->string i)))))
	(run dialog)
	(destroy dialog)
	(set! i (+ 1 i))))))

(define (interactive-dialog-clicked parent entry1 entry2)
  (let (
	;; original demos makes use of gtk_dialog_new_with_buttons()
	(dialog (make <gtk-dialog> 
		  :title "Interactive Dialog" 
		  :modal #t :destroy-with-parent #t))
	(hbox   (make <gtk-hbox> :homogeneous #f :spacing 8))
	(stock  (make <gtk-image> 
		  :stock (gtk-stock-id 'dialog-question)
		  :icon-size (genum->value 
			      (make <gtk-icon-size> #:value 'dialog))))
	(table  (make <gtk-table> :n-columns 2 :n-rows 2 :homogeneous #f))
	(label1 (make <gtk-label> :label "_Entry 1" :use-underline #t))
	(local-entry1 (make <gtk-entry>))
	(label2 (make <gtk-label> :label "E_ntry 2" :use-underline #t))
	(local-entry2 (make <gtk-entry>)))
    (add-button dialog 
		(gtk-stock-id 'ok)
		(genum->value 
		 (make <gtk-response-type> :value 'ok)))
    (add-button dialog 
		"_Non-stock Button"
		(genum->value 
		 (make <gtk-response-type> :value 'cancel)))
		      
    (set-border-width hbox 8)
    (pack-start (get-vbox dialog) hbox #f #f 0)

    (pack-start hbox stock #f #f 0)

    (set-row-spacings table 4)
    (set-col-spacings table 4)
    (pack-start hbox table #t #t 0)

    (attach-defaults table label1 0 1 0 1)
    (set-text local-entry1 (get-text entry1))
    (attach-defaults table local-entry1 1 2 0 1)
    (set-mnemonic-widget label1 local-entry1)

    (attach-defaults table label2 0 1 1 2)
    (set-text local-entry2 (get-text entry2))
    (attach-defaults table local-entry2 1 2 1 2)
    (set-mnemonic-widget label2 local-entry2)

    (show-all hbox)

    (if (eq? 'ok (genum->symbol (make <gtk-response-type> 
				  :value (run dialog))))
	(begin
	  (set-text entry1 (get-text local-entry1))
	  (set-text entry2 (get-text local-entry2))))
    (destroy dialog)))

(define (main)
  (let ((window (make <gtk-window> :type 'toplevel))
	(frame  (make <gtk-frame> :label "Dialogs"))
	(vbox   (make <gtk-vbox> :homogeneous #f :spacing 8)))
    (set-title window "Dialogs")
    (set-border-width window 8)

    (add window frame)

    (set-border-width vbox 8)
    (add frame vbox)

    ;; standard message dialog
    (let ((hbox   (make <gtk-hbox> :homogeneous #f :spacing 8))
	  (button (make <gtk-button> 
		    :label "_Message Dialog" :use-underline #t)))
      (pack-start vbox hbox #f #f 0)
      (connect button 'clicked (lambda (w)
				 (message-dialog-clicked window)))
      (pack-start hbox button #f #f 0))

    (pack-start vbox (make <gtk-hseparator>) #f #f 0)

    ;; interactive dialog
    (let ((hbox   (make <gtk-hbox> :homogeneous #f :spacing 8))
	  (vbox2  (make <gtk-vbox> :homogeneous #f :spacing 0))
	  (button (make <gtk-button> 
		    :label "_Interactive Dialog" :use-underline #t))
	  (table  (make <gtk-table> :n-columns 2 :n-rows 2 :homogeneous #f))
	  (label1 (make <gtk-label> :label "_Entry 1" :use-underline #t))
	  (entry1 (make <gtk-entry>))
	  (label2 (make <gtk-label> :label "E_ntry 2" :use-underline #t))
	  (entry2 (make <gtk-entry>)))
      (pack-start vbox hbox #f #f 0)
      (connect button 'clicked (lambda (w)
				 (interactive-dialog-clicked window 
							     entry1 entry2)))
      (pack-start hbox vbox2 #f #f 0)
      (pack-start vbox2 button #f #f 0)

      (set-row-spacings table 4)
      (set-col-spacings table 4)
      (pack-start hbox table #f #f 0)

      (attach-defaults table label1 0 1 0 1)
      (attach-defaults table entry1 1 2 0 1)
      (set-mnemonic-widget label1 entry1)

      (attach-defaults table label2 0 1 1 2)
      (attach-defaults table entry2 1 2 1 2)
      (set-mnemonic-widget label2 entry2))

    (show-all window)))


(define name "Dialog and Message Boxes")
(define description
  (string-append
   "Dialog widgets are used to pop up a transient window for user feedback."))
