;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos entry-completion)
  :use-module (gnome gtk))


(define (create-completion-model)
  (let ((store (gtk-list-store-new (list gtype:gchararray))))
    (for-each
     (lambda (v)
       ;; append a word
       (set-value store (append store) 0 v))
     '("GNOME" "total" "totally"))
    store))

(define (main)
  (let (
	;; original demos uses gtk_dialog_new_with_buttons
	(window     (make <gtk-dialog> 
		      :title "GtkEntryCompletion" 
		      :resizable #f))
	(vbox       (make <gtk-vbox> 
		      :homogeneous #f :spacing 5 :border-width 5))
	(label      (make <gtk-label> 
		      :label (string-append 
			      "Completion demo, try writing <b>total</b> "
			      "or <b>gnome</b> for example.")
		      :use-markup #t))
	(entry      (make <gtk-entry>))
	(completion (make <gtk-entry-completion>
		      :model (create-completion-model))))
    ;; do here what gtk_dialog_new_with_buttons() normally do
    (add-button window 
		(gtk-stock-id 'close)
                (genum->value 
                 (make <gtk-response-type> :value 'none)))

    (connect window 'response (lambda (w a)
				(destroy window)))

    (pack-start (get-vbox window) vbox #t #t 0)

    (pack-start vbox label #f #f 0)

    (pack-start vbox entry #f #f 0)

    ;; assign the completion to the entry
    (set-completion entry completion)

    ;; use model column 0 as the text column
    (set-text-column completion 0)
    
    (show-all window)))


(define name "Entry Completion")
(define description
  (string-append
   "GtkEntryCompletion provides a mechanism for adding support for "
   "completion in GtkEntry."))
