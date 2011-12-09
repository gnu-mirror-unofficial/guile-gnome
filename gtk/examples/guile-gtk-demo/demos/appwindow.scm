;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos appwindow)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk)
  :use-module (gnome gtk gdk-event))


(define window #f)


(define (activate-action action)
  (let* ((name     (get-name action))
	 (dialog   (make <gtk-message-dialog> 
		     :parent window
		     :flags 'destroy-with-parent
		     :message-type 'info
		     :buttons 'close
		     :text (format 
			    #f
			    "You activated action: \"~A\" of type \"~A\""
			    name (class-of action)))))
    ;; close dialog on user response
    (connect dialog 'response (lambda (d arg1)
				(gtk-widget-destroy dialog)))

    (show dialog)))

(define (activate-radio-action action current)
  (if (get-active current)
      (let ((dialog (make <gtk-message-dialog>
		      :parent window
		      :flags 'destroy-with-parent
		      :message-type 'info
		      :buttons 'close
		      :text (format 
			     #f
			     "You activated radio action: \"~A\" of type \"~A\".\nCurrent value: ~A"
			     (get-name current)
			     (gtype-name 
			      (gtype-class->type (class-of action)))
			     (get-current-value current)))))
	;; close dialog on user response
	(connect dialog 'response (lambda (d arg1)
				    (destroy dialog)))
	
	(show dialog))))


(define entries
  `(("FileMenu"  #f "_File")                         ; name, stock id, label
    ("PreferencesMenu" #f "_Preferences")            ; name, stock id, label
    ("ColorMenu" #f "_Color")                        ; name, stock id, label
    ("ShapeMenu" #f "_Shape")                        ; name, stock id, label
    ("HelpMenu"  #f "_Help")                         ; name, stock id, label
    ("New"       ,(gtk-stock-id 'new) "_New"         ; name, stock id, label
     "<control>N"                                    ; accelerator
     "Create a new file"                             ; tooltip
     ,activate-action)
    ("Open"      ,(gtk-stock-id 'open) "_Open"       ; name, stock id, label
     "<control>O"                                    ; accelerator
     "Open a file"                                   ; tooltip
     ,activate-action)
    ("Save"      ,(gtk-stock-id 'save) "_Save"       ; name, stock id, label
     "<control>S"                                    ; accelerator     
     "Save current file"                             ; tooltip
     ,activate-action)
    ("SaveAs"    ,(gtk-stock-id 'save) "Save _As..." ; name, stock id, label
     #f                                              ; accelerator     
     "Save to a file"                                ; tooltip
     ,activate-action)
    ("Quit"      ,(gtk-stock-id 'quit) "_Quit"       ; name, stock id, label
     "<control>Q"                                    ; accelerator     
     "Quit"                                          ; tooltip
     ,activate-action)
    ("About"     #f "_About"                         ; name, stock id, label
     "<control>A"                                    ; accelerator     
     "About"                                         ; tooltip  
     ,activate-action)
    ("Logo"      "demo-gtk-logo" #f                  ; name, stock id, label
     #f                                              ; accelerator     
     "GTK+"                                          ; tooltip
     ,activate-action)))

(define toggle-entries
  `(("Bold" ,(gtk-stock-id 'bold) "_Bold" ; name, stock id, label
     "<control>B"                         ; accelerator
     "Bold"                               ; tooltip
     ,activate-action
     #t)                                  ; is_active
    ))

(define color-red   0)
(define color-green 1)
(define color-blue  2)

(define color-entries
  `(("Red" #f "_Red"       ; name, stock id, label
     "<control>R"          ; accelerator
     "Blood" ,color-red)   ; tooltip, value
    ("Green" #f "_Green"   ; name, stock id, label 
     "<control>G"          ; label, accelerator      
     "Grass" ,color-green) ; tooltip, value 
    ("Blue" #f "_Blue"     ; name, stock id, label 
     "<control>B"          ; label, accelerator      
     "Sky" ,color-blue)    ; tooltip, value 
    ))

(define shape-square    0)
(define shape-rectangle 1)
(define shape-oval      2)

(define shape-entries
  `(("Square"    #f "_Square"      ; name, stock id, label
     "<control>S"                  ; accelerator     
     "Square"    ,shape-square)    ; tooltip, value
    ("Rectangle" #f "_Rectangle"   ; name, stock id, label
     "<control>R"                  ; accelerator     
     "Rectangle" ,shape-rectangle) ; tooltip, value
    ("Oval"      #f "_Oval"        ; name, stock id, label
     "<control>O"                  ; accelerator     
     "Egg"       ,shape-oval)      ; tooltip, value  
    ))

(define ui-info "
<ui>
  <menubar name='MenuBar'>
    <menu action='FileMenu'>
      <menuitem action='New'/>
      <menuitem action='Open'/>
      <menuitem action='Save'/>
      <menuitem action='SaveAs'/>
      <separator/>
      <menuitem action='Quit'/>
    </menu>
    <menu action='PreferencesMenu'>
      <menu action='ColorMenu'>
	   <menuitem action='Red'/>
	   <menuitem action='Green'/>
	   <menuitem action='Blue'/>
      </menu>
      <menu action='ShapeMenu'>
       <menuitem action='Square'/>
       <menuitem action='Rectangle'/>
       <menuitem action='Oval'/>
      </menu>
      <menuitem action='Bold'/>
    </menu>
    <menu action='HelpMenu'>
      <menuitem action='About'/>
    </menu>
  </menubar>
  <toolbar  name='ToolBar'>
    <toolitem action='Open'/>
    <toolitem action='Quit'/>
    <separator action='Sep1'/>
    <toolitem action='Logo'/>
  </toolbar>
</ui>
")

(define (main)

  (define (update-statusbar buffer statusbar)
    (let ((iter      (get-iter-at-mark buffer (get-insert buffer))))
      ;; clear any previous message, underflow is allowed
      (pop  statusbar 0)
      (push statusbar 0
	    (format #f "Cursor at row ~A column ~A - ~A chars in document"
		    (gtk-text-iter-get-line iter)
		    (gtk-text-iter-get-line-offset iter)
		    (get-char-count buffer)))))

  (define (update-resize-grip widget event statusbar)
    (let ((changed-mask     (gdk-event-window-state:changed-mask event))
	  (new-window-state (gdk-event-window-state:new-window-state event)))
      (or (memq 'maximized  changed-mask)
	  (memq 'fullscreen changed-mask)
	  (set-has-resize-grip statusbar 
			       (not 
				(or (memq 'maximized  new-window-state)
				    (memq 'fullscreen new-window-state))))))
    #f)

  ;; create window, etc
  (set! window (make <gtk-window> 
		 :type 'toplevel :title "Application Window"
		 :default-width 200 :default-height 200))
  (let* ((table     (make <gtk-table> 
		      :n-rows 1 :n-columns 4 :homogeneous #f))
	 (actions   (make <gtk-action-group> :name "AppWindowActions"))
	 (merge     (make <gtk-ui-manager>))
	 ;; create document
	 (sw        (make <gtk-scrolled-window>
		      :hscrollbar-policy 'automatic 
		      :vscrollbar-policy 'automatic
		      :shadow-type 'in))
	 (contents  (make <gtk-text-view>))
	 (buffer    (get-buffer contents))
	 ;; create statusbar
	 (statusbar (make <gtk-statusbar>)))

    (add window table)

    (add-actions actions entries)
    (add-toggle-actions actions toggle-entries)
    (add-radio-actions actions color-entries color-red activate-radio-action)
    (add-radio-actions actions shape-entries shape-oval activate-radio-action)

    (insert-action-group merge actions 0)
    (add-accel-group window (get-accel-group merge))

    (add-ui-from-string merge ui-info)

    (let ((bar1 (get-widget merge "/MenuBar"))
	  (bar2 (get-widget merge "/ToolBar")))
      (show bar1)
      (attach table bar1
	      ; X direction   ; Y direction
	      0 1             0 1
	      '(expand fill)  0
	      0               0)

      (set-tooltips bar2 #t)
      (show bar2)
      (attach table bar2
	      ; X direction   ; Y direction
	      0 1             1 2
	      '(expand fill)  0
	      0               0))

    (attach table sw
	    ; X direction   ; Y direction
	    0 1             2 3
	    '(expand fill)  '(expand fill)
	    0               0)

    (grab-focus contents)

    (add sw contents)

    (attach table statusbar
	    ; X direction   ; Y direction
	    0 1             3 4
	    '(expand fill)  0
	    0               0)
    ;; show text widget info in the statusbar
    (connect buffer 'changed  
	     (lambda (buffer) (update-statusbar buffer statusbar)))
    (connect buffer 'mark-set 
	     (lambda (buffer l m) (update-statusbar buffer statusbar)))
    (connect window 'window-state-event
	     (lambda (w e) (update-resize-grip w e statusbar)))

    (connect window 'delete-event 
	     (lambda (w e) 
	       (destroy w) #f))

    (update-statusbar buffer statusbar)

    (show-all window)))


(define name "Application main window")
(define description
  (string-append
   "Demonstrates a typical application window, with menubar, toolbar, "
   "statusbar."))
