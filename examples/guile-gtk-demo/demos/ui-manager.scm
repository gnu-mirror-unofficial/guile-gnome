;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos ui-manager)
  :use-module (gnome gtk))


(define (activate-action action)
  (display (format #f "Action \"~A\" activated\n" (get-name action))))

(define (activate-radio-action action current)
  (display (format #f "Radio action \"~A\" activated\n" (get-name current))))


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
  (let* (
	 ;; create window, etc
	 (window    (make <gtk-window> 
		      :type 'toplevel :title "UI Manager" :border-width 0))
	 (actions   (make <gtk-action-group> :name "Actions"))
	 (ui        (make <gtk-ui-manager>))
	 (box1      (make <gtk-vbox> :homogeneous #f :spacing 0))
	 (label     (make <gtk-label> 
		      :label "Type\n<alt>\nto start"
		      :width-request 200 :height-request 200
		      :xalign 0.5 :yalign 0.5))
	 (separator (make <gtk-hseparator>))
	 (box2      (make <gtk-vbox> 
		      :homogeneous #f :spacing 10 :border-width 10))
	 (button    (make <gtk-button> :label "close")))

    (add-actions actions entries)
    (add-toggle-actions actions toggle-entries)
    (add-radio-actions actions color-entries color-red activate-radio-action)
    (add-radio-actions actions shape-entries shape-oval activate-radio-action)

    (insert-action-group ui actions 0)
    (add-accel-group window (get-accel-group ui))

    (catch #t
	   (lambda ()
	     (add-ui-from-string ui ui-info -1))
	   (lambda (key . args)
	     (case key
	       ((g-error)
		(display (format #f "building menus failed: ~A\n"
				 (caddr args)))))))

    (add window box1)

    (pack-start box1 (gtk-ui-manager-get-widget ui "/MenuBar") #f #f 0)

    (pack-start box1 label #t #t 0)

    (pack-start box1 separator #f #t 0)

    (pack-start box1 box2 #f #t 0)

    (connect window 'delete-event (lambda (w e) (gtk-true)))

    (connect button 'clicked (lambda (w)
			       (destroy window)))
    (pack-start box2 button #t #t 0)
    ;(set-flags button 'can-default)
    ;(grab-default button)

    (show-all window)))


(define name "UI Manager")
(define description
  (string-append
   "The GtkUIManager object allows the easy creation of menus "
   "from an array of actions and a description of the menu hierarchy."))
