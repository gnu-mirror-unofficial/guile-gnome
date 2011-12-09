;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos menus)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk))


(define (main)

  (define (create-menu depth tearoff)
    (and=> (>= depth 1)
	   (lambda (x)
	     (let ((menu  (make <gtk-menu>)))
	       (if tearoff (append menu (make <gtk-tearoff-menu-item>)))
	       (let loop ((i     0)
			  (group #f))
		 (let ((menuitem (gtk-radio-menu-item-new-with-label 
				  group
				  (format #f "item ~A - ~A" depth (+ 1 i)))))
		   (append menu menuitem)
		   (set-sensitive menuitem (not (eq? i 3)))
		   (if (> depth 1)
		       (set-submenu menuitem 
				    (create-menu (- depth 1) tearoff)))
		   (if (< i 4)
		       (loop (+ i 1) (get-group menuitem)))))
	       menu))))
  
  (let ((window     (make <gtk-window> 
		      :type 'toplevel :title "menus" :border-width 0))
	(accelgroup (make <gtk-accel-group>))
	(box1       (make <gtk-vbox> :homogeneous #f :spacing 0))
	(menubar    (make <gtk-menu-bar>))
	(box2       (make <gtk-vbox> 
		      :homogeneous #f :spacing 10 :border-width 10))
	(button     (make <gtk-button> :label "close")))
    (connect window 'delete-event (lambda (w e) (gtk-true)))

    (add-accel-group window accelgroup)

    (add window box1)

    (pack-start box1 menubar #f #t 0)

    (for-each 
     (lambda (m)
       (let ((menu       (create-menu (cdr m) #t))
	     (menuitem   (gtk-menu-item-new-with-label (car m))))
	 (set-submenu menuitem menu)
	 (set-right-justified menuitem (string=? (car m) "bar"))
	 (append menubar menuitem)))
     '(("test\nline2" . 2) 
       ("foo" . 3) 
       ("bar" . 4)))

    (pack-start box1 box2 #f #t 0)

    (connect button 'clicked (lambda (w)
			       (destroy window)))
    (pack-start box2 button #t #t 0)
;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;    (grab-default button)

    (show-all window)))


(define name "Menus")
(define description
  (string-append
   "There are several widgets involved in displaying menus. The"
   "GtkMenuBar widget is a horizontal menu bar, which normally appears"
   "at the top of an application. The GtkMenu widget is the actual menu"
   "that pops up. Both GtkMenuBar and GtkMenu are subclasses of"
   "GtkMenuShell; a GtkMenuShell contains menu items"
   "(GtkMenuItem). Each menu item contains text and/or images and can"
   "be selected by the user."
   "\n"
   "There are several kinds of menu item, including plain GtkMenuItem,"
   "GtkCheckMenuItem which can be checked/unchecked, GtkRadioMenuItem"
   "which is a check menu item that's in a mutually exclusive group,"
   "GtkSeparatorMenuItem which is a separator bar, GtkTearoffMenuItem"
   "which allows a GtkMenu to be torn off, and GtkImageMenuItem which"
   "can place a GtkImage or other widget next to the menu text."
   "\n"
   "A GtkMenuItem can have a submenu, which is simply a GtkMenu to pop"
   "up when the menu item is selected. Typically, all menu items in a menu bar"
   "have submenus."
   "\n"
   "GtkUIManager provides a higher-level interface for creating menu bars"
   "and menus; while you can construct menus manually, most people don't"
   "do that. There's a separate demo for GtkUIManager."))
