;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.

(define-module (demos panes)
  :use-module (gnome gtk))


(define (toggle-resize child)
  (let* ((paned     (get-parent child))
	 (is-child1 (eq? child (get-child1 paned)))
	 (resize    (make <gboolean> #:value #f))
	 (shrink    (make <gboolean> #:value #f)))
    ;; hack: child-get-property should be wrapped more nicely
    (child-get-property paned child "resize" resize) 
    (child-get-property paned child "shrink" shrink)

    (remove paned child)
    (if is-child1
	(pack1 paned child (not (gvalue->scm resize)) (gvalue->scm shrink))
	(pack2 paned child (not (gvalue->scm resize)) (gvalue->scm shrink)))))

(define (toggle-shrink child)
  (let* ((paned     (get-parent child))
	 (is-child1 (eq? child (get-child1 paned)))
	 (resize    (make <gboolean> #:value #f))
	 (shrink    (make <gboolean> #:value #f)))
    ;; hack: child-get-property should be wrapped more nicely
    (child-get-property paned child "resize" resize)
    (child-get-property paned child "shrink" shrink)

    (remove paned child)
    (if is-child1
	(pack1 paned child (gvalue->scm resize) (not (gvalue->scm shrink)))
	(pack2 paned child (gvalue->scm resize) (not (gvalue->scm shrink))))))

(define (create-pane-options paned frame-label label1 label2)
  (let ((frame  (make <gtk-frame> :label frame-label :border-width 4))
	(table  (make <gtk-table> :n-rows 3 :n-columns 2 :homogeneous #t)))

    (add frame table)

    (let ((label         (make <gtk-label> 
			   :label label1))
	  (check-button1 (make <gtk-check-button> 
			   :label "_Resize" :use-underline #t))
	  (check-button2 (make <gtk-check-button>
			   :label "_Shrink" :use-underline #t :active #t)))
      (attach-defaults table label 0 1 0 1)
      (attach-defaults table check-button1 0 1 1 2)
      (connect check-button1 'toggled (lambda (w) 
					(toggle-resize (get-child1 paned))))
      (attach-defaults table check-button2 0 1 2 3)
      (connect check-button2 'toggled (lambda (w) 
					(toggle-shrink (get-child1 paned)))))

    (let ((label         (make <gtk-label> :label label2))
	  (check-button1 (make <gtk-check-button>
			   :label "_Resize" :use-underline #t))
	  (check-button2 (make <gtk-check-button>
			   :label "_Shrink" :use-underline #t :active #t)))
      (attach-defaults table label 1 2 0 1)
      (attach-defaults table check-button1 1 2 1 2)
      (connect check-button1 'toggled (lambda (w) 
					(toggle-resize (get-child2 paned))))
      (attach-defaults table check-button2 1 2 2 3)
      (connect check-button2 'toggled (lambda (w) 
					(toggle-shrink (get-child2 paned)))))

    frame))

(define (main)
  (let* ((window (make <gtk-window> 
		   :type 'toplevel :title "Panes" :border-width 0))
	 (vbox   (make <gtk-vbox> :homogeneous #f :spacing 0))
	 (vpaned (make <gtk-vpaned> :border-width 5))
	 (hpaned (make <gtk-hpaned>))
	 (frame1 (make <gtk-frame>
		   :shadow-type 'in :width-request 60 :height-request 60))
	 (frame2 (make <gtk-frame> 
		   :shadow-type 'in :width-request 80 :height-request 60))
	 (frame3 (make <gtk-frame>
		   :shadow-type 'in :width-request 60 :height-request 80))
	 (button (make <gtk-button> :label "_Hi there" :use-underline #t))
	 (vpane-options (create-pane-options vpaned
					     "Vertical" "Top" "Bottom"))
	 (hpane-options (create-pane-options hpaned
					     "Horizontal" "Left" "Right")))
    (add window vbox)

    (pack-start vbox vpaned #t #t 0)

    (add1 vpaned hpaned)

    (add1 hpaned frame1)
    (add frame1 button)

    (add2 hpaned frame2)

    (add2 vpaned frame3)

    ;;
    (pack-start vbox hpane-options #f #f 0)
    (pack-start vbox vpane-options #f #f 0)

    (show-all window)))


(define name "Paned Widgets")
(define description
  (string-append
   "The GtkHPaned and GtkVPaned Widgets divide their content"
   "area into two panes with a divider in between that the"
   "user can adjust. A separate child is placed into each"
   "pane."
   "\n"
   "There are a number of options that can be set for each pane."
   "This test contains both a horizontal (HPaned) and a vertical"
   "(VPaned) widget, and allows you to adjust the options for"
   "each side of each widget."))
