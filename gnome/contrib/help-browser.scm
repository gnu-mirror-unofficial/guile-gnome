;; guile-gnome
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;A help browser for guile-gnome applications. Designed to be used with
;;stexinfo documents.
;;
;;There can only be one help browser in an application.
;;
;;Example use:
;;
;;@example
;; (use-modules (gnome gtk) (gnome contrib help-browser))
;;
;; ;; optional
;; (set-default-help-document! @var{my-stexi-doc}) ;; optional
;;
;; ;; @var{document} can be a nodal tree or an stexi document
;; (add-help-root! @var{document})
;;
;; ;; shows default page
;; (show-help)
;;
;; ;; shows the node named @var{node}
;; (show-help @var{node})
;;
;; ;; shows the node named @var{node} in manual (root) @var{root}
;; (show-help @var{node} @var{root})
;;@end example
;;
;;; Code:

(define-module (gnome contrib help-browser)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome gtk)
  #:use-module (srfi srfi-13)
  #:use-module (texinfo)
  #:use-module (texinfo indexing)
  #:use-module (sxml simple)
  #:use-module (container nodal-tree)
  #:use-module (container delay-tree)
  #:use-module (gnome contrib texinfo-buffer)
  #:use-module (gnome contrib delay-tree-model)
  #:use-module (gnome contrib filtered-list)
  #:export (set-default-help-document! add-help-root! show-help
            populate-help-hook the-help-window))

(warn "(gnome contrib help-browser) is still in development. ")
(warn "It might eat your baby! In any case, don't rely on it yet.")

(pk "mmm, babies taste good")

;; Add a `buffer' column to a normal delay-tree-model
(define-class <help-tree> (<delay-tree-model>))
(define-method (on-get-n-columns (obj <help-tree>))
  3) ;; name, value, buffer
(define-method (on-get-column-type (obj <help-tree>) index)
  (case index
    ((2) gtype:gboxed-scm)
    (else (next-method))))
(define-method (on-get-value (obj <help-tree>) iter index)
  (case index
    ((2)
     (or (node-ref iter 'buffer)
         (let ((buf (stexi->gtk-text-buffer (force-ref iter 'value))))
           (node-set! iter 'buffer buf)
           buf)))
    (else
     (next-method))))

(define default-document
  '(texinfo
    (% (title "guile-gnome help browser"))
    (node (% (name "top")))
    (chapter "Help")
    (para
     "Welcome to guile-gnome's help system.")
    (para
     "The available sections can be browsed on the left.")
    (para
     "If you are looking for something specific, try the index.")
    (para
     "(This message can be customized. See the documentation for details.)")))

(define default-buffer #f)

;; exported
(define (set-default-help-document! document)
  "Doc me"
  (or (eq? (car document) 'texinfo)
      (error "The default help document must be stexinfo."))
  (set! default-buffer #f)
  (set! default-document document))

(define the-help-tree (make <help-tree>))

(define (add-help-root! document)
  (append-root! the-help-tree document))

;; the index
(define-class <help-index> (<filtered-list-model>))
(define-method (on-get-n-columns (obj <help-index>))
  3) ;; name, node name, tree-iter
(define-method (on-get-column-type (obj <help-index>) index)
  (case index
    ((0 1) gtype:gchararray)
    ((2) (gtype-from-name "GtkTreeIter"))
    (else (error "Invalid index" index))))
(define-method (on-get-value (obj <help-index>) iter index)
  (case index
    ((0 1 2)
     (list-ref iter index))
    (else
     (error "Invalid index" index))))
(define-method (set-filter (obj <help-index>) filter)
  (define (make-index)
    (let loop ((index '()) (iter (iter-nth-child the-help-tree #f 0)))
      (if (not iter)
          (sort! index (lambda rows (apply string-ci<=? (map car rows))))
          (let ((this-index (map
                             (lambda (x) (list (car x) (cdr x) iter))
                             (stexi-extract-index
                              (get-value the-help-tree iter 1) #f 'all)))
                (kids-index (let ((iter (iter-nth-child the-help-tree iter 0)))
                              (if iter (loop '() iter) '()))))
            (loop (append this-index kids-index index)
                  (iter-next the-help-tree iter))))))

  (if (null? (slot-ref obj 'list))
      ;; Don't actually do the indexing until the first filter is set,
      ;; that is, until the user has typed something in the search box
      (slot-set! obj 'list (make-index)))
  (next-method))

(define the-help-window #f)

;; the parts of the help window that we want to remember
(define *topics-treeview* #f)
(define *textbuffer* #f)
(define *textview* #f)
(define *current-node* #f)

(define (get-help-buffer iter)
  (cond
   (iter (get-value the-help-tree iter 2))
   (default-buffer default-buffer)
   (else
    (set! default-buffer (stexi->gtk-text-buffer default-document))
    default-buffer)))

(define (get-help-mark iter node-name)
  (get-mark (get-help-buffer iter) (string-append "node-" node-name)))

(define (make-help-window)
  (define (add-topics-page w notebook)
    (let* ((treemodel the-help-tree)
           (treeview (make <gtk-tree-view> #:model treemodel #:headers-visible #f))
           (scroll (make <gtk-scrolled-window>
                     #:hscrollbar-policy 'automatic #:vscrollbar-policy 'automatic
                     #:shadow-type 'in))
           (cellrenderer (make <gtk-cell-renderer-text>))
           (column (make <gtk-tree-view-column>))
           (selection (get-selection treeview)))
      (set! *topics-treeview* treeview)
      (append-page notebook scroll (make <gtk-label> #:label "Topics"))
      (add scroll treeview)
      (set-mode selection 'single)
      (pack-start column cellrenderer #t)
      (add-attribute column cellrenderer "text" 0)
      (append-column treeview column)
      (connect
       selection 'changed
       (lambda (selection)
         (call-with-values (lambda () (get-selected selection))
           (lambda (model iter)
             (set! *current-node* iter)
             (set-buffer *textview* (get-help-buffer iter))
             (set the-help-window 'title
                  (if iter
                      (get-value the-help-tree iter 0)
                      (sxml->string
                       (assq-ref (cdadr default-document) 'title))))))))
      (emit selection 'changed)))

  (define (add-index-page w notebook)
    ;; The "Index" page
    (let* ((vbox (make <gtk-vbox>))
           (entry (make <gtk-entry>))
           (list (make <gtk-tree-view>))
           (treemodel (make <help-index>))
           (scroll (make <gtk-scrolled-window>
                     #:hscrollbar-policy 'automatic #:vscrollbar-policy 'automatic
                     #:shadow-type 'in))
           (treeview (make <gtk-tree-view> #:model treemodel #:headers-visible #f))
           (cellrenderer (make <gtk-cell-renderer-text>))
           (column (make <gtk-tree-view-column>))
           (selection (get-selection treeview)))
      (append-page notebook vbox (make <gtk-label> #:label "Index"))
      (pack-start vbox entry #f #f 0)
      (pack-start vbox scroll #t #t 0)
      (add scroll treeview)
      (set-mode selection 'single)
      (pack-start column cellrenderer #t)
      (add-attribute column cellrenderer "text" 0)
      (append-column treeview column)
      (set-text entry "(indexing takes some time)")
      (select-region entry 0 -1)
      (connect
       entry 'changed
       (lambda (entry)
         (let ((text (get-text entry)))
           (set-filter treemodel
                       (if (equal? text "")
                           #f
                           (lambda (row)
                             (string-contains-ci (car row) text)))))))
      (connect
       selection 'changed
       (lambda (selection)
         (call-with-values (lambda () (get-selected selection))
           (lambda (model iter)
             (if iter
                 (select-node (get-value model iter 2)
                              (get-value model iter 1)))))))))

  (let* ((w (make <gtk-window>
              #:default-height 400
              #:default-width 700))
         (textbuffer (make <gtk-text-buffer>))
         (textview (make <gtk-text-view>
                     #:editable #f #:cursor-visible #f #:wrap-mode 'word
                     #:pixels-above-lines 2 #:pixels-below-lines 6
                     #:pixels-inside-wrap 1 ;; #:justification 'fill <- not supported yet
                     #:right-margin 10 #:left-margin 10))
         (pane (make <gtk-hpaned> #:position 200))
         (notebook (make <gtk-notebook> #:show-border #f))
         (text-scroll (make <gtk-scrolled-window>
                        #:hscrollbar-policy 'automatic #:vscrollbar-policy 'automatic
                        #:shadow-type 'in)))
    
    (add w pane)
    (pack1 pane notebook #f #t)
    (pack2 pane text-scroll #t #t)
    (add text-scroll textview)
    (set! the-help-window w)
    (set! *textbuffer* textbuffer)
    (set! *textview* textview)
    
    (connect w 'delete-event (lambda (w e) (hide w) #t))
    
    (add-topics-page w notebook)
    (add-index-page w notebook)
    
    (show-all w)
    (hide w)))

;; If manual-name is #f, look in the currently selected manual.
;; If node-name is #f, select the top of the manual.
(define (find-node node-name manual-name)
  (define (find-node-named top name)
    (or-map
     (lambda (node)
       (if (get-help-mark node name)
           node
           (find-node-named node name)))
     (iter-children the-help-tree top)))
  (let ((manual (if manual-name
                    (let loop ((manual (get-iter-first the-help-tree)))
                      (cond
                       ((not manual)
                        (error "Unknown manual:" manual-name))
                       ((string=? (get-value the-help-tree manual 0)
                                  manual-name)
                        manual)
                       (else
                        (loop (iter-next the-help-tree manual)))))
                    (if (not *current-node*)
                        (error
                         "The TOP node must specify a manual in its cross-references.")
                        (let find-top ((node *current-node*))
                          (let ((parent (iter-parent the-help-tree node)))
                            (if (not parent)
                                node
                                (find-top parent))))))))

    (if (and node-name (not (string=? node-name "top")))
        (or
         (find-node-named manual node-name)
         (error "No such node in manual:" manual-name node-name))
        manual)))

(define (select-node iter node-name)
  (let* ((treeview *topics-treeview*)
         (path (get-path the-help-tree iter)))
    (expand-to-path treeview path)
    (select-path (get-selection treeview) path)
    (scroll-to-cell treeview path #f #f 0 0))
    ;; now the buffer should be correct...
    ;; scroll in an idle loop to give the textview time to compute line heights
    (g-idle-add
     (lambda ()
       (scroll-to-mark *textview*
                       (if node-name
                           (get-help-mark iter node-name)
                           (create-mark (get-help-buffer iter)
                                        #f
                                        (get-start-iter (get-help-buffer iter))
                                        #t))
                       0.15 #f 0 0)
       #f)))

(define populate-help-hook (make-hook 0))

(define (ensure-help-window)
  (if (not the-help-window)
      (begin
        (run-hook populate-help-hook)
        (make-help-window)))
  the-help-window)

(define* (show-help #:optional (node-name #f) (manual-name #f))
  "Show the help window. If @var{manual-name} is @code{#f}, look in the
currently selected manual. If @var{node-name} is @code{#f}, select the
top of the manual."
  (let ((window (ensure-help-window)))
    (show window)
    (if (or node-name manual-name)
        (select-node (find-node node-name manual-name) node-name))
    (present window)))

(add-hook! stexi-buffer-xref-activated-hook show-help)
