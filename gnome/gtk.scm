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
;;A GTK+ 2.x wrapper for Guile.
;;
;;; Code:

(define-module (gnome gtk))

(if (getenv "GUILE_GTK_DEBUG")
    (define progress display)
    (define progress identity))

(progress "(gnome gtk): [")

(progress "goops ")
(use-modules (oop goops))
(progress "gobject ")
(use-modules (gnome gobject) (gnome gobject generics))
(progress "glib ")
(use-modules (gnome glib))
(progress "atk ")
(use-modules (gnome gw atk))
(progress "pango ")
(use-modules (gnome gw pango))
(progress "gdk ")
(use-modules (gnome gw gdk))
(progress "gtk ")
(use-modules (gnome gw gtk))
(progress "support")

;; Support explicit object destruction.
(define-method (initialize (instance <gtk-object>) initargs)
  (next-method)
  (connect instance 'destroy
           (lambda args
             (%gtype-instance-destroy! instance))))

(define-public <guile-gtk-tree-model> <guile-gtk-generic-tree-model>)

;; Support tree models written in guile.
(define-method (on-get-flags (obj <guile-gtk-tree-model>))
  (make <gtk-tree-model-flags> #:value 0))
(define-method (on-get-n-columns (obj <guile-gtk-tree-model>))
  (error "This method needs to be overridden by a subclass."))
(define-method (on-get-column-type (obj <guile-gtk-tree-model>) index)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-get-iter (obj <guile-gtk-tree-model>) path)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-get-path (obj <guile-gtk-tree-model>) iter)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-get-value (obj <guile-gtk-tree-model>) iter index)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-iter-next (obj <guile-gtk-tree-model>) iter)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-iter-children (obj <guile-gtk-tree-model>) parent)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-iter-has-child (obj <guile-gtk-tree-model>) iter)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-iter-n-children (obj <guile-gtk-tree-model>) iter)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-iter-nth-child (obj <guile-gtk-tree-model>) parent n)
  (error "This method needs to be overridden by a subclass."))
(define-method (on-iter-parent (obj <guile-gtk-tree-model>) iter)
  (error "This method needs to be overridden by a subclass."))

(export on-get-flags on-get-n-columns on-get-column-type
        on-get-iter on-get-path on-get-value on-iter-next
        on-iter-children on-iter-has-child on-iter-n-children
        on-iter-nth-child on-iter-parent)

;; Miscellany.
(define-public (gtk-tree-or-list-store-set store iter . args)
  (or (even? (length args)) (scm-error 'gruntime-error "Invalid arguments"))
  (let loop ((args args))
    (if (eq? args '())
        *unspecified*
        (begin
          (set-value store iter (car args) (cadr args))
          (loop (cddr args))))))

(define-method (set (store <gtk-list-store>) (iter <gtk-tree-iter>) . args)
  (apply gtk-tree-or-list-store-set store iter args))

(define-method (set (store <gtk-tree-store>) (iter <gtk-tree-iter>) . args)
  (apply gtk-tree-or-list-store-set store iter args))

(define-public (gtk-text-buffer-create-tag buffer tag-name . properties)
  (let ((tag (make <gtk-text-tag> #:name tag-name)))
    (if (not (even? (length properties)))
        (scm-error 'gruntime-error "Invalid property list: ~A" properties))
    (add (get-tag-table buffer) tag)
    (let loop ((props properties))
      (if (null? props)
          tag
          (begin
            (set tag (car props) (cadr props))
            (loop (cddr props)))))))
(define-method (create-tag (buffer <gtk-text-buffer>) tag-name . properties)
  (apply gtk-text-buffer-create-tag buffer tag-name properties))
(export create-tag)

;; Make <gtk-message-dialog> have a specific metaclass so we can do
;; class methods.
(define-class <gtk-message-dialog-class> ((class-of <gtk-message-dialog>)))
(especify-metaclass! <gtk-message-dialog> <gtk-message-dialog-class>)
(define-method (make-instance (class <gtk-message-dialog-class>) . initargs)
  (let ((parent (get-keyword #:parent initargs #f))
        (flags (get-keyword #:flags initargs #f))
        (message-type (get-keyword #:message-type initargs 'error))
        (buttons (get-keyword #:buttons initargs 'close))
        (text (get-keyword #:text initargs "Error")))
    (%gtk-message-dialog-new parent flags message-type buttons text)))

(define-public (gtk-stock-id nick)
  (string-append "gtk-" (symbol->string nick)))

(use-modules (gnome gw support modules))

(re-export-modules (gnome gw gdk)
                   (gnome gw gtk))

(progress "]\n")
