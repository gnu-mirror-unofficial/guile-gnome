;; guile-gnome
;; Copyright (C) 2003,2004,2009 Andy Wingo <wingo at pobox dot com>

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

(define-module (gnome gtk)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gobject generics)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gw support modules)
  #:export (<guile-gtk-tree-model>
            on-get-flags on-get-n-columns on-get-column-type
            on-get-iter on-get-path on-get-value on-iter-next
            on-iter-children on-iter-has-child on-iter-n-children
            on-iter-nth-child on-iter-parent

            gtk-tree-or-list-store-set
            gtk-text-buffer-create-tag create-tag
            gtk-stock-id))

(define-macro (time-debug . forms)
  `(begin ,@forms))

(eval-when (expand load eval)
  (time-debug (use-modules (gnome gw gdk)))
  (time-debug (use-modules (gnome gw gtk)))
  (re-export-modules (gnome gw gdk)
		     (gnome gw gtk)))

;; Support explicit object destruction.
(define-method (initialize (instance <gtk-object>) initargs)
  (next-method)
  (connect instance 'destroy
           (lambda args
             (gtype-instance-destroy! instance))))

(define <guile-gtk-tree-model> <guile-gtk-generic-tree-model>)

;; FIXME: doc me!
(define-generic-with-docs on-get-flags
  "")
(define-generic-with-docs on-get-n-columns
  "")
(define-generic-with-docs on-get-column-type
  "")
(define-generic-with-docs on-get-iter
  "")
(define-generic-with-docs on-get-path
  "")
(define-generic-with-docs on-get-value
  "")
(define-generic-with-docs on-iter-next
  "")
(define-generic-with-docs on-iter-children
  "")
(define-generic-with-docs on-iter-has-child
  "")
(define-generic-with-docs on-iter-n-children
  "")
(define-generic-with-docs on-iter-nth-child
  "")
(define-generic-with-docs on-iter-parent
  "")

;; Support tree models written in guile.
(define-method (on-get-flags (obj <guile-gtk-tree-model>))
  (make <gtk-tree-model-flags> #:value 0))

;; Miscellany.
(define (gtk-tree-or-list-store-set store iter . args)
  (or (even? (length args)) (scm-error 'gruntime-error #f "Invalid arguments" '() #f))
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

(define (gtk-text-buffer-create-tag buffer tag-name . properties)
  (let ((tag (make <gtk-text-tag> #:name tag-name)))
    (if (not (even? (length properties)))
	(scm-error 'gruntime-error #f "Invalid property list: ~A" properties #f))
    (add (get-tag-table buffer) tag)
    (let loop ((props properties))
      (if (null? props)
          tag
          (begin
            (set tag (car props) (cadr props))
            (loop (cddr props)))))))
(define-method (create-tag (buffer <gtk-text-buffer>) tag-name . properties)
  (apply gtk-text-buffer-create-tag buffer tag-name properties))

(eval-when (expand load eval)
  (export create-tag))

(define (gtk-stock-id nick)
  (string-append "gtk-" (symbol->string nick)))

