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
;;A lazy tree model. The value of the rows, as well the rows' children,
;;are promises made by the @code{delay} operator.
;;
;;; Code:

(define-module (gnome contrib delay-tree-model)
  #:use-module (gnome gtk)
  #:use-module (container nodal-tree)
  #:use-module (container delay-tree)
  #:use-module (scheme documentation)
  #:export (<delay-tree-model> append-root!))

(define-class-with-docs <delay-tree-model> (<guile-gtk-tree-model>)
  "An interface that exports delay trees as GTK+ tree models. Suitable
for use with @code{<gtk-tree-view>}."
  (top-nodes #:init-value '()))

(define-method (on-get-n-columns (obj <delay-tree-model>))
  2) ;; name and value

(define-method (on-get-column-type (obj <delay-tree-model>) index)
  (case index
    ((0) gtype:gchararray)
    ((1) gtype:gboxed-scm)
    (else (error "Invalid index:" index))))

(define-method (on-get-iter (obj <delay-tree-model>) path)
  (let loop ((node #f) (path path))
    (if (null? path)
        node
        (let ((children (if node
                            (force-ref node 'children)
                            (slot-ref obj 'top-nodes))))
          (cond
           ((null? children) ;; can be the case for path == (0)
            #f)
           ((>= (car path) (length children))
            #f)              ;; nonexistent path, but no error
           (else
            (loop (list-ref children (car path)) (cdr path))))))))

(define-method (on-get-path (obj <delay-tree-model>) iter)
  (let loop ((node iter) (path '()))
    (let ((parent (node-ref node 'parent)))
      (if (not parent)
          (cons (list-index (slot-ref obj 'top-nodes) node) path)
          (loop parent
                (cons (list-index (node-ref parent 'children) node) path))))))

(define-method (on-get-value (obj <delay-tree-model>) iter index)
  (case index
    ((0)
     (force-ref iter 'name))
    ((1)
     (force-ref iter 'value))
    (else
     (error "Invalid index" index))))

(define-method (on-iter-next (obj <delay-tree-model>) iter)
  (let* ((parent (node-ref iter 'parent))
         (siblings (if parent
                       (node-ref parent 'children)
                       (slot-ref obj 'top-nodes)))
         (new-position (1+ (list-index siblings iter))))
    (and (< new-position (length siblings))
         (list-ref siblings new-position))))

(define-method (on-iter-children (obj <delay-tree-model>) parent)
  (let ((children (if parent
                      (force-ref parent 'children)
                      (slot-ref obj 'top-nodes))))
    (and (pair? children)
         (car children))))

(define-method (on-iter-has-child (obj <delay-tree-model>) iter)
  ;; would be nice to avoid forcing the children if there are none.
  (not (null? (force-ref iter 'children))))

(define-method (on-iter-n-children (obj <delay-tree-model>) iter)
  (length (if iter
              (force-ref iter 'children)
              (slot-ref obj 'top-nodes))))

(define-method (on-iter-nth-child (obj <delay-tree-model>) parent n)
  (let ((children (if parent
                      (force-ref parent 'children)
                      (slot-ref obj 'top-nodes))))
    (and (< n (length children))
         (list-ref children n))))

(define-method (on-iter-parent (obj <delay-tree-model>) iter)
  (node-ref iter 'parent))

;; To the on-* methods, the iter is just a scheme object. But outside
;; those methods it is a boxed type. So we need to call the Gtk+
;; notification functions (row-inserted, row-has-child-toggled) with the
;; boxed types, not with SCM values.
(define-method (append-root! (obj <delay-tree-model>) delay-tree)
  (define (emit-signals path)
    (row-inserted obj path (get-iter obj path)))

  (slot-set! obj 'top-nodes (append! (slot-ref obj 'top-nodes)
                                     (list delay-tree)))
  (let ((path (get-path obj (get-iter obj (on-get-path obj delay-tree)))))
    (emit-signals path)
    (get-iter obj path)))
