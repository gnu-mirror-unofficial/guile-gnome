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
;;An abstract tree model supporting filtered lists. Uses a scheme
;;predicate to determine if a member of a list is a row or not. Needs to
;;be subclassed to implement get-n-columns, get-column-type, and
;;get-value.
;;
;;; Code:

(define-module (gnome contrib filtered-list)
  #:use-module (gnome gtk)
  #:use-module (scheme documentation)
  #:export (<filtered-list-model>)
  #:re-export (set-filter)) ;; <gtk-file-chooser> has a set-filter
			    ;; method too

(define-class-with-docs <filtered-list-model> (<guile-gtk-tree-model>)
  "An abstract tree model supporting filtered lists. Uses a scheme
predicate to determine if a member of a list is a row or not. Needs to
be subclassed to implement get-n-columns, get-column-type, and
get-value."
  list
  filter
  filtered-len
  filtered-list)

(define-method (initialize (obj <filtered-list-model>) initargs)
  (next-method)
  (slot-set! obj 'list '())
  (slot-set! obj 'filter #f)
  (slot-set! obj 'filtered-len 0)
  (slot-set! obj 'filtered-list '()))

(define-method (on-get-flags (obj <filtered-list-model>))
  #f)

(define-method (on-get-iter (obj <filtered-list-model>) path)
  (if (and (eq? (length path) 1) (< (car path) (slot-ref obj 'filtered-len)))
      (list-ref (slot-ref obj 'filtered-list) (car path))
      #f))

(define-method (on-get-path (obj <filtered-list-model>) iter)
  (list (list-index (slot-ref obj 'filtered-list) iter)))

(define-method (on-iter-next (obj <filtered-list-model>) iter)
  (let loop ((l (slot-ref obj 'filtered-list)))
    (cond
     ((null? l)
      (error "Unknown iter" iter))
     ((eq? (car l) iter)
      (if (null? (cdr l))
          #f
          (cadr l)))
     (else
      (loop (cdr l))))))

(define-method (on-iter-children (obj <filtered-list-model>) parent)
  (if (not parent)
      ((lambda (l)
         (if (null? l)
             #f
             (car l)))
       (slot-ref obj 'filtered-list))
      #f))

(define-method (on-iter-has-child (obj <filtered-list-model>) iter)
  #f)

(define-method (on-iter-n-children (obj <filtered-list-model>) iter)
  (if iter
      #f
      (slot-ref obj 'filtered-len)))

(define-method (on-iter-nth-child (obj <filtered-list-model>) parent n)
  (if ((lambda (len)
         (if len
             (< n len)
             #f))
       (on-iter-n-children obj parent))
      (list-ref (slot-ref obj 'filtered-list) n)))

(define-method (on-iter-parent (obj <filtered-list-model>) iter)
  #f)

;; fixme: make me shorter by using higher-level operators
(define-method (set-filter (obj <filtered-list-model>) filter)
  (let ((old-filtered (slot-ref obj 'filtered-list))
        (new-filtered (if filter
                          (let loop ((list (slot-ref obj 'list))
                                     (filtered '()))
                            (if (null? list)
                                (reverse filtered)
                                (loop (cdr list)
                                      (if (filter (car list))
                                          (cons (car list) filtered)
                                          filtered))))
                          (slot-ref obj 'list))))
    (slot-set! obj 'filter filter)
    (slot-set! obj 'filtered-len (length new-filtered))
    (slot-set! obj 'filtered-list new-filtered)
    
    ;; Now to signal the changes
    (let loop ((l (slot-ref obj 'list))
               (old old-filtered)
               (new new-filtered)
               (i 0))
      (cond
       ((null? l)
        *unspecified*)
       ((and (not (null? old)) (eq? (car l) (car old)))
        ;; If the row was there before
        (if (and (not (null? new)) (eq? (car l) (car new)))
            ;; If it's still there, just keep going
            (loop (cdr l) (cdr old) (cdr new) (1+ i))
            (begin
              ;; But if not, show it was deleted
              (row-deleted obj (list i))
              (loop (cdr l) (cdr old) new i))))
       ((and (not (null? new)) (eq? (car l) (car new)))
        ;; We already know that it wasn't there before
        (row-inserted obj (list i) (get-iter obj (list i)))
        (loop (cdr l) old (cdr new) (1+ i)))
       (else
        ;; It wasn't there before, and it's not there now
        (loop (cdr l) old new i))))))
