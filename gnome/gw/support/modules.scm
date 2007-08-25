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
;;This module implements some procedures useful to modules that use
;;g-wrapped libraries.
;;
;;; Code:

(define-module (gnome gw support modules)
  #:export-syntax (re-export-modules)
  #:export (export-all-lazy!))

(define-macro (re-export-modules . args)
  (if (not (null? args))
      (begin
        (or (list? (car args))
            (error "Invalid module specification" (car args)))
        `(begin
           (module-use! (module-public-interface (current-module))
                        (resolve-interface ',(car args)))
           (re-export-modules ,@(cdr args))))))
(set-object-property! re-export-modules 'documentation
  "Re-export the public interface of a module; used like
@code{use-modules}.")

(define (export-all-lazy! symbols)
  (define (symbol-in? s exp)
    (if (pair? exp)
        (let lp ((exp exp))
          (if (null? exp)
              #f
              (or (symbol-in? s (car exp))
                  (lp (cdr exp)))))
        (eq? s exp)))

  (let ((mod (current-module)))
    (cond
     ((and=> (procedure-source module-make-local-var!)
             (lambda (exp) (symbol-in? 'module-variable exp)))
      ;; We have a broken module-make-local-var!; allowing lazy bindings
      ;; by making the public interface use the module will make things
      ;; really really slow. Settle on merely slow, forcing creation of
      ;; all classes
      (module-export! mod symbols))
     (else
      ;; We have a sensible module-make-local-var!; export the
      ;; already-bound variables, and install a module binder in the
      ;; interface to lazily bind the rest.
      (let ((obarray (module-obarray mod)))
        (module-export! mod
                        (filter
                         (lambda (s)
                           (hashq-ref obarray s))
                         symbols)))
      (set-module-binder!
       (module-public-interface mod)
       (lambda (interface sym define?)
         (let ((var (module-local-variable mod sym)))
           (if var (module-add! interface sym var))
           var)))))))
