;; guile-gnome
;; Copyright (C) 2003,2004,2009,2011 Andy Wingo <wingo at pobox dot com>

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
;; @c
;;
;; Support routines for automatically-generated scheme G-Wrap modules.
;;
;;; Code:

(define-module (gnome gw support modules)
  #:export-syntax (re-export-modules)
  #:export (export-all-lazy!))

(define (force-bindings module)
  (cond
   ((and (eq? (module-kind module) 'interface) (module-binder module))
    (force-bindings (resolve-module (module-name module))))
   ((module-variable module '%gw-latent-variables-hash)
    => (lambda (var)
         (for-each
          (lambda (k)
            (module-variable (module-public-interface module) k))
          ;; copy list of syms because the module binder mutates the hash
          (hash-map->list (lambda (k v) k) (variable-ref var)))))
   (else
    (for-each force-bindings (module-uses module)))))

(eval-when (expand load eval)
  (and=> (and (not (batch-mode?))
	      (module-variable (resolve-module '(ice-9 session))
			       'apropos-hook))
	 (lambda (v)
	   (add-hook! (variable-ref v)
		      (lambda (mod pat) (force-bindings mod))))))

(define-macro (re-export-modules . args)
  "Re-export the public interface of a module; used like
@code{use-modules}."
   (if (null? args)
       '(if #f #f)
       `(begin
          ,@(map (lambda (mod)
                   (or (list? mod)
                       (error "Invalid module specification" mod))
                   `(module-use! (module-public-interface (current-module))
                                 (resolve-interface ',mod)))
                 args))))

(define (export-all-lazy! symbols)
  "Export the @var{symbols} from the current module.

Most generic functions and classes that G-Wrap defines are bound lazily,
as needed in evaluation. This is done by placing module binder
procedures on the generated modules. However, if we export all symbols
by name, this will force the binding eagerly for all values, which is
slow.

This procedure exports all bindings named in @var{symbols} that are
already bound in the current module, and then installs a module binder
procedure on the public interface, which allows lazy binding to work."
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
         (and (memq sym symbols)
              (let ((var (module-local-variable mod sym)))
                (if var (module-add! interface sym var))
                var))))))))
