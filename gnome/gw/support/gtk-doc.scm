;; guile-gnome
;; Copyright (C) 2007 Free Software Foundation

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
;;Parsing a subset of the docbook emitted by gtk-doc into @code{stexi}.
;;
;;; Code:

(define-module (gnome gw support gtk-doc)
  #:use-module (sxml ssax)
  #:use-module (sxml xpath)
  #:use-module (sxml transform)
  #:use-module (ice-9 regex)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module (srfi srfi-13)

  #:use-module (gnome gobject utils)

  #:export (gtk-doc-file->stexi-list))

(define (attr-ref attrs name . default)
  (or (and=> (assq name (cdr attrs)) cadr)
      (if (pair? default)
          (car default)
          (error "missing attribute" name))))

;; Make SSAX understand &nbsp; -- nasty, but that's how it is
(set! ssax:predefined-parsed-entities
      (assoc-set! ssax:predefined-parsed-entities
                  'nbsp " "))

(define (docbook-fragment-fold proc seed docbook-fragment)
  (let lp ((in ((sxpath '(refentry
                          refsect1
                          (refsect2 (title
                                     anchor
                                     @ 
                                     role
                                     (equal? "function")))))
                (call-with-input-file docbook-fragment
                  (lambda (port) (ssax:xml->sxml port '())))))
           (seed seed))
    (if (null? in)
        seed
        (lp (cdr in) (proc (car in) seed)))))

(define-macro (match-bind regex str vars consequent . alternate)
  (or (string? regex) (error "regex needs to be a string so we can compile it"))
  (let ((re (gensym)) (match (gensym)))
    `(let ((,re ,(make-regexp regex)))
       ((lambda (,match)
          (if ,match
              (apply (lambda ,vars ,consequent)
                     (map (lambda (x) (match:substring ,match x))
                          (iota (match:count ,match))))
              ,@alternate))
        (regexp-exec ,re ,str)))))

(define (string-value xml)
  ;; inefficient, but hey!
  (cond ((symbol? xml) "")
        ((string? xml) xml)
        ((pair? xml)
         (if (eq? (car xml) '@)
             ""
             (apply string-append (map string-value xml))))))

(define (parse-deffn-args elt)
  (define (trim-const type)
    (match-bind "^(const )?(.*)$" type (_ const rest)
                rest))
  (define (parse-arg arg args)
    ;; ignores const.
    (match-bind "^(.*) (\\**)([^ ]+)$" arg (_ type stars name)
                (acons (string-append (trim-const
                                       (string-trim-both type))
                                      stars)
                       name
                       args)
                (if (string=? "void" arg)
                    args
                    (error "could not parse" arg))))
  (let ((flat (string-value elt)))
    (match-bind "^(.*?) ([^ ]+) +\\((.*)\\);" flat (_ ret-type name args)
                `((data-type
                   . ,(trim-const (string-trim-both ret-type)))
                  (name
                   . ,name)
                  (arguments
                   . ,(reverse
                       (fold parse-arg
                             '()
                             (map string-trim-both
                                  (string-split args #\,)))))))))

(define *immediate-types*
  '("double"))

(define (output-type? type)
  (let* ((base (substring type 0 (or (string-index type #\*)
                                    (string-length type))))
         (nstars (string-length (substring type (string-length base)))))
    (> nstars 
       (if (member base *immediate-types*) 0 1))))

(define (strip-star type)
  (let ((len (string-length type)))
    (if (eqv? (string-ref type (1- len)) #\*)
        (substring type 0 (1- len))
        type)))

(define (c-arguments->scheme-arguments args return-type)
  (define (arg-texinfo arg)
    `(" (" (var ,(gtype-name->scheme-name (cdr arg))) " "
      (code ,(gtype-name->class-name (strip-star (car arg)))) ")"))
  (define (finish input-args output-args)
    (let ((inputs (append-map arg-texinfo input-args))
          (outputs (append-map arg-texinfo
                               (if (string=? return-type "void")
                                   output-args
                                   (acons return-type "ret" output-args)))))
      (if (null? outputs)
          inputs
          (append inputs '(" " (result)) outputs))))
  (let lp ((args args) (out '()))
    (cond
     ((null? args)
      (finish (reverse out) '()))
     ((output-type? (caar args))
      (finish (reverse out) args))
     (else
      (lp (cdr args) (cons (car args) out))))))

(define (make-deffn-args elt)
  (let ((args (parse-deffn-args elt)))
    `(% (category "Function")
        (name ,(gtype-name->scheme-name (assq-ref args 'name)))
        (arguments ,@(c-arguments->scheme-arguments 
                      (assq-ref args 'arguments)
                      (assq-ref args 'data-type))))))
                      
(define (identity . args) args)
(define *docbook-fragment->stexi-stylesheet*
  `((refsect2
     *macro*
     . ,(lambda (tag . body)
          (let lp ((body body))
            (let ((elt (car body)))
              (if (and (pair? elt) (eq? (car elt) 'programlisting))
                  `(deffn ,(make-deffn-args elt)
                     ,@(cdr body)
                     (para "This documentation was automatically generated."))
                  (lp (cdr body)))))))
    (deffn
      . ,identity)
    (@
     *preorder*
     . ,identity)
    (%
     *preorder*
     . ,identity)
    (para
     . ,(lambda (tag . body)
          (if (and (pair? body) (pair? (car body)) (eq? (caar body) '@))
              `(para ,@(cdr body))
              `(para ,@body))))
    (link
     . ,(lambda (tag args body)
          body))
    (parameter
     . ,(lambda (tag body)
          `(var ,(gtype-name->scheme-name body))))
    (type
     . ,(lambda (tag body)
          `(code ,(gtype-name->class-name body))))
    (function
     . ,(lambda (tag body)
          `(code ,(gtype-name->scheme-name body))))
    (literal
     . ,(lambda (tag . body)
          `(samp ,@body)))
    (variablelist
     ((varlistentry
       . ,(lambda (tag term . body)
            `(entry (% (heading ,@(cdr term))) ,@body)))
      (listitem
       . ,(lambda (tag simpara)
            simpara)))
     . ,(lambda (tag attrs . body)
          `(table (% (formatter (var))) ,@body)))
    (orderedlist
     ((listitem
       . ,(lambda (tag . body)
            `(item ,@body))))
     . ,(lambda (tag . body)
          `(enumerate ,@body)))
    (emphasis
     . ,(lambda (tag . body)
          `(em ,@body)))
    (term
     . ,(lambda (tag param . rest)
          param))
    (simpara
     . ,(lambda (tag . body)
          `(para ,@body)))
    (informalexample
     . ,(lambda (tag programlisting)
          programlisting))
    (programlisting
     . ,(lambda (tag . contents)
          `(example ,@contents)))
    (firstterm
     . ,(lambda (tag . contents)
          `(dfn ,@contents)))
    (*text*
     . ,(lambda (tag text)
          (or (assoc-ref '(("NULL" . '(code "#f"))
                           ("Returns" . "ret")) text)
              text)))))

(define (docbook-fragment->stexi docbook)
  (pre-post-order docbook *docbook-fragment->stexi-stylesheet*))

(define (gtk-doc-file->stexi-list file)
  (docbook-fragment-fold
   (lambda (fragment seed)
     (cons (docbook-fragment->stexi fragment) seed))
   '()
   file))
