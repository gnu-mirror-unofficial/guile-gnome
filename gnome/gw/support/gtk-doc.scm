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

  #:use-module (texinfo docbook)
  #:use-module (match-bind)

  #:use-module (gnome gobject utils)

  #:export (docbook->sdocbook
            gtk-doc-sdocbook-title
            gtk-doc-sdocbook-subtitle
            gtk-doc-sdocbook->description-fragment
            gtk-doc-sdocbook->def-list))

(define (attr-ref attrs name . default)
  (or (and=> (assq name (cdr attrs)) cadr)
      (if (pair? default)
          (car default)
          (error "missing attribute" name))))

;; Make SSAX understand &nbsp; -- nasty, but that's how it is
(set! ssax:predefined-parsed-entities
      (assoc-set! ssax:predefined-parsed-entities
                  'nbsp " "))

(define (docbook->sdocbook docbook-fragment)
  "Parse a docbook file @var{docbook-fragment} into SXML. Simply calls
SSAX's @code{xml->sxml}, but having made sure that @samp{&nbsp;} 
elements are interpreted correctly. Does not deal with XInclude."
  (call-with-input-file docbook-fragment
    (lambda (port) (ssax:xml->sxml port '()))))

(define (sdocbook-fold-defuns proc seed sdocbook-fragment)
  "Fold over the defuns in the gtk-doc-generated docbook fragment
@var{sdocbook-fragment}. Very dependent on the form of docbook that
gtk-doc emits."
  (let lp ((in ((sxpath '(refentry
                          refsect1
                          (refsect2 (title
                                     anchor
                                     @ 
                                     role
                                     (equal? "function")))))
                sdocbook-fragment))
           (seed seed))
    (if (null? in)
        seed
        (lp (cdr in) (proc (car in) seed)))))

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
    `("(" (var ,(gtype-name->scheme-name (cdr arg))) " "
      (code ,(symbol->string
              (gtype-name->class-name (strip-star (car arg))))) ")"))
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

(define strip-final-parens (s/// " *\\(\\)$" ""))

(define *gtk-doc-sdocbook->stexi-rules*
  `((variablelist
     ((varlistentry
       . ,(lambda (tag term . body)
            `(entry (% (heading ,@(cdr term))) ,@body)))
      (listitem
       . ,(lambda (tag simpara)
            simpara)))
     . ,(lambda (tag attrs . body)
          `(table (% (formatter (var))) ,@body)))
    (term
     . ,(lambda (tag param . rest)
          param))
    (parameter
     . ,(lambda (tag body)
          `(var ,(gtype-name->scheme-name body))))
    (type
     . ,(lambda (tag body)
          `(code ,(symbol->string (gtype-name->class-name body)))))
    (function
     . ,(lambda (tag body)
          `(code ,(gtype-name->scheme-name (strip-final-parens body)))))
    (indexterm
     *preorder*
     . ,(lambda (tag . body)
          (let ((entry (string-join
                        (apply append (map cdr body)) ", ")))
            (if (string-null? entry)
                #f
                `(cindex (% (entry ,entry)))))))
    (*text*
     . ,(lambda (tag text)
          (or (assoc-ref '(("NULL" . (code "#f"))
                           ("Returns" . "ret")) text)
              text)))
    ,@*sdocbook->stexi-rules*))

(define *gtk-doc-sdocbook->stexi-desc-rules*
  `((link
     . ,(lambda (tag args body)
          body))
    ,@*gtk-doc-sdocbook->stexi-rules*))

(define *gtk-doc-sdocbook->stexi-def-rules*
  `((refsect2
     *macro*
     . ,(lambda (tag . body)
          (let lp ((body body))
            (let ((elt (car body)))
              (if (and (pair? elt) (eq? (car elt) 'programlisting))
                  `(deffn ,(make-deffn-args elt)
                     ,@(filter-empty-elements
                        (replace-titles
                         (sdocbook-flatten
                          (cons '*fragment* (cdr body)))))
                     (para "This documentation was automatically generated."))
                  (lp (cdr body)))))))
    (deffn
      . ,identity)
    (link
     . ,(lambda (tag args body)
          body))
    ,@*gtk-doc-sdocbook->stexi-rules*))

(define (gtk-doc-sdocbook-title sdocbook)
  "Extract the title from a fragment of docbook, as produced by gtk-doc.
May return @code{#f} if the title is not found."
  (let ((l ((sxpath '(refentry refnamediv refname)) sdocbook)))
    (if (null? l)
        #f
        (cdar l))))

(define (gtk-doc-sdocbook-subtitle sdocbook)
  "Extract the subtitle from a fragment of docbook, as produced by gtk-doc.
May return @code{#f} if the subtitle is not found."
  (let ((l ((sxpath '(refentry refnamediv refpurpose)) sdocbook)))
    (if (null? l)
        #f
        (cdar l))))

(define (sdocbook-description sdocbook)
  (filter-empty-elements
   (replace-titles
    (sdocbook-flatten
     ;; cdr past title... ugh.
     (cons '*fragment*
           (cdr ((sxpath '(refentry
                           (refsect1 (@ role (equal? "desc")))
                           *))
                 sdocbook)))))))
   
(define (gtk-doc-sdocbook->description-fragment sdocbook)
  "Extract the \"description\" of a module from a fragment of docbook,
as produced by gtk-doc, translated into texinfo."
  (cons '*fragment*
        (map
         (lambda (x)
           (pre-post-order x *gtk-doc-sdocbook->stexi-desc-rules*))
         (sdocbook-description sdocbook))))

(define (gtk-doc-sdocbook->def-list sdocbook process-def)
  "Extract documentation for all functions defined in the docbook
nodeset @var{sdocbook}.

When a function is found and translated into texinfo, @var{process-def}
will be called with two arguments, the name of the procedure as a
symbol, and the documentation as a @code{deffn}. @var{process-def} may
return @code{#f} to indicate that the function should not be included in
the documentation; otherwise, the return value of @var{process-def} will
be used as the documentation.

This mechanism allows the caller of @code{gtk-doc-sdocbook->def-list} to
perform further processing on the documentation, including the
possiblity of replacing it completely with documenation from another
source, for example a file of hand-written documentation overrides."
  (reverse
   (sdocbook-fold-defuns
    (lambda (fragment seed)
      (let* ((parsed (pre-post-order
                      fragment *gtk-doc-sdocbook->stexi-def-rules*))
             (name (string->symbol (attr-ref (cadr parsed) 'name)))
             (def (process-def name parsed)))
        (if def
            (cons def seed)
            seed)))
    '()
    sdocbook)))
