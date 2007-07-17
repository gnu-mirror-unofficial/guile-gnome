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
  (call-with-input-file docbook-fragment
    (lambda (port) (ssax:xml->sxml port '()))))

(define (sdocbook-fold-defuns proc seed sdocbook-fragment)
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

(define *sdocbook->stexi-rules*
  `((@
     *preorder*
     . ,identity)
    (%
     *preorder*
     . ,identity)
    (para
     . ,(lambda (tag . body)
          `(para ,@(if (and (pair? body) (pair? (car body))
                            (eq? (caar body) '@))
                       (cdr body)
                       body))))
    (parameter
     . ,(lambda (tag body)
          `(var ,body)))
    (type
     . ,(lambda (tag body)
          `(code ,body)))
    (function
     . ,(lambda (tag body)
          `(code ,body)))
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
    (section . ,identity)
    (subsection . ,identity)
    (subsubsection . ,identity)
    (*text*
     . ,(lambda (tag text)
          text))))

(define-macro (make-state-parser states initial)
  `(lambda (port)
     (let lp ((state ',initial)
              (c (read-char port))
              (out '())
              (accum '()))
       (case state
         ((*eof*) (reverse out))
         ,@(map
            (lambda (desc)
              (let ((name (car desc))
                    (cont (cadr desc))
                    (cases (map
                            (lambda (kase)
                              (let ((condition (car kase))
                                    (new-state (cdr kase)))
                                (cond
                                 ((not (symbol? new-state))
                                  (error "invalid new-state in spec" new-state))
                                 ((number? condition)
                                  `((= (length accum) ,condition) ',new-state))
                                 ((list? condition)
                                  `((memv c ',condition) ',new-state))
                                 (else
                                  `(,condition ',new-state)))))
                            (cddr desc))))
                `((,name)
                  (let ((new-state (cond ((eof-object? c) '*eof*) ,@cases))
                        (cont ,cont))
                    (if (eq? state new-state)
                        (lp state (read-char port) out (cons c accum))
                        (lp new-state c
                            (if cont
                                (cons (cont (reverse accum)) out)
                                out)
                            '()))))))
            states)
         (else (error "invalid state" state (reverse out) (reverse accum)))))))

(define (s/// pat subst)
  (define (make-item-list subst)
    (call-with-input-string
     subst
     (make-state-parser
      ((string
        list->string
        ((#\\) . quote-head)
        ((#\$) . variable-head)
        (else . string))
       (quote-head
        #f
        (0 . quote-head)
        (else . quote))
       (quote
        list->string
        (0 . quote)
        ((#\\) . quote-head)
        ((#\$) . variable-head)
        (else . string))
       (variable-head
        #f
        (0 . variable-head)
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . variable)
        (else . error))
       (variable
        (lambda (l) (string->number (list->string l)))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . variable)
        ((#\\) . quote-head)
        ((#\$) . variable-head)
        (else . string)))
      string)))
  (let ((re (make-regexp pat))
        (items `(pre ,@(make-item-list subst) post)))
    (lambda (string)
      (let ((match (regexp-exec re string)))
        (if match
            (apply regexp-substitute #f match items)
            string)))))

(define strip-final-parens (s/// " *\\(\\)$" ""))

(define *gtk-doc-sdocbook->stexi-rules*
  `((parameter
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
                '(*fragment*)
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
                     ,@(cdr
                        (filter-empty
                         (fold-titles
                          (docbook-flatten
                           (cons '*fragment* (cdr body))))))
                     (para "This documentation was automatically generated."))
                  (lp (cdr body)))))))
    (deffn
      . ,identity)
    (link
     . ,(lambda (tag args body)
          body))
    ,@*gtk-doc-sdocbook->stexi-rules*))

(define (atom? x)
  (not (pair? x)))

(define (fold-values proc list . seeds)
  "A variant of @ref{present fold fold,,fold} that
allows multi-valued seeds. Note that the order of the arguments differs
from that of @code{fold}."
  (if (null? list)
      (apply values seeds)
      (call-with-values
          (lambda () (apply proc (car list) seeds))
        (lambda seeds
          (apply fold-values proc (cdr list) seeds)))))

(define (foldts*-values fdown fup fhere tree . seeds)
  "A variant of @ref{present fold foldts*,,foldts*} that allows multi-valued seeds.
Originally defined in Andy Wingo's 2007 paper, @emph{Applications of
fold to XML transformation}."
  (if (atom? tree)
      (apply fhere tree seeds)
      (call-with-values
          (lambda () (apply fdown tree seeds))
        (lambda (tree . kseeds)
          (call-with-values
              (lambda ()
                (apply fold-values
                       (lambda (tree . seeds)
                         (apply foldts*-values
                                fdown fup fhere tree seeds))
                       tree kseeds))
            (lambda kseeds
              (apply fup tree (append seeds kseeds))))))))

(define (inline-command? command)
  (not (memq command '(para programlisting informalexample
                       variablelist orderedlist refsect1
                       refsect2 refsect3 refsect4 title))))

(define (docbook-flatten sdocbook)
  (define (fhere str accum block cont)
    (values (cons str accum)
            block
            cont))
  (define (fdown node accum block cont)
    (let ((command (car node))
          (attrs (and (pair? (cdr node)) (pair? (cadr node))
                      (eq? (caadr node) '%)
                      (cadr node))))
      (values (if attrs (cddr node) (cdr node))
              '()
              '()
              (lambda (accum block)
                (values
                 `(,command ,@(if attrs (list attrs) '())
                            ,@(reverse accum))
                 block)))))
  (define (fup node paccum pblock pcont kaccum kblock kcont)
    (call-with-values (lambda () (kcont kaccum kblock))
      (lambda (ret block)
        (if (inline-command? (car ret))
            (values (cons ret paccum) (append kblock pblock) pcont)
            (values paccum (append kblock (cons ret pblock)) pcont)))))
  (call-with-values
      (lambda () (foldts*-values fdown fup fhere sdocbook '() '() #f))
    (lambda (accum block cont)
      (reverse block))))
    
(define (filter-empty sdocbook)
  (reverse
   (fold
    (lambda (x rest)
      (if (and (pair? x) (null? (cdr x)))
          rest
          (cons x rest)))
    '()
    sdocbook)))

(define (gtk-doc-sdocbook-title sdocbook)
  (let ((l ((sxpath '(refentry refnamediv refname)) sdocbook)))
    (if (null? l)
        #f
        (cdar l))))

(define (gtk-doc-sdocbook-subtitle sdocbook)
  (let ((l ((sxpath '(refentry refnamediv refpurpose)) sdocbook)))
    (if (null? l)
        #f
        (cdar l))))

;(define doc (docbook->sdocbook "/home/wingo/src/gnome2/cairo/doc/public/xml/cairo-version.xml"))
;(sdocbook-description doc)
;(gtk-doc-sdocbook-title doc)

(define (fold-titles sdocbook-fragment)
  (define sections '((refsect1 . chapter)
                     (refsect2 . section)
                     (refsect3 . subsection)
                     (refsect4 . subsubsection)))
  (let lp ((in sdocbook-fragment) (out '()))
    (cond
     ((null? in)
      (reverse out))
     ((and (pair? (car in)) (assq (caar in) sections))
      => (lambda (pair)
           (lp (cddr in) (cons `(,(cdr pair) ,@(cdadr in)) out))))
     (else
      (lp (cdr in) (cons (car in) out))))))

(define (sdocbook-description sdocbook)
  (filter-empty
   (fold-titles
    (docbook-flatten
     ;; cdr past title... ugh.
     (cons '*fragment*
           (cdr ((sxpath '(refentry
                           (refsect1 (@ role (equal? "desc")))
                           *))
                 sdocbook)))))))
   
(define (gtk-doc-sdocbook->description-fragment sdocbook)
  (cons '*fragment*
        (map
         (lambda (x)
           (pre-post-order x *gtk-doc-sdocbook->stexi-desc-rules*))
         (sdocbook-description sdocbook))))

;(gtk-doc-sdocbook->description-fragment doc)

(define (gtk-doc-sdocbook->def-list sdocbook)
  (sdocbook-fold-defuns
   (lambda (fragment seed)
     (cons
      (pre-post-order fragment *gtk-doc-sdocbook->stexi-def-rules*)
      seed))
   '()
   sdocbook))
