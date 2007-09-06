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

  #:use-module (texinfo)
  #:use-module (texinfo docbook)
  #:use-module (texinfo serialize)
  #:use-module (match-bind)

  #:use-module (g-wrap)

  #:use-module (gnome gobject utils)

  #:export (docbook->sdocbook
            gtk-doc-sdocbook-title
            gtk-doc-sdocbook-subtitle
            gtk-doc-sdocbook->description-fragment
            gtk-doc-sdocbook->def-list
            gtk-doc-sdocbook->def-list/g-wrap
            gtk-doc->texi-stubs
            gtk-doc->texi-defuns))

(define (attr-ref attrs name . default)
  (or (and=> (assq name (cdr attrs)) cadr)
      (if (pair? default)
          (car default)
          (error "missing attribute" name))))

;; Make SSAX understand &nbsp; and &percnt; -- nasty, but that's how it
;; is
(for-each
 (lambda (pair)
   (set! ssax:predefined-parsed-entities
         (assoc-set! ssax:predefined-parsed-entities
                     (car pair) (cdr pair))))
 '((nbsp . " ")
   (percnt . "%")
   (oacute . "ó")
   (mdash . "&#x2014;")
   (ast . "&#x002A;")
   (num . "&#x0023;")))

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
                (cond ((string=? "void" arg) args)
                      ((string=? "void" arg) args)
                      (else (error "could not parse" arg)))))
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
    `("(" (var ,(gtype-name->scheme-name (cdr arg))) (tie)
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
          (append inputs '(" " (result) (tie)) outputs))))
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
     . ,(lambda (tag body . ignored)
          (or (null? ignored) (warn "ignored function tail" ignored))
          `(code ,(if (pair? body) body
                      (gtype-name->scheme-name (strip-final-parens body))))))
    (xref . ,(lambda (tag attrs)
               `(emph "(the missing figure, " ,(cadr (assq 'linkend (cdr attrs))))))
    (figure
     *preorder*
     . ,(lambda (tag attrs . body)
          `(para "(The missing figure, "  ,(cadr (assq 'id (cdr attrs))))))
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
                           ("FALSE" . (code "#f"))
                           ("TRUE" . (code "#t"))
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

(define (gtk-doc->texi-stubs files)
  (for-each
   (lambda (file)
     (let* ((sdocbook (docbook->sdocbook file))
            (basename (basename file))
            (docs `(*fragment*
                    (node (% (name ,@(gtk-doc-sdocbook-title sdocbook))))
                    (chapter ,@(gtk-doc-sdocbook-title sdocbook))
                    (para ,@(gtk-doc-sdocbook-subtitle sdocbook))
                    (section "Overview")
                    ,@(cdr (gtk-doc-sdocbook->description-fragment sdocbook))
                    (section "Usage")
                    (include ,(string-append "defuns-" basename ".texi")))))
       (with-output-to-file (string-append "section-" basename ".texi")
         (lambda ()
           (display (stexi->texi docs))))))
   files))

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

(define (input-arg-names func)
  (map name (input-arguments func)))
(define (input-arg-type-names func)
  (map name (map type (map typespec (input-arguments func)))))
(define (output-arg-names func)
  (map name (output-arguments func)))
(define (output-arg-type-names func)
  (map name (map type (map typespec (output-arguments func)))))
(define (return-type-name func)
  (name (return-type func)))

(define (make-function-hash wrapset)
  (let ((ret (make-hash-table)))
    (fold-functions
     (lambda (f nil)
       (let ((in (input-arguments f))
             (out (output-arguments f)))
         (hashq-set! ret (name f) f)))
     #f
     wrapset)
    ret))

(define (parse-func-name elt)
  (string->symbol
   (gtype-name->scheme-name
    (let ((flat (string-value elt)))
      (match-bind "^(.*?) ([^ ]+) +\\((.*)\\);"
                  flat (_ ret-type name args)
                  name
                  (error "could not parse" flat))))))

(define (function-stexi-arguments f)
  (define (arg-texinfo name type)
    `(" (" ,(symbol->string name) (tie)
      (code ,(symbol->string type)) ")"))
  (let ((inputs (append-map arg-texinfo (input-arg-names f)
                            (input-arg-type-names f)))
        (outputs (apply append-map arg-texinfo
                        (if (eq? (return-type-name f) 'void)
                            (list (output-arg-names f)
                                  (output-arg-type-names f))
                            (list (cons 'ret
                                        (output-arg-names f))
                                  (cons (return-type-name f)
                                        (output-arg-type-names f)))))))
    (if (null? outputs)
        inputs
        (append inputs '(" " (result) (tie)) outputs))))

(define (make-defs/g-wrap elt funcs body process-def)
  (or
   (and=>
    (hashq-ref funcs (parse-func-name elt))
    (lambda (f)
      (let ((deffn (process-def
                    (name f)
                    `(deffn (% (name ,(symbol->string (name f)))
                               (category "Function")
                               (arguments
                                ,@(function-stexi-arguments f)))
                       ,@(map
                          (lambda (fragment)
                            (pre-post-order
                             fragment
                             *gtk-doc-sdocbook->stexi-def-rules*))
                          (filter-empty-elements
                           (replace-titles
                            (sdocbook-flatten
                             (cons '*fragment* body))))))))
            (generic (generic-name f)))
        (if generic
            `((,(car deffn)
               ,(cadr deffn)
               (deffnx (% (name ,(symbol->string generic))
                          (category "Method")))
               ,@(cddr deffn)))
            (list deffn)))))
   '()))

(define (gtk-doc-sdocbook->def-list/g-wrap sdocbook process-def wrapset)
  "Extract documentation for all functions defined in the docbook
nodeset @var{sdocbook}.

This procedure is similar to @code{gtk-doc-sdocbook->def-list}, except
that instead using heuristics to guess at what the input and output
function arguments are, we can read them directly from the G-Wrap
wrapset @var{wrapset}. Only those procedures present in @var{wrapset}
are output."
  (let ((funcs (make-function-hash wrapset)))
    (reverse
     (sdocbook-fold-defuns
      (lambda (fragment seed)
        (append
         (let lp ((body fragment))
           (let ((elt (car body)))
             (if (and (pair? elt) (eq? (car elt) 'programlisting))
                 (reverse
                  (make-defs/g-wrap elt funcs (cdr body) process-def))
                 (lp (cdr body)))))
         seed))
      '()
      sdocbook))))

(define (def-name def)
  (string->symbol (cadr (assq 'name (cdadr def)))))

(define (gtk-doc->texi-defuns overrides module wrapset-name . files)
  (let* ((defs ((sxpath '(deffn))
                (call-with-input-file overrides texi-fragment->stexi)))
         (defs-alist (map cons (map def-name defs) defs)))
    (for-each
     (lambda (file)
       (let* ((sdocbook (docbook->sdocbook file))
              (basename (basename file))
              (wrapset (begin
                         (resolve-interface (call-with-input-string module read))
                         (get-wrapset 'guile (string->symbol wrapset-name))))
              (docs (stexi->texi
                     `(*fragment*
                       ,@(gtk-doc-sdocbook->def-list/g-wrap
                          sdocbook (lambda (name def) def) wrapset)))))
         (with-output-to-file (string-append "defuns-" basename ".texi")
           (lambda ()
             (display docs)))))
     files)))
