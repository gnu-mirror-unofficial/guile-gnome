;; guile-lib
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
;;Renders texinfo into a GtkTextBuffer; requires guile-lib to work.
;;        
;;; Code:

(define-module (gnome gtk texinfo-buffer)
  :use-module (sxml texinfo)
  :use-module (sxml transform)
  :use-module (scheme documentation)
  :use-module (gnome gtk)
  :use-module (gnome gtk gdk-event)
  :use-module (gnome gobject)
  :use-module (gnome gw pango)
  :use-module (gnome gw libgnome)
  :export (stexi->gtk-text-buffer
           stexi-buffer-xref-activated-hook))

;; The two arguments are the node name and the manual name or #f
(define-with-docs stexi-buffer-xref-activated-hook
  "A hook run when the user activates a cross-reference. The two
arguments to the functions are the name of the node and the name of the
manual."
  (make-hook 2))

(define tag-table (make <gtk-text-tag-table>))
(define tag-prop-alist
  ;; List the non-inline styles first so inlines get priority
  '((center . (:justification center))
    (example . (:font "Monospace" :wrap-mode none
                :pixels-below-lines 0 :pixels-above-lines 0 :left-margin 20))
    (smallexample . (:font "Monospace" :wrap-mode none :scale 0.9
                     :pixels-below-lines 0 :pixels-above-lines 0 :left-margin 20))
    (cartouche . (:pixels-below-lines 6 :pixels-above-lines 6 :pixels-inside-wrap 0
                  :left-margin 25 :right-margin 25 :background "grey"
                  :justification center))
    (title . (:pixels-above-lines 6 :pixels-below-lines 12 :scale 1.4399999999 :weight 700))
    (subtitle . (:pixels-above-lines 0 :pixels-below-lines 12 :scale 1.2 :weight 700))
    (author . (:pixels-above-lines 0 :pixels-below-lines 12 :scale 1.2 :weight 700))
    ;; Assuming chapters and sections fall at the beginning of a buffer.
    ;; Not too good of an assumption. Better would be to detect if we're
    ;; at the start of a buffer, then adjust the tag accordingly.
    (chapter . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (majorheading . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (chapheading . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (section . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (heading . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (appendix . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (appendixsec . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (unnumbered . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (unnumberedsec . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (subsection . (:pixels-above-lines 12 :pixels-below-lines 6 :scale 1.2 :weight 700))
    (subheading . (:pixels-above-lines 12 :pixels-below-lines 6 :scale 1.2 :weight 700))
    (subsubsection . (:pixels-above-lines 6 :pixels-below-lines 6 :weight 700))
    (subsubheading . (:pixels-above-lines 6 :pixels-below-lines 6 :weight 700))
    (appendixsubsec . (:pixels-above-lines 12 :pixels-below-lines 6 :scale 1.2 :weight 700))
    (appendixsubsubsec . (:pixels-above-lines 6 :pixels-below-lines 6 :weight 700))
    (unnumberedsubsec . (:pixels-above-lines 12 :pixels-below-lines 6 :scale 1.2 :weight 700))
    (unnumberedsubsubsec . (:pixels-above-lines 6 :pixels-below-lines 6 :weight 700))
    (para . (:pixels-below-lines 6))
    (table . (:left-margin 50 :right-margin 25))
    (ftable . (:left-margin 50 :right-margin 25))
    (vtable . (:left-margin 50 :right-margin 25))
    (def-body . (:right-margin 50)) ;; left is handled by the *nest*
    (quotation . (:left-margin 50 :right-margin 50 :scale 0.9))

    (bold . (:weight 700))
    (item . (:left-margin 25 :right-margin 25 :pixels-below-lines 0))
    (itemx . (:left-margin 25 :right-margin 25 :pixels-below-lines 0))
    (sample . (:font "Monospace"))
    (samp . (:font "Monospace"))
    (code . (:font "Monospace"))
    (kbd . (:font "Monospace" :style oblique))
    (key . (:font "Monospace" :variant small-caps))
    (var . (:font "Monospace" :style italic))
    (env . (:font "Monospace"))
    (file . (:font "Monospace"))
    (command . (:font "Monospace"))
    (option . (:font "Monospace"))
    (dfn . (:style italic :weight 700))
    (cite . (:style italic))
    (acro . (:scale 0.8333333333))
    (url . (:font "Monospace"))
    (email . (:font "Monospace"))
    (emph . (:style italic))
    (strong . (:weight 700))
    (sc . (:variant small-caps))
    (small . (:scale 0.5 :pixels-below-lines 0)) ;; only used for weird newlines in examples
    ))
(for-each ;; Make the tags now so the priorities are correct
 (lambda (pair)
   (let ((tag (apply make <gtk-text-tag> :name (symbol->string (car pair)) (cdr pair))))
     (add tag-table tag)))
 tag-prop-alist)

(let ((tabs (pango-tab-array-new 1 #t)))
  (pango-tab-array-set-tab tabs 0 'left 325)
  (add tag-table (make <gtk-text-tag>
                   :name "def-header" :tabs tabs :pixels-below-lines 0)))

(define (get-tag name)
  (lookup tag-table (symbol->string name)))

(define (arg-ref name args)
  (and=> (assq name (cdr args)) cdr))
(define (arg-req name args)
  (or (arg-ref name args)
      (error "Required argument missing" name args)))
(define (arg-req* names args)
  (or (or-map (lambda (name) (arg-ref name args)) names)
      (error "Required argument missing" names args)))

;; We can't have after-paragraph space, because there are newlines in
;; examples. Hack around it by putting an extra newline at the end.
(define (example tag . body)
  `(example ,@body #\newline (small #\newline)))
(define (smallexample tag . body)
  `(smallexample ,@body #\newline (small #\newline)))

(define-class <stext-ref-tag> (<gtk-text-tag>)
  (node :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                      :flags (read write)))
  (manual :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                        :flags (read write))))

(define-method (initialize (obj <stext-ref-tag>) initargs)
  (next-method)
  (set obj 'foreground "blue")
  (set obj 'underline 'single)
  (connect obj 'event
           (lambda (tag object event iter)
             (if (eq? (gdk-event:type event) 'button-press)
                 (run-hook stext-buffer-xref-activated-hook
                           (slot-ref obj 'node-name)
                           (slot-ref obj 'manual)))
             #f)))

(define (ref tag args)
  (list (let ((tag (make <stext-ref-tag>
                     :node (car (arg-req 'node args))
                     :manual (and=> (arg-ref 'manual args) car))))
          (add tag-table tag)
          tag)
        (case type
          ((xref) "See ") ((pxref) "see ") (else ""))
        (car (arg-req* '(node section) args))))

(define-class <stext-uref-tag> (<gtk-text-tag>)
  (uri :param-spec `(,<gparam-string> :flags (read write))))

(define-method (initialize (obj <stext-uref-tag>) initargs)
  (next-method)
  (set obj 'foreground "blue")
  (set obj 'underline 'single)
  ;; Unfortunately, we can't make the cursor go to a hand when the mouse
  ;; is over a tag, because the tag has no window. The proper way to do
  ;; this is to put a GtkLabel in the text flow. The future!
  (connect obj 'event
           (lambda (tag object event iter)
             (if (eq? (gdk-event:type event) 'button-press)
                 (begin
                   (format #t "\nshowing ~A in new window\n" (slot-ref obj 'uri))
                   (gnome-url-show (slot-ref obj 'uri) #f)))
             #f)))

(define (uref tag args)
  (list (let ((tag (make <stext-uref-tag>
                     :uri (car (arg-req 'url args)))))
          (add tag-table tag)
          tag)
        (car (arg-req* '(title uri) args))))

(define (node tag args)
  `(*mark* ,(string-append "node-" (car (arg-req 'name args)))))

(define (nest . args)
  (list '*nest* args))

(define (def tag args . body)
  (define (code x) (and x (cons 'code x)))
  (define (var x) (and x (cons 'var x)))
  (define (b x) (and x (cons 'bold x)))
  (define (list/spaces . elts)
    (let lp ((in elts) (out '()))
      (cond ((null? in) (reverse! out))
            ((null? (car in)) (lp (cdr in) out))
            (else (lp (cdr in)
                      (cons (car in)
                            (if (null? out) out (cons " " out))))))))
  (define (header)
    (list/spaces (code (arg-ref 'data-type args))
                 (b (list (code (arg-ref 'class args)))) ;; is this right?
                 (b (list (code (arg-ref 'name args))))
                 (if (memq tag '(deftypeop deftypefn deftypefun))
                     (code (arg-ref 'arguments args))
                     (var (list (code (arg-ref 'arguments args)))))))

  (let* ((category (case tag
                     ((defun) "Function")
                     ((defspec) "Special Form")
                     ((defvar) "Variable")
                     (else (car (arg-req 'category args))))))
    `((def-header ,@(header) #\tab ,category #\newline)
      (*nest* (def-body ,@body)))))


(define ignore-list
  '(page setfilename setchapternewpage iftex ifhtml ifplaintext ifxml sp vskip
    menu ignore syncodeindex))

(define (default-handler tag . body)
  (cond
   ((assq tag tag-prop-alist)
    `(,tag
      ,@body
      ,(if (memq (cadr (assq tag texi-command-specs))
                 '(EOL-TEXT ENVIRON PARAGRAPH ITEM ENTRY))
           #\newline
           #f)))
   ((memq tag ignore-list)
    #f)
   (else
    (warn "No handler for" tag)
    body)))

;; strategy: first do pre-post-order transformation from stexi to a tree
;; headed with tags indicating which tag should be applied to the text.
;; then display the document.
(define rules
  `((% *preorder*        . ,(lambda args args))
    (example             . ,example)
    (lisp                . ,example)
    (verbatim            . ,example)
    (smallexample        . ,smallexample)
    (smalllisp           . ,smallexample)
    (xref                . ,ref)
    (ref                 . ,ref)
    (pxref               . ,ref)
    (uref                . ,uref)
    (node                . ,node)
    (anchor              . ,node)
    (table               . ,nest)
    (enumerate           . ,nest)
    (itemize             . ,nest)
    (copyright           . ,(lambda args (string #\302 #\251)))
    (results             . ,(lambda args (string #\342 #\207 #\222)))
    (deftp               . ,def)
    (defcv               . ,def)
    (defivar             . ,def)
    (deftypeivar         . ,def)
    (defop               . ,def)
    (deftypeop           . ,def)
    (defmethod           . ,def)
    (deftypemethod       . ,def)
    (defopt              . ,def)
    (defvr               . ,def)
    (defvar              . ,def)
    (deftypevr           . ,def)
    (deftypevar          . ,def)
    (deffn               . ,def)
    (deftypefn           . ,def)
    (defmac              . ,def)
    (defspec             . ,def)
    (defun               . ,def)
    (deftypefun          . ,def)
    (*text*              . ,(lambda (tag x) x))
    (*default*           . ,default-handler)))

(define (with-tag tag buffer proc . args)
  (let ((left (create-mark buffer #f (get-end-iter buffer) #t)))
    (apply proc args)
    (apply-tag buffer tag
               (get-iter-at-mark buffer left) (get-end-iter buffer))
    (delete-mark buffer left)))

(define *indent* (make-fluid))
(fluid-set! *indent* 0)

(define (make-nesting-tag)
  ((lambda (x) (add tag-table x) x)
   (make <gtk-text-tag> :left-margin (fluid-ref *indent*))))

(define (with-nesting buffer proc . args)
  (with-fluids ((*indent* (+ (fluid-ref *indent*) 25)))
    (apply with-tag (make-nesting-tag) buffer proc args)))

(define (fill-buffer buffer tree)
  (define (handle-body l)
    (for-each (lambda (x) (fill-buffer buffer x)) l))
  (cond
   ((string? tree)
    (insert buffer (get-end-iter buffer) tree))
   ((char? tree)
    (insert buffer (get-end-iter buffer) (string tree)))
   ((or (not tree) (null? tree))) ;; nothing
   (else
    (cond
     ((not (pair? tree))
      (error "invalid stexi tree"))
     ((symbol? (car tree))
      (case (car tree)
        ((*nest*)
         (with-nesting buffer handle-body (cdr tree)))
        ((*mark*)
         (create-mark buffer (cadr tree) (get-end-iter buffer) #t))
        ((%) #f) ;; do nothing
        (else
         (with-tag (or (get-tag (car tree)) (error "yikes!" tree))
                   buffer handle-body (cdr tree)))))
     ((is-a? (car tree) <gtk-text-tag>)
      (with-tag (car tree) buffer handle-body (cdr tree)))
     (else
      ;; maybe the tree transform made a nontagged list
      (handle-body tree))))))

(define-public (test stexi)
  (pre-post-order stexi rules))

(define (stexi->gtk-text-buffer stexi)
  (let ((buffer (make <gtk-text-buffer> :tag-table tag-table)))
    (fill-buffer buffer (pre-post-order stexi rules))
    buffer))
