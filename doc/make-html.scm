#! /usr/bin/guile -s
!#

(read-set! keywords 'prefix)

;; Export the help tree as HTML.

(use-modules (srfi srfi-13)
             (srfi srfi-1)
             (ice-9 regex)
             (oop goops)
             (text structured html)
             (text structured named-tree-nodes)
             (text structured plain-text)
             (text structured texinfo))

(define-public (build-path . args)
  "Builds a path from elements in @var{args}, ignoring those equal to
\"\"."
  (string-join (filter (lambda (x) (not (equal? x ""))) args) "/"))

(if (not (eq? (length (program-arguments)) 3))
    (begin
      (format #t "Usage: make-html.scm TEXINFO-FILE DOCUMENT-PATH\n")
      (exit 1)))

(define texinfo-file (cadr (program-arguments)))
(define top-node (stext->named-tree-nodes (texinfo->stext texinfo-file) 1))
(define document-path (caddr (program-arguments)))

(if (not (and (file-exists? document-path)
              (file-is-directory? document-path)))
    (error (string-append "Error: " document-path " is not a valid directory")))

(format #t "Building documents in ~A...\n" document-path)

(define (get-depth node)
  (let loop ((node node) (depth -1))
    (if node
        (loop (slot-ref node 'parent) (1+ depth))
        depth)))

(define-public (list-join l infix)
  "Infixes @var{infix} into list @var{l}."
  (let loop ((ret '()) (l l))
    (cond
     ((null? l)
      (reverse ret))
     ((null? (cdr l))
      (loop (cons (car l) ret) (cdr l)))
     (else
      (loop (cons* infix (car l) ret) (cdr l))))))

(define (write-html-file path node)
  (let* ((children (slot-ref node 'children))
         (depth (get-depth node))
         (leaf-node? (not children))
         (header-path (string-append (apply build-path (make-list (+ depth 2) "..")) "/")))
    (define (post-processor head body)
      (define (link-tail node)
        (string-append
         (urlify (slot-ref node 'name))
         "/"))
      (define (make-navigation)
        (let* ((siblings (if (zero? depth)
                             (list node)
                             (slot-ref (slot-ref node 'parent) 'children)))
               (index (list-index (lambda (n) (eq? n node)) siblings)) ;; srfi-1
               (numsiblings (length siblings))
               (prev (if (zero? index)
                         #f
                         (list-ref siblings (1- index))))
               (next (if (eq? index (1- (length siblings)))
                         #f
                         (list-ref siblings (1+ index))))
               (next-following (if leaf-node?
                                   next
                                   (car children)))
               (up-path "../"))
          `(div :class "book-navigation reversed"
                ,(if prev
                     `(a :href ,(string-append  "../" (link-tail prev))
                         "&lt;")
                     "&lt;")
                " | "
                (a :href ,up-path
                   "^")
                " | "
                ,(if next
                     `(a :href ,(string-append "../" (link-tail next))
                         "&gt;")
                     "&gt;")
                " | "
                (a :href ,(if next-following
                              (if leaf-node?
                                  (string-append "../" (link-tail next-following))
                                  (link-tail next-following))
                              up-path)
                   ":&gt;"))))
      (define (make-subsections)
        (if children
            `((p :class "subsections"
                 ,@(list-join
                    (map
                     (lambda (node)
                       `(a :href ,(link-tail node)
                           ,(slot-ref node 'name)))
                     children)
                    " | ")))
            '()))
      (list
       '(raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
       `(html :xmlns "http://www.w3.org/1999/xhtml"
              (head
               (title
                ,(string-append "guile-gnome: docs: "
                                (stext->plain-text-title (cons 'text (cdr head)))))
               (style :type "text/css"
                      ,(string-append "@import url(" header-path "base.css);")))
              (body
               (div :id "body"
                    (div :id "heading"
                         (h1 "guile-gnome")
                         (div :id "menu-bar" :class "reversed"
                              (a :href ,(apply string-append (make-list depth "../")) "top") " "
                              (a :href ,(string-append header-path "docs/") "docs") " "
                              (a :href ,(string-append header-path "download/") "download") " "
                              (a :href ,(string-append header-path "dev/") "developers") " "
                              (a :href ,(string-append header-path "contact/") "contact") " "
                              (a :href ,(string-append header-path "links/") "links")))
                    (div :id "text"
                         ,(make-navigation)
                         ,@(make-subsections)
                         ,@(cdr body)
                         ,(make-navigation)))))))
    (let* ((file (open-output-file path))
           (stext (slot-ref node 'value))
           (html (stext->html stext `(post-processor . ,post-processor))))
      (display html file)
      (close-port file))))

(define (maybe-mkdir path)
  (if (or (not (file-exists? path)) (not (file-is-directory? path)))
      (mkdir path)))

;; keep in sync with the one in (text structured html)
(define (urlify str)
  (string-downcase
   (string-map
    (lambda (c)
      (case c
        ((#\space #\/ #\:) #\-)
        (else c)))
    str)))

(define (make-document-path parent-path . nodes)
  (apply
   build-path
   parent-path
   (map
    (lambda (node)
      (urlify (slot-ref node 'name)))
    nodes)))

(define (write-node parent-path node)
  (let ((children (slot-ref node 'children)))
    (maybe-mkdir parent-path)
    (let ((path (build-path parent-path "index.html")))
      (format #t "Making node ~A...\n" path)
      (write-html-file path node)
      (for-each
       (lambda (node)
         (write-node (make-document-path parent-path node) node))
       (or children '())))))

(define (ref-resolver node-name manual-name)
  (define (find-node-named node name)
    (or
     (let loop ((tokens (slot-ref node 'value)))
       (cond
        ((null? tokens)
         #f)
        ((and (list? (car tokens))
              (eq? (caar tokens) 'node)
              (equal? (cadar tokens) name))
         node)
        (else
         (loop (cdr tokens)))))
     (or-map
      (lambda (node)
        (find-node-named node name))
      (or (slot-ref node 'children) '()))))
  
  (if manual-name
      #f ;; We don't support refs to named manuals at the moment..
      (let* ((node (cond
                    ((or (not node-name) (string=? node-name "top"))
                     top-node)
                    (else
                     (or
                      (find-node-named top-node node-name)
                      (error "No such node in manual:" manual-name node-name)))))
             (depth (get-depth node))
             (top-of-docs-path (apply build-path (make-list (1- depth) ".."))))
        (string-append
         (apply
          make-document-path
          top-of-docs-path
          (let loop ((nodes (list node)))
            (let ((parent (slot-ref (car nodes) 'parent)))
              (if (not parent)
                  (cdr nodes)
                  (loop (cons parent nodes))))))
         "/#"
         (urlify node-name)))))
         
;; Install our own ref resolver.
(set! html-ref-resolvers
      (cons ref-resolver html-ref-resolvers))

;; finally, do the work.
(write-node (make-document-path document-path) top-node)
