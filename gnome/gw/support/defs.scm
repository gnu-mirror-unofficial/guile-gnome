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
;;Support for reading in Gtk .defs files as g-wrap instructions
;;
;;; Code:

(define-module (gnome gw support defs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 slib)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  
  #:use-module (g-wrap)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap enumeration)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gw support gobject)
  
  #:export (load-defs
            load-defs-with-overrides))

(require 'glob)

(define (mklist x)
  (if (list? x) x (list x)))
(define-macro (push x tail)
  `(begin (set! ,tail (cons ,x ,tail)) ,tail))
(define-macro (pop tail)
  `(begin (set! ,tail (cdr ,tail)) ,tail))

;; find the gwrap type name for a given type name in a defs file, or
;; wrap the type as an opaque gpointer
(define* (type-lookup ws type return? #:key (ownership #f))
  (define (scan-for-ownership type k)
    (let ((const? (string-prefix? "const-" type)))
      (k (if const? (substring type 6) type)
         const?
         (cond (const?
                (list 'const (or ownership (if return? 'callee-owned 'caller-owned))))
               ((and (string-contains type "char*") (not const?))
                ;; we own non-const strings
                (list (or ownership (if return? 'caller-owned 'callee-owned))))
               ((string-index type #\*)
                ;; treat pointers conservatively
                (list (or ownership (if return? 'callee-owned 'caller-owned))))
               (else
                '())))))

  (define (scan-for-recursion type const? options k)
    (cond
     ((string-contains type "-of-")
      => (lambda (index)
           (k (substring type 0 index)
              const?
              (cons (mklist (type-lookup
                             ws (substring type (+ index 4)) return?))
                    options))))
     (else (k type const? options))))

  (define (return-type-form type const? options)
    (let ((type-obj (or (lookup-type ws type)
                        (wrap-opaque-pointer! ws type))))
      (cond ((null? options) (name type-obj))
            ((is-a? type-obj <gw-wct>)
             ;; gw:wct does not take caller/callee owned type options
             (if const? (list (name type-obj) 'const) (name type-obj)))
            (else (cons (name type-obj) options)))))

  (scan-for-ownership
   (regexp-substitute/global #f "\\[\\]" type 'pre "*" 'post)
   (lambda (type const? options)
     (scan-for-recursion
      type const? options
      (lambda (type const? options)
        (return-type-form type const? options))))))

(define-method (construct-argument-list (ws <gobject-wrapset-base>)
                                        (parameters <list>))
  
  (define (parse-restargs restargs)
    (fold
     (lambda (restarg options)
       (case (car restarg)
         ((null-ok)
          (cons 'null-ok options))
         ((callee-owned)
          (cons 'callee-owned options))
         ((default)
          (cons restarg options))
         (else
          (warn "unknown rest arg" restarg)
          options)))
     '() restargs))
  
  (let loop ((result '()) (params parameters))
    (if (null? params)
        (reverse result)
        (let-values (((count type) (find-type-rule ws params)))
          (let* ((defs-parameter-spec (car params))
                 (looked-up
                  (or type (type-lookup ws (car defs-parameter-spec) #f)))
                 (parsed (parse-restargs
                          (cddr defs-parameter-spec)))
                 (arg-name (string->symbol
                            (cadr defs-parameter-spec))))
            (let-values (((options extras) (partition! symbol? parsed)))
              ;; Ah, hackery...
              (if (memq 'callee-owned options)
                  (set! looked-up
                        (delq! 'callee-owned
                               (delq! 'caller-owned looked-up))))
              (loop (cons
                     (if (list? looked-up)
                         (append! (list (append looked-up options)
                                        arg-name) extras)
                         (append! (list looked-up arg-name) extras))
                     result)
                    (list-tail params (if (= count 0) 1 count)))))))))

(define (proc-name-from-cname cname)
  (string->symbol (gtype-name->scheme-name cname)))

(define* (load-defs ws file #:optional (overrides #f))
  (let* ((log-file-name (string-append (symbol->string (name ws)) ".log"))
         (log-file (open-output-file log-file-name))
         (abs-path (or (search-path %load-path file)
                       (error "Could not find file in path" file %load-path)))
         (already-included '())
         (overridden '())
         (bad-methods '())
         (num-types 0)
         (num-functions 0)
         (ignore-matchers '())
         (ignored-types '())
         (methods-used? #f))

    ;; The handlers...
    (define (scan-type! wrap-function immediate? args)
      ;; The gtype system has enough introspection power that we can
      ;; disregard a lot of the information in the defs files and just
      ;; look at the ctype and gtype-id, as well as the values, for
      ;; enums without a gtype-id.
      (catch
       'ignored
       (lambda ()
         (let ((ctype (car (or (assq-ref args 'c-name)
                               (error "Type lacks a c-name" args))))
               (gtype-id (assq-ref args 'gtype-id))
               (values (assq-ref args 'values)))
           (if (member ctype ignored-types)
               (throw 'ignored))

           (set! num-types (1+ num-types))
                
           (if (and (not gtype-id) (not values))
               (error "Non-enum/flags-type lacks a gtype-id" args))

           (let ((wrapped-type
                  (apply
                   wrap-function ws
                   (append
                    `(#:ctype ,ctype)
                    (if gtype-id `(#:gtype-id ,(car gtype-id)) '())
                    (if values
                        `(#:values ,(map
                                     (lambda (entry)
                                       (cons
                                        (string->symbol (caadr entry))
                                        (cadadr entry)))
                                     values))
                        '())))))
             (add-type-alias! ws (if immediate?
                                     ctype
                                     (string-append ctype "*"))
                              (name wrapped-type))
             wrapped-type)))
       noop))
    (define (arg-ref args field default)
      (cond ((assq field args) => cadr)
            (else default)))
    (define (get-generic-name func-name of-object)
      (let* (;; Fixme: do parameter spec creation for gw:wrap-function
             ;; before this, we can then use recursive-type-find
             (looked-up (type-lookup ws of-object #f))
             (of-obj (if (list? looked-up) (car looked-up)
                         looked-up))
             (of-obj-str (symbol->string of-obj))
             (sanitized-of-obj
              (substring of-obj-str 1
                         (or (string-index of-obj-str #\*)
                             (1- (string-length of-obj-str))))))
        (cond
         ((and (string-prefix? (string-append sanitized-of-obj "-")
                               func-name)
               ;; Temporary hack -- <gw-wct> should be a metaclass
               (not (is-a? (lookup-type ws of-object) <gw-wct>)))
          (string->symbol (substring func-name (1+ (string-length sanitized-of-obj)))))
         (else
          (push (list func-name of-object) bad-methods)
          #f))))
    (define (scan-function! is-method? name args)
      (catch
       'ignored
       (lambda ()
         (let ((c-name (arg-ref args 'c-name #f))
               (scm-name #f)
               (of-object (and=> (assq 'of-object args)
                                 (lambda (x) (string-append (cadr x) "*"))))
               (return-type (arg-ref args 'return-type "none"))
               (caller-owns-return (arg-ref args 'caller-owns-return #f))
               (parameters (or (and=> (assq-ref args 'parameters)
                                      (lambda (x) (map cadr x)))
                               '()))
               (generic-name #f))

           (cond
            ((ignored? c-name)
             (display "x") (throw 'ignored))
            ((member c-name overridden)
             (throw 'ignored))
            ((and=> (assq 'varargs args) cadr)
             (display "v") (throw 'ignored))
            ((any
              (lambda (x) (eq? (string-ref (cadr x) 0) #\())
              parameters)
             (format #t "\nWarning, not binding function ~A ~A"
                     "because I can't deal with function pointers\n"
                     name)
             (throw 'ignored))
            ((and is-method? (not of-object))
             (error "Method name lacks an of-object!" c-name)))

           (and=> (memq 'is-constructor-of (map car args))
                  ;; If we're the constructor, we own the return value,
                  ;; unless told otherwise (e.g. with gtkwindow).
                  (lambda (l)
                    (if (not (memq 'caller-owns-return l))
                        (set! caller-owns-return #t))))
           (and=> (arg-ref args 'overrides #f)
                  (lambda (x)
                    (set! scm-name (proc-name-from-cname x))
                    (if (member x overridden)
                        (error "Function ~S already overridden" x)
                        (push x overridden))))

           (if is-method?
               (push (list of-object "self") parameters))
           (if (not scm-name)
               (set! scm-name (proc-name-from-cname c-name)))

           (set! num-functions (1+ num-functions))

           (display ".")
           (wrap-function!
            ws
            #:name scm-name
            #:returns (type-lookup ws return-type #t
                                   #:ownership (if caller-owns-return
                                                   'caller-owned
                                                   'callee-owned))
            #:c-name c-name
            #:arguments (construct-argument-list ws parameters)
            #:generic-name (and is-method?
                                (get-generic-name (symbol->string scm-name) of-object)))))
       noop))
    (define (ignored? c-name)
      (any (lambda (matcher) (matcher c-name))
           ignore-matchers))
    (define (ignore . args)
      (for-each
       (lambda (c-name)
         (if (member c-name overridden)
             (error "Function ~S already overridden" c-name)
             (push c-name overridden)))
       args))
    (define (ignore-types . args)
      (set! ignored-types (append args ignored-types)))
    (define (ignore-glob . args)
      (for-each
       (lambda (glob)
         (push (glob:make-matcher glob char=? char<=?)
               ignore-matchers))
       args))

    ;; "type helper" / "function helper"
    (define (th wrap immediate?)
      (lambda (exp) (scan-type! wrap immediate? (cddr exp))))
    (define (fh method?)
      (lambda (exp) (scan-function! method? (cadr exp) (cddr exp))))

    (define (scan-defs)
      (define defs-handlers
        `((define-enum      ,(th wrap-enum! #t))
          (define-flags     ,(th wrap-flags! #t))
          (define-object    ,(th wrap-object! #f))
          (define-interface ,(th wrap-interface! #f))
          (define-pointer   ,(th wrap-pointer! #f))
          (define-boxed     ,(th wrap-boxed! #f))
          (define-function  ,(fh #f))
          (define-method    ,(fh #t))
          (ignore           ,(lambda (exp) (apply ignore (cdr exp))))
          (ignore-glob      ,(lambda (exp) (apply ignore-glob (cdr exp))))
          (ignore-types     ,(lambda (exp) (apply ignore-types (cdr exp))))))
      (let lp ((exp (read)))
        (cond
         ((eof-object? exp))
         ((not (list? exp))
          (warn "Invalid form in .defs file" exp file)
          (lp (read)))
         ((assq (car exp) defs-handlers)
          => (lambda (x) ((cadr x) exp) (lp (read))))
         ((eq? (car exp) 'include)
          (let ((f (cond
                    ((eq? (cadr exp) 'overrides)
                     (or overrides (error "no overrides file for defs" file)))
                    ((symbol? (cadr exp))
                     (let ((fname (string-append "gnome/overrides/" (basename file)
                                                 "-" (symbol->string (cadr exp)))))
                       (or (file-exists? (search-path %load-path fname))
                           (error "file does not exist in load path" fname))
                       fname))
                    (else (cadr exp)))))
            (if (member f already-included)
                (error ".defs file has recursively included itself"
                       f already-included))
            (push f already-included)
            (with-input-from-file (or (search-path %load-path f)
                                      (error "Could not find file" f %load-path))
              scan-defs)
            (lp (read))))
         (else
          (warn "Unknown .defs form" exp file)
          (lp (read))))))

    (dynamic-wind
        (lambda () (push (dirname abs-path) %load-path))
        (lambda ()
          (with-input-from-file abs-path scan-defs)
          ;; FIXME: re-establish
          ;;     (format log-file "Opaque types in the ~A wrapset: c-name scm-name\n\n"
          ;;             (name ws))
          ;;     (for-each
          ;;      (lambda (pair)
          ;;        (format log-file "~A ~A\n" (car pair) (cadr pair)))
          ;;      opaque-types)
          (format log-file "\n\nBad method names in the ~A wrapset: c-name of-object\n\n"
                  (name ws))
          (for-each
           (lambda (pair)
             (format log-file "~A ~A\n" (car pair) (cadr pair)))
           bad-methods)
          (close log-file)
          ;;     (format #t "\n\nWrapped ~A types (~A opaque) and ~A functions.\n"
          ;;             (+ num-types (length opaque-types)) (length opaque-types) num-functions)
          (format #t "\nA list of opaque types and bad method names has been written to ~A.\n\n"
                  log-file-name))

        (lambda () (pop %load-path)))))

(define (load-defs-with-overrides ws defs)
  (load-defs ws defs
             (string-append "gnome/overrides/" (basename defs))))
