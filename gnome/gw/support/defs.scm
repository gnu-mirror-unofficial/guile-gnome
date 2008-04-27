;; guile-gnome
;; Copyright (C) 2005 Andreas Rottmann <rotty at debian dot org>
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
;; This module serves as a way to automatically populate G-Wrap wrapsets
;; using information parsed out of C header files.
;;
;; First, the C header files are parsed into S-expression API
;; description forms and written into @code{.defs} files. These files
;; are typically included in the distribution, and regenerated
;; infrequently. Then, the binding author includes a call to
;; @code{load-defs} in their G-Wrap wrapset definition, which loads
;; those API definitions into the wrapset.
;;
;; The @code{.defs} files are usually produced using the API scanner
;; script, @code{h2defs.py}, included in the Guile-GNOME source
;; distribution.
;;
;; Code in this module is only loaded when generating wrapsets; as such,
;; it is not for end users.
;;
;; As an example, ATK is wrapped with the following code, from
;; @code{atk/gnome/gw/atk-spec.scm}:
;;
;; @example
;; (define-module (gnome gw atk-spec)
;;   #:use-module (oop goops)
;;   #:use-module (gnome gw support g-wrap)
;;   #:use-module (gnome gw gobject-spec)
;;   #:use-module (gnome gw support gobject)
;;   #:use-module (gnome gw support defs))
;;
;; (define-class <atk-wrapset> (<gobject-wrapset-base>)
;;   #:id 'gnome-atk
;;   #:dependencies '(standard gnome-glib gnome-gobject))
;;
;; (define-method (global-declarations-cg (self <atk-wrapset>))
;;   (list
;;    (next-method)
;;    "#include <atk/atk.h>\n"
;;    "#include <atk/atk-enum-types.h>\n"))
;;
;; (define-method (initialize (ws <atk-wrapset>) initargs)
;;   (next-method ws (append '(#:module (gnome gw atk)) initargs))
;;   ;; manually wrap AtkState as a 64 bit uint
;;   (add-type-alias! ws "AtkState" 'unsigned-int64)
;;   (load-defs-with-overrides ws "gnome/defs/atk.defs"))
;; @end example
;;
;; The wrapper-specifiction modules are actually installed, along with
;; the .defs files, so that other wrappers which use ATK's types, such
;; as GTK+, can have them available.
;;
;; A full discussion of the Makefile mechanics of how to generate and
;; compile the C file, or how to interact with the wrapset objects, is
;; probably prone to bitrot here. Your best bet is to poke at
;; Guile-GNOME's source, or especially the source of a module
;; distributed independently of @code{guile-gnome-platform}, such as
;; @code{guile-gnome-libwnck}.
;;
;; Further details about the procedural API available for use e.g.
;; within the wrapset's @code{initialize} function can be found in the
;; documentation for @code{(gnome gw support gobject)}, and in G-Wrap's
;; documentation.
;;
;;; Code:

(define-module (gnome gw support defs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)

  #:use-module (gnome gw support g-wrap)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap enumeration)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support slib)
  
  #:export (load-defs
            load-defs-with-overrides))

(define (mklist x)
  (if (list? x) x (list x)))
(define-macro (push x tail)
  `(begin (set! ,tail (cons ,x ,tail)) ,tail))
(define-macro (pop tail)
  `(begin (set! ,tail (cdr ,tail)) ,tail))

;; find the gwrap type name for a given type name in a defs file, or
;; wrap the type as an opaque gpointer
;;
;; return type: g-wrap typespec, as a list of symbols
(define* (type-lookup ws type return? #:key (ownership #f) (for-proc #f))
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
                ;; For pointers, assume that we own arguments, and that
                ;; the proc owns return values
                (list (or ownership (if return? 'callee-owned 'caller-owned))))
               (else
                '())))))

  (define (scan-for-recursion type const? options k)
    (cond
     ((string-contains type "-of-")
      ;; Because the container type is generated by the wrapper, we own
      ;; it by default
      => (lambda (index)
           (k (substring type 0 index)
              const?
              (cons (mklist (type-lookup
                             ws (substring type (+ index 4)) return?
                             #:for-proc for-proc))
                    (if (memq 'const options)
                        options
                        (cons (if return? 'caller-owned 'callee-owned)
                              (fold delq options
                                    '(caller-owned callee-owned))))))))
     (else (k type const? options))))

  (define (return-type-form type const? options)
    (let ((type-obj (or (lookup-type-by-alias ws type)
                        (begin (warn "opaque type for proc" for-proc type)
                               (throw 'ignored)))))
      (if (is-a? type-obj <gw-wct>)
          ;; gw:wct does not take caller/callee owned type options
          (cons (name type-obj) (if const? '(const) '()))
          (cons (name type-obj) options))))

  (scan-for-ownership
   (regexp-substitute/global #f "\\[\\]" type 'pre "*" 'post)
   (lambda (type const? options)
     (scan-for-recursion
      type const? options
      (lambda (type const? options)
        (return-type-form type const? options))))))

(define-method (construct-argument-list (ws <gobject-wrapset-base>)
                                        (parameters <list>) scm-name)
  (define (parse-restargs restargs)
    (partition!
     symbol?
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
      '() restargs)))
  
  (define (parse-argument param)
    (let ((looked-up (or (find-type-rule ws (car param))
                         (type-lookup ws (car param) #f #:for-proc scm-name)))
          (arg-name (string->symbol (cadr param))))
      (let-values (((options extras) (parse-restargs (cddr param))))
        `((,@(if (memq 'callee-owned options)
                 ;; why is this here? wcts?
                 (fold delq looked-up '(callee-owned caller-owned))
                 looked-up)
           ,@options)
          ,arg-name
          ,@extras))))

  (map parse-argument parameters))

(define (proc-name-from-cname cname)
  (string->symbol (gtype-name->scheme-name cname)))

(define* (load-defs ws file #:optional (overrides #f))
  "Load G-Wrap type and function information from @var{file} into the
G-Wrap wrapset @var{ws}.

@var{file} should be a relative path, which will be searched in the
vicinity of Guile's @code{%load-path}. @code{include} directives in the
file will be searched relative to the absolute path of the file.

The following forms are understood: @code{define-enum},
@code{define-flags}, @code{define-object}, @code{define-interface},
@code{define-pointer}, @code{define-boxed}, @code{define-function},
@code{define-method}, @code{ignore}, @code{ignore-glob}, and
@code{ignore-types}.

The optional argument, @var{overrides}, specifies the location of an
overrides file that will be spliced into the @code{.defs} file at the
point of an @code{(include overrides)} form."
  (let ((abs-path (or (search-path %load-path file)
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
             (looked-up (type-lookup ws of-object #f #:for-proc func-name))
             (of-obj (if (list? looked-up) (car looked-up)
                         looked-up))
             (of-obj-str (symbol->string of-obj))
             (sanitized-of-obj
              (substring of-obj-str 1
                         (or (string-index of-obj-str #\*)
                             (1- (string-length of-obj-str)))))
             (g-wrap-type (lookup-type-by-alias ws of-object))
             (hack-module (resolve-module '(gnome gw support gobject))))
        (cond
         ((or (is-a? g-wrap-type <gw-wct>)
              (is-a? g-wrap-type (module-ref hack-module '<gobject-boxed-type>))
              (is-a? g-wrap-type (module-ref hack-module '<gobject-custom-boxed-type>))
              (is-a? g-wrap-type (module-ref hack-module '<gobject-enum-type>))
              (is-a? g-wrap-type (module-ref hack-module '<gobject-flags-type>)))
          #f)
         ((string-prefix? (string-append sanitized-of-obj "-") func-name)
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
               (flags (if (if (assq 'leave-guile-mode args)
                              (car (assq-ref args 'leave-guile-mode))
                              #t)
                          "GW_FUNCTION_FLAG_LEAVE_RUNTIME"
                          "0"))
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
                                                   'callee-owned)
                                   #:for-proc scm-name)
            #:c-name c-name
            #:arguments (construct-argument-list ws parameters scm-name)
            #:generic-name (and is-method?
                                (get-generic-name (symbol->string
                                                   scm-name) of-object))
            #:flags flags)))
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
          (define-object    ,(th wrap-instance! #f))
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
          => (lambda (x)
               (guard
                (c (#t (raise-stacked c "while processing def ~S" exp)))
                ((cadr x) exp))
               (lp (read))))
         ((eq? (car exp) 'include)
          (let ((f (cond
                    ((eq? (cadr exp) 'overrides)
                     (or overrides (error "no overrides file for defs" file)))
                    ((symbol? (cadr exp))
                     (let ((fname (string-append "gnome/overrides/" (basename file)
                                                 "-" (symbol->string (cadr exp)))))
                       (or (and=> (search-path %load-path fname)
                                  file-exists?)
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
          (guard
           (c
            (#t (raise-stacked c "while processing defs `~A'" abs-path)))
           (with-input-from-file abs-path scan-defs)))
        (lambda () (pop %load-path)))))

(define (load-defs-with-overrides ws defs)
  "Equivalent to:
@lisp
  (load-defs ws defs
             (string-append \"gnome/overrides/\"
                            (basename defs)))
@end lisp"
  (load-defs ws defs
             (string-append "gnome/overrides/" (basename defs))))
