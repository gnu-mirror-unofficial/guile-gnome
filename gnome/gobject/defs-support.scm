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

(define-module (gnome gobject defs-support)
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
  #:use-module (gnome gobject gw-spec-utils)
  #:export (load-defs))

(require 'glob)

;; find the gwrap type name for a given type name in a defs file, or
;; wrap the type as an opaque gpointer -- this thing is getting nasty!
;; i could really use some keyword args here, ugh
(define* (type-lookup ws type return? #:key (ownership #f))
  (let ((options (list))
        (const? #f))
    ;;(format #t "lookup: ~S\n" type)
    (set! type (regexp-substitute/global #f "\\[\\]" type 'pre "*" 'post))
    (cond
     ((string-prefix? "const-" type)
      (set! options (cons (or ownership (if return? 'callee-owned 'caller-owned))
                          options))
      (set! options (cons 'const options))
      (set! const? #t)
      (set! type (substring type (string-length "const-"))))
     ((string-contains type "char*")
      ;; without an explicit value for ownership, we only assume that we
      ;; own non-const strings
      (set! options (cons (or ownership (if return? 'caller-owned 'callee-owned))
                          options)))
     ((string-index type #\*)
      ;; other things represented by pointers are treated conservatively
      (set! options (cons (or ownership (if return? 'callee-owned 'caller-owned))
                          options))))

    ;; support GList*-of-GtkWindow*
    (let ((index (string-contains type "-of-")))
      (if index
          (begin
            (set! options (cons
                           (let ((subtype (type-lookup
                                           ws
                                           (substring type (+ index 4))
                                           return?)))
                             (if (list? subtype)
                                 subtype
                                 (list subtype)))
                           options))
            (set! type (substring type 0 index)))))
    (let ((type-obj (or (lookup-type ws type)
                        (wrap-opaque-pointer! ws type))))
      
      ;; gw:wct does not take caller/callee owned type options
      (if (null? options)
          (name type-obj)
          (if (is-a? type-obj <gw-wct>)
              (if const? (cons (name type-obj) '(const)) (name type-obj))
              (cons (name type-obj) options))))))

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
          (cons restarg options))))
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

(define (load-defs ws file . already-included)
  (let* ((old-load-path %load-path)
         (log-file-name (string-append (symbol->string (name ws)) ".log"))
         (log-file (open-output-file log-file-name))
         (overrides '())
         (bad-methods '())
         (num-types 0)
         (num-functions 0)
         (ignore-matchers '())
         (methods-used? #f))

    (let ((absolute-path (%search-load-path file)))
      (if absolute-path
          (format #f "Loading defs file \"~S\"...\n" file)
          (error (format #t "Could not find file ~S in the load path ~A\n"
                         file %load-path))))

    ;; hm, should use dynamic-wind here...
    (set! %load-path (list (dirname (%search-load-path file))))
    (set! file (basename file))

    (let* ((scan-type!
            (lambda (wrap-function immediate? args)
              ;; The gtype system has enough introspection power that
              ;; we can disregard a lot of the information in the defs
              ;; files and just look at the ctype and gtype-id, as
              ;; well as the values, for enums without a gtype-id.
              (let ((ctype (assq-ref args 'c-name))
                    (gtype-id (assq-ref args 'gtype-id))
                    (values (assq-ref args 'values)))
                (set! num-types (1+ num-types))
                
                (if (not ctype)
                    (error "Type lacks a c-name:\n\n" args))
                
                (if (and (not gtype-id) (not values))
                    (error "Non-enum/flags-type lacks a gtype-id:\n\n" args))
                (let ((wrapped-type
                       (apply
                        wrap-function ws
                        (append
                         (if ctype `(#:ctype ,(car ctype)) '())
                         (if gtype-id `(#:gtype-id ,(car gtype-id)) '())
                         (if values
                             `(#:values ,(map
                                          (lambda (entry)
                                            (cons
                                             (string->symbol (cadr entry))
                                             (cdr entry)))
                                          (cdar values)))
                             '())))))
                  (add-type-alias! ws (if immediate?
                                          (car ctype)
                                          (string-append (car ctype) "*"))
                                   (name wrapped-type))
                  wrapped-type))))
           
           (ignored?
            (lambda (c-name)
              (any (lambda (matcher) (matcher c-name))
                   ignore-matchers)))

           (scan-function!
            (lambda (is-method? name args)
              (call-with-current-continuation
               (lambda (exit)
                 (let ((c-name #f)
                       (scm-name #f)
                       (of-object #f)
                       (return-type "none")
                       (caller-owns-return #f)
                       (parameters (list))
                       (generic-name #f))
                   (for-each
                    (lambda (arg)
                      (case (car arg)
                        ((return-type)
                         (set! return-type (cadr arg)))
                        ((caller-owns-return)
                         (set! caller-owns-return (cadr arg)))
                        ((is-constructor-of)
                         ;; so, if the caller really doesn't own it (as
                         ;; in gtkwindow), put (caller-owns-return #f)
                         ;; after the (is-constructor-of)
                         (set! caller-owns-return #t))
                        ((parameters)
                         (if (any
                              (lambda (name)
                                (eq? (string-ref name 0) #\())
                              (map cadadr (cdr arg)))
                             (begin
                               (format #t "\nWarning, not binding function ~A because I can't deal with function pointers\n"
                                       name)
                               (exit #f)))
                         (set! parameters (map second (cdr arg))))
                        ((of-object)
                         (set! of-object (string-append (cadr arg) "*")))
                        ((overrides)
                         (set! scm-name (glib:func-cname->symbol (cadr arg)))
                         (if (member (cadr arg) overrides)
                             (error "Function ~S already overridden" (cadr arg))
                             (set! overrides (cons (cadr arg) overrides))))
                        ((varargs)
                         (display "x") (exit #f))
                        ((c-name)
                         (set! c-name (cadr arg))
                         (if (ignored? c-name)
                             (begin (display "x") (exit #f)))
                         (if (member c-name overrides)
                             (exit #f)))))
                    args)
                   
                   (set! num-functions (1+ num-functions))

                   (if (not scm-name)
                       (set! scm-name (glib:func-cname->symbol c-name)))

                   (cond
                    (is-method?
                     (if (not of-object)
                         (error "Method name lacks an of-object!" c-name))
                     (set! parameters (cons (list of-object "self")
                                            parameters))
                     (let* ((func-name (symbol->string scm-name))
                            ; Fixme: do parameter spec creation for
                            ; gw:wrap-function before this, we can
                            ; then use recursive-type-find
                            (looked-up (type-lookup ws of-object #f))
                            (of-obj (if (list? looked-up) (car looked-up)
                                        looked-up))
                            (of-obj-str (symbol->string of-obj))
                            (sanitized-of-obj
                             (substring of-obj-str 1
                                        (or (string-index of-obj-str #\*)
                                            (1- (string-length of-obj-str))))))
                       (cond
                        ((string-prefix? (string-append sanitized-of-obj "-")
                                         func-name)
                         (set! generic-name (string->symbol (substring func-name (1+ (string-length sanitized-of-obj)))))
                         (set! of-object of-obj-str))
                        (else
                         (set! bad-methods (cons (list func-name of-object)
                                                bad-methods)))))))

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
                    #:generic-name generic-name)))))))

      (letrec ((define-enum (procedure->syntax
                             (lambda (x env)
                               (scan-type! wrap-enum! #t (cddr x)))))
               (define-flags (procedure->syntax
                              (lambda (x env)
                                (scan-type! wrap-flags! #t (cddr x)))))
               (define-object (procedure->syntax
                               (lambda (x env)
                                 (scan-type! wrap-object! #f (cddr x)))))
               (define-interface (procedure->syntax
                                  (lambda (x env)
                                    (scan-type! wrap-interface! #f (cddr x)))))
               (define-pointer (procedure->syntax
                                (lambda (x env)
                                  (scan-type! wrap-pointer! #f (cddr x)))))
               (define-boxed (procedure->syntax
                              (lambda (x env)
                                (scan-type! wrap-boxed! #f (cddr x)))))
               (define-function (procedure->syntax
                                 (lambda (x env)
                                   (scan-function! #f (cadr x) (cddr x)))))
               (define-method (procedure->syntax
                               (lambda (x env)
                                 (scan-function! #t (cadr x) (cddr x)))))

               (ignore (lambda args
                         (for-each
                          (lambda (c-name)
                            (if (member c-name overrides)
                                (error "Function ~S already overridden" c-name)
                                (set! overrides (cons c-name overrides))))
                          args)))

               (ignore-glob (lambda args
                              (for-each
                               (lambda (glob)
                                 (set! ignore-matchers
                                       (cons (filename:match?? glob)
                                             ignore-matchers)))
                               args)))

               (include (lambda (file)
                          (if (not (member file already-included))
                              (let* ((filename (%search-load-path file)))
                                (if (not filename)
                                    (error "Could not find file" file %load-path)
                                    (let* ((port (open-input-file filename))
                                           (sexp (read port)))
                                      (set! already-included (cons file already-included))
                                      (while (not (eof-object? sexp))
                                             (local-eval sexp (the-environment))
                                             (set! sexp (read port))))))))))

        
        ;; we're now in an environment to interpret the defs files natively.
        ;; nice :-)

        (include file)))
      
        
    ;; FIXME: re-establish
;     (format log-file "Opaque types in the ~A wrapset: c-name scm-name\n\n"
;             (name ws))
;     (for-each
;      (lambda (pair)
;        (format log-file "~A ~A\n" (car pair) (cadr pair)))
;      opaque-types)

    (format log-file "\n\nBad method names in the ~A wrapset: c-name of-object\n\n"
            (name ws))
    (for-each
     (lambda (pair)
       (format log-file "~A ~A\n" (car pair) (cadr pair)))
     bad-methods)

    (close log-file)

;     (format #t "\n\nWrapped ~A types (~A opaque) and ~A functions.\n"
;             (+ num-types (length opaque-types)) (length opaque-types) num-functions)
    (format #t "A list of opaque types and bad method names has been written to ~A.\n\n"
            log-file-name)

    (set! %load-path old-load-path)))
