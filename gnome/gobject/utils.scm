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
;; @c
;; Common utility routines.
;;
;;; Code:

(define-module (gnome gobject utils)
  #:use-module (oop goops)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 documentation)
  #:export     (GStudlyCapsExpand
                gtype-name->scheme-name-alist gtype-name->scheme-name
                gtype-name->class-name gtype-class-name->method-name
                re-export-modules
                define-macro-with-docs define-with-docs
                define-generic-with-docs define-class-with-docs
                unless with-accessors))

;;;
;;; {Miscellaneous}
;;;

(define-macro (define-macro-with-docs form docs . body)
  `(begin
     (define-macro ,form ,@body)
     (set-object-property! ,(car form) 'documentation ,docs)))

(define-macro-with-docs (define-with-docs name docs val)
  "Define @var{name} as @var{val}, documenting the value with
@var{docs}."
  `(begin
     (define ,name ,val)
     (set-object-property! ,name 'documentation ,docs)))

(define-macro-with-docs (define-generic-with-docs name documentation)
  "Define a generic named @var{name}, with documentation
@var{documentation}."
  `(define-with-docs ,name ,documentation
     (make-generic ',name)))

(define-macro-with-docs (define-class-with-docs name supers docs . rest)
  "Define a class named @var{name}, with superclasses @var{supers}, with
documentation @var{docs}."
  `(begin
     (define-class ,name ,supers ,@rest)
     (set-object-property! ,name 'documentation ,docs)))

;;;
;;; {Name Transformations}
;;;

;; Based on code from slib's strcase.scm, written 1992 by Dirk
;; Lutzebaeck (lutzeb@cs.tu-berlin.de). Public domain.
(define (GStudlyCapsExpand nstr)
  "Expand the StudlyCaps @var{nstr} to a more schemey-form, according to
the conventions of GLib libraries. For example:
@lisp
 (GStudlyCapsExpand \"GSource\") @result{} g-source
 (GStudlyCapsExpand \"GtkIMContext\") @result{} gtk-im-context
 (GStudlyCapsExpand \"GtkHBox\") @result{} gtk-hbox
@end lisp"
  (do ((idx (+ -1 (string-length nstr)) (+ -1 idx)))
      ((> 1 idx) (string-downcase nstr))
    (cond ((and (> idx 2)
                (char-lower-case? (string-ref nstr (+ -3 idx)))
                (char-upper-case? (string-ref nstr (+ -2 idx)))
                (char-upper-case? (string-ref nstr (+ -1 idx)))
                (char-lower-case? (string-ref nstr idx)))
           (set! idx (1- idx))
           (set! nstr
                 (string-append (substring nstr 0 (+ -1 idx))
                                "-"
                                (substring nstr (+ -1 idx)
                                           (string-length nstr)))))
          ((and (> idx 1)
                (char-upper-case? (string-ref nstr (+ -1 idx)))
                (char-lower-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 (+ -1 idx))
                                "-"
                                (substring nstr (+ -1 idx)
                                           (string-length nstr)))))
          ((and (char-lower-case? (string-ref nstr (+ -1 idx)))
                (char-upper-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 idx)
                                "-"
                                (substring nstr idx
                                           (string-length nstr))))))))

;; Default name transformations can be overridden
(define-with-docs gtype-name->scheme-name-alist
  "An alist of exceptions to the name transformation algorithm
implemented in @code{GStudlyCapsExpand}."
  '(("GObject" . "gobject")
    ("GValue" . "gvalue")
    ("GClosure" . "gclosure")
    ("GParamSpec" . "gparam")
    ;; i hate inconsistency!
    ("GEnum" . "genum")
    ("GFlags" . "gflags")
    ("GValueArray" . "gvalue-array")
    ("GBoxed" . "gboxed")
    ("GBoxedSCM" . "gboxed-scm")
    ("GInterface" . "ginterface")
    ("GParam" . "gparam")
    ("GParamBoolean" . "gparam-boolean")
    ("GParamChar" . "gparam-char")
    ("GParamUChar" . "gparam-uchar")
    ("GParamInt" . "gparam-int")
    ("GParamUInt" . "gparam-uint")
    ("GParamLong" . "gparam-long")
    ("GParamULong" . "gparam-ulong")
    ("GParamInt64" . "gparam-int64")
    ("GParamUInt64" . "gparam-uint64")
    ("GParamFloat" . "gparam-float")
    ("GParamDouble" . "gparam-double")
    ("GParamString" . "gparam-string")
    ("GParamEnum" . "gparam-enum")
    ("GParamFlags" . "gparam-flags")
    ("GParamObject" . "gparam-object")
    ("GParamBoxed" . "gparam-boxed")
    ("GParamPointer" . "gparam-pointer")))

(define (gtype-name->scheme-name type-name)
  "Transform a name of a @code{<gtype>}, such as \"GtkWindow\", to a
scheme form, such as @code{gtk-window}, taking into account the
exceptions in @code{gtype-name->scheme-name-alist}, and trimming
trailing dashes if any."
  (or (assoc-ref gtype-name->scheme-name-alist type-name)
      (string-trim-right
       (GStudlyCapsExpand
        ;; only change _ to -, other characters are not valid in a type name
        (string-map (lambda (c) (if (eq? c #\_) #\- c)) type-name))
       #\-)))

;; "GtkAccelGroup" => <gtk-accel-group>
;; "GSource*" => <g-source*>
(define (gtype-name->class-name type-name)
  "Transform a name of a @code{<gtype>}, such as \"GtkWindow\", to a
suitable name of a Scheme class, such as @code{<gtk-window>}. Uses
@code{gtype-name->scheme-name}."
  (string->symbol
   (string-append "<" (gtype-name->scheme-name type-name) ">")))

(define (gtype-class-name->method-name class-name name)
  "Generate the name of a method given the name of a @code{<gtype>} and
the name of the operation. For example:
@lisp
 (gtype-name->method-name \"GtkFoo\" \"bar\") @result{} gtk-foo:bar
@end lisp
Uses @code{gtype-name->scheme-name}."
  (let ((class-string (symbol->string class-name)))
    (string->symbol
     (string-append (substring class-string 1 (1- (string-length class-string)))
                    ":" (symbol->string name)))))

(define-macro-with-docs (re-export-modules . args)
  "Re-export the public interface of a module or modules. Invoked as
@code{(re-export-modules (mod1) (mod2)...)}."
  (if (not (null? args))
      (begin
        (or (list? (car args))
            (error "Invalid module specification" (car args)))
        `(begin
           (module-use! (module-public-interface (current-module))
                        (resolve-interface ',(car args)))
           (re-export-modules ,@(cdr args))))))

(define-macro (unless test . body)
  `(if (not ,test) (begin ,@body)))

(define-macro (with-accessors names . body)
  `(let (,@(map (lambda (name)
                  `(,name ,(make-procedure-with-setter
                            (lambda (x) (slot-ref x name))
                            (lambda (x y) (slot-set! x name y)))))
                names))
     ,@body))
