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
;; Common utility routines.
;;
;;; Code:

(define-module (gnome gobject utils)
  :use-module (srfi srfi-13)
  :export     (gtype-name->scheme-name-alist gtype-name->scheme-name
               gtype-name->class-name gtype-name->method-name
               re-export-modules
               define-with-docs define-generic-with-docs))

;;;
;;; {Name Transformations}
;;;

;; Based on code from slib's strcase.scm, written 1992 by Dirk
;; Lutzebaeck (lutzeb@cs.tu-berlin.de). Public domain.
;;
;; GSource => g-source, GtkIMContext => gtk-im-context,
;; GtkHBox => gtk-hbox.
(define (GStudlyCapsExpand nstr)
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
(define gtype-name->scheme-name-alist
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
  (or (assoc-ref gtype-name->scheme-name-alist type-name)
      (string-trim-right
       (GStudlyCapsExpand
        ;; only change _ to -, other characters are not valid in a type name
        (string-map (lambda (c) (if (eq? c #\_) #\- c)) type-name))
       #\-)))

;; "GtkAccelGroup" => <gtk-accel-group>
;; "GSource*" => <g-source*>
(define (gtype-name->class-name type-name)
  (string->symbol
   (string-append "<" (gtype-name->scheme-name type-name) ">")))

(define (gtype-name->method-name type-name name)
  (string->symbol
   (string-append type-name ":" (symbol->string name))))

(define-macro (re-export-modules . args)
  (if (not (null? args))
      (begin
        (or (list? (car args))
            (error "Invalid module specification" (car args)))
        `(begin
           (module-use! (module-public-interface (current-module))
                        (resolve-interface ',(car args)))
           (re-export-modules ,@(cdr args))))))
(set-object-property! re-export-modules 'documentation
  "Re-export the public interface of a module; used like
@code{use-modules}.")

;;;
;;; {Miscellaneous}
;;;

(defmacro define-with-docs args
  (define (syntax)
    (error "bad syntax" (list 'define-public args)))
  (define (get-name n)
    (cond
      ((symbol? n) n)
      ((pair? n) (get-name (car n)))
      (else (syntax))))
  (define (get-documentation n)
    (cond
      ((and (pair? n) (string? (car n))) (car n))
      (else (syntax))))
  (cond
    ((null? args)
     (syntax))
    (#t
     (let ((name (get-name (car args)))
	   (object-documentation (get-documentation (cdr args))))
       `(begin
	  (define ,(car args) ,@(cddr args))
	  (set-object-property! ,name 'documentation ,object-documentation)
          *unspecified*)))))

(define-macro (define-generic-with-docs name documentation)
  `(define-with-docs ,name ,documentation
     (make-generic ',name)))
