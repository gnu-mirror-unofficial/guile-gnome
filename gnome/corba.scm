;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
;;               2003,2004 Andy Wingo <wingo at pobox dot com>

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
;;A CORBA wrapper for Guile.
;;
;;; Code:

(define-module (gnome corba)
  :use-module (gnome gw corba)
  :use-module (gnome corba types)
  :use-module (gnome corba primitives)
  :use-module (gnome gobject)
  :use-module (oop goops))

(re-export <PortableServer-ServantBase> <CORBA:Object>
	   gnome-corba-error)

(%init-gnome-corba)

(or (corba-primitive-open-module "Bonobo")
    (gnome-corba-error "Can't open `Bonobo' module"))


;;; {Records}
;;;

;; 0: type-name, 1: fields
(define corba-record-type-vtable
  (make-vtable-vtable "prpr" 0
		      (lambda (s p)
			(cond ((eq? s corba-record-type-vtable)
			       (display "#<corba-record-type-vtable>" p))
			      (else
			       (display "#<corba-record-type " p)
			       (display (corba-record-typecode s) p)
			       (display ">" p))))))

(define (make-corba-record-type typecode . opt)
  (let ((printer-fn (and (pair? opt) (car opt))))
    (let* ((corba-fields (corba-struct-fields typecode))
	   (type-name (corba-typecode-primitive->name typecode))
	   (struct (make-struct corba-record-type-vtable 0
				(make-struct-layout
				 (apply string-append
					(map (lambda (f) "pw") corba-fields)))
				(or printer-fn
				    (lambda (s p)
				      (display "#<" p)
				      (display type-name p)
				      (let loop ((fields corba-fields)
						 (off 0))
					(cond
					 ((not (null? fields))
					  (display " " p)
					  (display (car fields) p)
					  (display ": (" p)
					  (display (corba-struct-ref s off) p)
					  (display ")" p)
					  (loop (cdr fields) (+ 1 off)))))
				      (display ">" p)))
				typecode
				(copy-tree corba-fields))))
      ;; Temporary solution: Associate a name to the corba-record type descriptor
      ;; so that the object system can create a wrapper class for it.
      (set-struct-vtable-name! struct type-name)
      struct)))

(define the-environment
  (procedure->syntax
   (lambda (x e)
     e)))

(define the-corba-environment (the-environment))

(define (corba-record-type? obj)
  (and (struct? obj) (eq? corba-record-type-vtable (struct-vtable obj))))

(define (corba-record-typecode obj)
  (if (corba-record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-corba-record-type obj)))

(define (corba-record-type-fields obj)
  (if (corba-record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-corba-record-type obj)))

(define (corba-record-constructor rtd . opt)
  (let ((field-names (if (pair? opt) (car opt) (corba-record-type-fields rtd))))
    (local-eval `(lambda ,field-names
		   (let ((struct (make-corba-struct (corba-record-typecode ',rtd) 1)))
		     (struct-set! struct %corba-struct-vtable-offset-printer
				  (struct-ref ',rtd %corba-struct-vtable-offset-printer))
		     (struct-set! struct (+ 1 %corba-struct-vtable-offset-user) ',rtd)
		     (let loop ((fields (list ,@(map (lambda (f)
						       (if (memq f field-names)
							   f
							   #f))
						     (corba-record-type-fields `,rtd))))
				(off 0))
		       (cond
			((not (null? fields))
			 (corba-struct-set! struct off (car fields))
			 (loop (cdr fields) (+ 1 off)))))
		     struct))
		the-corba-environment)))

(define (corba-record-constructor-from-struct rtd)
  (let ((field-names (corba-record-type-fields rtd)))
    (local-eval `(lambda (corba-struct)
		   (let ((struct (make-corba-struct (corba-record-typecode ',rtd) 1 corba-struct)))
		     (struct-set! struct %corba-struct-vtable-offset-printer
				  (struct-ref ',rtd %corba-struct-vtable-offset-printer))
		     (struct-set! struct (+ 1 %corba-struct-vtable-offset-user) ',rtd)
		     struct))
		the-corba-environment)))

(define (corba-record-predicate rtd)
  (lambda (obj) (and (corba-struct? obj) (eq? rtd (corba-record-type-descriptor obj)))))

(define (corba-record-accessor rtd field-name)
  (let* ((pos (list-index (corba-record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (local-eval `(lambda (obj)
		   (and (eq? ',rtd (corba-record-type-descriptor obj))
			(corba-struct-ref obj ,pos)))
		the-corba-environment)))

(define (corba-record-modifier rtd field-name)
  (let* ((pos (list-index (corba-record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (local-eval `(lambda (obj val)
		   (and (eq? ',rtd (corba-record-type-descriptor obj))
			(corba-struct-set! obj ,pos val)))
		the-corba-environment)))

(define (corba-record? obj)
  (and (corba-struct? obj) (corba-record-type? (corba-record-type-descriptor obj))))

(define (corba-record-type-descriptor obj)
  (if (corba-struct? obj)
      (struct-ref obj (+ 1 %corba-struct-vtable-offset-user))
      (error 'not-a-corba-record obj)))

(define (corba-struct->record struct)
  (let* ((typecode (corba-struct-type struct))
	 (record-type (make-corba-record-type typecode))
	 (constructor (corba-record-constructor-from-struct record-type)))
    (constructor struct)))

(define (corba-sequence->list sequence)
  (let* ((length (corba-sequence-length sequence))
	 (thelist (list)))
    (do ((i 0 (+ i 1)))
	((>= i length) thelist)
      (let ((this (corba-sequence-ref sequence i)))
	(and (corba-struct? this) (set! this (corba-struct->record this)))
	(set! thelist (append! thelist (list this)))))))

(provide 'corba-record)

(export corba-record-type-vtable
	corba-record-type? make-corba-record-type corba-record-typecode
	corba-record-type-fields corba-record-constructor corba-record-predicate
	corba-record-accessor corba-record-modifier corba-record?
	corba-record-type-descriptor corba-record-constructor-from-struct
	corba-struct->record corba-sequence->list)
