;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
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
;; GClosure support.
;;
;; See the guile-gnome tutorial for more details.
;;
;;; Code:

(define-module (gnome gobject gclosure)
  :use-module (gnome gobject config)
  :use-module (gnome gobject gtype)
  :use-module (gnome gobject gvalue)
  :use-module (oop goops)

  :export     (<gclosure> gclosure-invoke))

(dynamic-call "scm_init_gnome_gobject_closures"
              (dynamic-link *guile-gnome-gobject-lib-path*))

(define-class <gclosure-class> (<gtype-class>))
(define-class <gclosure> ()
  closure
  return-type
  param-types
  #:gtype gtype:gclosure
  #:metaclass <gclosure-class>)

;;;
;;; {Instance Allocation and Initialization}
;;;

(define-method (allocate-instance (class <gclosure-class>) initargs)
  (next-method))

(define-method (initialize (closure <gclosure>) initargs)
  (let* ((func (get-keyword #:func initargs *unspecified*))
	 (rettype (get-keyword #:return-type initargs gtype:void))
	 (paramtypes (get-keyword #:param-types initargs '())))
    (if (unspecified? func)
        (gruntime-error "Missing #:func argument"))
    (or (procedure? func)
	(gruntime-error "Wrong type argument: ~S" func))
    (if (not (unspecified? rettype))
        (if (not (is-a? rettype <gtype>))
            (if (is-a? rettype <gtype-class>)
                (set! rettype (gtype-class->type rettype))
                (gruntime-error "#:rettype must be a <gtype> or a <gtype-class>: ~A"
                                rettype))))
    (set! paramtypes
          (list->vector
           (map
            (lambda (ptype)
              (cond
               ((is-a? ptype <gtype>)
                ptype)
               ((is-a? ptype <gtype-class>)
                (gtype-class->type ptype))
               (else
                (gruntime-error "Invalid closure parameter type: ~A" ptype))))
            paramtypes)))

    (let* ((newfunc (lambda (. args)
		      (let* ((newargs (map (lambda (x) (gvalue->scm x)) args))
			     (retval (apply func newargs)))
                        (if (and (not (eq? rettype gtype:void))
                                 (unspecified? retval))
                            (gruntime-error
                             "Function returned no value, but expected ~S" rettype)
                            (scm->gvalue rettype retval))))))
      (slot-set! closure 'closure (gclosure-primitive-new newfunc)))
    (slot-set! closure 'return-type rettype)
    (slot-set! closure 'param-types paramtypes)
    (next-method)))
    
;;;
;;; {Methods for Writing}
;;;

(define-method (write (closure <gclosure>) file)
  (let* ((class (class-of closure)))
    (display "#<gclosure " file)
    (display (class-name class) file)
    (display #\space file)
    (display (slot-ref closure 'return-type) file)
    (display " - " file)
    (display (slot-ref closure 'param-types) file)
    (display #\> file)))

;;;
;;; {Miscellaneous}
;;;

(define-method (gclosure-invoke (closure <gclosure>) . args)
  (let* ((primitive-closure (slot-ref closure 'closure))
	 (return-type (slot-ref closure 'return-type))
	 (param-types (slot-ref closure 'param-types))
	 (num-params (vector-length param-types)))
    (or (eq? (length args) (vector-length param-types))
	(gruntime-error "Wrong number of arguments"))
    (let* ((params (do ((index 0 (+ index 1))
			(params (make-vector num-params #f)))
		       ((>= index num-params) params)
		     (let* ((value-type (vector-ref param-types index))
			    (init-value (list-ref args index))
			    (value (scm->gvalue value-type init-value)))
		       (vector-set! params index value))))
	   (retval (gclosure-primitive-invoke primitive-closure
                                              return-type params)))
      (if (not (unspecified? retval))
          (gvalue->scm retval)))))
