;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2014 David Pirotte <david at altosw dot be>

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

(define-module (gnome corba primitives)
  :use-module (gnome gw corba)
  :use-module (gnome corba types)
  :use-module (gnome gobject)
  :use-module (oop goops)

  :export (<PortableServer-ServantBase>
	   <CORBA:Object>))

(define-class <PortableServer-ServantBase> (<class>)
  (%orbit-iinterface #:allocation #:each-subclass)
  (servant))

(define-class <CORBA:Object> (<class>)
  (corba-typecode #:allocation #:each-subclass)
  (corba-objref))

(eval-when (expand load eval)
  (%init-gnome-corba-primitives))

(define-method (allocate-instance (class <PortableServer-ServantBase>) initargs)
  (corba-primitive-make-poa-instance class))

(define-method (allocate-instance (type <CORBA:Object>) initargs)
  (if (get-keyword #:dsupers initargs #f)
      (next-method)
      (let* ((object (next-method))
	     (servant (get-keyword #:servant initargs *unspecified*))
	     (ior (get-keyword #:ior initargs *unspecified*)))
	(gnome-corba-error "Can't make instances of this type: ~A" type))))

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (write (o <CORBA:Object>) file)
  (let ((class (class-of o)))
    (if (slot-bound? class 'name)
	(begin
	  (display "#<" file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))
