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
;; The GLib type system supports the creation and invocation of
;; ``closures'', objects which can be invoked like procedures. Its
;; infrastructure allows one to pass a Scheme function to C, and have C
;; call into Scheme, and vice versa.
;; 
;; In Scheme, @code{<gclosure>} holds a Scheme procedure, the
;; @code{<gtype>} of its return value, and a list of the
;; @code{<gtype>}'s of its arguments. Closures can be invoked with
;; @code{gclosure-invoke}.
;;
;; However since on the C level, closures do not carry a description of
;; their argument and return types, when we invoke a closure we have to
;; be very explicit about the types involved. For example:
;; 
;; @lisp
;; (gclosure-invoke (make <gclosure>
;;                   #:return-type <gint>
;;                   #:param-types (list <gulong>)
;;                   #:func (lambda (x) (* x x)))
;;                  <gulong>
;;                  (scm->gvalue <gulong> 10))
;; @result{} 100
;; @end lisp
;;; Code:

(define-module (gnome gobject gclosure)
  #:use-module (gnome gobject config)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject gtype)
  #:use-module (gnome gobject gvalue)
  #:use-module (oop goops)

  #:export     (<gclosure> gclosure-invoke))

(dynamic-call "scm_init_gnome_gobject_closures"
              (dynamic-link *guile-gnome-gobject-lib-path*))

(define-class-with-docs <gclosure> (<gboxed>)
  "The Scheme representation of a GLib closure: a typed procedure
object that can be passed to other languages." ;; FIXME say something about initargs
  #:gtype-name "GClosure")

;;;
;;; {Instance Allocation and Initialization}
;;;

(define-method (initialize (closure <gclosure>) initargs)
  ;; don't chain up, we do our own init
  (let ((return-type (get-keyword #:return-type initargs #f))
        (param-types (get-keyword #:param-types initargs '()))
        (func (get-keyword #:func initargs #f)))
    (%gclosure-construct closure return-type param-types func)))
