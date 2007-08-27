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
;;Generic functions for procedures in the (gnome gobject) module. Say
;;something here about the nature of generics in guile-gnome
;;
;;; Code:

(define-module (gnome gobject generics)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject)
  #:use-module (oop goops)
  #:export (get set emit connect connect-after block unblock
            disconnect connected? invoke create-signal get-signals
            get-properties get-property-names find-property))

(define-generic-with-docs get
  "A shorthand for @code{gobject-get-property}.")

(define-generic-with-docs set
  "A shorthand for @code{gobject-set-property}.")

(define-generic-with-docs emit
  "A shorthand for @code{gtype-instance-signal-emit}.")

(define-with-docs connect
  "A shorthand for @code{gtype-instance-signal-connect}, which also
defaults to the core Guile definition of @code{connect}."
  (ensure-generic (module-ref the-scm-module 'connect)))

(define-generic-with-docs connect-after
  "A shorthand for @code{gtype-instance-signal-connect-after}.")

(define-generic-with-docs block
  "A shorthand for @code{gsignal-handler-block}.")

(define-generic-with-docs unblock
  "A shorthand for @code{gsignal-handler-unblock}.")

(define-generic-with-docs disconnect
  "A shorthand for @code{gsignal-handler-disconnect}.")

(define-generic-with-docs connected?
  "A shorthand for @code{gsignal-handler-connected?}")

(define-generic-with-docs invoke
  "A shorthand for @code{gclosure-invoke}.")

(define-generic-with-docs create-signal
  "A shorthand for @code{gtype-class-create-signal}.")

(define-generic-with-docs get-signals
  "A shorthand for @code{gtype-class-get-signals}.")

(define-generic-with-docs get-properties
  "A shorthand for @code{gobject-class-get-properties}.")

(define-generic-with-docs get-property-names
  "A shorthand for @code{gobject-class-get-property-names}.")

(define-generic-with-docs find-property
  "A shorthand for @code{gobject-class-find-property}.")

(define-method (get (object <gobject>) (name <symbol>))
  (gobject-get-property object name))

(define-method (set (object <gobject>) (name <symbol>) value)
  (gobject-set-property object name value))

(define-method (emit (object <gtype-instance>) (name <symbol>) . args)
  (apply gtype-instance-signal-emit object name args))

(define-method (connect (object <gtype-instance>) (name <symbol>) (func <procedure>))
  (gtype-instance-signal-connect object name func))

(define-method (connect-after (object <gtype-instance>) (name <symbol>) (func <procedure>))
  (gtype-instance-signal-connect-after object name func))

(define-method (block (object <gtype-instance>) id)
  (gsignal-handler-block object id))

(define-method (unblock (object <gtype-instance>) id)
  (gsignal-handler-unblock object id))

(define-method (disconnect (object <gtype-instance>) id)
  (gsignal-handler-disconnect object id))

(define-method (connected? (object <gtype-instance>) id)
  (gsignal-handler-connected? object id))

(define-method (equal? (o1 <gobject>) (o2 <gobject>))
  (eq? (slot-ref o1 'gtype-instance) (slot-ref o2 'gtype-instance)))

(define-method (invoke (closure <gclosure>) . args)
  (apply gclosure-invoke closure args))

(define-method (create-signal (class <gtype-class>) (name <symbol>) return-type param-types)
  (gtype-class-create-signal class name return-type param-types))

;; this one's a macro, i'm too lazy to make the method now...
;;(define-method (define-signal (class <gtype-class>) return-type . param-types)

(define-method (get-signals (class <gtype-class>))
  (gtype-class-get-signals class))

(define-method (get-properties (class <gtype-class>))
  (gobject-class-get-properties class))

(define-method (get-property-names (class <gtype-class>))
  (gobject-class-get-property-names class))

(define-method (find-properties (class <gtype-class>))
  (gobject-class-find-properties class))
