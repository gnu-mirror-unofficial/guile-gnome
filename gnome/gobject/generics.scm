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
;;Generic functions for procedures in the (gnome gobject) module.
;;
;;; Code:

(define-module (gnome gobject generics)
  :use-module (gnome gobject)
  :use-module (oop goops))

(define exports '(get set emit connect connect-after block unblock
                  disconnect connected? equal? invoke create-signal get-signals
                  get-properties get-property-names find-property))

;; make sure we have generic methods
(for-each
 (lambda (sym)
   (let* ((existing-var (module-variable (current-module) sym))
          (existing-val (if existing-var (variable-ref existing-var) #f)))
     (module-add! (current-module) sym (make-variable 
                                        (ensure-generic existing-val sym)))))
 exports)

(define-method (get (object <gobject>) (name <symbol>))
  (gobject-get-property object name))

(define-method (set (object <gobject>) (name <symbol>) value)
  (gobject-set-property object name value))

(define-method (emit (object <gtype-instance>) (name <symbol>) . args)
  (apply gtype-instance-signal-emit (append (list object name) args)))

;; This one shadows 'connect' from (guile). Oh well!
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
  (apply gclosure-invoke (cons closure args)))

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

;; Put these generics in the root module for all to enjoy
(for-each
 (lambda (sym)
   (module-define! the-root-module sym (module-ref (current-module) sym)))
 exports)
