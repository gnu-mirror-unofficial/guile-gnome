#! /bin/sh
# -*- scheme -*-
exec guile-gnome-2 -s $0 "$@"
!#
;; guile-gnome
;; Copyright (C) 2003,2004 Free Software Foundation, Inc.

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


(use-modules (oop goops) (gnome gobject) (gnome gtk))

;; define the hello as a function -- there are many other ways to do this,
;; of course...
(define (hello)
  ;; we can make new widgets just like we make goops objects -- there is
  ;; a corresponding goops class for every GType we know about. the
  ;; arguments to make, after the class, are interpreted as properties
  ;; to set. in this case we make a toplevel window and a button with
  ;; the label, "Hello, World!".
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (button (make <gtk-button> #:label "Hello, World!")))

    ;; there are scheme functions corresponding to all of the c ones...
    ;; and of course we don't have to cast anything.
    (gtk-container-set-border-width window 10)
    (gtk-container-add window button)
    
    ;; and of course you can attach a lambda to a signal :-)
    (gtype-instance-signal-connect button 'clicked (lambda (b) (gtk-main-quit)))

    (gtk-widget-show-all window)

    ;; this will block until gtk-main-quit is called...
    (gtk-main)))

;; meaning this blocks until the button is clicked.
(hello)
