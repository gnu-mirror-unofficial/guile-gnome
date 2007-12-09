#! /bin/sh
# -*- scheme -*-
exec guile-gnome-0 -s $0 "$@"
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

(define (hello)
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (button (make <gtk-button> #:label "Hello, World!")))

    ;; up to here, everything is the same as hello.scm. however, now we
    ;; can make use of generic functions:

    ;; since window is a container, this generic maps onto the function
    ;; gtk-container-set-border-width
    (set-border-width window 10)

    ;; note that we can set the border width with a gobject property as
    ;; well:
    (gobject-set-property window 'border-width 15)

    ;; (gnome gobject generics), re-exported by (gnome gtk), defines a
    ;; generic `set' method for gobject-set-property, se we can also do
    ;; it like this:
    (set window 'border-width 20)

    ;; this is much less typing :-)
    (add window button)
    
    ;; see (gnome gobject generics) for a full list of gobject generic
    ;; functions
    (connect button 'clicked (lambda (b) (gtk-main-quit)))

    ;; generic functions for .defs apis are defined in the .defs files,
    ;; not manually
    (show-all window)

    (gtk-main)))

(hello)
