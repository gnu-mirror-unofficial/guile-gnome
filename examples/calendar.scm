;; guile-gnome
;; Copyright (C) 2001,2002,2003,2004 Free Software Foundation, Inc.

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

(use-modules (gnome gtk))

(define (calendar-example)
 (let ((window (make <gtk-window> #:type 'toplevel))
       (calendar (make <gtk-calendar>)))
   (connect window 'delete-event (lambda (w e) (gtk-main-quit) #f))
   (add window calendar)
   (show-all window)
   (gtk-main)))

(calendar-example)

;;Ariel Rios, Andy Wingo
