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
;;Guile wrappers for libgnome only (not GTK, not libgnomeui, ...).
;;
;;; Code:

(define-module (gnome gnome)
  :use-module (gnome gnome gw-gnome)
  :use-module (gnome gobject gw-utils)
  :use-module (gnome gobject)
  :use-module (oop goops)
  :export (gnome-program-init))

(re-export-modules (gnome gnome gw-gnome))

(define (gnome-program-init name version . properties)
  (let ((program (%gnome-program-init name version)))
    (if (not (even? (length properties)))
        (scm-error 'gruntime-error "Invalid property list: ~A" properties))
    (let loop ((props properties))
      (if (null? props)
          program
          (begin
            (set program (car props) (cadr props))
            (loop (cddr props)))))))
