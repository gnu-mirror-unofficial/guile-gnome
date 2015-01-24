;; guile-gnome
;; Copyright (C) 2003,2004,2015 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>

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

(define-module (gnome corba types)
  :use-module (gnome gw corba)
  :use-module (gnome gobject)
  :use-module (oop goops)
  :export (gnome-corba-error))

(define (gnome-corba-error format-string . args)
  (scm-error 'gnome-corba-error #f format-string args '()))

(eval-when (load eval)
  (%init-gnome-corba-types))
