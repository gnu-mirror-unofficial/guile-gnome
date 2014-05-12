;; guile-gnome
;; Copyright (C) 2005 Andy Wingo <wingo at pobox dot com>

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
;;G-Wrap compatibility layer, to ensure that all wrapsets have the right
;;generic functions defined.
;;
;;; Code:

(define-module (gnome gw support g-wrap)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap scm-codegen)
  #:use-module (gnome gw support modules))

(eval-when (expand load eval)
  (re-export-modules (g-wrap)
		     (g-wrap guile)
		     (g-wrap c-codegen)
		     (g-wrap scm-codegen)))
