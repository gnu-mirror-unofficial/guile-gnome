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
;;This module implements some miscellaneous useful procedures.
;;
;;; Code:

(define-module (gnome gobject gw-utils)
  :use-module (srfi srfi-13)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (ice-9 syncase)
  :export (re-export-module))

(define-syntax re-export-module
  (syntax-rules ()
    ((_ module)
     (module-use! (module-public-interface (current-module))
                  (resolve-interface 'module)))))
(set-object-property! re-export-module 'documentation
  "Re-export the public interface of a module; used like
@code{use-modules}.")
