;; guile-gnome
;; Copyright (C) 2003,2004-2005 Andy Wingo <wingo at pobox dot com>

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
;; This module exists so that all (gnome gw) modules have a common place
;; to put their generic functions. Whenever a wrapset is loaded, it adds
;; method definitions to generics defined in this module.
;;
;;; Code:

(define-module (gnome gw generics)
  #:use-module (gnome gobject))

(let ((mod (current-module)))
  (set-module-binder!
   (module-public-interface mod)
   (lambda (interface sym define?)
     (let ((var (module-local-variable mod sym)))
       (if var (module-add! interface sym var))
       var))))
