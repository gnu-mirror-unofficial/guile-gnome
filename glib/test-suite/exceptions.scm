;; guile-gnome
;; Copyright (C) 2003,2004 Andreas Rottmann <a.rottmann gmx.at>

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

(define-module (test-suite exceptions)
  #:export (exception:value-arg-missing
            exception:wrong-number-of-args))

(defmacro deferr (name-frag error re)
  (let ((name (symbol-append 'exception: name-frag)))
    `(define ,name (cons ,error ,re))))

(deferr value-arg-missing     'gruntime-error "^Missing #:value argument")
(deferr wrong-number-of-args  'gruntime-error "^Wrong number of arguments")
