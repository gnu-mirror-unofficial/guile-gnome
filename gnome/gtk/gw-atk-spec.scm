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
;;g-wrap specification for ATK.
;;
;;; Code:

(define-module (gnome gtk gw-atk-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject gw-gobject-spec)
  #:use-module (gnome gobject defs-support))

(define-class <atk-wrapset> (<gobject-wrapset-base>)
  #:language guile #:id 'gnome-atk)

(define-method (initialize (ws <atk-wrapset>) initargs)

  (next-method)

  (depends-on! ws 'standard 'gnome-glib 'gnome-gobject)
  
  (set! (module ws) '(gnome gtk gw-atk))

  (add-cs-global-declarator! ws
                             (lambda (wrapset)
                               (list
                                "#include <atk/atk.h>\n"
                                "#include <atk/atk-enum-types.h>\n")))

  (add-type-alias! ws "AtkState" 'long-long)
  
  (load-defs ws "gnome/defs/atk.defs"))
