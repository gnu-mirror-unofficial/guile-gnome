;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
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
;; This is the GObject wrapper for Guile.
;;
;; See the guile-gnome tutorial for more details.
;;
;;; Code:

(define-module (gnome gobject)
  :use-module (gnome gobject gtype)
  :use-module (gnome gobject gvalue)
  :use-module (gnome gobject gclosure)
  :use-module (gnome gobject gsignal)
  :use-module (gnome gobject gparameter)
  :use-module (gnome gobject gobject)
  :use-module (gnome gobject gw-utils))

(re-export-modules (gnome gobject gtype)
                   (gnome gobject gvalue)
                   (gnome gobject gclosure)
                   (gnome gobject gsignal)
                   (gnome gobject gparameter)
                   (gnome gobject gobject))

;(let* ((doc-dir (gobject-scheme-dir))
;       (doc-file (in-vicinity doc-dir "guile-gnome-gobject-procedures.txt")))
;  (set! documentation-files (append! documentation-files (list doc-file))))
