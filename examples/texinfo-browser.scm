#! /bin/sh
# -*- scheme -*-
exec guile-gnome-0 -s $0 "$@"
!#
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

(define (usage)
  (display "Usage: texinfo-browser.scm TEXINFO-FILE\n")
  (exit 1))

(or (= (length (command-line)) 2)
    (usage))

(use-modules (gnome glib)
             (gnome gtk)
             (texinfo)
             (texinfo nodal-tree)
             (gnome contrib help-browser))

(let* ((texinfo-file (cadr (command-line)))
       (stexinfo (call-with-file-and-dir texinfo-file texi->stexi))
       (nodes (stexi->nodal-tree stexinfo 3)))

  (add-help-root! nodes)

  (show-help)

  (connect the-help-window 'delete-event
           (lambda args (exit 0)))

  (g-main-loop-run (g-main-loop-new #f #f)))
