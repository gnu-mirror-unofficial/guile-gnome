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
;;g-wrap specification for libglade.
;;
;;; Code:

(define-module (gnome gtk gw-glade-spec)
  :use-module (g-wrap)
  :use-module (gnome gtk gw-gtk-spec)
  :use-module (gnome gobject defs-support))

(let ((ws (gw:new-wrapset "guile-gnome-gw-glade")))

  (gw:wrapset-set-guile-module! ws '(gnome gtk gw-glade))
  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-glib")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gobject")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gtk")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (not client-wrapset)
         '("#include <glade/glade.h>\n"
           "#include \"glade-support.h\"\n")
         '("#include <glade/glade.h>\n"))))

  (gw:wrapset-add-cs-wrapper-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         '("glade_set_custom_handler (guile_glade_custom_handler, NULL);\n")
         '())))

  (load-defs ws "gnome/defs/libglade.defs"))
