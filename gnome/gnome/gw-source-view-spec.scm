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
;;g-wrap specification for libgtksourceview.
;;
;;; Code:

(define-module (gnome gnome gw-source-view-spec)
  :use-module (oop goops)
  :use-module (g-wrap)
  :use-module (g-wrap guile)
  :use-module (gnome gobject gw-gobject-spec)
  :use-module (gnome gtk gw-gtk-spec)
  :use-module (gnome gobject gw-spec-utils)
  :use-module (gnome gobject defs-support))

(define-class <gnome-source-view-wrapset> (<gobject-wrapset-base>)
  #:language guile #:id 'gnome-source-view)

(define-method (initialize (ws <gnome-source-view-wrapset>) initargs)
  (next-method ws (append '(#:module (gnome gnome gw-source-view)) initargs))

  (depends-on! ws
               'standard 'gnome-glib 'gnome-gobject
               'gnome-atk 'gnome-gdk 'gnome-pango 'gnome-gtk)

  (add-cs-global-declarator!
   ws (lambda (lang)
        (list "#include <vte/vte.h>\n")))
  
  (add-cs-global-declarator!
   ws (lambda (lang)
        (list
         "#include <gtksourceview/gtksourceview.h>\n"
         "#include <gtksourceview/gtksourceview-typebuiltins.h>\n"
         "#include <gtksourceview/gtksourceprintjob.h>\n"
         "#include <gtksourceview/gtksourcetag.h>\n"
         "#include <gtksourceview/gtksourcelanguagesmanager.h>\n"
         "#include \"source-view-support.h\"\n")))

  (add-client-cs-global-declarator!
   ws (lambda (lang)
        (list
         "#include <gtksourceview/gtksourceview.h>\n"
         "#include <gtksourceview/gtksourceview-typebuiltins.h>\n"
         "#include <gtksourceview/gtksourceprintjob.h>\n"
         "#include <gtksourceview/gtksourcetag.h>\n"
         "#include <gtksourceview/gtksourcelanguagesmanager.h>\n")))

  (load-defs ws "gnome/defs/gtksourceview.defs"))
