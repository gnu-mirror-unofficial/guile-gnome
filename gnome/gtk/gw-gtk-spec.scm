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
;;g-wrap specification for GTK+.
;;
;;; Code:

(define-module (gnome gtk gw-gtk-spec)
  :use-module (g-wrap)
  :use-module (gnome gtk gw-atk-spec)
  :use-module (gnome gtk gw-gdk-spec)
  :use-module (gnome gobject defs-support)
  :use-module (gnome gobject gw-spec-utils))

(let ((ws (gw:new-wrapset "guile-gnome-gw-gtk")))

  (gw:wrapset-set-guile-module! ws '(gnome gtk gw-gtk))
  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-glib")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gobject")
  (gw:wrapset-depends-on ws "guile-gnome-gw-atk")
  (gw:wrapset-depends-on ws "guile-gnome-gw-pango")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gdk")

  (register-type "guile-gnome-gw-gtk" "GtkType" '<gtype>)

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (not client-wrapset)
         '("#include <gtk/gtk.h>\n"
           "#include \"gtk-support.h\"\n"
           "#include \"guile-gtk-tree-model.h\"\n")
         '("#include <gtk/gtk.h>\n"))))

  (gw:wrapset-add-cs-definitions!
   ws
   (lambda (wrapset client-wrapset)
     (if (not client-wrapset)
         '("static void\n"
           "sink_gtkobject (GObject *object)\n"
           "{\n"
           "  if (GTK_OBJECT_FLOATING (object)) {\n"
           "    g_object_ref (object);\n"
           "    gtk_object_sink (GTK_OBJECT (object));\n"
           "  }\n"
           "}\n")
         '())))

  (gw:wrapset-add-cs-wrapper-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         '("gtk_init (NULL, NULL);\n"
           "guile_gobject_register_sinkfunc (GTK_TYPE_OBJECT, sink_gtkobject);\n"
           "guile_gobject_register_postmakefunc (GTK_TYPE_WINDOW, g_object_ref);\n"
           "guile_gobject_register_postmakefunc (GTK_TYPE_INVISIBLE, g_object_ref);\n")
         '())))

  (gobject:gwrap-helper-with-class
   ws "GTK_TYPE_TREE_PATH" "GtkTreePath"
   (lambda (typespec) "GtkTreePath*")
   (lambda (c-var scm-var typespec status-var)
     (list "if (!(" c-var " = guile_gtk_scm_to_tree_path (" scm-var ")))\n"
           "  " `(gw:error ,status-var type ,scm-var)))
   (lambda (scm-var c-var typespec status-var)
     (list scm-var " = guile_gtk_tree_path_to_scm (" c-var ");\n"
           "gtk_tree_path_free (" c-var ");\n"))
   (lambda (c-var typespec status-var force?)
     (list))
   "Custom")
  (register-type "guile-gnome-gw-gtk" "GtkTreePath*" '<gtk-tree-path>)

  ;; Opaquely wrap groups for radio buttons and menu items
  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (not client-wrapset)
         '("#define GtkRadioGroup GSList\n")
         '())))

  (load-defs ws "gnome/defs/gtk.defs"))
