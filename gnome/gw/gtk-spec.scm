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

(define-module (gnome gw gtk-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw atk-spec)
  #:use-module (gnome gw gdk-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <gtk-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-gtk
  #:dependencies '(standard gnome-glib gnome-gobject
                   gnome-atk gnome-pango gnome-gdk))

(define-method (global-declarations-cg (self <gtk-wrapset>))
  (list (next-method)
        "#include <gtk/gtk.h>\n"
        "#include \"gtk-support.h\"\n"
        "#include \"guile-gtk-tree-model.h\"\n"
        "\n"
        ;; Opaquely wrap groups for radio buttons and menu items
        "#define GtkRadioGroup GSList\n"))

(define-method (global-definitions-cg (self <gtk-wrapset>))
  (list (next-method)
        "static void\n"
        "sink_gtkobject (GTypeInstance *i)\n"
        "{\n"
        "  GObject *object = (GObject*)i;\n"
        "  if (GTK_OBJECT_FLOATING (object)) {\n"
        "    g_object_ref (object);\n"
        "    gtk_object_sink (GTK_OBJECT (object));\n"
        "  }\n"
        "}\n"))
  
(define-method (initializations-cg (self <gtk-wrapset>) err)
  (list
   (next-method)
   "gtk_init (NULL, NULL);\n"
   "scm_register_gtype_instance_sinkfunc (GTK_TYPE_OBJECT, sink_gtkobject);\n"
   "scm_register_gobject_postmakefunc (GTK_TYPE_WINDOW, g_object_ref);\n"
   "scm_register_gobject_postmakefunc (GTK_TYPE_INVISIBLE, g_object_ref);\n"))
  
(custom-wrap-decls
 "GtkTreePath"
 ;; unwrap
 (unwrap-null-checked
  value status-var
  (list c-var " = guile_gtk_scm_to_tree_path (" scm-var ");\n"))
 ;; wrap
 (list scm-var " = guile_gtk_tree_path_to_scm (" c-var ");\n"
       "gtk_tree_path_free (" c-var ");\n"))

(define-method (initialize (ws <gtk-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw gtk) initargs)))
  
  (add-type-alias! ws "GtkType" '<gtype>)

  (wrap-custom-pointer! "GtkTreePath")
  
  (load-defs-with-overrides ws "gnome/defs/gtk.defs"))
