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
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gtk gw-atk-spec)
  #:use-module (gnome gtk gw-gdk-spec)
  #:use-module (gnome gobject defs-support)
  #:use-module (gnome gobject gw-spec-utils))

(define-class <gtk-wrapset> (<gobject-wrapset-base>)
  #:language guile #:id 'gnome-gtk)

(define-method (initialize (ws <gtk-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gtk gw-gtk) initargs)))
  
  (depends-on! ws 'standard 'gnome-glib 'gnome-gobject 'gnome-atk 'gnome-pango
               'gnome-gdk)
  
  (add-type-alias! ws "GtkType" '<gtype>)

  (add-cs-global-declarator! ws
                             (lambda (wrapset)
                               '("#include <gtk/gtk.h>\n"
                                 "#include \"gtk-support.h\"\n"
                                 "#include \"guile-gtk-tree-model.h\"\n")))

  (add-cs-definer! ws
                   (lambda (wrapset)
                     '("static void\n"
                       "sink_gtkobject (GObject *object)\n"
                       "{\n"
                       "  if (GTK_OBJECT_FLOATING (object)) {\n"
                       "    g_object_ref (object);\n"
                       "    gtk_object_sink (GTK_OBJECT (object));\n"
                       "  }\n"
                       "}\n")))
  
  (add-cs-initializer!
   ws
   (lambda (wrapset status-var)
     '("gtk_init (NULL, NULL);\n"
       "guile_gobject_register_sinkfunc (GTK_TYPE_OBJECT, sink_gtkobject);\n"
       "guile_gobject_register_postmakefunc (GTK_TYPE_WINDOW, g_object_ref);\n"
       "guile_gobject_register_postmakefunc (GTK_TYPE_INVISIBLE, g_object_ref);\n")))
  
  (add-type! ws (make <gtk-tree-path-type>
                  #:gtype-id "GTK_TYPE_TREE_PATH" 
                  #:ctype "GtkTreePath"
                  #:c-type-name "GtkTreePath*"
                  #:c-const-type-name "GtkTreePath*"
                  #:ffspec 'pointer
                  #:wrapped "Custom"))
  
  (add-type-alias! ws "GtkTreePath*" '<gtk-tree-path>)
  
  ;; Opaquely wrap groups for radio buttons and menu items
  (add-cs-global-declarator!
   ws
   (lambda (wrapset)
     '("#define GtkRadioGroup GSList\n")))

  (load-defs ws "gnome/defs/gtk.defs"))

(define-class <gtk-tree-path-type> (<gobject-type-base>))

(define-method (unwrap-value-cg (lang <gw-guile>)
                                (type <gtk-tree-path-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list "if (!(" c-var " = guile_gtk_scm_to_tree_path (" scm-var ")))\n"
          "  " `(gw:error ,status-var type ,scm-var))))

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <gtk-tree-path-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list scm-var " = guile_gtk_tree_path_to_scm (" c-var ");\n"
          "gtk_tree_path_free (" c-var ");\n")))
