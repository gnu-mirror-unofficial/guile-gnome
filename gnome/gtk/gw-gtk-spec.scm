;; wrap gtk2

(define-module (gnome gtk gw-gtk-spec)
  :use-module (g-wrap)
  :use-module (gnome gtk gw-atk-spec)
  :use-module (gnome gtk gw-gdk-spec)
  :use-module (gnome gobject defs-support))

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
     (if (eq? client-wrapset wrapset)
         '()
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

  (load-defs ws "gnome/defs/gtk.defs"))
