;; wrap gdk

(define-module (gnome gtk gw-gdk-spec)
  :use-module (g-wrap)
  :use-module (gnome gtk gw-pango-spec)
  :use-module (gnome gobject defs-support))

(let ((ws (gw:new-wrapset "guile-gnome-gw-gdk")))

  (gw:wrapset-set-guile-module! ws '(gnome gtk gw-gdk))
  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-glib")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gobject")
  (gw:wrapset-depends-on ws "guile-gnome-gw-pango")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (eq? client-wrapset wrapset)
         '()
         (list
          "#include <gdk/gdk.h>\n"))))

  (gw:wrapset-add-cs-wrapper-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         '("gdk_init (NULL, NULL);\n")
         '())))

  (register-type "guile-gnome-gw-gdk" "GdkWChar" '<gw:unsigned-long>)

  ;; a hack now -- dunno what to do with this...
  (register-type "guile-gnome-gw-gdk" "GdkNativeWindow" '<gw:unsigned-long>)

  (load-defs ws "gnome/defs/gdk.defs"))

