;; wrap gtk2

(define-module (gnome gtk gw-pango-spec)
  :use-module (g-wrap)
  :use-module (gnome gobject gw-glib-spec)
  :use-module (gnome gobject gw-gobject-spec)
  :use-module (gnome gobject defs-support))

(let ((ws (gw:new-wrapset "guile-gnome-gw-pango")))

  (gw:wrapset-set-guile-module! ws '(gnome gtk gw-pango))
  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-glib")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gobject")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (eq? client-wrapset wrapset)
         '()
         (list
          "#include <pango/pango.h>\n"))))

  (register-type "guile-gnome-gw-pango" "PangoGlyph" '<gw:unsigned-long>)

  (load-defs ws "gnome/defs/pango.defs"))

