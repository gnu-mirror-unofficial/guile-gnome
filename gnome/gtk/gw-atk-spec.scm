;; wrap atk

(define-module (gnome gtk gw-atk-spec)
  :use-module (g-wrap)
  :use-module (gnome gobject gw-gobject-spec)
  :use-module (gnome gobject defs-support))

(let ((ws (gw:new-wrapset "guile-gnome-gw-atk")))

  (gw:wrapset-set-guile-module! ws '(gnome gtk gw-atk))
  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-glib")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gobject")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (eq? client-wrapset wrapset)
         '()
         (list
          "#include <atk/atk.h>\n"
          "#include <atk/atk-enum-types.h>\n"))))

  (register-type "guile-gnome-gw-atk" "AtkState" '<gw:long-long>)

  (load-defs ws "gnome/defs/atk.defs"))

