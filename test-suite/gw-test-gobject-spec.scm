(define-module (gw-test-gobject-spec)
  #:use-module (g-wrap)
  #:use-module (g-wrap gw-standard-spec)
  #:use-module (gnome gobject gw-gobject-spec)
  #:use-module (gnome gobject defs-support))

(let ((ws (gw:new-wrapset "gw-test-gobject")))

  (gw:wrapset-set-guile-module! ws '(test-suite gw-test-gobject))
  
  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gobject")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         "#include \"test-gobject.h\"\n")))

  (load-defs ws "test-suite/test-gobject.defs")
  
  ws)
