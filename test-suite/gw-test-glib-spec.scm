(define-module (gw-test-glib-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (gnome gw glib-spec)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <test-glib-wrapset> (<gobject-wrapset-base>)
  #:id 'test-glib)

(define-method (global-declarations-cg (ws <test-glib-wrapset>))
  (list (next-method)
        "#include \"test-glib.h\"\n"))

(define-method (initialize (ws <test-glib-wrapset>) initargs)
  (next-method ws (append '(#:module (test-suite gw-test-glib)) initargs))
  
  (depends-on! ws 'standard 'gnome-glib)

  (load-defs ws "test-suite/test-glib.defs"))

