(define-module (gw-test-gobject-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <test-gobject-wrapset> (<gobject-wrapset-base>)
  #:id 'test-gobject #:dependencies '(standard gnome-gobject))

(define-method (global-declarations-cg (ws <test-gobject-wrapset>))
  (list
   (next-method)
   "#include \"test-gobject.h\"\n"))

(define-method (initialize (ws <test-gobject-wrapset>) initargs)
  (next-method ws (append '(#:module (test-suite gw-test-gobject)) initargs))
  
  (load-defs ws "test-suite/test-gobject.defs"))

