(define-module (gw-test-gobject-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <test-gobject-wrapset> (<gobject-wrapset-base>)
  #:language guile #:id 'test-gobject)

(define-method (initialize (ws <test-gobject-wrapset>) initargs)

  (next-method ws (append '(#:module (test-suite gw-test-gobject)) initargs))
  
  (depends-on! ws 'standard 'gnome-gobject)

  (add-cs-global-declarator!
   ws
   (lambda (lang)
     (list "#include \"test-gobject.h\"\n")))
  
  (load-defs ws "test-suite/test-gobject.defs"))

