(define-module (gnome corba types)
  :use-module (gnome corba gw-corba)
  :use-module (gnome gobject)
  :use-module (oop goops))

(define (gnome-corba-error format-string . args)
  (save-stack)
  (scm-error 'gnome-corba-error #f format-string args '()))

(%init-gnome-corba-types)

(export gnome-corba-error)
