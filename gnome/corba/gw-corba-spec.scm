;;; -*-scheme-*-

(define-module (gnome corba gw-corba-spec)
  :use-module (g-wrap)
  :use-module (gnome gobject gw-standard-spec)
  :use-module (g-wrap simple-type))

;; gw-corba: an internal glue binding, probably not useful to other people...

(let ((ws (gw:new-wrapset "guile-gnome-gw-corba")))

  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")

  (gw:wrapset-set-guile-module! ws '(gnome corba gw-corba))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <guile-gnome-corba.h>\n")))

  (gw:wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         (list "scm_pre_init_gnome_corba_generic ();\n"
               "scm_pre_init_gnome_corba_types ();\n"
               "scm_pre_init_gnome_corba_primitives ();\n")
         '())))

  ;; Here we wrap some functions to bootstrap the core library.

  (gw:wrap-function
   ws
   '%init-gnome-corba
   '<gw:void>
   "scm_init_gnome_corba"
   '()
   "Export a number of fundamental gtypes and functions to operate on objects.")

  (gw:wrap-function
   ws
   '%init-gnome-corba-primitives
   '<gw:void>
   "scm_init_gnome_corba_primitives"
   '()
   "Export a number of fundamental gtypes and functions to operate on objects.")

  (gw:wrap-function
   ws
   '%init-gnome-corba-types
   '<gw:void>
   "scm_init_gnome_corba_types"
   '()
   "Export a number of fundamental gtypes and functions to operate on objects."))
