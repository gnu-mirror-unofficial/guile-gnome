(define-module (gnome gtk))

(display "(gnome gtk): [")

(display "glib ")
(use-modules (gnome glib))
(display "gobject ")
(use-modules (gnome gobject))
(display "generics ")
(use-modules (gnome gobject generics))
(display "gw-gdk ")
(use-modules (gnome gtk gw-gdk))
(display "gw-gtk ")
(use-modules (gnome gtk gw-gtk))
(display "generics ")
(use-modules (gnome gtk generics))
(use-modules (gnome gobject gw-utils))

(display "exports")

;; re-export everything you need to have a nice gtk session...

(re-export-bindings (gnome glib))
(re-export-bindings (gnome gobject))
(re-export-bindings (gnome gobject generics))

;; leaving out bindings for pango and atk, but they're actually loaded
;; up by gw-gtk -- just use them explicitly if you want them...
;;
;; (re-export-bindings (gnome gtk gw-atk))
;; (re-export-bindings (gnome gtk gw-pango))

(re-export-bindings (gnome gtk gw-gdk))
(re-export-bindings (gnome gtk gw-gtk))
(re-export-bindings (gnome gtk generics))
(re-export-bindings (oop goops))

(display "]\n")
