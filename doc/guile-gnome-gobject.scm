;; About the package
(define *name* "Guile-GNOME: GObject")
(define *description* "The GLib object system in Scheme")
(define *version* "2.15.93")
(define *updated* "25 August 2007")
(define *authors*
  '(("Andy Wingo" . "wingo at pobox.com")
    ("Martin Baulig" . "baulig at suse.de")))

;; Copying the documentation
(define *copyright-holder* "Free Software Foundation")
(define *years* '(2003 2004 2005 2006 2007))
(define *permissions*
  "Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU General Public License, Version 2 or any
later version published by the Free Software Foundation.")

;; Texinfo info
(define *texinfo-basename* "guile-gnome-gobject")
(define *texinfo-category* "The Algorithmic Language Scheme")
(define *extra-texinfo-menu-entries*
  '(("Function Index")))
(define *texinfo-epilogue*
  `((node (% (name "Function Index")))
    (unnumbered "Function Index")
    (printindex (% (type "fn")))))

;; HTML foo
(define *html-relative-root-path* "../../../")
(define *extra-html-entry-files*
  '()) ;("scripts.texi" "org-to-pdf-presentation"
     ;"Make PDF presentations from Org Mode files")))

;; The modules to document
(define *modules*
  '(((gnome gobject gtype)
     "The base of the GObject type system")
    ((gnome gobject gvalue)
     "Generic boxed values")
    ((gnome gobject gparameter)
     "Parameters with constraints and default values")
    ((gnome gobject gclosure)
     "Language-portable closures")
    ((gnome gobject gsignal)
     "Using closures as extension points")
    ((gnome gobject gobject)
     "GLib's main object implementation")
    ((gnome gobject generics)
     "Shorthand for many common GObject operations")
    ((gnome gobject utils)
     "Miscellaneous useful functions")
    ((gnome gw support gobject)
     "Integration between G-Wrap and GObject types")
    ((gnome gw support defs)
     "Create G-Wrap wrapsets from ``defs'' files")
    ((gnome gw support gtk-doc)
     "Parse C documentation from gtk-doc into texinfo")
    ((gnome gw support modules)
     "Fondling Guile's module system")))

(define *module-sources* '())
