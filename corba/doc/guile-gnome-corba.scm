;; About the package
(define *name* "Guile-GNOME: CORBA")
(define *description* "Remote objects with CORBA in Scheme")
(define *version* "2.15.98")
(define *updated* "27 April 2008")
(define *authors*
  '(("Andy Wingo" . "wingo at pobox.com")
    ("Martin Baulig" . "baulig at suse.de")))

;; Copying the documentation
(define *copyright-holder* "Free Software Foundation")
(define *years* '(2001 2003 2004 2008))
(define *permissions*
  "Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU General Public License, Version 2 or any
later version published by the Free Software Foundation.")

;; Texinfo info
(define *texinfo-basename* "guile-gnome-corba")
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
  '(((gnome corba)
     "The CORBA wrapper")
    ((gnome corba primitives)
     "Primitive functions")
    ((gnome corba types)
     "Primtive types")))

(define *module-sources* '())
