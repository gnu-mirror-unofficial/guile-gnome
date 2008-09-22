;; guile-gnome
;; Copyright (C) 2001 Martin Baulig <martin@gnome.org>
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; This is the Guile wrapper of @code{libgobject}, an implementation of
;; a runtime, dynamic type system for C. Besides providing an object
;; system to C, @code{libgobject}'s main design goal was to increase the
;; ease with which C code can be wrapped by interpreted languages, such
;; as Guile or Perl.
;;
;; This module, @code{(gnome gobject)}, just re-exports procedures from
;; other modules, so its documentation seems an opportune spot for a
;; more tutorial-like introduction. So open up a Guile session and let's
;; begin.
;;
;; First, if you haven't done it, load the appropriate version of
;; Guile-GNOME:
;;
;; @lisp
;; guile> (use-modules (gnome-2))
;; @end lisp
;;
;; Import @code{(gnome gobject)} also:
;;
;; @lisp
;; guile> (use-modules (gnome gobject))
;; @end lisp
;;
;; @code{(gnome gobject)} is based heavily on GOOPS, Guile's object
;; system, so go ahead and load up that too:
;;
;; @lisp
;; guile> (use-modules (oop goops))
;; @end lisp
;;
;; We will leave off the @code{guile>} prompt in the rest of this
;; tutorial. When we want to show the value of an expression, we use
;; @result{}:
;;
;; @lisp
;; (+ 3 5)
;; @result{} 8
;; @end lisp
;;
;; @section Basic types
;;
;; When communicating with @code{libgobject}, most values need to be
;; strictly-typed. There is a type class corresponding to each basic type
;; in C: @code{<gchar>}, @code{<guchar>}, @code{<gboolean>},
;; @code{<gint>}, @code{<guint>}, @code{<glong>}, @code{<gulong>},
;; @code{<gint64>}, @code{<guint64>}, @code{<gfloat>}, @code{<gdouble>},
;; and @code{<gchararray>}.
;;
;; You can make instances of these class with @code{make}:
;;
;; @lisp
;; (make <gboolean> #:value #f)
;; @result{} #<gvalue <gboolean> 40529040 #f>
;;
;; (make <guint> #:value 85)
;; @result{} #<gvalue <guint> 4054f040 85>
;;
;; (make <gfloat> #:value 3.1415)
;; @result{} #<gvalue <gfloat> 40556af0 3.1414999961853>
;;
;; (make <gchararray> #:value "Hello World!")
;; @result{} #<gvalue <gchararray> 4055af90 Hello World!>
;; @end lisp
;;
;; You can get the normal Scheme values back with @code{gvalue->scm}:
;;
;; @lisp
;; (gvalue->scm (make <gchararray> #:value "Hello World!"))
;; @result{} "Hello World!"
;; @end lisp
;;
;; @section Enums and flags
;;
;; Enumerated values and bitflags are an essential part of many C APIs,
;; and so they are specially wrapped in the GLib type system. You can
;; create new enumerated types in Scheme by subclassing @code{<genum>}:
;;
;; @lisp
;; (define-class <foo> (<genum>)
;;   #:vtable '#((hello "Hello World" 1) (test "Test" 2)))
;; @end lisp
;;
;; Instances are created with @code{make}, just like with the other
;; types:
;;
;; @lisp
;; (make <foo> #:value 'hello)
;; (make <foo> #:value "Hello World")
;; (make <foo> #:value 1)
;;
;; ;; These three all do the same thing
;; @result{} #<gvalue <foo> 406275f8 (hello Hello World 1)>
;; @end lisp
;;
;; If there is an already existing enum or flags class, you can get
;; information about it:
;;
;; @lisp
;; (genum-class->value-table <foo>)
;; @result{} #((hello "Hello World" 1) (test "Test" 2))
;; @end lisp
;;
;; Enums and flags have a special representation on the Scheme side. You
;; can convert them to Scheme values as symbols, names, or as a numeric
;; value.
;;
;; @lisp
;; (define foo (make <foo> #:value 'hello))
;; (genum->symbol foo)
;; @result{} hello
;; (genum->name foo)
;; @result{} "Hello World"
;; (genum->value foo)
;; @result{} 1
;; @end lisp
;;
;; @section GType
;;
;; All of the types that GLib knows about are available to Guile,
;; regardless of which language defined them. GLib implements this via a
;; type system, where every type has a name. So if you make a type
;; called ``Foo'' in C, you can get to it in Scheme via
;; @code{gtype-name->class}:
;;
;; @lisp
;; ;; Retrieve the type for the foo enum we made earlier in the tutorial
;; (define copy-of-<foo> (gtype-name->class "Foo"))
;; (eq? <foo> copy-of-<foo>)
;; @result{} #t
;;
;; (make copy-of-<foo> #:value 2)
;; @result{} #<gvalue <foo> 40535e50 (test Test 2)>
;; @end lisp
;;
;; @section GObject
;;
;; @code{<gobject>} (@code{GObject} in C) is the basic object type in
;; @code{libgobject}. @code{(gnome gobject)} allows you to access
;; existing GObject types, as well as to create new GObject types in
;; Scheme.
;;
;; Before we start, let's pull in some generic functions that reduce the
;; amount of typing we have to do:
;;
;; @lisp
;; (use-modules (gnome gobject generics))
;; @end lisp
;;
;; Let's assume we start with @code{<gtk-window>} from @code{(gnome
;; gtk)}. The keyword arguments to @code{make} are interpreted as
;; GObject properties to set:
;;
;; @lisp
;; (define window (make <gtk-window>
;;                  #:type 'toplevel #:title "Hello, World!"))
;; @end lisp
;;
;; You can connect to signals on the new instance:
;;
;; @lisp
;; (connect window 'delete-event
;;          (lambda (window event)
;;            ;; Returns #t to ignore this event
;;            #t))
;;
;; ;; connect is a generic function implemented by
;; ;; gtype-instance-signal-connect
;; @end lisp
;;
;; And get and set properties...
;;
;; @lisp
;; (get window 'title)
;; @result{} "Hello, World!"
;; (set window 'resizable #f)
;;
;; ;; get and set are also generics, implemented by gobject-get-property
;; ;; and gobject-set-property
;; @end lisp
;;
;; @section Deriving your own GObject types
;;
;; You can create new GObject types directly from Scheme, deriving either
;; from a C object type or one you made in Scheme.
;;
;; @lisp
;; ;; deriving from <gobject>
;; (define-class <test> (<gobject>)
;;   ;; a normal object slot
;;   my-data
;;
;;   ;; an object slot exported as a gobject property
;;   (pub-data #:gparam (list <gparam-long> #:name 'test))
;;
;;   ;; a signal with no arguments and no return value
;;   #:gsignal '(frobate #f))
;;
;; ;; deriving from <test> -- also inherits properties and signals
;; (define-class <hungry> (<test>))
;; @end lisp
;;
;; Adding a signal automatically defines the default method:
;;
;; @lisp
;; ;; This is the default handler for this signal.
;; (define-method (test:frobate (object <test>))
;;   (format #t "Frobating ~A\n" object))
;;
;; ;; We can override it for subclasses
;; (define-method (test:frobate (object <hungry>))
;;   (next-method) ;; chain up
;;   (format #t "I'm hungry\n"))
;;
;; (emit (make <hungry>) 'frobate)
;; ;; Try it!
;; @end lisp
;;
;; You can override the @code{initialize}, @code{gobject:get-property},
;; and @code{gobject:set-property} methods. For an extended example, see
;; @code{tic-tac-toe.scm} in the @code{gtk/examples/gtk} directory of
;; the distribution.
;;
;;; Code:

(define-module (gnome gobject)
  #:use-module (gnome gobject gtype)
  #:use-module (gnome gobject gvalue)
  #:use-module (gnome gobject gclosure)
  #:use-module (gnome gobject gsignal)
  #:use-module (gnome gobject gparameter)
  #:use-module (gnome gobject gobject)
  #:use-module (gnome gw support modules))

(re-export-modules (gnome gobject gtype)
                   (gnome gobject gvalue)
                   (gnome gobject gclosure)
                   (gnome gobject gsignal)
                   (gnome gobject gparameter)
                   (gnome gobject gobject))

;(let* ((doc-dir (gobject-scheme-dir))
;       (doc-file (in-vicinity doc-dir "guile-gnome-gobject-procedures.txt")))
;  (set! documentation-files (append! documentation-files (list doc-file))))
