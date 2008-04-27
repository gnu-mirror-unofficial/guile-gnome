;; guile-gnome
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
;; Generic functions for procedures in the @code{(gnome gobject)}
;; module.
;;
;; @subsection Mapping class libraries to Scheme
;;
;; Guile-GNOME exists to wrap a C library, @code{libgobject}, its types,
;; and the set of libraries that based themselves on the GLib types.
;;
;; Procedure invocation feels very similar in Scheme and in C. For
;; example, the C @code{gtk_widget_show (widget)} transliterates almost
;; exactly to the Scheme @code{(gtk-widget-show widget)}.
;;
;; GLib-based libraries are not random collections of functions,
;; however. GLib-based libraries also implement classes and methods,
;; insofar that it is possible in C. For example, in the above example,
;; @code{show} may be seen to be a method on instances of the
;; @code{<gtk-widget>} class.
;;
;; Indeed, other object-oriented languages such as Python express this
;; pattern directly, translating the @code{show} operation as the
;; pleasantly brief @code{widget.show()}. However this representation of
;; methods as being bound to instances, while common, has a number of
;; drawbacks.
;;
;; The largest drawback is that the method itself is not bound to a
;; generic operation. For example, mapping the @code{show} operation
;; across a set of widgets cannot be done with the straightforward
;; @code{map(show, set)}, because there is no object for the @code{show}
;; operation. Instead the user must locally bind each widget to a
;; variable in order to access a method of the abstract @code{show}
;; operation: @code{map(lambda widget: widget.show(), set)}.
;;
;; Additionally, most languages which express methods as bound to
;; instances only select the method via the type of the first (implicit)
;; argument. The rule for these lanugages is, ``@code{gtk-widget-show}
;; is an applicable method of the @code{show} operation when the first
;; argument to @code{show} is a @code{<gtk-widget>}.'' Note the lack of
;; specification for other arguments; the same object cannot have two
;; applicable methods of the @code{show} operation. A more complete
;; specification would be, ``@code{gtk-widget-show} is an applicable
;; method of the @code{show} operation when applied to one argument, a
;; @code{<gtk-widget>}.'' It is a fine difference, but sometimes
;; important.
;;
;; For these and other reasons, the conventional way to implement
;; generic operations in Lisp has been to define @dfn{generic
;; functions}, and then associate specific methods with those functions.
;; For example, one would write the following:
;;
;; @lisp
;; ;; defining a generic function, and one method implementation
;; (define-generic show)
;; (define-method (show (widget <gtk-widget>))
;;   (gtk-widget-show widget))
;;
;; ;; invoking the generic function
;; (show my-widget)
;; @end lisp
;;
;; One benefit of this approach is that method definitions can be made
;; far away in space and time from type definitions. This leads to a
;; more dynamic environment, in which methods can be added to existing
;; types at runtime, which then can apply to existing instances.
;;
;; @subsection The semantics of generic functions in Guile-GNOME
;;
;; Naturally, there is an impedance mismatch between the conventions
;; used in the C libraries and their Scheme equivalents. Operations in
;; GLib-based libraries do not form a coherent whole, in the sense that
;; there is no place that defines the meaning of an abstract @code{show}
;; operation. For example, @code{gtk-widget-set-state}, which can make a
;; widget become uneditable, and @code{gst-element-set-state}, which can
;; start a video player, would both map to the generic function
;; @code{set-state}, even though they have nothing to do with each other
;; besides their name.
;;
;; There is no conflict here; the methods apply on disjoint types.
;; However there is a problem of modularity, in that @emph{both methods
;; must be defined on the same generic function}, so that
;; @code{(set-state foo bar)} picks the correct method, depending on the
;; types of @var{foo} and @var{bar}.
;;
;; This point leads to the conclusion that @emph{generic functions in
;; Guile-GNOME have no abstract meaning, apart from their names}.
;; Semantically, generics in Guile-GNOME are abbreviations to save
;; typing, not abstract operations with defined meanings.
;;
;; @subsection Practicalities
;;
;; This module defines a number of ``abbreviations'', in the form of
;; generic functions, for operations on types defined in the
;; @code{(gnome gobject)} modules. Generic functions for generated
;; bindings like @code{(gnome gtk)} are defined in another module,
;; @code{(gnome gw generics)}, which re-exports the public bindings from
;; this module.
;;
;;; Code:

(define-module (gnome gobject generics)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject)
  #:use-module (oop goops)
  #:export (get set emit connect connect-after block unblock
            disconnect connected? invoke create-signal get-signals
            get-properties get-property-names find-property))

(define-method (get (object <gobject>) (name <symbol>))
  "A shorthand for @code{gobject-get-property}."
  (gobject-get-property object name))

(define-method (set (object <gobject>) (name <symbol>) value)
  "A shorthand for @code{gobject-set-property}."
  (gobject-set-property object name value))

(define-method (emit (object <gtype-instance>) (name <symbol>) . args)
  "A shorthand for @code{gtype-instance-signal-emit}."
  (apply gtype-instance-signal-emit object name args))

(define %connect (module-ref the-root-module 'connect))
(define-generic-with-docs connect "")
(define-method (connect . args)
  "The core Guile implementation of the connect(2) POSIX call"
  (apply %connect args))
(define-method (connect (object <gtype-instance>) (name <symbol>) (func <procedure>))
  "A shorthand for @code{gtype-instance-signal-connect}."
  (gtype-instance-signal-connect object name func))

(define-method (connect-after (object <gtype-instance>) (name <symbol>) (func <procedure>))
  "A shorthand for @code{gtype-instance-signal-connect-after}."
  (gtype-instance-signal-connect-after object name func))

(define-method (block (object <gtype-instance>) id)
  "A shorthand for @code{gsignal-handler-block}."
  (gsignal-handler-block object id))

(define-method (unblock (object <gtype-instance>) id)
  "A shorthand for @code{gsignal-handler-unblock}."
  (gsignal-handler-unblock object id))

(define-method (disconnect (object <gtype-instance>) id)
  "A shorthand for @code{gsignal-handler-disconnect}."
  (gsignal-handler-disconnect object id))

(define-method (connected? (object <gtype-instance>) id)
  "A shorthand for @code{gsignal-handler-connected?}."
  (gsignal-handler-connected? object id))

(define-method (equal? (o1 <gobject>) (o2 <gobject>))
  (eq? (slot-ref o1 'gtype-instance) (slot-ref o2 'gtype-instance)))

(define-method (invoke (closure <gclosure>) . args)
  "A shorthand for @code{gclosure-invoke}."
  (apply gclosure-invoke closure args))

(define-method (create-signal (class <gtype-class>) (name <symbol>) return-type param-types)
  "A shorthand for @code{gtype-class-create-signal}."
  (gtype-class-create-signal class name return-type param-types))

;; this one's a macro, i'm too lazy to make the method now...
;;(define-method (define-signal (class <gtype-class>) return-type . param-types)

(define-method (get-signals (class <gtype-class>))
  "A shorthand for @code{gtype-class-get-signals}."
  (gtype-class-get-signals class))

(define-method (get-properties (class <gtype-class>))
  "A shorthand for @code{gobject-class-get-properties}."
  (gobject-class-get-properties class))

(define-method (get-property-names (class <gtype-class>))
  "A shorthand for @code{gobject-class-get-property-names}."
  (gobject-class-get-property-names class))

(define-method (find-property (class <gtype-class>) (name <symbol>))
  "A shorthand for @code{gobject-class-find-property}."
  (gobject-class-find-property class name))
