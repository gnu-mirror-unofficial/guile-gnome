;; guile-gnome
;; Copyright (C) 2003,2004,2008,2009 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2004,2007 Andreas Rottmann <rotty at debian dot org>

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
;; @c
;;
;; G-Wrap support for @code{(gnome gobject)} types. Code in this module
;; is only loaded when generating wrapsets; as such, it is not for end
;; users.
;;
;;; Code:

(define-module (gnome gw support gobject)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 pretty-print)

  #:use-module (gnome gw support g-wrap)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  
  #:use-module ((g-wrap util) #:select (call-with-output-file/cleanup
                                        any-str->c-sym-str))
  #:use-module (gnome gobject utils)
  #:use-module (gnome gw support slib)
  
  #:export (<gobject-wrapset-base>
            add-type-alias! lookup-type-by-alias
            add-type-rule! find-type-rule
            
            <gobject-type-base>
            
            <gobject-classed-type>
            gtype-id
            
            <gobject-classed-pointer-type>
            unwrap-null-checked
            
            wrap-instance!
            wrap-boxed!
            wrap-pointer!
            wrap-opaque-pointer!
            wrap-freeable-pointer!
            wrap-refcounted-pointer!
            wrap-structure!
            wrap-interface!

            ;; we extend the one from (g-wrap enumeration)
            ;; wrap-enum!

            wrap-flags!
            wrap-gobject-class!
            
            wrap-custom-boxed!
            wrap-custom-gvalue!))

(define-class-with-docs <gobject-wrapset-base> (<gw-guile-wrapset>)
  "The base class for G-Wrap wrapsets that use @code{<gobject>} types."
  (type-aliases #:init-form (make-hash-table 31))
  (type-rules #:init-form (make-hash-table 7)))

(define-method (initializations-cg (wrapset <gobject-wrapset-base>) err)
  (list
   (next-method)
   "gw_guile_set_generics_module_x (scm_c_resolve_module\n"
   "                                (\"gnome gw generics\"));\n"))

(define-method (add-type-alias! (wrapset <gobject-wrapset-base>)
                                (alias <string>)
                                (name <symbol>))
  "Add a type alias to @var{wrapset}, that the string @var{alias} is
associated with the type named @var{symbol}. For example,
@code{\"GtkWindow*\"} might be associated with a type named
@code{<gtk-window>}. See @code{lookup-type-by-alias}."
  (hash-set!
   (slot-ref wrapset 'type-aliases)
   alias
   (or (lookup-type wrapset name)
       (error "tried to alias unknown type" name))))

(define (gobject-wrapsets-lookup-recursive ws slot key)
  (define (gobject-wrapsets-depended-on wrapset)
    (filter (lambda (ws) (is-a? ws <gobject-wrapset-base>))
            (wrapsets-depended-on wrapset)))
  (define (or-map f l)
    (if (null? l)
        #f
        (or (f (car l)) (or-map f (cdr l)))))
  (define (lookup wrapset)
    (or (hash-ref (slot-ref wrapset slot) key)
        (or-map lookup
                (gobject-wrapsets-depended-on wrapset))))
  (lookup ws))

(define-method (lookup-type-by-alias (wrapset <gobject-wrapset-base>)
                                     (name <string>))
  "Lookup a type aliased @var{name} in @var{wrapset}, and all wrapsets
on which @var{wrapset} depends. This interface is used by
@code{load-defs} to associate G-Wrap types with the strings parsed out
of the C header files."
  (gobject-wrapsets-lookup-recursive wrapset 'type-aliases name))

(define-method (add-type-rule! (self <gobject-wrapset-base>)
                               (param-type <string>) typespec)
  "Add a type rule to @var{wrapset}, that the string @var{param-type}
maps directly to the g-wrap typespec @var{typespec}. For example,
@code{\"int*\"} might map to the typespec @code{(int out)}. See
@code{find-type-rule}."
  (hash-set! (slot-ref self 'type-rules) param-type typespec))

(define-method (find-type-rule (self <gobject-wrapset-base>) 
                               (param-type <string>))
  "See if the parameter type @var{param-type} has a type rule present in
@var{wrapset} or in any wrapset on which @var{wrapset} depends. This
interface is used by @code{load-defs} to associate G-Wrap typespecs with
the strings parsed out of the C header files."
  (gobject-wrapsets-lookup-recursive self 'type-rules param-type))

;; "gtk_accel_group" => gtk-accel-group
(define (glib-function-name->scheme-name cname)
  ;; only change _ to -, other characters are not valid c names
  (string->symbol (gtype-name->scheme-name cname)))

(define (print-info how-wrapped c-name scm-name ws)
  (printf "%-8.8s|%-18.18s|%-25.25s|%-25.25s\n"
          how-wrapped c-name scm-name (name ws)))

(define-class-with-docs <gobject-type-base> (<gw-guile-rti-type>)
  "A base G-Wrap type class for GLib types."
  (ctype #:init-keyword #:ctype)
  (how-wrapped #:init-keyword #:wrapped #:init-value #f))

(define-method (initialize (type <gobject-type-base>) initargs)
  (let-keywords
   initargs #t (class-name ctype name)
   (let ((name-sym (gtype-name->class-name ctype)))
     (next-method
      type
      (append!
       (if class-name '() (list #:class-name name-sym))
       (if name '() (list #:name name-sym))
       initargs)))))

(define-method (add-type! (ws <gobject-wrapset-base>)
                          (type <gobject-type-base>))
  (next-method)
  (let ((how-wrapped (slot-ref type 'how-wrapped)))
    (if how-wrapped
        (print-info how-wrapped (slot-ref type 'ctype) (name type) ws))))

(define-class-with-docs <gobject-classed-type> (<gobject-type-base>)
  "A base G-Wrap type class for classed GLib types (see
@code{gtype-classed?})."
  (gtype-id #:init-keyword #:gtype-id #:getter gtype-id)
  (define-class? #:init-keyword #:define-class? #:init-value #t))

(define-method (initialize (type <gobject-classed-type>) initargs)
  (let-keywords
   initargs #t (c-type-name class-name ctype name)
   (let ((name-sym (gtype-name->class-name ctype)))
     (next-method
      type
      (append!
       (if c-type-name '() (list #:c-type-name ctype))
       (if class-name '() (list #:class-name name-sym))
       (if name '() (list #:name name-sym))
       initargs)))))

;; Perhaps make this one also use a loop over an array?
(define-method (initializations-cg (wrapset <gobject-wrapset-base>)
                                   (type <gobject-classed-type>)
                                   status-var)
  (list
   (next-method)
   (if (slot-ref type 'define-class?)
       (list
        "gw_guile_make_latent_variable\n"
        "  (scm_from_locale_symbol (\"" (symbol->string (class-name type)) "\"), "
        "scm_sys_gtype_to_class, scm_from_ulong (" (gtype-id type) "));\n")
       '())))

(define-method (add-type! (ws <gobject-wrapset-base>)
                          (type <gobject-classed-type>))
  (next-method)
  (if (slot-ref type 'define-class?)
      (add-module-export! ws (class-name type))))

(define-class-with-docs <gobject-classed-pointer-type> (<gobject-classed-type>)
  "A base G-Wrap type class for for classed GLib types whose values are
pointers.")

(define-method (initialize (type <gobject-classed-pointer-type>) initargs)
  (let-keywords
   initargs #t (ctype)
   (next-method type
                (append!
                 (list #:c-type-name (string-append ctype "*")
                       #:ffspec 'pointer)
                 initargs))))

(define-method (unwrap-null-checked (value <gw-value>)
                                    status-var
                                    code)
  "Unwrap a value into a C pointer, optionally unwrapping @code{#f} as
@code{NULL}.

This function checks the typespec options on @var{value}, which should
be a @code{<gw-value>}. If the @code{null-ok} option is set (which is
only the case for value classes with @code{null-ok} in its
@code{#:allowed-options}), this function generates code that unwraps
@code{#f} as @code{NULL}. If @code{null-ok} is unset, or the value is
not @code{#f}, @var{code} is run instead."
  (if-typespec-option
   value 'null-ok
   (list "if (SCM_FALSEP (" (scm-var value) "))\n"
         "  " (var value) " = NULL;\n"
         "else {\n"
         code
         "}\n")
   code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap objects.

(define-class <gobject-instance-type> (<gobject-classed-pointer-type>)
  #:allowed-options '(null-ok))

(define-method (wrap-instance! (ws <gobject-wrapset-base>) . args)
  "Define a wrapper for a specific
instantiatable (@code{<gtype-instance>}-derived) type in @var{ws}.
Required keyword arguments are @code{#:ctype} and @code{#:gtype-id}. For
example,

@lisp
 (wrap-instance! ws #:ctype \"GtkWidget\"
                    #:gtype-id \"GTK_TYPE_WIDGET\")
@end lisp

Normally only called from @code{load-defs}."
  (let ((type (apply make <gobject-instance-type> args)))
    (set! (class-name type) (name type))
    (slot-set! type 'how-wrapped "GTypeInstance")
    (add-type! ws type)
    type))

(define-method (unwrap-value-cg (type <gobject-instance-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     (unwrap-null-checked
      value status-var
      (list
       "if (!(" c-var " = (" (c-type-name type) ") "
       "scm_c_scm_to_gtype_instance_typed (" scm-var ", " (gtype-id type) ")))\n"
       `(gw:error ,status-var type ,(wrapped-var value)))))))

(define-method (wrap-value-cg (type <gobject-instance-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else\n"
     "  " scm-var " = scm_c_gtype_instance_to_scm (" c-var ");\n"
     (if-typespec-option value 'caller-owned
         ;; the _to_scm will ref the object; if the function is a
         ;; constructor, we don't need that ref
          (list "if (" c-var ") scm_c_gtype_instance_unref (" c-var ");\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap boxed types, represented on the scheme side by GValues.

(define-class <gobject-boxed-type> (<gobject-classed-pointer-type>)
  #:allowed-options '(null-ok))

(define-method (make-typespec (type <gobject-boxed-type>) (options <list>))
  (next-method type (cons 'unspecialized options)))

(define-method (initialize (type <gobject-boxed-type>) initargs)
  (let-keywords
   initargs #t (ctype)
   (next-method
    type
    (append! (list #:c-type-name (string-append ctype "*")) initargs))))

(define-method (wrap-boxed! (ws <gobject-wrapset-base>) . args)
  "Define a wrapper for a specific boxed type in @var{ws}. Required
keyword arguments are @code{#:ctype} and @code{#:gtype-id}, as in
@code{wrap-instance!}."
  (let ((type (apply make <gobject-boxed-type> args)))
    (slot-set! type 'how-wrapped "GBoxed")
    (add-type! ws type)
    type))

;; fixme: how to deal with consts?
(define-method (unwrap-value-cg (type <gobject-boxed-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (ctype (c-type-name type)))
     (list
      (unwrap-null-checked
       value status-var
       (list
        "if (scm_c_gvalue_holds (" scm-var ", " (gtype-id type) ")) {\n"
        (if-typespec-option
         value 'callee-owned
         (list
          "  " c-var " = scm_c_gvalue_dup_boxed (" scm-var ");\n")
         (list
          "  " c-var " = scm_c_gvalue_peek_boxed (" scm-var ");\n"))
        " } else {\n"
        "  " c-var " = NULL;\n"
        `(gw:error ,status-var type ,(wrapped-var value))
        "}\n")))))

(define-method (wrap-value-cg (type <gobject-boxed-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     (if-typespec-option
      value 'callee-owned
      (list
       "  " scm-var " = scm_c_gvalue_new_from_boxed (" (gtype-id type) ", " c-var ");\n")
      (list
       "  " scm-var " = scm_c_gvalue_new_take_boxed (" (gtype-id type) ", " c-var ");\n"))
     "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap pointers. This is an opaque value type; scheme doesn't know what
;; to do with it.
(define-class <gobject-pointer-type> (<gobject-classed-pointer-type>))

(define-method (wrap-pointer! (ws <gobject-wrapset-base>) . args)
  "Define a wrapper for a specific pointer type in @var{ws}. Required
keyword arguments are @code{#:ctype} and @code{#:gtype-id}, as in
@code{wrap-instance!}."
  (let ((type (apply make <gobject-pointer-type> args)))
    (slot-set! type 'how-wrapped "GPointer")
    (add-type! ws type)
    type))

(define-method (unwrap-value-cg (type <gobject-pointer-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (ctype (c-type-name type)))
    (unwrap-null-checked
     value status-var
     (list                    
      "if (scm_c_gvalue_holds (" scm-var ", " (gtype-id type) "))\n"
      "  " c-var " = g_value_get_pointer (scm_c_gvalue_peek_value (" scm-var "));\n"
      "else {\n"
      "  " c-var " = NULL;\n"
      `(gw:error ,status-var type ,(wrapped-var value))
      "}\n"))))

(define-method (wrap-value-cg (type <gobject-pointer-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     "  " scm-var " = scm_c_make_gvalue (" (gtype-id type) ");\n"
     "  g_value_set_pointer (scm_c_gvalue_peek_value (" scm-var "), " c-var ");\n"
     "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap interfaces. We only understand interfaces implemented by objects.

(define-class <gobject-interface-type> (<gobject-classed-pointer-type>)
  #:allowed-options '(null-ok))

(define-method (wrap-interface! (ws <gobject-wrapset-base>) . args)
  "Define a wrapper for an interface type in @var{ws}. Required keyword
arguments are @code{#:ctype} and @code{#:gtype-id}, as in
@code{wrap-instance!}."
  (let ((type (apply make <gobject-interface-type> args)))
    (slot-set! type 'how-wrapped "GInterface")
    (add-type! ws type)
    type))

(define-method (unwrap-value-cg (type <gobject-interface-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (unwrap-null-checked
     value status-var
     (list                    
      c-var " = scm_c_scm_to_gtype_instance_typed (" scm-var ", G_TYPE_OBJECT);\n"
      
      "if (!" c-var " || !g_type_is_a (G_TYPE_FROM_INSTANCE (" c-var "), " (gtype-id type) "))\n"
      `(gw:error ,status-var type ,(wrapped-var value)))
     )))

(define-method (wrap-value-cg (type <gobject-interface-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else {\n"
     "  " scm-var " = scm_c_gtype_instance_to_scm ((GTypeInstance *)" c-var ");\n"
     (if-typespec-option
      value 'caller-owned
      (list "if (" c-var ") g_object_ref ((GObject*)" c-var ");\n"))
     "}\n")))


;;;
;;; Enums
;;;

(define-class <gobject-enum-type> (<gobject-classed-type>))

(define-method (initialize (type <gobject-enum-type>) initargs)
  (let-keywords
   initargs #t (ctype)
   (next-method type
                (append!
                 (list #:ffspec 'uint)
                 initargs))))

(define-method (make-typespec (type <gobject-enum-type>) (options <list>))
  (next-method type (append '(caller-owned unspecialized) options)))

(define-method (wrap-enum! (ws <gobject-wrapset-base>) . args)
  "Define a wrapper for an enumerated type in @var{ws}. The
@code{#:ctype} keyword argument is required.

If @code{#:gtype-id} is also given, the type will be queried at runtime
for its possible values; otherwise a @code{#:values} argument is
necessary, which is a list of symbol-integer pairs specifying all
possible values."
  (let-keywords
   args #t (gtype-id ctype)
   (cond
    (gtype-id
     (let ((type (apply make <gobject-enum-type> args)))
       (slot-set! type 'how-wrapped "GEnum")
       (add-type! ws type)
       type))
   (else
      (print-info "C Enum" ctype ctype ws)
      (apply next-method ws (append!
                             (list #:name (gtype-name->class-name ctype)
                                   #:c-type-name ctype)
                             args))))))

;; enums are just guints...
(define-method (unwrap-value-cg (type <gobject-enum-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (gtype-id (gtype-id type)))
    (list
     "if (scm_c_gvalue_holds (" scm-var ", " gtype-id "))\n"
     "  " c-var " = g_value_get_enum (scm_c_gvalue_peek_value (" scm-var "));\n"
     "else {\n"
     "  GValue newval = {0,}; g_value_init (&newval, " gtype-id ");\n"
     "  scm_c_gvalue_set (&newval, " scm-var ");\n"
     "  " c-var " = g_value_get_enum (&newval);\n"
     "}\n")))

(define-method (wrap-value-cg (type <gobject-enum-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     scm-var " = scm_c_make_gvalue (" (gtype-id type)");\n"
     "g_value_set_enum (scm_c_gvalue_peek_value (" scm-var "), " c-var ");\n")))


;;;
;;; Flags, represented on the scheme side as GValues.
;;;

(define-class <gobject-flags-type> (<gobject-enum-type>))

(define-method (make-typespec (type <gobject-flags-type>) (options <list>))
  (next-method type (append '(unspecialized caller-owned) options)))

;; (wrap-flags! wrapset #:gtype-id foo [#:values '((a 1) ...)])
;; (wrap-flags! wrapset #:values '((a 1) ...))
(define-method (wrap-flags! (ws <gobject-wrapset-base>) . args)
  "Define a wrapper for a flags type in @var{ws}. Required keyword
arguments are @code{#:ctype} and @code{#:gtype-id} or @code{#:values},
as in @code{wrap-enum!}."
  (let-keywords
   args #t (gtype-id ctype)
   (cond
    (gtype-id
     (let ((type (apply make <gobject-flags-type> args)))
       (slot-set! type 'how-wrapped "GFlags")
       (add-type! ws type)
       type))
    (else
     (let ((type (apply wrap-enum! ws args)))
       (print-info "C Flags" ctype ctype ws)
       type)))))

;; flags are just guints...
(define-method (unwrap-value-cg (type <gobject-flags-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (gtype-id (gtype-id type)))
    (list
     "if (SCM_FALSEP (" scm-var "))\n"
     "  " c-var " = 0;\n"
     "else if (scm_c_gvalue_holds (" scm-var ", " gtype-id "))\n"
     "  " c-var " = g_value_get_flags (scm_c_gvalue_peek_value (" scm-var "));\n"
     "else {\n"
     "  GValue newval = {0,}; g_value_init (&newval, " gtype-id ");\n"
     "  scm_c_gvalue_set (&newval, " scm-var ");\n"
     "  " c-var " = g_value_get_flags (&newval);\n"
     "}\n")))

(define-method (wrap-value-cg (type <gobject-flags-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value)) (scm-var (scm-var value)))
    (list
     scm-var " = scm_c_make_gvalue (" (gtype-id type) ");\n"
     "g_value_set_flags (scm_c_gvalue_peek_value (" scm-var "), " c-var ");\n")))


(define-class <gobject-opaque-pointer> (<gw-wct> <gw-guile-rti-type>)
  wct-var-name)

(define-method (initialize (wct <gobject-opaque-pointer>) initargs)
  (next-method)
  (slot-set! wct 'wct-var-name
	     (gen-c-tmp (string-append
			 "wct_info_for"
			 (any-str->c-sym-str (symbol->string (name wct)))))))

(define-method (wrap-value-cg (wct <gobject-opaque-pointer>)
			      (value <gw-value>)
			      status-var
			      (inlined? <boolean>))
  (let ((wct-var (slot-ref wct 'wct-var-name))
	(sv (scm-var value))
	(cv (var value)))
    (list
     "if(" cv " == NULL) " sv " = SCM_BOOL_F;\n"
     "else {\n"
     sv " = gw_wcp_assimilate_ptr((void *) " cv ", " wct-var ");\n"
     "}\n")))


(define-method (unwrap-value-cg (wct <gobject-opaque-pointer>)
				(value <gw-value>)
				status-var
				(inlined? <boolean>))
  (let* ((wct-var (slot-ref wct 'wct-var-name))
	 (sv (scm-var value))
	 (c-var (var value))
	 (unwrap-code
	  (list "if (gw_wcp_is_of_type_p (" wct-var ", " sv "))\n"
		"  " c-var " = gw_wcp_get_ptr (" sv ");\n"
		"else\n"
		`(gw:error ,status-var type ,(wrapped-var value)))))

    (list
     (if-typespec-option value 'null-ok
			 (list "if (SCM_FALSEP (" sv "))\n"
			       "  " c-var " = NULL;\n"
			       "else " unwrap-code)
			 unwrap-code))))

(define-method (initializations-cg (wrapset <gobject-wrapset-base>)
				   (wct <gobject-opaque-pointer>)
				   error-var)
  (let ((wct-var (slot-ref wct 'wct-var-name))
	(wcp-type-name (symbol->string (name wct)))
	(wcp-mark (wcp-mark-function wct))
	(wcp-free (wcp-free-function wct))
	(wcp-equal? (wcp-equal-predicate wct)))
  (list
   (next-method)

   wct-var "= gw_wct_create (\"" wcp-type-name "\", " wcp-equal? ", NULL, "
   wcp-mark ", " wcp-free ");\n"
   "scm_c_define(\"" wcp-type-name "\", " wct-var ");\n")))

(define (wct-var-decl-cg wct)
  (list "static SCM "  (slot-ref wct 'wct-var-name) " = SCM_BOOL_F;\n"))

(define-method (global-declarations-cg (wrapset <gobject-wrapset-base>)
				       (wct <gobject-opaque-pointer>))
  (wct-var-decl-cg wct))

(define-method (client-global-declarations-cg (wrapset <gobject-wrapset-base>)
					      (wct <gobject-opaque-pointer>))
  (wct-var-decl-cg wct))

(define-method (client-initializations-cg (wrapset <gobject-wrapset-base>)
					  (wct <gobject-opaque-pointer>)
					  error-var)
  (let ((wct-var (slot-ref wct 'wct-var-name))
	(wcp-type-name (symbol->string (name wct))))
    (list
     "    " wct-var " = scm_c_eval_string(\"" wcp-type-name "\");\n")))

(define (wrap-opaque-pointer! ws ctype)
  "Define a wrapper for an opaque pointer with the C type @var{ctype}.
It will not be possible to create these types from Scheme, but they can
be received from a library, and passed as arguments to other calls into
the library."
  (let ((type (make <gobject-opaque-pointer>
                #:name (gtype-name->class-name ctype)
                #:c-type-name ctype
                #:c-const-type-name (string-append "const " ctype))))
    (add-type! ws type)
    (add-type-alias! ws ctype (name type))
    (print-info "Opaque" ctype ctype ws)
    (add-module-export! ws (name type))
    ;; don't export, unlike wrap-as-wct!
    type))


(define-class <gobject-freeable-pointer> (<gobject-opaque-pointer>)
  (free-function #:init-keyword #:free-function))

(define-method (global-declarations-cg (wrapset <gobject-wrapset-base>)
				       (wct <gobject-freeable-pointer>))
  (list "static size_t " (wcp-free-function wct) " (void *wcp) {\n"
        (slot-ref wct 'free-function) " (wcp); return 0;\n"
        "}"
        (next-method)))

(define (wrap-freeable-pointer! ws ctype free)
  "foo"
  (let* ((type (make <gobject-freeable-pointer>
                 #:name (gtype-name->class-name ctype)
                 #:c-type-name (string-append ctype "*")
                 #:c-const-type-name (string-append "const " ctype "*")
                 #:wcp-free-function (string-append "_wcp_free_" free)
                 #:free-function free)))
    (print-info "Freeable" ctype ctype ws)
    (add-type! ws type)
    (add-type-alias! ws (string-append ctype "*") (name type))
    type))


(define-class <gobject-ref-pointer> (<gobject-freeable-pointer>)
  (ref-function #:init-keyword #:ref-function))

(define-method (wrap-value-cg (wct <gobject-ref-pointer>)
			      (value <gw-value>)
			      status-var
			      (inlined? <boolean>))
  ;; potential FIXME, caller-owned unwraps
  (let ((sv (scm-var value))
        (cv (var value))
        (ref (slot-ref wct 'ref-function)))
    (append
     (if-typespec-option value 'callee-owned
                         (list "if (" cv ")\n  " ref " (" cv ");\n")
                         '())
     (next-method))))

(define (wrap-refcounted-pointer! ws ctype ref unref)
  "foo"
  (let* ((type (make <gobject-ref-pointer>
                 #:name (gtype-name->class-name ctype)
                 #:c-type-name (string-append ctype "*")
                 #:c-const-type-name (string-append "const " ctype "*")
                 #:wcp-free-function (string-append "_wcp_free_" unref)
                 #:ref-function ref
                 #:free-function unref)))
    (print-info "RefPtr" ctype ctype ws)
    (add-type! ws type)
    (add-type-alias! ws (c-type-name type) (name type))
    type))


(define-class-with-docs <gobject-structure-type> (<gw-type>)
  "foo docs here."
  (c-type-name #:init-keyword #:c-type-name #:getter c-type-name)
  (wrap-func #:init-keyword #:wrap-func #:getter wrap-func)
  (unwrap-func #:init-keyword #:unwrap-func #:getter unwrap-func))

(define-method (check-typespec-options (type <gobject-structure-type>) (options <list>))
  (let ((remainder options))
    (define (del*! . syms)
      (set! remainder (lset-difference eq? remainder syms)))
    (del*! 'const 'out 'unspecialized)
    (cond
     ((and (memq 'caller-owned remainder) (memq 'callee-owned remainder))
      (raise-bad-typespec type options "both caller and callee owned"))
     ((not (or (memq 'caller-owned remainder) (memq 'callee-owned remainder)))
      (raise-bad-typespec type options "must be caller or callee owned"))
     (else
      (apply del*! 'caller-owned 'callee-owned 
             (slot-ref type 'allowed-options))
      (if (not (null? remainder))
          (raise-bad-typespec type options
                              "spurious options in RTI type: ~S" remainder))))))

(define-method (c-type-name (type <gobject-structure-type>)
                            (typespec <gw-typespec>))
  (c-type-name type))

(define-method (make-typespec (type <gobject-structure-type>) (options <list>))
  (next-method type (cons 'unspecialized options)))

(define-method (unwrap-value-cg (type <gobject-structure-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (unwrap-null-checked
     value status-var
     (list
      ;; memset c-var to 0 ?
      (unwrap-func type) " (" scm-var ", &" c-var ");\n"))))

(define-method (call-arg-cg (type <gobject-structure-type>)
                            (value <gw-value>))
  (list "&" (var value)))

(define-method (wrap-value-cg (type <gobject-structure-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     scm-var " = " (wrap-func type) " (&" c-var ");\n")))

(define (wrap-structure! ws ctype wrap unwrap)
  "Define a wrapper for structure values of type @var{ctype}.

@var{wrap} and @var{unwrap} are the names of C functions to convert a C
structure to Scheme and vice versa, respectively. When in a function
call, parameters of this type of the form `@var{StructName}*' are
interpreted as `out' parameters, while `const-@var{StructName}*' are
treated as `in' parameters.

Note that @var{ctype} should be the type of the structure, not a pointer
to the structure."
  (let* ((type (make <gobject-structure-type>
                 #:name (gtype-name->class-name ctype)
                 #:c-type-name ctype
                 #:wrap-func wrap
                 #:unwrap-func unwrap)))
    (print-info "Structure" ctype ctype ws)
    (add-type! ws type)
    (add-type-rule! ws (string-append ctype "*")
                    (list (name type) 'caller-owned 'out))
    (add-type-rule! ws (string-append "const-" ctype "*")
                    (list (name type) 'callee-owned))
    type))


;; Used for functions that operate on classes, e.g.
;; gtk_widget_class_install_style_property,
;; gst_element_class_get_pad_template

(define-class <gobject-class-type> (<gobject-classed-type>))

(define-method (initialize (self <gobject-class-type>) initargs)
  (let-keywords
   initargs #t (ctype)
   (next-method self
                (append!
                 (list #:c-type-name (string-append ctype "*")
                       #:ffspec 'pointer)
                 initargs))))

;; (wrap-gobject-class! ws #:ctype "GstElementClass" #:gtype-id "GST_TYPE_ELEMENT")
(define-method (wrap-gobject-class! (ws <gobject-wrapset-base>) . args)
  "Define a wrapper for GObject class values @var{ws}. Required keyword
arguments are @code{#:ctype} and @code{#:gtype-id}, as in
@code{wrap-instance!}.

@code{#:ctype} should refer to the type of the class and not the
instance; e.g. @code{\"GtkWidgetClass\"} and not @code{\"GtkWidget\"}.
This function will not be called by @code{load-defs}, and should be
invoked manually in a wrapset as needed."
  (let ((type (apply make <gobject-class-type> args)))
    (slot-set! type 'how-wrapped "GObjectClass")
    (slot-set! type 'define-class? #f)
    (add-type! ws type)
    (add-type-alias! ws (c-type-name type) (name type))
    type))

(define-method (unwrap-value-cg (type <gobject-class-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (ctype (c-type-name type)))
    (unwrap-null-checked
     value status-var
     (list
      "if (g_type_is_a (scm_c_gtype_class_to_gtype (" scm-var "), " (gtype-id type) "))\n"
      "  " c-var " = g_type_class_ref (scm_c_gtype_class_to_gtype (" scm-var "));\n"
      "else " `(gw:error ,status-var type ,(wrapped-var value))))))

(define-method (wrap-value-cg (type <gobject-class-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL)\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "else\n"
     "  " scm-var " = scm_c_gtype_to_class (G_TYPE_FROM_CLASS (" c-var "));\n")))

(define-class <gobject-custom-boxed-type> (<gobject-classed-pointer-type>)
  (wrap-func #:init-keyword #:wrap-func #:getter wrap-func)
  (wrap #:init-keyword #:wrap #:getter wrap)
  (unwrap-func #:init-keyword #:unwrap-func #:getter unwrap-func)
  (unwrap #:init-keyword #:unwrap #:getter unwrap)
  
  #:allowed-options '(null-ok))

(define-method (make-typespec (type <gobject-custom-boxed-type>) (options <list>))
  (next-method type (cons 'unspecialized options)))

(define gen-c-tmp
  (let ((i -1))
    (lambda (suffix)
      (set! i (1+ i))
      (format #f "gw__~A_~A" i suffix))))

(define-method (global-definitions-cg (wrapset <gobject-wrapset-base>)
                                      (type <gobject-custom-boxed-type>))
  (let ((scm-var (gen-c-tmp "scm_val"))
        (c-var (gen-c-tmp "c_val")))
    (list
     (next-method)
     ((wrap type) scm-var c-var)
     ((unwrap type) scm-var c-var))))

(define-method (global-declarations-cg (wrapset <gobject-wrapset-base>)
                                       (type <gobject-custom-boxed-type>))
  (list
   (next-method)
   "static SCM " (wrap-func type) " (const GValue *);\n"
   "static void " (unwrap-func type) " (SCM, GValue *);\n"))

(define-method (initializations-cg (wrapset <gobject-wrapset-base>)
                                   (type <gobject-custom-boxed-type>)
                                   status-var)
  (list
   (next-method)
   "scm_c_register_gvalue_wrappers (" (gtype-id type) ", "
   (wrap-func type) ", " (unwrap-func type) ");\n"))

(define-macro (make-custom-wrapper type wrap-form)
  `(let ((ctype (,c-type-name ,type))
         (wrap-func (,wrap-func ,type)))
     (lambda (scm-var c-var)
       (list
        "static SCM " wrap-func " (const GValue* gvalue) {\n"
        "  SCM " scm-var " = SCM_BOOL_F;\n"
        "  " ctype " " c-var " = g_value_get_boxed (gvalue);\n"
        ,wrap-form
        "  return " scm-var ";\n"
        "}\n"))))
     
(define-macro (make-custom-unwrapper type unwrap-form)
  `(let ((ctype (,c-type-name ,type))
         (unwrap-func (,unwrap-func ,type)))
     (lambda (scm-var c-var)
       (list
        "static void " unwrap-func " (SCM " scm-var ", GValue* gvalue) {\n"
        "  " ctype " " c-var " = NULL;\n"
        ,unwrap-form
        "  g_value_take_boxed (gvalue, " c-var ");\n"
        "}\n"))))
     
(define-method (unwrap-value-cg (type <gobject-custom-boxed-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (unwrap-null-checked
     value status-var
     (list
      "GValue lvalue = { 0, };\n"
      "g_value_init (&lvalue, " (gtype-id type) ");\n"
      (unwrap-func type) " (" scm-var ", &lvalue);\n"
      "if (G_IS_VALUE (&lvalue)) {"
      ;; leaks memory... need to write a destructor for c-var in the
      ;; case of a caller-owned argument
      "  " c-var " = g_value_get_boxed (&lvalue);\n"
      "} else {\n"
      "  " c-var " = NULL;\n"
      "}\n"))))

(define-method (wrap-value-cg (type <gobject-custom-boxed-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "GValue rvalue = { 0, };\n"
     "g_value_init (&rvalue, " (gtype-id type) ");\n"
     "g_value_set_static_boxed (&rvalue, " c-var ");\n"
     scm-var " = " (wrap-func type) " (&rvalue);\n")))

(define-macro-with-docs (wrap-custom-boxed! ctype gtype wrap unwrap)
  "Wrap a boxed type using custom wrappers and unwrappers.

FIXME: missing a wrapset argument!

@var{ctype} and @var{gtype} are as @code{#:ctype} and @code{#:gtype-id}
in @code{wrap-instance!}. @var{wrap} and @var{unwrap} are G-Wrap forms in
which @code{scm-var} and @code{c-var} will be bound to the names of the
SCM and C values, respectively. For example:

@lisp
  (wrap-custom-boxed!
   \"GdkRectangle\" \"GDK_TYPE_RECTANGLE\"
   (list scm-var \" = \"
         c-var \" ?  scm_gdk_rectangle_to_scm (\" c-var \")\"
         \" : SCM_BOOL_F;\")
   (list c-var \" = scm_scm_to_gdk_rectangle (\" scm-var \");\"))
@end lisp"
  (let* ((pname (string-append ctype "*"))
         (func-infix (string-map (lambda (c) (case c ((#\-) #\_) (else c)))
                                 (GStudlyCapsExpand ctype)))
         (wrap-func (string-append "gw__gvalue_" func-infix "_wrap"))
         (unwrap-func (string-append "gw__gvalue_" func-infix "_unwrap")))
    `(let ((t (make (@@ (gnome gw support gobject)
                        <gobject-custom-boxed-type>)
                #:ctype ,ctype
                #:gtype-id ,gtype
                #:c-type-name ,pname
                #:wrapped "Custom"
                #:wrap-func ,wrap-func
                #:unwrap-func ,unwrap-func)))
       (slot-set! t 'wrap ((@@ (gnome gw support gobject) make-custom-wrapper)
                           t ,wrap))
       (slot-set! t 'unwrap ((@@ (gnome gw support gobject) make-custom-unwrapper)
                             t ,unwrap))
       (add-type! ws t)
       (add-type-alias! ws ,pname (name t)))))


(define-class <gobject-custom-gvalue-type> (<gobject-classed-type>)
  (wrap-func #:init-keyword #:wrap-func #:getter wrap-func)
  (unwrap-func #:init-keyword #:unwrap-func #:getter unwrap-func)
  
  #:allowed-options '(null-ok))

(define-method (make-typespec (type <gobject-custom-gvalue-type>) (options <list>))
  (next-method type (cons 'unspecialized options)))

(define-method (initializations-cg (wrapset <gobject-wrapset-base>)
                                   (type <gobject-custom-gvalue-type>)
                                   status-var)
  (list
   (next-method)
   "scm_c_register_gvalue_wrappers (" (gtype-id type) ", "
   (wrap-func type) ", " (unwrap-func type) ");\n"))

(define-method (unwrap-value-cg (type <gobject-custom-gvalue-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (unwrap-null-checked
     value status-var
     (list
      c-var " = g_new0 (GValue, 1);\n"
      (unwrap-func type) " (" scm-var ", " c-var ");\n"
      "if (!G_IS_VALUE (" c-var ")) {"
      "  g_free (" c-var ");\n"
      "  " c-var " = NULL;\n"
      "}\n"))))

(define-method (wrap-value-cg (type <gobject-custom-gvalue-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     scm-var " = " (wrap-func type) " (" c-var ");\n")))

(define-macro (wrap-custom-gvalue! ctype gtype wrap-func unwrap-func)
  "Wrap a GValue type using custom wrap and unwrap functions.

FIXME: missing a wrapset argument!

@var{ctype} and @var{gtype} are as @code{#:ctype} and @code{#:gtype-id}
in @code{wrap-instance!}. @var{wrap-func } and @var{unwrap-func} are names
of functions to convert to and from Scheme values, respectively. For
example:

@lisp
 (wrap-custom-gvalue! \"GstFraction\" \"GST_TYPE_FRACTION\"
                      \"scm_from_gst_fraction\"
                      \"scm_to_gst_fraction\")
@end lisp"
  `(let ((t (make (@@ (gnome gw support gobject) <gobject-custom-gvalue-type>)
              #:ctype ,ctype
              #:gtype-id ,gtype
              #:c-type-name "GValue*"
              #:ffspec 'pointer
              #:wrapped "Custom"
              #:wrap-func ,wrap-func
              #:unwrap-func ,unwrap-func)))
     (add-type! ws t)))

;;; We override the generation of the scheme wrapper, because we want to
;;; avoid listing all exports of the module. See the lengthy comment in
;;; guile/g-wrap/guile-runtime.c in g-wrap for a rationale.
(define-method (generate-wrapset (lang <symbol>)
                                 (wrapset <gobject-wrapset-base>)
                                 (basename <string>))
  (define (register-generics-without-rti)
    (fold-functions
     (lambda (func rest)
       (cond
        ((and (not (uses-rti-for-function? wrapset func))
              (generic-name func)
              (> (argument-count func) 0)
              (class-name (first (argument-types func))))
         (cons `(%gw:procedure->method-public
                 ,(name func)
                 ;; Specializers
                 ',(map (lambda (arg)
                          (let ((typespec (typespec arg)))
                            (and (not (memq 'unspecialized
                                            (options typespec)))
                                 (class-name (type typespec)))))
                        (filter visible? (arguments func)))
                 ',(generic-name func)
                 ;; Required argument count
                 ,(- (input-argument-count func)
                     (optional-argument-count func))
                 ;; Optional arguments?
                 ,(not (zero? (optional-argument-count func))))
               rest))
        (else rest)))
     '() wrapset))

  ;; The next method is in (g-wrap guile), which will generate the scm
  ;; file. We overwrite it afterwards.
  (next-method)

  (if (module wrapset)
      (call-with-output-file/cleanup
       (string-append basename ".scm")
       (lambda (port)
         (define (++ . args)
           (apply string-append args))

         (for-each
          (lambda (x) (display x port) (newline port))
          '(";; Generated by G-Wrap: an experimental Guile C API-wrapper engine."
            ";; Customized by guile-gnome; see (gnome gw support g-wrap) for details."))
         (for-each
          (lambda (x) (pretty-print x port))
          (list
           `(define-module ,(module wrapset)
              #:use-module (oop goops)
              #:use-module (gnome gobject)
              #:use-module (gnome gw support modules)
              ,@(if (slot-ref wrapset 'shlib-abs?)
                    '(#:use-module (g-wrap config))
                    '()))

           (let ((wrapset-name-c-sym (any-str->c-sym-str
                                      (symbol->string (name wrapset)))))
             `(dynamic-call ,(++ "gw_init_wrapset_" wrapset-name-c-sym)
                            (dynamic-link
                             ,(if (slot-ref wrapset 'shlib-abs?)
                                  `(string-append
                                    *g-wrap-shlib-dir*
                                    ,(slot-ref wrapset 'shlib-path))
                                  (slot-ref wrapset 'shlib-path)))))
           
           ;; This is what we avoid:
           ;;   `(export ',@(module-exports wrapset))
           ;; Instead we do this lovely hack:
           `(export-all-lazy! ',(module-exports wrapset))
           `(begin ,@(register-generics-without-rti))
           `(if (defined? '%generics)
                (module-use! (module-public-interface (current-module))
                             (module-public-interface %generics)))))))))
