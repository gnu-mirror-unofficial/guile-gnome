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
;;A g-wrap specification for GObject.
;;
;;; Code:

;;; -*-scheme-*-

(define-module (gnome gobject gw-gobject-spec)
  :use-module (g-wrap)
  :use-module (g-wrap simple-type)
  :use-module (gnome gobject gw-glib-spec)
  :use-module (gnome gobject defs-support)
  :use-module (gnome gobject gw-spec-utils)
  :export (gobject:gwrap-object
           gobject:gwrap-boxed
           gobject:gwrap-pointer
           gobject:gwrap-interface
           gobject:gwrap-flags
           gobject:gwrap-enum))

;; gw-gobject: a wrapset to assist in wrapping gobject-based apis

(for-each
 (lambda (pair) (register-type "guile-gnome-gw-gobject" (car pair) (cadr pair)))
 '(("GType" <gtype>)
   ("GValue*" <gvalue>)
   ("GObject*" <gobject>)
   ("GClosure*" <gclosure>)
   ("GParamSpec*" <gparam>)))

(let ((ws (gw:new-wrapset "guile-gnome-gw-gobject")))

  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-glib")

  (gw:wrapset-set-guile-module! ws '(gnome gobject gw-gobject))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <guile-gnome-gobject.h>\n")))

  (gw:wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         (list "g_type_init ();\n"
               "scm_pre_init_gnome_gobject_primitives ();\n"
               "scm_pre_init_gnome_gobject ();\n")
         ;; needed for gtype->class for exporting gobject types
         (gw:inline-scheme '(use-modules (gnome gobject))))))

  (gw:wrap-simple-type
   ws '<gtype> "GType"
   '("SCM_TYP16_PREDICATE (scm_tc16_gtype, " scm-var ")")
   '(c-var " = (GType) SCM_SMOB_DATA (" scm-var ");\n")
   '(scm-var " = scm_c_register_gtype (" c-var ");\n"))

  (gw:wrap-simple-type
   ws '<gtype-instance> "GTypeInstance*"
   '("SCM_TYP16_PREDICATE (scm_tc16_gtype_instance, " scm-var ")")
   '(c-var " = (GTypeInstance*) SCM_SMOB_DATA (" scm-var ");\n")
   '(scm-var " = scm_c_gtype_instance_to_scm (" c-var ");\n")) ; fixme: unref the scm_var

  ;; should <gobject> really be <g-object>?
  (set! glib:type-cname->symbol-alist
        (acons "GObject" '<gobject> glib:type-cname->symbol-alist))
  (gobject:gwrap-object ws "GObject" "G_TYPE_OBJECT")

  (set! glib:type-cname->symbol-alist
        (acons "GValue" '<gvalue> glib:type-cname->symbol-alist))
  (gobject:gwrap-helper
   ws "GValue"
   (lambda (typespec)
     (if (memq 'const (gw:typespec-get-options typespec))
         "const GValue*"
         "GValue*"))
   (lambda (c-var scm-var typespec status-var)
     ;; should use scm_c_scm_to_gvalue here, except we'd have to make a
     ;; cleanup function to free the value, and I can't think right now
     (list "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var "))\n"
           "  " c-var " = (GValue*) SCM_SMOB_DATA (" scm-var ");\n"
           "else " `(gw:error ,status-var type ,scm-var)))
   (lambda (scm-var c-var typespec status-var)
     (list "SCM_NEWSMOB (" scm-var ", scm_tc16_gvalue, " c-var ");\n"))
   (lambda (c-var typespec status-var force?)
     (list))
   "Custom")
  
  (set! glib:type-cname->symbol-alist
        (acons "GClosure" '<gclosure> glib:type-cname->symbol-alist))
  (gobject:gwrap-helper
   ws "GClosure"
   (lambda (typespec) "GClosure*")
   (lambda (c-var scm-var typespec status-var)
     (list "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var "))\n"
           "  " c-var " = (GClosure*) g_value_get_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
           "else if (SCM_NFALSEP (scm_call_2 (SCM_VARIABLE_REF (scm_c_lookup (\"is-a?\")),\n"
           "                                  " scm-var ",\n"
           "                                  SCM_VARIABLE_REF (scm_c_lookup (\"<gclosure>\")))))\n"
           "  " c-var " = (GClosure*) g_value_get_boxed ((GValue*)SCM_SMOB_DATA (scm_slot_ref (" scm-var ", scm_str2symbol (\"closure\"))));\n"
           "else " `(gw:error ,status-var type ,scm-var)))
   (lambda (scm-var c-var typespec status-var) ; not ideal but ok
     (list scm-var " = scm_c_make_gvalue (G_TYPE_CLOSURE);\n"
           "g_value_set_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "), " c-var ");\n"))
   (lambda (c-var typespec status-var force?)
     (list))
   "Custom")

  (set! glib:type-cname->symbol-alist
        (acons "GParamSpec" '<gparam> glib:type-cname->symbol-alist))
  (gobject:gwrap-helper
   ws "GParamSpec"
   (lambda (typespec) "GParamSpec*")
   (lambda (c-var scm-var typespec status-var)
     (list (if (memq 'null-ok (gw:typespec-get-options typespec))
               (list
                "if (SCM_FALSEP (" scm-var "))\n"
                "  " c-var " = NULL;\n"
                "else ")
               '())
           "if (!(" c-var " = (GParamSpec*)scm_c_scm_to_gtype_instance (" scm-var ", G_TYPE_PARAM)))\n"
           `(gw:error ,status-var type ,scm-var)))
   (lambda (scm-var c-var typespec status-var)
     (list scm-var " = scm_c_gtype_instance_to_scm ((GTypeInstance*)" c-var ");\n"))
   (lambda (c-var typespec status-var force?)
     (list))
   "Custom")

  ;; Here we wrap some functions to bootstrap the core library.

  (gw:wrap-function
   ws
   '%init-gnome-gobject
   '<gw:void>
   "scm_init_gnome_gobject"
   '()
   "Export a number of fundamental gtypes and functions to operate on objects.")

  (gw:wrap-function
   ws
   '%post-init-gnome-gobject
   '<gw:void>
   "scm_post_init_gnome_gobject"
   '()
   "Pull scheme definitions back into the C world.")

  (gw:wrap-function
   ws
   '%init-gnome-gobject-primitives
   '<gw:void>
   "scm_init_gnome_gobject_primitives"
   '()
   "Export some functions to operate on primitive data structures, and
pull scheme definitions back into the C world.")

  ;; And here we wrap the g_type_* functions, just because it's nice and
  ;; easy with g-wrap.

  (gw:wrap-function
   ws
   'gtype-name
   '(<gw:mchars> callee-owned const)
   "g_type_name"
   '((<gtype> type))
   "Return the name of a gtype.")

  (gw:wrap-function
   ws
   'gtype-from-name
   '<gtype>
   "g_type_from_name"
   '(((<gw:mchars> caller-owned const) name))
   "Given a name, return the corresponding gtype or #f if not found.")

  (gw:wrap-function
   ws
   'gtype-from-instance
   '<gtype>
   "G_TYPE_FROM_INSTANCE"
   '((<gtype-instance> instance))
   "Given a primitive GTypeInstance, return its corresponding gtype.")

  (gw:wrap-function
   ws
   'gtype-parent
   '<gtype>
   "g_type_parent"
   '((<gtype> type))
   "Returns the parent gtype of a gtype.")

  (gw:wrap-function
   ws
   'gtype-is-a?
   '<gw:bool>
   "g_type_is_a"
   '((<gtype> type) (<gtype> is-a-type))
   "Returns #t if IS-A-TYPE is a parent of TYPE, #f otherwise.")

  (gw:wrap-function
   ws
   'gtype-is-classed?
   '<gw:bool>
   "G_TYPE_IS_CLASSED"
   '((<gtype> type))
   "Returns #t if TYPE is classed, #f otherwise.")

  (gw:wrap-function
   ws
   'gtype-is-instantiatable?
   '<gw:bool>
   "G_TYPE_IS_INSTANTIATABLE"
   '((<gtype> type))
   "Returns #t if TYPE is instantiatable, #f otherwise.")

  (gw:wrap-function
   ws
   'g-source-set-closure
   '<gw:void>
   "g_source_set_closure"
   '((<g-source*> source) (<gclosure> closure))
   "Set the closure for SOURCE to CLOSURE."))

