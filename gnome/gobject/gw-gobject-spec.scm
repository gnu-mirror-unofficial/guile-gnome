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
     (gw:inline-scheme '(use-modules (gnome gobject)))))

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

  (gobject:gwrap-object ws "GObject" "G_TYPE_OBJECT")

  (let* ((c-type-name-func
          (lambda (typespec)
            (if (memq 'const (gw:typespec-get-options typespec))
                "const GValue*"
                "GValue*")))
         (scm->c-codegen
          (lambda (c-var scm-var typespec status-var)
            (list "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var "))\n"
                  ;; We allow mutation of the argument unless
                  ;; 'callee-owned is present in the options, in which
                  ;; case we make a copy.
                  (if (memq 'callee-owned (gw:typespec-get-options typespec))
                      `("  {\n"
                        "    " ,c-var " = g_new0 (GValue, 1);\n"
                        "    g_value_init (" ,c-var ", G_VALUE_TYPE (SCM_SMOB_DATA (" scm-var ")));\n"
                        "    g_value_copy ((GValue*)SCM_SMOB_DATA (" ,scm-var "), " c-var ");\n"
                        "  }\n")
                      `("  " ,c-var " = (GValue*) SCM_SMOB_DATA (" ,scm-var ");\n"))
                  "else " `(gw:error ,status-var type ,scm-var))))
         (c->scm-codegen
          (lambda (scm-var c-var typespec status-var)
            (list "if (" c-var " != NULL)\n"
                  (if (memq 'const (gw:typespec-get-options typespec))
                      `("  {\n"
                        "    GValue * tmp = g_new0 (GValue, 1);\n"
                        "    g_value_init (tmp, G_VALUE_TYPE (" ,c-var "));\n"
                        "    g_value_copy (" ,c-var ", tmp);\n"
                        "    SCM_NEWSMOB (" ,scm-var ", scm_tc16_gvalue, tmp);\n"
                        "  }\n")
                      `("  SCM_NEWSMOB (" ,scm-var ", scm_tc16_gvalue, " ,c-var ");\n"))
                  "else\n"
                  "  " scm-var " = SCM_BOOL_F;\n")))
         (cleanup
          (lambda (c-var typespec status-var force?)
            (list))))
    (gobject:gwrap-helper ws "GValue"
                          c-type-name-func scm->c-codegen
                          c->scm-codegen cleanup
                          "Custom"))
  
  (gobject:gwrap-helper
   ws "GClosure"
   (lambda (typespec) "GClosure*")
   (lambda (c-var scm-var typespec status-var)
     (list "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var "))\n"
           "  " c-var " = (GClosure*) g_value_get_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
           "else if (SCM_GCLOSUREP (" scm-var "))\n"
           "  " c-var " = (GClosure*) g_value_get_boxed ((GValue*)SCM_SMOB_DATA (scm_slot_ref (" scm-var ", scm_str2symbol (\"closure\"))));\n"
           "else " `(gw:error ,status-var type ,scm-var)))
   (lambda (scm-var c-var typespec status-var) ; not ideal but ok
     (list scm-var " = scm_c_make_gvalue (G_TYPE_CLOSURE);\n"
           "g_value_set_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "), " c-var ");\n"))
   (lambda (c-var typespec status-var force?)
     (list))
   "Custom")

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

  ;; Wrap the pariah function of gobject -- can't be done in C, because
  ;; <g-source*> is a wcp.

  (gw:wrap-function
   ws
   'g-source-set-closure
   '<gw:void>
   "g_source_set_closure"
   '((<g-source*> source) (<gclosure> closure))
   "Set the closure for SOURCE to CLOSURE."))

