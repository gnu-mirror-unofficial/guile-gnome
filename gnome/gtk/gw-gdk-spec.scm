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
;;g-wrap specification for GDK.
;;
;;; Code:

(define-module (gnome gtk gw-gdk-spec)
  :use-module (g-wrap)
  :use-module (gnome gtk gw-pango-spec)
  :use-module (gnome gobject gw-spec-utils)
  :use-module (gnome gobject defs-support))

(define (gdk:gwrap-event ws ctype)
  (define (c-type-name-func typespec)
    (if (memq 'const (gw:typespec-get-options typespec))
        (string-append "const " ctype " *")
        (string-append ctype " *")))

  (define (scm->c-ccg c-var scm-var typespec status-var)
    (list
     (if (memq 'null-ok (gw:typespec-get-options typespec))
         (list
          "if (SCM_FALSEP (" scm-var "))\n"
          "  " c-var " = NULL;\n"
          "else ")
         '())
     "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), GDK_TYPE_EVENT))\n"
     "  " c-var " = (" (c-type-name-func typespec) ") g_value_get_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
     "else {\n"
     "  " c-var " = NULL;\n"
     `(gw:error ,status-var type ,scm-var)
     "}\n"))

  (define (c->scm-ccg scm-var c-var typespec status-var)
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     "  " scm-var " = scm_c_make_gvalue (GDK_TYPE_EVENT);\n"
     "  g_value_set_boxed ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"
     "}\n"))
  
  (define (c-destructor c-var typespec status-var force?)
    ;; our temp vars are just pointers, there's nothing to clean up
    '())
  
  (gobject:gwrap-helper ws ctype c-type-name-func scm->c-ccg
                        c->scm-ccg c-destructor "GdkEvent"))

(let ((ws (gw:new-wrapset "guile-gnome-gw-gdk")))

  (gw:wrapset-set-guile-module! ws '(gnome gtk gw-gdk))
  (gw:wrapset-depends-on ws "guile-gnome-gw-standard")
  (gw:wrapset-depends-on ws "guile-gnome-gw-glib")
  (gw:wrapset-depends-on ws "guile-gnome-gw-gobject")
  (gw:wrapset-depends-on ws "guile-gnome-gw-pango")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (not client-wrapset)
         (list
          "#include <gdk/gdk.h>\n"
          "#include \"gdk-support.h\"\n")
         (list
          "#include <gdk/gdk.h>\n"))))

  (gw:wrapset-add-cs-wrapper-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         '("gdk_init (NULL, NULL);\n")
         '())))

  (register-type "guile-gnome-gw-gdk" "GdkWChar" '<gw:unsigned-long>)

  (for-each
   (lambda (ctype)
     (register-type
      "guile-gnome-gw-gdk"
      (string-append ctype "*")
      (gw:type-get-name (gdk:gwrap-event ws ctype))))
   '("GdkEventAny"
     "GdkEventKey"
     "GdkEventButton"
     "GdkEventScroll"
     "GdkEventMotion"
     "GdkEventExpose"
     "GdkEventVisibility"
     "GdkEventCrossing"
     "GdkEventFocus"
     "GdkEventConfigure"
     "GdkEventProperty"
     "GdkEventSelection"
     "GdkEventDND"
     "GdkEventProximity"
     "GdkEventClient"
     "GdkEventNoExpose"
     "GdkEventWindowState"
     "GdkEventSetting"))

  ;; a hack now -- dunno what to do with this...
  (register-type "guile-gnome-gw-gdk" "GdkNativeWindow" '<gw:unsigned-long>)

  (load-defs ws "gnome/defs/gdk.defs"))

