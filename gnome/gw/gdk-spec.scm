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

(define-module (gnome gw gdk-spec)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw pango-spec)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <gdk-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-gdk)

(define-method (global-declarations-cg (self <gobject-wrapset-base>))
  (list
   (next-method)
   "#include <gdk/gdk.h>\n"
   "#include \"gdk-support.h\"\n"))
  
(define-method (initializations-cg (self <gobject-wrapset-base>) err)
  '("gdk_init (NULL, NULL);\n"))
  
(define-method (initialize (ws <gdk-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gtk gw-gdk) initargs)))
  
  (depends-on! ws 'standard 'gnome-glib 'gnome-gobject 'gnome-pango)

  (add-type-alias! ws "GdkWChar" 'unsigned-long)
  
  (for-each
   (lambda (ctype)
     (let ((event (make <gdk-event-type>
                    #:ctype ctype
                    #:c-type-name (string-append ctype "*"))))
       (add-type! ws event)
       (add-type-alias! ws (string-append ctype "*") (name event))))
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
  (add-type-alias! ws "GdkNativeWindow" 'unsigned-long)
  
  (load-defs ws "gdk.defs"))


(define-class <gdk-event-type> (<gobject-classed-pointer-type>))

(define-method (initialize (type <gdk-event-type>) initargs)
  (next-method type (cons #:gtype-id (cons "GDK_TYPE_EVENT" initargs))))

(define-method (unwrap-value-cg (type <gdk-event-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     (unwrap-null-check value status-var)
     "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), GDK_TYPE_EVENT))\n"
     "  " c-var " = (" (c-type-name type)  ") g_value_get_boxed ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
     "else {\n"
     "  " c-var " = NULL;\n"
     `(gw:error ,status-var type ,scm-var)
    "}\n")))

(define-method (wrap-value-cg (type <gdk-event-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     "  " scm-var " = scm_c_make_gvalue (GDK_TYPE_EVENT);\n"
     "  g_value_set_boxed ((GValue *) SCM_SMOB_DATA (" scm-var "), " c-var ");\n"
     "}\n")))

