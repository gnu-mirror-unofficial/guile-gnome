;; guile-gnome
;; Copyright (C) 2005 Andread Rottmann <rotty at debian dot org>
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
  #:use-module (gnome gw support g-wrap)
  #:use-module (gnome gw cairo-spec)
  #:use-module (gnome gw pango-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <gdk-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-gdk
  #:dependencies '(standard gnome-glib gnome-gobject gnome-cairo gnome-pango))

(define-method (global-declarations-cg (self <gdk-wrapset>))
  (list
   (next-method)
   "#include <gdk-pixbuf/gdk-pixbuf.h>\n"
   "#include \"gdk-pixbuf-support.h\"\n"
   "#include <gdk/gdk.h>\n"
   "#include \"gdk-support.h\"\n"))
  
(define-method (initializations-cg (self <gdk-wrapset>) err)
  (list (next-method)
        "if (!gdk_init_check (NULL, NULL))\n"
        "  scm_misc_error (\"gdk-init\","
        "                  \"GDK failed to initialize; is $DISPLAY set correctly?\","
        "                  SCM_EOL);\n"
        ))
  
(define-class <gdk-event-type> (<gobject-classed-pointer-type>))

(define-method (initialize (type <gdk-event-type>) initargs)
  (next-method type (cons #:gtype-id (cons "GDK_TYPE_EVENT" initargs))))

(define-method (unwrap-value-cg (type <gdk-event-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (unwrap-null-checked
     value status-var
     (list
      "if (scm_c_gvalue_holds (" scm-var ", GDK_TYPE_EVENT))\n"
      "  " c-var " = scm_c_gvalue_peek_boxed (" scm-var ");\n"
      "else {\n"
      "  " c-var " = NULL;\n"
      `(gw:error ,status-var type ,(wrapped-var value))
      "}\n"))))

(define-method (wrap-value-cg (type <gdk-event-type>)
                              (value <gw-value>)
                              status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     "if (" c-var " == NULL) {\n"
     "  " scm-var " = SCM_BOOL_F;\n"
     "} else {\n"
     "  " scm-var " = scm_c_gvalue_new_from_boxed (GDK_TYPE_EVENT, " c-var ");\n"
     "}\n")))

(define-method (initialize (ws <gdk-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw gdk) initargs)))
  
  (add-type-alias! ws "GdkWChar" 'unsigned-int32)
  
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
  
  (wrap-custom-boxed!
   "GdkRectangle" "GDK_TYPE_RECTANGLE"
   ;; wrap
   (list scm-var " = " c-var " ? scm_gdk_rectangle_to_scm (" c-var ") : SCM_BOOL_F;\n")
   ;; unwrap
   (list c-var " = scm_scm_to_gdk_rectangle (" scm-var ");\n"))

  (wrap-custom-boxed!
   "GdkColor" "GDK_TYPE_COLOR"
   ;; wrap
   (list scm-var " = " c-var " ? scm_gdk_color_to_scm (" c-var ") : SCM_BOOL_F;\n")
   ;; unwrap
   (list c-var " = scm_scm_to_gdk_color (" scm-var ");\n"))

  (wrap-opaque-pointer! ws "GdkPixbufFormat*")
  (wrap-opaque-pointer! ws "GdkAtom")
  (add-type-rule! ws "GdkAtom*" '(<gdk-atom> out))
  (wrap-freeable-pointer! ws "GdkRegion" "gdk_region_destroy")

  (wrap-instance! ws #:ctype "GdkDrawable" #:gtype-id "GDK_TYPE_DRAWABLE")
  (add-type-alias! ws "GdkDrawable*" '<gdk-drawable>)
  (add-type-alias! ws "GdkBitmap*" '<gdk-drawable>)

  (load-defs-with-overrides ws "gnome/defs/gdk-pixbuf.defs")
  (load-defs-with-overrides ws "gnome/defs/gdk.defs"))
