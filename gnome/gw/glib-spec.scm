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
;;A g-wrap specification for GLib 2.x.
;;
;;; Code:

(define-module (gnome gw glib-spec)
  #:use-module (oop goops)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <glib-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-glib #:dependencies '(standard))

(define-class <client-actions> (<gw-item>))

(define-method (global-declarations-cg (ws <glib-wrapset>)
                                       (a <client-actions>))
  '("#include <glib.h>\n"))
  
(define-method (global-declarations-cg (ws <glib-wrapset>))
  (list
   (next-method)
   "#include <glib.h>\n"
   "#include \"glib-support.h\"\n"))

(define-method (initializations-cg (ws <glib-wrapset>) err)
  (list
   (next-method)
   "scm_init_glib ();\n"))

(define-method (initialize (ws <glib-wrapset>) initargs)
  (next-method ws (append '(#:module (gnome gw glib)) initargs))
  
  (add-client-item! ws (make <client-actions>))
  
  (for-each
   (lambda (pair) (add-type-alias! ws (car pair) (cadr pair)))
   '(
     ;; Basic C types
     ("void" void)
     ("int" int)
     ("char" char)
     ("float" float)
     ("double" double)
     ("short" short)
     ("unsigned-char" unsigned-char)
     ("unsigned-short" unsigned-short)
     ("unsigned" unsigned-int)
     ("unsigned-int" unsigned-int)
     ("long" long)
     ("unsigned-long" unsigned-long)
     ("long-long" long-long)
     ("unsigned-long-long" unsigned-long-long)

     ;; <inttypes.h>
     ("uint8_t" unsigned-int8)
     ("uint16_t" unsigned-int16)
     ("uint32_t" unsigned-int32)
     ("uint64_t" unsigned-int64)
     ("int8_t" int8)
     ("int16_t" int16)
     ("int32_t" int32)
     ("int64_t" int64)

     ("SCM" scm) ; not really glib, but oh well

     ;; GLib type aliases
     ("gboolean" bool)
     ("gchar" char)
     ("guchar" unsigned-char)
     ("char*" mchars)
     ("gchar*" mchars)
     ("gdouble" double)
     ("gfloat" float)
     ("gshort" short)
     ("gushort" unsigned-short)
     ("gint8" int8)
     ("guint8" unsigned-int8)
     ("gint" int)
     ("gint16" int16)
     ("guint" unsigned-int)
     ("guint16" unsigned-int16)
     ("gint32" int32)
     ("glong" long)
     ("gulong" unsigned-long)
     ("guint32" unsigned-int32)
     ("gint64" int64)
     ("guint64" unsigned-int64)
     
     ("GQuark" unsigned-int) ; need to wrap this one better
     ("GPid" int)
     
     ("gssize" int) ; fixme: system-dependant
     ("gsize" unsigned-int) ; fixme: system-dependant
     ("gunichar" unsigned-long)
     
     ("none" void)))

  (add-type! ws (make <glist-of-type> #:name 'glist-of
                      #:type-cname "GList*" #:func-prefix "g_list"))
  (add-type-alias! ws "GList*" 'glist-of)

  (add-type! ws (make <glist-of-type> #:name 'gslist-of
                      #:type-cname "GSList*" #:func-prefix "g_slist"))
  (add-type-alias! ws "GSList*" 'gslist-of)

  (add-type! ws (make <gerror-type> #:name '<GError>))
  (add-type-alias! ws "GError**" '<GError>)

  (add-type-rule! ws '(("gint*" "*")) '(int out))

  (load-defs ws "gnome/defs/glib.defs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ((glist-of (<gtk-window> gw:const) gw:const) win-list)
;; shamelessly stolen from upstream g-wrap so that we can use glib 2.0

(define-class <glist-of-type> (<gw-type>)
  (type-cname #:getter type-cname #:init-keyword #:type-cname)
  (func-prefix #:getter func-prefix #:init-keyword #:func-prefix))

(define-class <gw-collection-typespec> (<gw-typespec>)
  (sub-typespec #:getter sub-typespec #:init-keyword #:sub-typespec))

(define-method (all-types (ts <gw-collection-typespec>))
  (cons (type (sub-typespec ts)) (next-method)))

(define-method (c-type-name (type <glist-of-type>))
  (type-cname type))

(define-method (c-type-name (type <glist-of-type>)
                            (typespec <gw-collection-typespec>))
  (if (memq 'const (options typespec))
      (list "const " (type-cname type))
      (type-cname type)))

;; if this succeeds, the glist-of typespec-options will be
;; (sub-typespec (caller-owned | callee-owned) [const])
(define-method (make-typespec (type <glist-of-type>) (options <list>))
  (if (null? options)
      (raise (condition
              (&gw-bad-typespec
               (type type) (options options)
               (message "Missing glist-of options form.")))))
  (if (< (length options) 2)
      (raise
       (condition
        (&gw-bad-typespec
         (type type) (options options)
         (message "glist-of options form must have at least 2 options.")))))
  (let ((sub-typespec (car options))
        (glist-options (cdr options))
        (remainder (cdr options)))
    
    (if (not (is-a? sub-typespec <gw-typespec>))
        (raise (condition
                (&gw-bad-typespec
                 (type type) (options options)
                 (message "glist-of options form must have a sub-typespec as first option.")))))

    
    (set! remainder (delq 'const remainder))
    (if (and (memq 'caller-owned remainder)
             (memq 'callee-owned remainder))
        (raise (condition
                (&gw-bad-typespec
                 (type type) (options options)
                 (message
                  "Bad glist-of options form (caller and callee owned!).")))))
    
    (if (not (or (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder)))
        (raise
         (condition
          (&gw-bad-typespec
           (type type) (options options)
           (message
            "Bad glist-of options form (must be caller or callee owned!).")))))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'callee-owned remainder))
    (if (null? remainder)
        (make <gw-collection-typespec>
          #:type type
          #:sub-typespec sub-typespec
          #:options glist-options)
        (raise (condition
                (&gw-bad-typespec
                 (type type) (options options)
                 (message
                  (format #f "Bad glist-of options form - spurious options: ~S"
                          remainder))))))))

(define-method (unwrap-value-cg (glist-type <glist-of-type>)
                                (value <gw-value>)
                                status-var)

  (let* ((c-var (var value))
         (scm-var (scm-var value))
         (sub-typespec (sub-typespec (typespec value)))
         (sub-type (type sub-typespec))
         (tmp-rest-var (gen-c-tmp "scm_rest"))
         (func-prefix (func-prefix glist-type))
         (sub-item-c-type (c-type-name sub-type sub-typespec))
         (tmp-sub-item-c-var (gen-c-tmp "c_item"))
         (tmp-sub-item-scm-var (gen-c-tmp "scm_item"))
         (tmp-sub-item (make <gw-value>
                         #:typespec sub-typespec
                         #:var tmp-sub-item-c-var
                         #:wrapped-var (string-append
                                        "&" tmp-sub-item-scm-var)))
         (tmp-cursor (gen-c-tmp "cursor")))
      
      (list
       "{\n"
       "  SCM " tmp-rest-var " = " scm-var ";\n"
       "  " c-var "= NULL;\n"
       "  while(!SCM_NULLP(" tmp-rest-var ")\n"
       "        && (! " `(gw:error? ,status-var) "))\n"
       "  {\n"
       "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
       "    SCM " tmp-sub-item-scm-var " = SCM_CAR(" tmp-rest-var ");\n"
       "\n"
       (unwrap-value-cg sub-type tmp-sub-item status-var)
       "\n"
       "    if(! " `(gw:error? ,status-var) " )\n"
       "    {\n"
       "       " c-var " = " func-prefix "_prepend (" c-var ", (gpointer)" tmp-sub-item-c-var");\n"
       "    }\n"
       "    " tmp-rest-var " = SCM_CDR (" tmp-rest-var ");\n"
       "  }\n"
       "  if(!" `(gw:error? ,status-var) ")\n"
       "  {\n"
       "    " c-var " = " func-prefix "_reverse(" c-var ");\n"
       "  }\n"
       "  else\n"
       "  {\n"
       "    " (c-type-name glist-type (typespec value)) tmp-cursor " = " c-var ";\n"
       "    while(" tmp-cursor ")\n"
       "    {\n"
       "      " sub-item-c-type " " tmp-sub-item-c-var ";\n"
       "      " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
       (string-append tmp-cursor "->data") ";\n"
       ;; FIMXE: had force #t here
       (destroy-value-cg sub-type tmp-sub-item status-var) 
       tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
       "    }\n"
       "    " func-prefix "_free(" c-var ");\n"
       "    " c-var " = NULL;\n"
       "  }\n"
       "}\n")))

(define-method (wrap-value-cg (glist-type <glist-of-type>)
                              (value <gw-value>)
                              status-var)
  (let* ((c-var (var value))
         (scm-var (scm-var value))
         (sub-typespec (sub-typespec (typespec value)))
         (sub-type (type sub-typespec))
         (tmp-rest-var (gen-c-tmp "c_rest"))
         (sub-item-c-type (c-type-name sub-type sub-typespec))
         (tmp-sub-item-c-var (gen-c-tmp "c_item"))
         (tmp-sub-item-scm-var (gen-c-tmp "scm_item"))
         (tmp-sub-item (make <gw-value>
                         #:typespec sub-typespec
                         #:var tmp-sub-item-c-var
                         #:wrapped-var (string-append
                                        "&" tmp-sub-item-scm-var))))
                         
    (list
     (c-type-name glist-type (typespec value)) tmp-rest-var " = " c-var ";\n"
     scm-var "= SCM_EOL;\n"
     "while(" tmp-rest-var " && (! " `(gw:error? ,status-var) "))\n"
     "{\n"
     "  " sub-item-c-type " " tmp-sub-item-c-var ";\n"
     "  SCM " tmp-sub-item-scm-var ";\n"
     "\n"
     "  " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
     (string-append tmp-rest-var "->data") ";\n"
     "\n"
     (wrap-value-cg sub-type tmp-sub-item status-var)
     "\n"
     "  if(! " `(gw:error? ,status-var) " )\n"
     "  {\n"
     "     " scm-var " = scm_cons (" tmp-sub-item-scm-var ", " scm-var ");\n"
     "  }\n"
     "  " tmp-rest-var " = " (string-append tmp-rest-var "->next") ";\n"
     "}\n"
     "if(!" `(gw:error? ,status-var) ")\n"
     "{\n"
     "  " scm-var " = scm_reverse(" scm-var ");\n"
     "}\n")))

(define-method (destroy-value-cg (glist-type <glist-of-type>)
                                  (value <gw-value>)
                                  status-var)
  (let* ((c-var (var value))
         (scm-var (scm-var value))
         (options (options (typespec value)))
         (sub-typespec (sub-typespec (typespec value)))
         (sub-type (type sub-typespec))
         (func-prefix (func-prefix glist-type))
         (sub-item-c-type (c-type-name sub-type sub-typespec))
         (tmp-sub-item-c-var (gen-c-tmp "c_item"))
         (tmp-sub-item (make <gw-value>
                     #:typespec sub-typespec
                     #:var tmp-sub-item-c-var))
         (tmp-cursor (gen-c-tmp "cursor")))
    (list
     "{\n"
     "  " (c-type-name glist-type (typespec value)) tmp-cursor " = " c-var ";\n"
     "  while(" tmp-cursor ")\n"
     "  {\n"
     "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
     "    " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
     (string-append tmp-cursor "->data") ";\n"
     (destroy-value-cg sub-type tmp-sub-item status-var)
     tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
     "  }\n"
     (if (memq 'caller-owned options)
         (list "  if(" c-var ")\n"
               "  {\n"
               "    " func-prefix "_free(" c-var ");\n"
               "    " c-var " = NULL;\n"
               "  }\n")
         '())
     "}\n")))
  
;;; GError

(define-class <gerror-type> (<gw-type>))

(define-method (initialize (self <gerror-type>) initargs)
  (next-method self (append '(#:arguments-visible? #f) initargs)))
    
(define-method (c-type-name (type <gerror-type>))
  "GError*")

(define-method (check-typespec-options (type <gerror-type>) (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'null-ok remainder))
    (if (not (null? remainder))
        (raise (condition
                (&gw:bad-typespec (type type) (options options)))))))
    
(define-method (destroy-value-cg (t <gerror-type>)
                                  (value <gw-value>)
                                  status-var)
  (list "g_clear_error(&" (var value) ");\n"))

(define-method (pre-call-arg-cg (t <gerror-type>)
                                (value <gw-value>)
                                status-var)
  (list (var value) " =  NULL;\n"))


(define-method (call-arg-cg (t <gerror-type>) (value <gw-value>))
  (list "&" (var value)))

(define-method (post-call-arg-cg (t <gerror-type>)
                                 (value <gw-value>)
                                 status-var)
  (let* ((c-name (var value))
         (typespec (typespec value)))
    (list
     "if (" c-name ") {\n" 
     "  SCM scm_gerror = scm_list_3(scm_ulong2num(" c-name "->domain), scm_ulong2num(" c-name "->code), scm_makfrom0str(" c-name "->message));\n"
     (destroy-value-cg t value status-var)
     "  scm_throw(scm_str2symbol(\"g-error\"), scm_gerror);\n"
     "}\n")))
