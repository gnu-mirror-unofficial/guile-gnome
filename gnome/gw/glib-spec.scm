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
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (gnome gobject utils)
  #:use-module (gnome gobject gw-spec-utils)
  #:use-module (gnome gobject defs-support))

(define-class <glib-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-glib)

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
  
  (depends-on! ws 'standard)

  (add-client-item! ws (make <client-actions>))
  
  (for-each
   (lambda (pair) (add-type-alias! ws (car pair) (cadr pair)))
   '(("gboolean" bool)
     ("char" char)
     ("gchar" char)
     ("guchar" unsigned-char)
     ("char*" mchars)
     ("gchar*" mchars)
     ("double" double)
     ("gdouble" double)
     ("float" float)
     ("gfloat" float)
     ("short" short)
     ("gshort" short)
     ("gushort" unsigned-short)
     ("unsigned-short" unsigned-short)
     ("gint8" short)
     ("guint8" unsigned-short)
     ("int" int)
     ("gint" int)
     ("gint16" int)
     ("guint" unsigned-int)
     ("unsigned" unsigned-int)
     ("unsigned-int" unsigned-int)
     ("guint16" unsigned-int)

     ("SCM" scm) ; not really glib, but oh well

     ("GQuark" unsigned-int) ; need to wrap this one better
     ("GPid" int)
     
     ("gssize" int) ; fixme: system-dependant
     ("gsize" unsigned-int) ; fixme: system-dependant

     ("gint32" long) ; fixme: what about when longs are 64 bits?
     ("long" long)
     ("glong" long)
     ("unsigned-long" unsigned-long)
     ("gulong" unsigned-long)
     ("guint32" unsigned-long)
     ("gunichar" unsigned-long)
     ("long-long" long-long)
     ("gint64" long-long)
     ("unsigned-long-long" unsigned-long-long)
     ("guint64" unsigned-long-long)
     ("none" void)

     
     ("void" void)))

  (add-type! ws (make <glist-of-type> #:name 'glist-of))
  (add-type-alias! ws "GList*" 'glist-of)

  (add-type! ws (make <gerror-type> #:name '<GError>))
  (add-type-alias! ws "GError**" '<GError>)

  (add-type-rule! ws '(("gint*" "*")) '(int out))

  (load-defs ws "gnome/defs/glib.defs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ((glist-of (<gtk-window> gw:const) gw:const) win-list)
;; shamelessly stolen from upstream g-wrap so that we can use glib 2.0

(define-class <glist-of-type> (<gw-type>))

(define-class <gw-collection-typespec> (<gw-typespec>)
  (sub-typespec #:getter sub-typespec #:init-keyword #:sub-typespec))

(define-method (all-types (ts <gw-collection-typespec>))
  (cons (type (sub-typespec ts)) (next-method)))

(define-method (c-type-name (type <glist-of-type>))
  "GList*")

(define-method (c-type-name (type <glist-of-type>)
                            (typespec <gw-collection-typespec>))
  (if (memq 'const (options typespec))
      "const GList*"
      "GList *"))

;; if this succeeds, the glist-of typespec-options will be
;; (sub-typespec (caller-owned | callee-owned) [const])
(define-method (make-typespec (type <glist-of-type>) (options <list>))
  ;; FIXME: Use raise, not throw
  (if (null? options)
      (throw 'gw:bad-typespec
               "Missing glist-of options form." options))
  (if (< (length options) 2)
      (throw 'gw:bad-typespec
             "glist-of options form must have at least 2 options."
             options))
  (let ((sub-typespec (car options))
        (glist-options (cdr options))
        (remainder (cdr options)))
    
    (if (not (is-a? sub-typespec <gw-typespec>))
        (throw
         'gw:bad-typespec
         "glist-of options form must have a sub-typespec as first option."
         (list sub-typespec options)))
    
    (set! remainder (delq 'const remainder))
    (if (and (memq 'caller-owned remainder)
             (memq 'callee-owned remainder))
        (throw 'gw:bad-typespec
               "Bad glist-of options form (caller and callee owned!)."
                 options))
    
    (if (not (or (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder)))
        (throw
         'gw:bad-typespec
         "Bad glist-of options form (must be caller or callee owned!)."
         options))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'callee-owned remainder))
    (if (null? remainder)
        (make <gw-collection-typespec>
          #:type type
          #:sub-typespec sub-typespec
          #:options glist-options)
        (throw 'gw:bad-typespec
                 "Bad glist-of options form - spurious options: "
                 remainder))))

(define-method (unwrap-value-cg (glist-type <glist-of-type>)
                                (value <gw-value>)
                                status-var)

  (let* ((c-var (var value))
         (scm-var (scm-var value))
         (sub-typespec (sub-typespec (typespec value)))
         (sub-type (type sub-typespec))
         (tmp-rest-var (gen-c-tmp "scm_rest"))
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
       "       " c-var " = g_list_prepend (" c-var ", (gpointer)" tmp-sub-item-c-var");\n"
       "    }\n"
       "    " tmp-rest-var " = SCM_CDR (" tmp-rest-var ");\n"
       "  }\n"
       "  if(!" `(gw:error? ,status-var) ")\n"
       "  {\n"
       "    " c-var " = g_list_reverse(" c-var ");\n"
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
       (destruct-value-cg sub-type tmp-sub-item status-var) 
       tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
       "    }\n"
       "    g_list_free(" c-var ");\n"
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

(define-method (destruct-value-cg (glist-type <glist-of-type>)
                                  (value <gw-value>)
                                  status-var)
  (let* ((c-var (var value))
         (scm-var (scm-var value))
         (options (options (typespec value)))
         (sub-typespec (sub-typespec (typespec value)))
         (sub-type (type sub-typespec))
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
     (destruct-value-cg sub-type tmp-sub-item status-var)
     tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
     "  }\n"
     (if (memq 'caller-owned options)
         (list "  if(" c-var ")\n"
               "  {\n"
               "    g_list_free(" c-var ");\n"
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
    
(define-method (destruct-value-cg (t <gerror-type>)
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
     (destruct-value-cg t value status-var)
     "  scm_throw(scm_str2symbol(\"g-error\"), scm_gerror);\n"
     "}\n")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ((gslist-of (<gtk-window> gw:const) gw:const) win-list)
;; shamelessly stolen from upstream g-wrap so that we can use glib 2.0

;;; FIXME: Convert or generalize glist-of and refactor

;   (let ((glo (gw:wrap-type ws 'gslist-of)))
    
;     (define (c-type-name-func typespec)
;       (if (memq 'const (gw:typespec-get-options typespec))
;           "const GSList*"
;           "GSList*"))
    
;     ;; if this succeeds, the gslist-of typespec-options will be
;     ;; (sub-typespec (caller-owned | callee-owned) [const])
;     (define (typespec-options-parser options-form wrapset)
;       (if (null? options-form)
;           (throw 'gw:bad-typespec
;                  "Missing gslist-of options form."
;                  options-form))
;       (if (< (length options-form) 2)
;           (throw 'gw:bad-typespec
;                  "gslist-of options form must have at least 2 options."
;                  options-form))
;       (let* ((sub-typespec-form (car options-form))
;              (gslist-options (cdr options-form))
;              (sub-typespec (gw:prototype-form->typespec sub-typespec-form
;                                                         wrapset))
;              (remainder (cdr options-form)))
        
;         (set! remainder (delq 'const remainder))
;         (if (and (memq 'caller-owned remainder)
;                  (memq 'callee-owned remainder))
;             (throw 'gw:bad-typespec
;                    "Bad gslist-of options form (caller and callee owned!)."
;                    options-form))
        
;         (if (not (or (memq 'caller-owned remainder)
;                      (memq 'callee-owned remainder)))
;             (throw
;              'gw:bad-typespec
;              "Bad gslist-of options form (must be caller or callee owned!)."
;              options-form))
;         (set! remainder (delq 'caller-owned remainder))
;         (set! remainder (delq 'callee-owned remainder))
;         (if (null? remainder)
;             (cons sub-typespec gslist-options)
;             (throw 'gw:bad-typespec
;                    "Bad gslist-of options form - spurious options: "
;                    remainder))))
    
;     (define (scm->c-ccg c-var scm-var typespec status-var)
;       (let* ((options (gw:typespec-get-options typespec))
;              (sub-typespec (car options))
;              (sub-type (gw:typespec-get-type sub-typespec))
;              (gslist-options (cdr options))
;              (tmp-rest-var (gen-c-tmp "scm_rest"))
;              (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
;              (tmp-sub-item-c-var (gen-c-tmp "c_item"))
;              (tmp-sub-item-scm-var (gen-c-tmp "scm_item"))
;              (sub-scm->c-ccg (gw:type-get-scm->c-ccg sub-type))
;              (sub-destructor (gw:type-get-c-destructor sub-type))
;              (tmp-cursor (gen-c-tmp "cursor")))
        
;         (list
;          ;; Quiets gcc -Wall, although I think everything is covered here
;          c-var " = NULL;\n"
         
;          "{\n"
;          "  SCM " tmp-rest-var " = " scm-var ";\n"
;          "  GSList *" tmp-cursor "= NULL;\n"
;          "  " c-var "= NULL;\n"
;          "  while(!SCM_NULLP(" tmp-rest-var ")\n"
;          "        && (! " `(gw:error? ,status-var) "))\n"
;          "  {\n"
;          "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
;          "    SCM " tmp-sub-item-scm-var " = SCM_CAR(" tmp-rest-var ");\n"
;          "\n"
;          (sub-scm->c-ccg tmp-sub-item-c-var
;                          tmp-sub-item-scm-var
;                          sub-typespec
;                          status-var)
;          "\n"
;          "    if(! " `(gw:error? ,status-var) " )\n"
;          "    {\n"
;          "       " tmp-cursor " = g_slist_prepend (" tmp-cursor ", (gpointer)" tmp-sub-item-c-var");\n"
;          "    }\n"
;          "    " tmp-rest-var " = SCM_CDR (" tmp-rest-var ");\n"
;          "  }\n"
;          "  if(!" `(gw:error? ,status-var) ")\n"
;          "  {\n"
;          "    " c-var " = g_slist_reverse(" tmp-cursor ");\n"
;          "  }\n"
;          "  else\n"
;          "  {\n"
;          "    " tmp-cursor " = (GSList*)" c-var ";\n"
;          "    while(" tmp-cursor ")\n"
;          "    {\n"
;          "      " sub-item-c-type " " tmp-sub-item-c-var ";\n"
;          "      " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
;          (string-append tmp-cursor "->data") ";\n"
;          (if sub-destructor
;              (sub-destructor tmp-sub-item-c-var sub-typespec status-var #t)
;              '())
;          tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
;          "    }\n"
;          "    g_slist_free((GSList*)" c-var ");\n"
;          "    " c-var " = NULL;\n"
;          "  }\n"
;          "}\n")))
    
;     (define (c->scm-ccg scm-var c-var typespec status-var)
;       (let* ((options (gw:typespec-get-options typespec))
;              (sub-typespec (car options))
;              (sub-type (gw:typespec-get-type sub-typespec))
;              (gslist-options (cdr options))
;              (tmp-rest-var (gen-c-tmp "c_rest"))
;              (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
;              (tmp-sub-item-c-var (gen-c-tmp "c_item"))
;              (tmp-sub-item-scm-var (gen-c-tmp "scm_item"))
;              (sub-c->scm-ccg (gw:type-get-c->scm-ccg sub-type)))
        
;         (list
;          (c-type-name-func typespec) " " tmp-rest-var " = " c-var ";\n"
;          scm-var "= SCM_EOL;\n"
;          "while(" tmp-rest-var " && (! " `(gw:error? ,status-var) "))\n"
;          "{\n"
;          "  " sub-item-c-type " " tmp-sub-item-c-var ";\n"
;          "  SCM " tmp-sub-item-scm-var ";\n"
;          "\n"
;          "  " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
;          (string-append tmp-rest-var "->data") ";\n"
;          "\n"
;          (sub-c->scm-ccg tmp-sub-item-scm-var
;                          tmp-sub-item-c-var
;                          sub-typespec
;                          status-var)
;          "\n"
;          "  if(! " `(gw:error? ,status-var) " )\n"
;          "  {\n"
;          "     " scm-var " = scm_cons (" tmp-sub-item-scm-var ", " scm-var ");\n"
;          "  }\n"
;          "  " tmp-rest-var " = " (string-append tmp-rest-var "->next") ";\n"
;          "}\n"
;          "if(!" `(gw:error? ,status-var) ")\n"
;          "{\n"
;          "  " scm-var " = scm_reverse(" scm-var ");\n"
;          "}\n")))
    
;     (define (c-destructor c-var typespec status-var force?)
;       (let* ((options (gw:typespec-get-options typespec))
;              (sub-typespec (car options))
;              (sub-type (gw:typespec-get-type sub-typespec))
;              (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
;              (tmp-sub-item-c-var (gen-c-tmp "c_item"))
;              (sub-destructor (gw:type-get-c-destructor sub-type))
             
;              (tmp-cursor (gen-c-tmp "cursor")))
;         (list
;          "{\n"
;          "  " (c-type-name-func typespec) " " tmp-cursor " = " c-var ";\n"
;          "  while(" tmp-cursor ")\n"
;          "  {\n"
;          "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
;          "    " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
;          (string-append tmp-cursor "->data") ";\n"
;          (if sub-destructor
;              (sub-destructor tmp-sub-item-c-var sub-typespec status-var #f)
;              '())
;          tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
;          "  }\n"
;          (if (or (memq 'caller-owned (gw:typespec-get-options typespec))
;                  force?)
;              (list "  if(" c-var ")\n"
;                    "  {\n"
;                    "    g_slist_free((GSList*)" c-var ");\n"
;                    "    " c-var " = NULL;\n"
;                    "  }\n")
;              '())
;          "}\n")))
    
;     (define (pre-call-arg-ccg param status-var)
;       (let* ((scm-name (gw:param-get-scm-name param))
;              (c-name (gw:param-get-c-name param))
;              (typespec (gw:param-get-typespec param)))
;         (list
;          (scm->c-ccg c-name scm-name typespec status-var)
;          "if(" `(gw:error? ,status-var type) ")"
;          `(gw:error ,status-var arg-type)
;          "else if(" `(gw:error? ,status-var range) ")"
;          `(gw:error ,status-var arg-range))))
    
;     (define (call-ccg result func-call-code status-var)
;       (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
;     (define (post-call-result-ccg result status-var)
;       (let* ((scm-name (gw:result-get-scm-name result))
;              (c-name (gw:result-get-c-name result))
;              (typespec (gw:result-get-typespec result)))
;         (list
;          (c->scm-ccg scm-name c-name typespec status-var)
;          (c-destructor c-name typespec status-var #f))))
    
;     (define (post-call-arg-ccg param status-var)
;       (let* ((c-name (gw:param-get-c-name param))
;              (typespec (gw:param-get-typespec param)))
;         (c-destructor c-name typespec status-var #f)))
    
;     (gw:type-set-c-type-name-func! glo c-type-name-func)
;     (gw:type-set-typespec-options-parser! glo typespec-options-parser)
    
;     (gw:type-set-scm->c-ccg! glo scm->c-ccg)
;     (gw:type-set-c->scm-ccg! glo c->scm-ccg)
;     (gw:type-set-c-destructor! glo c-destructor)  
    
;     (gw:type-set-pre-call-arg-ccg! glo pre-call-arg-ccg)
;     (gw:type-set-call-ccg! glo call-ccg)
;     (gw:type-set-post-call-result-ccg! glo post-call-result-ccg)
;     (gw:type-set-post-call-arg-ccg! glo post-call-arg-ccg)
    
;     glo)

;;(register-type "guile-gnome-gw-glib" "GSList*" 'gslist-of)
  
