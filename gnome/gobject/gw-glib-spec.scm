;;; -*-scheme-*-

(define-module (gnome gobject gw-glib-spec)
  :use-module (g-wrap)
  :use-module (g-wrap gw-standard-spec)
  :use-module (gnome gobject defs-support))

(for-each
 (lambda (pair) (register-type "guile-gnome-gw-glib" (car pair) (cadr pair)))
 '(("gboolean" <gw:bool>)
   ("char" <gw:char>)
   ("gchar" <gw:char>)
   ("guchar" <gw:unsigned-char>)
   ("char*" <gw:mchars>)
   ("gchar*" <gw:mchars>)
   ("double" <gw:double>)
   ("gdouble" <gw:double>)
   ("float" <gw:float>)
   ("gfloat" <gw:float>)
   ("short" <gw:short>)
   ("gint8" <gw:short>)
   ("guint8" <gw:unsigned-short>)
   ("int" <gw:int>)
   ("gint" <gw:int>)
   ("gint16" <gw:int>)
   ("guint" <gw:unsigned-int>)
   ("guint16" <gw:unsigned-int>)

   ("SCM" <gw:scm>) ; not really glib, but oh well

   ("GQuark" <gw:unsigned-int>) ; need to wrap this one better

   ("gssize" <gw:int>) ; fixme: system-dependant
   ("gsize" <gw:unsigned-int>) ; fixme: system-dependant

   ("gint32" <gw:long>) ; fixme: what about when longs are 64 bits?
   ("glong" <gw:long>)
   ("guint32" <gw:unsigned-long>)
   ("gunichar" <gw:unsigned-long>)
   ("gulong" <gw:unsigned-long>)
   ("gint64" <gw:long-long>)
   ("guint64" <gw:unsigned-long-long>)
   ("none" <gw:void>)
   ("void" <gw:void>)))

(let ((ws (gw:new-wrapset "guile-gnome-gw-glib")))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-set-guile-module! ws '(gnome gobject gw-glib))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (not client-wrapset)
         '("#include <glib.h>\n"
           "#include \"glib-support.h\"\n")
         '("#include <glib.h>\n"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ((glist-of (<gtk-window> gw:const) gw:const) win-list)
  ;; shamelessly stolen from upstream g-wrap so that we can use glib 2.0
  (let ((glo (gw:wrap-type ws 'glist-of)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          "const GList*"
          "GList*"))
    
    ;; if this succeeds, the glist-of typespec-options will be
    ;; (sub-typespec (caller-owned | callee-owned) [const])
    (define (typespec-options-parser options-form wrapset)
      (if (null? options-form)
          (throw 'gw:bad-typespec
                 "Missing glist-of options form."
                 options-form))
      (if (< (length options-form) 2)
          (throw 'gw:bad-typespec
                 "glist-of options form must have at least 2 options."
                 options-form))
      (let* ((sub-typespec-form (car options-form))
             (glist-options (cdr options-form))
             (sub-typespec (gw:prototype-form->typespec sub-typespec-form
                                                        wrapset))
             (remainder (cdr options-form)))
        
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad glist-of options form (caller and callee owned!)."
                   options-form))
        
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw
             'gw:bad-typespec
             "Bad glist-of options form (must be caller or callee owned!)."
             options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            (cons sub-typespec glist-options)
            (throw 'gw:bad-typespec
                   "Bad glist-of options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (glist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "scm_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-scm->c-ccg (gw:type-get-scm->c-ccg sub-type))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        
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
         (sub-scm->c-ccg tmp-sub-item-c-var
                         tmp-sub-item-scm-var
                         sub-typespec
                         status-var)
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
         "    " (c-type-name-func typespec) tmp-cursor " = " c-var ";\n"
         "    while(" tmp-cursor ")\n"
         "    {\n"
         "      " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "      " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #t)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "    }\n"
         "    g_list_free(" c-var ");\n"
         "    " c-var " = NULL;\n"
         "  }\n"
         "}\n")))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (glist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "c_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-c->scm-ccg (gw:type-get-c->scm-ccg sub-type)))
        
        (list
         (c-type-name-func typespec) tmp-rest-var " = " c-var ";\n"
         scm-var "= SCM_EOL;\n"
         "while(" tmp-rest-var " && (! " `(gw:error? ,status-var) "))\n"
         "{\n"
         "  " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "  SCM " tmp-sub-item-scm-var ";\n"
         "\n"
         "  " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-rest-var "->data") ";\n"
         "\n"
         (sub-c->scm-ccg tmp-sub-item-scm-var
                         tmp-sub-item-c-var
                         sub-typespec
                         status-var)
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
    
    (define (c-destructor c-var typespec status-var force?)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        (list
         "{\n"
         "  " (c-type-name-func typespec) tmp-cursor " = " c-var ";\n"
         "  while(" tmp-cursor ")\n"
         "  {\n"
         "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "    " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #f)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "  }\n"
         (if (or (memq 'caller-owned (gw:typespec-get-options typespec))
                 force?)
             (list "  if(" c-var ")\n"
                   "  {\n"
                   "    g_list_free(" c-var ");\n"
                   "    " c-var " = NULL;\n"
                   "  }\n")
             '())
         "}\n")))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (list
         (c->scm-ccg scm-name c-name typespec status-var)
         (c-destructor c-name typespec status-var #f))))
    
    (define (post-call-arg-ccg param status-var)
      (let* ((c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (c-destructor c-name typespec status-var #f)))
    
    (gw:type-set-c-type-name-func! glo c-type-name-func)
    (gw:type-set-typespec-options-parser! glo typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! glo scm->c-ccg)
    (gw:type-set-c->scm-ccg! glo c->scm-ccg)
    (gw:type-set-c-destructor! glo c-destructor)  
    
    (gw:type-set-pre-call-arg-ccg! glo pre-call-arg-ccg)
    (gw:type-set-call-ccg! glo call-ccg)
    (gw:type-set-post-call-result-ccg! glo post-call-result-ccg)
    (gw:type-set-post-call-arg-ccg! glo post-call-arg-ccg)
    
    glo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ((gslist-of (<gtk-window> gw:const) gw:const) win-list)
  ;; shamelessly stolen from upstream g-wrap so that we can use glib 2.0
  (let ((glo (gw:wrap-type ws 'gslist-of)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          "const GSList*"
          "GSList*"))
    
    ;; if this succeeds, the gslist-of typespec-options will be
    ;; (sub-typespec (caller-owned | callee-owned) [const])
    (define (typespec-options-parser options-form wrapset)
      (if (null? options-form)
          (throw 'gw:bad-typespec
                 "Missing gslist-of options form."
                 options-form))
      (if (< (length options-form) 2)
          (throw 'gw:bad-typespec
                 "gslist-of options form must have at least 2 options."
                 options-form))
      (let* ((sub-typespec-form (car options-form))
             (gslist-options (cdr options-form))
             (sub-typespec (gw:prototype-form->typespec sub-typespec-form
                                                        wrapset))
             (remainder (cdr options-form)))
        
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad gslist-of options form (caller and callee owned!)."
                   options-form))
        
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw
             'gw:bad-typespec
             "Bad gslist-of options form (must be caller or callee owned!)."
             options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            (cons sub-typespec gslist-options)
            (throw 'gw:bad-typespec
                   "Bad gslist-of options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (gslist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "scm_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-scm->c-ccg (gw:type-get-scm->c-ccg sub-type))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        
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
         (sub-scm->c-ccg tmp-sub-item-c-var
                         tmp-sub-item-scm-var
                         sub-typespec
                         status-var)
         "\n"
         "    if(! " `(gw:error? ,status-var) " )\n"
         "    {\n"
         "       " c-var " = g_slist_prepend (" c-var ", (gpointer)" tmp-sub-item-c-var");\n"
         "    }\n"
         "    " tmp-rest-var " = SCM_CDR (" tmp-rest-var ");\n"
         "  }\n"
         "  if(!" `(gw:error? ,status-var) ")\n"
         "  {\n"
         "    " c-var " = g_slist_reverse(" c-var ");\n"
         "  }\n"
         "  else\n"
         "  {\n"
         "    GSList * " tmp-cursor " = " c-var ";\n"
         "    while(" tmp-cursor ")\n"
         "    {\n"
         "      " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "      " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #t)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "    }\n"
         "    g_slist_free(" c-var ");\n"
         "    " c-var " = NULL;\n"
         "  }\n"
         "}\n")))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (gslist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "c_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-c->scm-ccg (gw:type-get-c->scm-ccg sub-type)))
        
        (list
         "GSList * " tmp-rest-var " = " c-var ";\n"
         scm-var "= SCM_EOL;\n"
         "while(" tmp-rest-var " && (! " `(gw:error? ,status-var) "))\n"
         "{\n"
         "  " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "  SCM " tmp-sub-item-scm-var ";\n"
         "\n"
         "  " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-rest-var "->data") ";\n"
         "\n"
         (sub-c->scm-ccg tmp-sub-item-scm-var
                         tmp-sub-item-c-var
                         sub-typespec
                         status-var)
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
    
    (define (c-destructor c-var typespec status-var force?)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        (list
         "{\n"
         "  GSList * " tmp-cursor " = " c-var ";\n"
         "  while(" tmp-cursor ")\n"
         "  {\n"
         "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "    " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #f)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "  }\n"
         (if (or (memq 'caller-owned (gw:typespec-get-options typespec))
                 force?)
             (list "  if(" c-var ")\n"
                   "  {\n"
                   "    g_slist_free(" c-var ");\n"
                   "    " c-var " = NULL;\n"
                   "  }\n")
             '())
         "}\n")))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (list
         (c->scm-ccg scm-name c-name typespec status-var)
         (c-destructor c-name typespec status-var #f))))
    
    (define (post-call-arg-ccg param status-var)
      (let* ((c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (c-destructor c-name typespec status-var #f)))
    
    (gw:type-set-c-type-name-func! glo c-type-name-func)
    (gw:type-set-typespec-options-parser! glo typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! glo scm->c-ccg)
    (gw:type-set-c->scm-ccg! glo c->scm-ccg)
    (gw:type-set-c-destructor! glo c-destructor)  
    
    (gw:type-set-pre-call-arg-ccg! glo pre-call-arg-ccg)
    (gw:type-set-call-ccg! glo call-ccg)
    (gw:type-set-post-call-result-ccg! glo post-call-result-ccg)
    (gw:type-set-post-call-arg-ccg! glo post-call-arg-ccg)
    
    glo)

  (register-type "guile-gnome-gw-glib" "GList*" 'glist-of)
  (register-type "guile-gnome-gw-glib" "GSList*" 'gslist-of)

  (load-defs ws "gnome/defs/glib.defs")

  ws)
