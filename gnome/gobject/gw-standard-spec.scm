;; guile-gnome
;; Copyright (C) Rob Browning <rlb@defaultvalue.org>
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
;; This file was taken from the main g-wrap distribution. Hopefully the
;; changes in it will be incorporated upstream. In the meantime this
;; file so that guile-gobject can be released properly.
;;
;;; Code:

;; -*-scheme-*-

(define-module (gnome gobject gw-standard-spec))

(use-modules (g-wrap))
(use-modules (g-wrap simple-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple ranged integer types.
;;;
;;; code stolen from plain simple-types.  The same, but different :>
  
(define (wrap-simple-ranged-integer-type wrapset
                                         type-sym
                                         c-type-name
                                         c-minval-text ; for unsigned, #f
                                         c-maxval-text
                                         scm->c-function
                                         c->scm-function)
  
  (define (replace-syms tree alist)
    (cond
     ((null? tree) tree)
     ((list? tree) (map (lambda (elt) (replace-syms elt alist)) tree))
     ((symbol? tree)
      (let ((expansion (assq-ref alist tree)))
        (if (string? expansion)
            expansion
            (error "Expected string for expansion..."))))
     (else tree)))
  
  (let* ((simple-type (gw:wrap-type wrapset type-sym))
         (c-sym-name (gw:any-str->c-sym-str (symbol->string type-sym)))
         (minvar (gw:gen-c-tmp (string-append "range_minval" c-sym-name)))
         (maxvar (gw:gen-c-tmp (string-append "range_maxval" c-sym-name))))
    
    (define (c-type-name-func typespec)
      c-type-name)
    
    (define (global-declarations-ccg type client-wrapset)
      (if client-wrapset
          (list (if c-minval-text
                    (list "static SCM " minvar ";\n")
                    '())
                "static SCM " maxvar ";\n")
          '()))
    
    ;; TODO: maybe use status-var.
    (define (global-init-ccg type client-wrapset status-var)
      (if client-wrapset
          (list (if c-minval-text
                    (list minvar " = " c->scm-function "(" c-minval-text ");\n"
                          "scm_gc_protect_object(" minvar ");\n")
                    '())
                maxvar " = " c->scm-function "(" c-maxval-text ");\n"
                "scm_gc_protect_object(" maxvar ");\n")
          '()))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (list "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
            `(gw:error ,status-var type ,scm-var)
            (if c-minval-text
                (list
                 "else if(SCM_FALSEP(scm_geq_p(" scm-var ", " minvar "))"
                 "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))")
                (list
                 "else if(SCM_NFALSEP(scm_negative_p(" scm-var "))"
                 "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))"))
            `(gw:error ,status-var range ,scm-var)
            "else {\n"
            ;; here we pass NULL and 0 as the callers because we've already
            ;; checked the bounds on the argument
            "  " c-var " = " scm->c-function "(" scm-var ", 0, NULL);\n"  
            "}\n"
            "\n"
            "if(" `(gw:error? ,status-var type) ")"
            `(gw:error ,status-var arg-type)
            "else if(" `(gw:error? ,status-var range) ")"
            `(gw:error ,status-var arg-range)))
              
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (list scm-var " = " c->scm-function "(" c-var ");\n"))
    
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
        (c->scm-ccg scm-name c-name typespec status-var)))
    
    (gw:type-set-c-type-name-func! simple-type c-type-name-func)
    (gw:type-set-global-declarations-ccg! simple-type global-declarations-ccg)
    (gw:type-set-global-initializations-ccg! simple-type global-init-ccg)
    (gw:type-set-scm->c-ccg! simple-type scm->c-ccg)
    (gw:type-set-c->scm-ccg! simple-type c->scm-ccg)
    (gw:type-set-pre-call-arg-ccg! simple-type pre-call-arg-ccg)
    (gw:type-set-call-ccg! simple-type call-ccg)
    (gw:type-set-post-call-result-ccg! simple-type post-call-result-ccg)
    
    simple-type))

(let ((ws (gw:new-wrapset "guile-gnome-gw-standard"))
      (limits-requiring-types '()))

  (gw:wrapset-set-guile-module! ws '(gnome gobject gw-standard))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; void
  (let ((wt (gw:wrap-type ws '<gw:void>)))

    (gw:type-set-c-type-name-func!
     wt
     (lambda (typespec) "void"))

    (gw:type-set-scm->c-ccg!
     wt
     (lambda (c-var scm-var typespec status-var)
       (error "Can't convert a <gw:void> from Scheme to C.")))

    (gw:type-set-c->scm-ccg!
     wt
     (lambda (scm-var c-var typespec status-var)
       (error "Can't convert a <gw:void> from C to scm.")))

    (gw:type-set-c-destructor!
     wt
     (lambda (c-var typespec status-var force?)
       (error "Can't destroy a <gw:void>.")))

    (gw:type-set-pre-call-arg-ccg!
     wt
     (lambda (param status-var)
       (error "Can't use <gw:void> as an argument type.")))
    
    ;; no result assignment.
    (gw:type-set-call-ccg!
     wt
     (lambda (result func-call-code status-var)
       (list func-call-code ";\n")))

    (gw:type-set-post-call-result-ccg!
     wt
     (lambda (result status-var)
       (list (gw:result-get-scm-name result) " = SCM_UNSPECIFIED;\n")))
    
    (gw:type-declare-scm-result-var?! wt #f))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:scm> - pass scheme pointers through unmolested.
  (gw:wrap-simple-type ws '<gw:scm>
                       "SCM"
                       '("1")
                       '(c-var " = " scm-var ";\n")
                       '(scm-var " = " c-var ";\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:bool> - boolean type
  (gw:wrap-simple-type ws '<gw:bool> "int"
                       ;; Any scheme value is a valid bool.
                       '("1")
                       '(c-var "= SCM_NFALSEP(" scm-var ");\n")
                       '(scm-var "= (" c-var ") ? SCM_BOOL_T : SCM_BOOL_F;\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:char> -- FIXME: scm chars are 0-255, not [-128,127] like c chars
  (gw:wrap-simple-type ws '<gw:char> "char"
                       '("SCM_NFALSEP(scm_char_p(" scm-var "))\n")
                       '(c-var "= SCM_CHAR(" scm-var ");\n")
                       '(scm-var "= SCM_MAKE_CHAR(" c-var ");\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-char> -- scm chars are bounded to [0,255]
  (gw:wrap-simple-type ws '<gw:unsigned-char> "unsigned char"
                       '("SCM_NFALSEP(scm_char_p(" scm-var "))\n")
                       '(c-var "= SCM_CHAR(" scm-var ");\n")
                       '(scm-var "= SCM_MAKE_CHAR(" c-var ");\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:float>
  (gw:wrap-simple-type ws '<gw:float> "float"
                       '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
                       '(c-var "= gh_scm2double(" scm-var ");\n")
                       '(scm-var "= gh_double2scm(" c-var ");\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:double>
  (gw:wrap-simple-type ws '<gw:double> "double"
                       '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
                       '(c-var "= gh_scm2double(" scm-var ");\n")
                       '(scm-var "= gh_double2scm(" c-var ");\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:short>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:short> "short"
             "SHRT_MIN" "SHRT_MAX"
             "scm_num2short" "scm_short2num")))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-short>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:unsigned-short> "unsigned short"
             #f "USHRT_MAX"
             "scm_num2ushort" "scm_ushort2num")))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:int>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:int> "int"
             "INT_MIN" "INT_MAX"
             "scm_num2int" "scm_int2num")))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-int>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:unsigned-int> "unsigned int"
             #f "UINT_MAX"
             "scm_num2uint" "scm_uint2num")))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:long>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:long> "long"
             "LONG_MIN" "LONG_MAX"
             "scm_num2long" "scm_long2num")))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-long>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:unsigned-long> "unsigned long"
             #f "ULONG_MAX"
             "scm_num2ulong" "scm_ulong2num")))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  (if (string>=? (version) "1.6")
      (begin
        ;; There's a bit of a mess in some older guiles wrt long long
        ;; support. I don't know when it was fixed, but I know that the
        ;; 1.6 series works properly -- apw

        ;; FIXME: how to handle the no-long-longs case nicely?
        ;; Why can't an honest guy seem to get a hold of LLONG_MAX?

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; <gw:long-long>
        (let ((wt (wrap-simple-ranged-integer-type
                   ws '<gw:long-long> "long long"
                   "((long long)0x7fffffffffffffffLL)" "((long long)0x8000000000000000LL)"
                   "scm_num2long_long" "scm_long_long2num")))
          (set! limits-requiring-types (cons wt limits-requiring-types)))
  
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; <gw:unsigned-long-long>
        (let ((wt (wrap-simple-ranged-integer-type
                   ws '<gw:unsigned-long-long> "unsigned long long"
                   #f "((unsigned long long)0xffffffffffffffffLL)"
                   "scm_num2ulong_long" "scm_ulong_long2num")))
          (set! limits-requiring-types (cons wt limits-requiring-types)))))
  
  (let* ((mchars (gw:wrap-type ws '<gw:mchars>)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          "const char *"
          "char *"))

    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form (caller and callee owned!)."
                   options-form))
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form (must be caller or callee owned!)."
                   options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (set! remainder (delq 'null-ok remainder))
        (if (null? remainder)
            options-form
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (list
       c-var " = NULL;\n"
       "\n"
       (if (memq 'null-ok (gw:typespec-get-options typespec))
           (list
            "if (SCM_FALSEP (" scm-var "))\n"
            "  " c-var " = NULL;\n"
            "else ")
           '())
       "if(SCM_STRINGP(" scm-var "))\n"
       "  " c-var " = gh_scm2newstr(" scm-var ", NULL);\n"
       "else\n"
       `(gw:error ,status-var type ,scm-var)))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (list
       "  /* we coerce to (char *) here b/c broken guile 1.3.4 prototype */\n"
       "if(" c-var " == NULL) " scm-var " = SCM_BOOL_F;\n"
       "else "
       scm-var " = gh_str02scm((char *) " c-var ");\n"))
    
    (define (c-destructor c-var typespec status-var force?)
      (if (or force?
              (memq 'caller-owned (gw:typespec-get-options typespec)))
          (list "if(" c-var ") free((void *) " c-var ");\n")
          '()))
    
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
    
    (define (post-call-arg-ccg param status-var)
      (let* ((c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (c-destructor c-name typespec status-var #f)))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (list
         (c->scm-ccg scm-name c-name typespec status-var)
         (c-destructor c-name typespec status-var #f))))

    (gw:type-set-c-type-name-func! mchars c-type-name-func)
    (gw:type-set-typespec-options-parser! mchars typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! mchars scm->c-ccg)
    (gw:type-set-c->scm-ccg! mchars c->scm-ccg)
    (gw:type-set-c-destructor! mchars c-destructor)  
    
    (gw:type-set-pre-call-arg-ccg! mchars pre-call-arg-ccg)
    (gw:type-set-call-ccg! mchars call-ccg)
    (gw:type-set-post-call-arg-ccg! mchars post-call-arg-ccg)
    (gw:type-set-post-call-result-ccg! mchars post-call-result-ccg)
    
    mchars)

  (gw:wrapset-add-cs-before-includes!
   ws
   (lambda (wrapset client-wrapset)
     (if (and client-wrapset
              (gw:any? (lambda (x) (gw:wrapset-uses-type? client-wrapset x))
                       limits-requiring-types))
         "#define _GNU_SOURCE\n"
         '())))
  
  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (and client-wrapset
              (gw:any? (lambda (x) (gw:wrapset-uses-type? client-wrapset x))
                       limits-requiring-types))
         "#include <limits.h>\n"
         '())))

  )
