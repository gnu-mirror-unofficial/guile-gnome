(define-module (gnome gda)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gobject primitives)
  #:use-module (gnome gobject gw-utils)
  #:use-module (gnome gda gw-gda)
  
  #:export (value))

(define-method (make-instance (class <gda-primitive-class>) . initargs)
  (let ((instance (allocate-instance class initargs))
        (primitive (get-keyword #:%primitive-instance initargs #f)))
    (if primitive
        (slot-set! instance 'gtype-primitive primitive)
        (initialize instance initargs))
    instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GdaValue
;;;
(define-method (initialize (value <gda-value>) initargs)
  (let ((init-value (get-keyword #:value initargs *unspecified*)))
    (if (unspecified? init-value)
        (gruntime-error "Missing #:value argument"))
    (slot-set! value 'gtype-primitive (gda-value-primitive-new init-value))))

(define-accessor value)

(define-method (value (v <gda-value>))
  (gda-value-primitive-get (slot-ref v 'gtype-primitive)))

(define-method ((setter value) (v <gda-value>) newval)
  (gda-value-primitive-set! (slot-ref v 'gtype-primitive) newval))

(define-method (write (v <gda-value>) port)
  (display "#<gda-value " port)
  (write (value v) port)
  (display #\> port))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GdaCommand
;;;
(define-method (write (command <gda-command>) port)
  (display "#<gda-command " port)
  (display (genum->symbol (get-command-type command)) port)
  (display #\space port)
  (write (get-text command) port)
  (display #\> port))

(define-method (initialize (command <gda-command>) initargs)
  (let ((type (get-keyword #:type initargs *unspecified*))
        (text (get-keyword #:text initargs *unspecified*))
        (options (get-keyword #:options initargs '())))
    (if (unspecified? type)
        (gruntime-error "Missing #:type argument"))
    (if (unspecified? text)
        (gruntime-error "Missing #:text argument"))
    ;; WART: This creates a uneeded instance
    (slot-set! command 'gtype-primitive
               (slot-ref (gda-command-new text type options)
                         'gtype-primitive))))

(re-export-bindings (oop goops))
(re-export-bindings (gnome gda gw-gda))

