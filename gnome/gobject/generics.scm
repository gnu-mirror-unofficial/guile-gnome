(define-module (gnome gobject generics)
  :use-module (gnome gobject)
  :use-module (oop goops))
;  :export (get set emit connect))
; connect-after block unblock disconnect
;           connected? invoke create-signal get-signals get-properties
;           get-property-names find-property))

(define exports '(get set emit connect connect-after block unblock
                  disconnect connected? invoke create-signal get-signals
                  get-properties get-property-names find-property))

(for-each
 (lambda (sym)
   (let* ((existing-var (module-variable (current-module) sym))
          (existing-val (if existing-var (variable-ref existing-var) #f)))
     (module-add! (current-module) sym (make-variable 
                                        (ensure-generic existing-val sym)))))
 exports)

(define-method (get (object <gobject>) (name <symbol>))
  (gobject-get-property object name))

(define-method (set (object <gobject>) (name <symbol>) value)
  (gobject-set-property object name value))

(define-method (emit (object <gobject>) (name <symbol>) . args)
  (apply gobject-signal-emit (append (list object name) args)))

(define-method (connect (object <gobject>) (name <symbol>) (func <procedure>))
  (gobject-signal-connect object name func))

(define-method (connect-after (object <gobject>) (name <symbol>) (func <procedure>))
  (gobject-signal-connect-after object name func))

(define-method (block (object <gobject>) id)
  (gsignal-handler-block object id))

(define-method (unblock (object <gobject>) id)
  (gsignal-handler-unblock object id))

(define-method (disconnect (object <gobject>) id)
  (gsignal-handler-disconnect object id))

(define-method (connected? (object <gobject>) id)
  (gsignal-handler-connected? object id))

(define-method (invoke (closure <gclosure>) . args)
  (apply gclosure-invoke (cons closure args)))

(define-method (create-signal (class <gtype-class>) (name <symbol>) return-type param-types)
  (gobject-class-create-signal class name return-type param-types))

;; this one's a macro, i'm too lazy to make the method now...
;;(define-method (define-signal (class <gtype-class>) return-type . param-types)

(define-method (get-signals (class <gtype-class>))
  (gobject-class-get-signals class))

(define-method (get-properties (class <gtype-class>))
  (gobject-class-get-properties class))

(define-method (get-property-names (class <gtype-class>))
  (gobject-class-get-property-names class))

(define-method (find-properties (class <gtype-class>))
  (gobject-class-find-properties class))

(for-each
 (lambda (sym)
   (let ((mpi (module-public-interface (current-module))))
     (module-add! mpi sym (module-variable (current-module) sym))))
 exports)