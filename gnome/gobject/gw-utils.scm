;; things that are useful to people that use g-wrapped libraries.

(define-module (gnome gobject gw-utils)
  :use-module (srfi srfi-13)
  :use-module (oop goops))

;; 1. methods of generic functions can come from any module.
;;    eg gst_props_entry_get and g_object_get.
;;
;; 2. the generic function can only be defined in one place, or it loses
;;    all knowledge of other methods (gst_props_entry_get replaces all
;;    definitions from other modules, eg g_object_get.)
;;
;; 3. there is no one sensible place to put the generic function
;;    definition. 
;;
;; 4. therefore, as a HACK, modules calling function->methods-public
;;    should first include other generic modules.

(define (function->method proc mpi gw-mpi module)
  (let* ((of-object (module-ref gw-mpi (procedure-property proc 'of-object)))
         (nargs (car (procedure-property proc 'arity)))
         (specializers (make-list nargs <top>))
         (generic-name (procedure-property proc 'generic-name))
         (default-val (if (module-bound? module generic-name)
                          (module-ref module generic-name)
                          #f)))
    
    ;; until we fix g-wrap to make classes from wcts, we have to
    ;; silently ignore non-classes... or perhaps the solution is on the
    ;; guile side, to make smob types classes automagically...
    (if (is-a? of-object <class>)
        (begin
          (list-set! specializers 0 of-object)
          
          ;; this is broken -- generics from one module are overriding
          ;; generics from others...
          (if (not (module-variable mpi generic-name))
              (module-add! mpi generic-name (make-variable (ensure-generic
                                                            default-val generic-name))))
          
          ;; apparently, #:procedure needs to be a closure and not a
          ;; primitive procedure....
          (add-method! (module-ref mpi generic-name)
                       (make <method>
                         #:specializers specializers 
                         #:procedure (lambda args
                                       (apply proc args))))))))

(define-public re-export-bindings
  (procedure->syntax
   (lambda (args env)
     (let ((m (environment-module env)))
       (for-each
        (lambda (l)
          (let* ((gw-mpi (nested-ref the-root-module
                                     (append '(app modules)
                                             l
                                             '(%module-public-interface))))
                 (mpi (module-public-interface m))
                 (mpi-obarray (module-obarray mpi)))

            (module-for-each (lambda (sym var)
                               (module-obarray-set! mpi-obarray sym var))
                             gw-mpi)
            (module-modified mpi)))
        
        (cdr args))))))

(define-public functions->methods-public
  (procedure->syntax
   (lambda (args env)
     (let ((m (environment-module env)))
       (for-each
        (lambda (l)
          (let* ((gw-mpi (nested-ref the-root-module
                                     (append '(app modules)
                                             l
                                             '(%module-public-interface))))
                 (mpi (module-public-interface m))
                 (mpi-obarray (module-obarray mpi)))

            (module-for-each (lambda (sym var)
                               (let ((value (variable-ref var)))
                                 (if (and (procedure? value)
                                          (procedure-property value 'generic-name))
                                     (function->method value mpi gw-mpi (current-module)))))
                             gw-mpi)))
        
        (cdr args))))))
