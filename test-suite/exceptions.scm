(define-module (test-suite exceptions)
  #:export (exception:value-arg-missing
            exception:wrong-number-of-args))

(defmacro deferr (name-frag error re)
  (let ((name (symbol-append 'exception: name-frag)))
    `(define ,name (cons ,error ,re))))

(deferr value-arg-missing     'gruntime-error "^Missing #:value argument")
(deferr wrong-number-of-args  'gruntime-error "^Wrong number of arguments")
