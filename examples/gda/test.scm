(use-modules (gnome gda))

(gda-init "Test" "0.1")

(let* ((client (make <gda-client>))
       (conn (open-connection client "sqlitetest" #f #f 'read-only)))

  (let* ((cmd (make <gda-command> #:type 'sql #:text "SELECT * FROM foobar"))
         (data (execute-single-command conn cmd #f))
         (n-rows (get-n-rows data))
         (n-cols (get-n-columns data)))
    (format #t "~S x ~S table\n" n-cols n-rows)
    (do ((row 0 (+ 1 row)))
        ((= row n-rows))
      (do ((col 0 (+ 1 col)))
          ((= col n-cols))
        (format #t "~A " (stringify (get-value-at data col row))))
      (newline)))
  
  (close conn))

  