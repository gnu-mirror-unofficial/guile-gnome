;; guile-gnome
;; Copyright (C) 2004 Free Software Foundation, Inc.

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


(use-modules (gnome gtk) (gnome gtk graphical-repl))

(define (guile-gtk-repl)
  (let* ((w (make <gtk-window> #:title "Guile-Gtk REPL"))
         (repl (make <guile-gtk-repl>))
         (main-loop (g-main-loop-new #f #f))
         (in-port (get repl 'in-port))
         (out-port (get repl 'out-port))
         (old-in-port #f)
         (old-out-port #f)
         (old-error-port #f)
         (old-repl-reader #f))

    (add w repl)
    (set-default-size w 600 400)
    (show-all w)
    (connect w 'delete-event (lambda args (apply throw 'quit args) #f))

    (dynamic-wind
        (lambda ()
          (set! old-in-port (set-current-input-port in-port))
          (set! old-out-port (set-current-output-port out-port))
          (set! old-error-port (set-current-error-port out-port))
          (set! old-repl-reader repl-reader)
          (set! repl-reader
                (lambda (prompt)
                  (display prompt)
                  (force-output)
                  (run-hook before-read-hook)
                  (read))))

        top-repl

        (lambda ()
          (set-current-input-port old-in-port)
          (set-current-output-port old-out-port)
          (set-current-error-port old-error-port)
          (set! repl-reader old-repl-reader)))))

(guile-gtk-repl)
