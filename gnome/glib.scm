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
;;A GLib 2.0 wrapper for Guile (mainly main loop-related pieces).
;;
;;; Code:

(define-module (gnome glib)
  :use-module (gnome gobject)
  :use-module (gnome gobject gw-glib)
  :use-module (gnome gobject gw-utils)
  :use-module (oop goops))

(re-export-modules (gnome gw glib))

(define-public (g-idle-add proc)
  (let ((closure (make <gclosure>
                   #:return-type gtype:gboolean
                   #:func proc))
        (source (g-idle-source-new)))
    (g-source-set-closure source closure)
    (g-source-set-priority source 200) ; G_PRIORITY_DEFAULT_IDLE
    (g-source-attach source #f)))

(define-public (g-timeout-add milliseconds proc)
  (let ((closure (make <gclosure>
                   #:return-type gtype:gboolean
                   #:func proc))
        (source (g-timeout-source-new milliseconds)))
    (g-source-set-closure source closure)
    (g-source-set-priority source 200) ; G_PRIORITY_DEFAULT_IDLE
    (g-source-attach source #f)))

(use-modules (gnome gobject event-repl))

;; taken from gnome-guile 0.10.0

(define-public (g-main-loop-console-repl)
  (define main-loop (g-main-loop-new #f #f))

  ;; for some reason, this doesn't work with readline... patches welcome
  ;; :)
  (define inport (fdes->inport 1))
  (define outport (current-output-port))

  (define unspecified (if #f #f))
  (define (prompt)
    (display "gtk> " outport)
    (force-output outport))
  (define (print val)
    (cond ((not (eq? unspecified val))
	   (write val outport)
	   (newline outport)))
    (prompt))
  (define (report data)
    (repl-display-backtrace data outport)
    (repl-display-error data outport)
    (prompt))
  (define (nonblocking-read port)
    (let loop ((res '()))
      (if (char-ready? port)
          (let ((ch (read-char port)))
            (if (eof-object? ch)
                (if (null? res)
                    ch 
                    (apply string (reverse res)))
                (loop (cons ch res))))
	  (apply string (reverse res)))))

  (let ((repl (make-event-repl read primitive-eval print report)))
    (g-timeout-add 50
                   (lambda ()
                     (let ((str (nonblocking-read inport)))
                       (if (eof-object? str)
                           (g-main-loop-quit main-loop)
                           (repl-input repl str))
                       #t)))
    (prompt)
    (g-main-loop-run main-loop)))
