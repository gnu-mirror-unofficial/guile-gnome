#! /usr/bin/guile -s
!#
;; guile-gnome
;; Copyright (C) 2004-2005 Free Software Foundation, Inc.

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

(debug-enable 'debug)
(debug-enable 'backtrace)

(use-modules (srfi srfi-8)
	     (gnome glib))

(define (stderr string . rest)
  (apply format (current-error-port) string rest)
  (force-output (current-error-port)))

(define (fifo-callback loop source condition)
  (stderr "fifo\n")
  (receive (status line len term) (g-io-channel-read-line source)
    (stderr "result: ~S\n" line)
    (if (equal? line "exit\n")
        (g-main-loop-quit loop)))
  #t)

(define (talk fifo-name)
  (sleep 1)
  (let ((fifo (open-file fifo-name "w")))
    (display "Do you read me?\n" fifo)
    (system (string-append "echo Do you read me>" fifo-name))
    (sleep 2)
    (display "exit\n" fifo)
    (system (string-append "echo exit >" fifo-name))))

(define (main)
  (let ((fifo (tmpnam))
        (loop (g-main-loop-new)))
    
    (mknod fifo 'fifo #o600 0)
    (if (= 0 (primitive-fork))
        (begin
          (talk fifo)
          (exit 0)))
    
    (g-io-add-watch
     (g-io-channel-new-file fifo) 'in
     (lambda (source condition) (fifo-callback loop source condition)))
    
    (g-main-loop-run loop)
    (delete-file fifo)))

(main)

