#! /usr/bin/guile -s
!#
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

(debug-enable 'debug)
(debug-enable 'backtrace)

(use-modules (srfi srfi-8)
	     (gnome gtk))

(define FIFO (tmpnam))

(define (stderr string . rest)
  (apply format (current-error-port) string rest)
  (force-output (current-error-port)))

(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

(define (fifo-callback source condition)
  (stderr "FIFO\n")
  (receive (status line len term) (g-io-channel-read-line source)
    (stderr "result: ~S\n" line)
    (if (equal? line "exit\n")
	(gtk-main-quit)))
  #t)

(define (talk)
  (sleep 1)
  (let ((fifo (open-file FIFO "w")))
    (display "Do you read me?\n" fifo)
    (system (string-append "echo Do you read me>" FIFO))
    (sleep 2)
    (display "exit\n" fifo)
    (system (string-append "echo exit >" FIFO))))
	 
(define (main)
  (mknod FIFO 'fifo #o600 0)
  (if (= 0 (primitive-fork))
      (talk))
  (g-io-add-watch (g-io-channel-new-file FIFO) 'in fifo-callback)
  (gtk-main)
  (delete-file FIFO))

(main)

