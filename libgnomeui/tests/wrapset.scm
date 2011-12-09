;;; ----------------------------------------------------------------------
;;;    unit test
;;;    Copyright (C) 2007 Andy Wingo
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; ----------------------------------------------------------------------
(use-modules (unit-test)
             (apicheck)
             (ice-9 pretty-print)
             (oop goops))

(define-class <test-wrapset-api> (<test-case>))

(define *modules*
  (call-with-input-string (getenv "WRAPSET_MODULES") read))
(define *api-file*
  (getenv "WRAPSET_API_FILE"))

(define-method (test-wrapset-api (self <test-wrapset-api>))
  (apicheck-validate
   (call-with-input-file *api-file* read)
   *modules*))

(define (main args)
  (exit-with-summary (run-all-defined-test-cases)))

(define (update-api args)
  (with-output-to-file *api-file*
    (lambda ()
      (pretty-print
       (apicheck-generate 
        *modules*)))))
