#! /bin/sh
# -*- scheme -*-
exec guile-gnome-0 -s $0 "$@"
!#
;; guile-gnome
;; Copyright (C) 2003,2004 Free Software Foundation, Inc.

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


(read-set! keywords 'prefix)
(use-modules (oop goops)
             (gnome gobject)
             (gnome glib)
             (gnome gtk)
             (srfi srfi-13)
             (srfi srfi-1)
             (ice-9 rw)
             (ice-9 receive))

;; Why aren't these part of the distro?
(define (file-size f)
  (stat:size (stat f)))
(define (read-file f)
  (let* ((size (file-size f))
         (str (make-string size #\ )))
    (with-input-from-file f
      (lambda () (if (eq? size (read-string!/partial str)) str "")))))

;; Detecting and loading up the demos
(define this-dir (dirname (car (program-arguments))))
(set! %load-path (cons this-dir %load-path))
(define (build-path . args)
  (string-join args "/"))
(define (get-demos)
  (let* ((demo-dir (build-path this-dir "demos"))
         (demos '())
         (dir (opendir demo-dir)))
    (do ((entry (readdir dir) (readdir dir)))
        ((eof-object? entry))
      (if (and (eq? 'regular (stat:type (stat (build-path demo-dir entry))))
               (string-suffix? ".scm" entry))
          ;; cons on the module and source of the file
          (set! demos
                (cons 
                 (cons
                  (resolve-module
                   (list 'demos
                         (string->symbol (substring entry 0 (- (string-length entry) 4))))
                   #t)
                  (read-file (build-path demo-dir entry)))
                 demos))))
    demos))
(define (demo-module demo)
  (car demo))
(define (demo-source demo)
  (cdr demo))
(define (demo-name demo)
  (eval 'name (demo-module demo)))
(define (demo-main demo)
  (eval 'main (demo-module demo)))
(define (demo-description demo)
  (eval 'description (demo-module demo)))
  
;; The main window
(define main-window (make <gtk-window> :title "Guile-Gtk Demo" :type 'toplevel))
(define (make-scrolled)
  (make <gtk-scrolled-window>
    :hscrollbar-policy 'automatic
    :vscrollbar-policy 'automatic))
(define tree-view
  (let* ((store (gtk-list-store-new
                 (list gtype:gboxed-scm gtype:gchararray)))
         (tree-view (make <gtk-tree-view> :model store))
         (cellrenderer (make <gtk-cell-renderer-text>))
         (column (make <gtk-tree-view-column> :title "Widget (double-click to show)"))
         (selection (get-selection tree-view)))
    (for-each
     (lambda (demo)
       (gtk-tree-or-list-store-set
        store (gtk-list-store-append store)
        0 demo
        1 (demo-name demo)))
     (get-demos))
    (set-mode selection 'single)
    (pack-start column cellrenderer #t)
    (add-attribute column cellrenderer "text" 1)
    (append-column tree-view column)
    (set-size-request tree-view 200 -1)
    tree-view))
(define notebook (make <gtk-notebook>))
(define (make-text-view wrap-mode pix name)
  (let* ((scrolled (make-scrolled))
         (text-view (make <gtk-text-view>))
         (text-buffer (get-buffer text-view)))
    (set text-view 'editable #f)
    (set text-view 'cursor-visible #f)
    (set text-view 'wrap-mode wrap-mode)
    (set text-view 'pixels-above-lines pix)
    (set text-view 'pixels-below-lines pix)
    (set scrolled 'shadow-type 'in)
    (add scrolled text-view)
    (append-page notebook scrolled
                 (make <gtk-label> :label name :use-underline #t))
    text-buffer))
(define info-buffer (make-text-view 'word 2 "_Info"))
(define source-buffer (make-text-view 'none 0 "_Source"))
(create-tag info-buffer "title" 'font "Sans 18")
(create-tag source-buffer "source" 'font "Courier 12"
            'pixels-above-lines 0 'pixels-below-lines 0)
(define (clear-buffer buf)
  (receive (start end) (get-bounds buf)
    (gtk-text-buffer-delete buf start end)))
(define (set-demo demo)
  (clear-buffer info-buffer)
  (let ((name (demo-name demo))
        (desc (demo-description demo))
        (iter (get-iter-at-offset info-buffer 0))
        (start #f))
    (insert info-buffer iter name)
    (set! start (get-iter-at-offset info-buffer 0))
    (apply-tag-by-name info-buffer "title" start iter)
    (insert info-buffer iter "\n")
    (insert info-buffer iter desc))
  (clear-buffer source-buffer)
  (let ((source (demo-source demo))
        (iter (get-iter-at-offset source-buffer 0))
        (start #f))
    (insert source-buffer iter source)
    (set! start (get-iter-at-offset source-buffer 0))
    (apply-tag-by-name source-buffer "source" start iter)))

(set-default-size main-window 600 400)

;; Pack the widgets
(let ((hbox (make <gtk-hbox>)))
  (add main-window hbox)
  (let ((scrolled (make-scrolled)))
    (add scrolled tree-view)
    (pack-start hbox scrolled #f #f 0))
  (pack-start hbox notebook #t #t 0))

;; Signals...
(connect main-window 'delete-event (lambda (w e) (gtk-main-quit) #f))
(connect
 tree-view 'row-activated
 (lambda (tview path col)
   (let* ((model (get-model tview))
          (iter (gtk-tree-model-get-iter model path))
          (demo (gtk-tree-model-get-value model iter 0))
          (main (demo-main demo)))
     (main))))
(connect
 (get-selection tree-view) 'changed
 (lambda (selection)
   (receive (model iter)
            (get-selected selection)
     (if iter
         (set-demo (gtk-tree-model-get-value model iter 0))))))

(emit (get-selection tree-view) 'changed)

(show-all main-window)
(gtk-main)
