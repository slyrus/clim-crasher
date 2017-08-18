;;; Copyright (c) 2011-2017 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.
;;;
;;; Note: Originally based on Troels' Henriksen's
;;; clim-demo:image-viewer
;;;

(cl:defpackage :clim-crasher
  (:use #:clim-lisp #:clim)
  (:export #:clim-crasher))

(in-package :clim-crasher)

(defclass clim-crasher-pane (application-pane) ())

(define-application-frame clim-crasher ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (theta :slider
          :min-value -180
          :max-value 180
          :decimal-places 1
          :value 0
          :show-value-p t
          :orientation :horizontal)
   (interactor :interactor
               :text-style (make-text-style :sans-serif nil nil)
               :min-height 100))
  (:layouts
   (default 
       (vertically (:height 250)
         (4/5
          (horizontally (:width 600)
            (1/6 (vertically ()
                   (labelling ())
                   (labelling (:label "Theta")
                     theta)
                   ))))
         (1/5 interactor)))))

(defmethod handle-repaint ((pane clim-crasher-pane) region)
  )

(defun clim-crasher (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'clim-crasher)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Clim-Crasher")
        (run))))

(define-clim-crasher-command (com-redraw :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'clim-crasher-pane)))
    (handle-repaint viewer (or (pane-viewport-region viewer)
                               (sheet-region viewer)))))

(define-clim-crasher-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)))
