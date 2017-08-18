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

(defclass clim-crasher-pane (application-pane) 
  ((image :initarg :image :accessor image)
   (transform-parameters :accessor transform-parameters :initarg :transform-parameters)
   (transform :accessor transform :initarg :transform)
   (lock-x-and-y-scale :accessor lock-x-and-y-scale :initarg :lock-x-and-y-scale)
   (clear-background-needed-p :accessor clear-background-needed-p :initarg :clear-background-needed-p))
  (:default-initargs :image nil
    :transform-parameters #(1d0 1d0 0d0 0d0 0d0 0d0 0d0)
    :lock-x-and-y-scale t
    :clear-background-needed-p nil
    :transform nil))

(defun y-scale-callback (gadget scale)
  (declare (ignore gadget)))

(defun x-scale-callback (gadget scale)
  (declare (ignore gadget)))

(defun theta-callback (gadget degrees)
  (declare (ignore gadget)))

(defun y-shear-callback (gadget shear)
  (declare (ignore gadget)))

(defun x-shear-callback (gadget shear)
  (declare (ignore gadget)))

(define-application-frame clim-crasher ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (lock-scale :toggle-button
               :label "Lock X and Y Scale"
               :value t
               :value-changed-callback
               (lambda (gadget value)
                 (declare (ignore gadget))
                 (let ((viewer (find-pane-named *application-frame* 'clim-crasher-pane)))
                   (with-accessors ((lock-x-and-y-scale lock-x-and-y-scale))
                       viewer
                     (setf lock-x-and-y-scale value)))))
   (y-scale :slider
            :min-value 0.1
            :max-value 4
            :decimal-places 2
            :value 1.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'y-scale-callback
            :value-changed-callback 'y-scale-callback)
   (x-scale :slider
            :min-value 0.1
            :max-value 4
            :decimal-places 2
            :value 1.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'x-scale-callback
            :value-changed-callback 'x-scale-callback)
   (theta :slider
          :min-value -180
          :max-value 180
          :decimal-places 1
          :value 0
          :show-value-p t
          :orientation :horizontal
          :drag-callback 'theta-callback
          :value-changed-callback 'theta-callback)
   (y-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'y-shear-callback
            :value-changed-callback 'y-shear-callback)
   (x-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'x-shear-callback
            :value-changed-callback 'x-shear-callback)
   (interactor :interactor
               :text-style (make-text-style :sans-serif nil nil)
               :min-height 100))
  (:layouts
   (default 
       (vertically (:height 250)
         (4/5
          (horizontally (:width 600)
            #+nil
            (5/6 viewer)
            (1/6 (vertically ()
                   (labelling ()
                     #+nil
                     (vertically ()
                       lock-scale
                       (labelling (:label "Y Scale")
                         y-scale)
                       (labelling (:label "X Scale")
                         x-scale)))
                   (labelling (:label "Theta")
                     theta)
                   (labelling (:label "Y Shear")
                     y-shear)
                   (labelling (:label "X Shear")
                     x-shear)))))
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
