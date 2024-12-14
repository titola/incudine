;;; Copyright (c) 2013-2024 Tito Latini
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:defcstruct mouse-event
    (x sample)
    (y sample)
    (button :int))

  (defun alloc-mouse-event ()
    (let ((ptr (cffi:foreign-alloc '(:struct mouse-event))))
      (cffi:with-foreign-slots ((x y button) ptr (:struct mouse-event))
        (setf x +sample-zero+ y +sample-zero+ button 0))
      ptr))

  (defvar *mouse-event* (alloc-mouse-event))
  (declaim (type foreign-pointer *mouse-event*))

  (incudine.util::defglobal *mouse-spinlock* (make-spinlock "Mouse"))
  (declaim (type spinlock *mouse-spinlock*))

  (defvar *mouse-thread* nil)

  (declaim (inline mouse-status))
  (defun incudine.util:mouse-status ()
    "Mouse-loop thread status. Return :STARTED or :STOPPED."
    (if (= (get-mouse-status) 1) :started :stopped))

  (defun incudine.util:mouse-start ()
    "Create the mouse-loop thread and return :STARTED if no error has occured."
    #+x11
    (labels ((start-thread ()
               (unless (and *mouse-thread*
                            (bt:thread-alive-p *mouse-thread*))
                 (setf *mouse-thread*
                       (bt:make-thread
                         (lambda ()
                           (if (minusp (mouse-init))
                               (incudine-error "Mouse initialization failed.")
                               (mouse-loop-start *mouse-event*)))
                         :name "mouse-loop"))
                 (loop do (sleep .05)
                       until (bt:thread-alive-p *mouse-thread*))
                 :started))
             (start ()
               (with-spinlock-held (*mouse-spinlock*)
                 (unless (eq (incudine.util:mouse-status) :started)
                   (start-thread)))))
      (if (rt-thread-p)
          (nrt-funcall #'start)
          (start)))
    #-x11
    (nrt-msg warn "Mouse pointer support requires X window system."))

  (defun incudine.util:mouse-stop ()
    "Stop the mouse-loop thread and return :STOPPED."
    #+x11
    (unless (eq (mouse-status) :stopped)
      (if (rt-thread-p)
          (nrt-funcall #'mouse-stop)
          (with-spinlock-held (*mouse-spinlock*)
            (cond ((zerop (mouse-loop-stop))
                   (sleep .05)
                   (loop while (bt:thread-alive-p *mouse-thread*))
                   :STOPPED)
                  (t
                   (incudine-error "Failed to stop the mouse-loop thread."))))))
    #-x11
    (nrt-msg warn "Mouse pointer support requires X window system."))

  (declaim (inline incudine.util:get-mouse-x))
  (defun incudine.util:get-mouse-x ()
    "Return the coordinate x of the mouse pointer position.
This value is of type SAMPLE in the range [0,1]."
    (cffi:foreign-slot-value *mouse-event* '(:struct mouse-event) 'x))

  (declaim (inline incudine.util:get-mouse-y))
  (defun incudine.util:get-mouse-y ()
    "Return the coordinate y of the mouse pointer position.
This value is of type SAMPLE in the range [0,1]."
    (cffi:foreign-slot-value *mouse-event* '(:struct mouse-event) 'y))

  (declaim (inline incudine.util:get-mouse-button))
  (defun incudine.util:get-mouse-button ()
    "Return the mouse button state. 1 means the button has been pressed,
0 means it hasn't."
    (incudine.util::truly-the bit
      (cffi:foreign-slot-value *mouse-event* '(:struct mouse-event) 'button)))

  (define-vug mouse-x ()
    "Return the coordinate x of the mouse pointer position.
This value is of type SAMPLE in the range [0,1]."
    (:pre-hook #'incudine.util:mouse-start)
    ;; :PRE-HOOK makes sense only during the compilation. A check during
    ;; the initialization is safe, especially if we use a DSP compiled
    ;; in a fasl file.
    (initialize (incudine.util:mouse-start))
    (incudine.util:get-mouse-x))

  (define-vug mouse-y ()
    "Return the coordinate y of the mouse pointer position.
This value is of type SAMPLE in the range [0,1]."
    (:pre-hook #'incudine.util:mouse-start)
    (initialize (incudine.util:mouse-start))
    (incudine.util:get-mouse-y)))

(define-vug mouse-button ()
  "Return the mouse button state. 1 means the button has been pressed,
0 means it hasn't."
  (:pre-hook #'incudine.util:mouse-start)
  (initialize (incudine.util:mouse-start))
  (incudine.util:get-mouse-button))

(define-vug lin-mouse-x (min max)
  "Return the coordinate x of the mouse pointer position, linearly
rescaled to be between MIN and MAX."
  (lin->lin (mouse-x) 0 1 min max))

(define-vug lin-mouse-y (min max)
  "Return the coordinate y of the mouse pointer position, linearly
rescaled to be between MIN and MAX."
  (lin->lin (mouse-y) 0 1 min max))

(define-vug exp-mouse-x (min max)
  "Return the coordinate x of the mouse pointer position,
exponentially rescaled to be between MIN and MAX.

MIN and MAX are non-zero values. The sign of MAX has to be the sign of MIN."
  (lin->exp (mouse-x) 0 1 min max))

(define-vug exp-mouse-y (min max)
  "Return the coordinate y of the mouse pointer position,
exponentially rescaled to be between MIN and MAX.

MIN and MAX are non-zero values. The sign of MAX has to be the sign of MIN."
  (lin->exp (mouse-y) 0 1 min max))
