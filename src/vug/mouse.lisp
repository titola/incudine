;;; Copyright (c) 2013-2014 Tito Latini
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

  (declaim (inline alloc-mouse-event))
  (defun alloc-mouse-event ()
    (let ((ptr (cffi:foreign-alloc '(:struct mouse-event))))
      (cffi:with-foreign-slots ((x y button) ptr (:struct mouse-event))
        (setf x +sample-zero+ y +sample-zero+ button 0))
      ptr))

  (defvar *mouse-event* (alloc-mouse-event))
  (declaim (type foreign-pointer *mouse-event*))

  (defvar *mouse-spinlock* (make-spinlock "Mouse"))
  (declaim (type spinlock *mouse-spinlock*))

  (defvar *mouse-thread* nil)

  (declaim (inline mouse-status))
  (defun mouse-status ()
    (case (get-mouse-status)
      (-1 :no-init)
      (0 :stopped)
      (1 :started)))

  #+linux
  (defun mouse-start ()
    (nrt-funcall
     (lambda ()
       (with-spinlock-held (*mouse-spinlock*)
         (flet ((start ()
                  (unless (and *mouse-thread*
                               (bt:thread-alive-p *mouse-thread*))
                    (setf *mouse-thread*
                          (bt:make-thread (lambda ()
                                            (mouse-loop-start *mouse-event*))
                                          :name "mouse-loop")))))
           (case (mouse-status)
             (:started nil)
             (:no-init (if (minusp (mouse-init))
                           (nrt-msg warn "MOUSE-START failed")
                           (start)))
             (otherwise (start))))))))

  #-linux
  (defun mouse-start ()
    (nrt-msg warn "MOUSE-START failed"))

  (declaim (inline get-mouse-x get-mouse-y get-mouse-button))
  (defun get-mouse-x ()
    (cffi:foreign-slot-value *mouse-event* '(:struct mouse-event) 'x))

  (defun get-mouse-y ()
    (cffi:foreign-slot-value *mouse-event* '(:struct mouse-event) 'y))

  (defun get-mouse-button ()
    (cffi:foreign-slot-value *mouse-event* '(:struct mouse-event) 'button)))

(define-vug mouse-x ()
  :pre-hook #'mouse-start
  ;; :PRE-HOOK makes sense only during the compilation. A check during
  ;; the initialization is safe, especially if we use a DSP compiled
  ;; in a fasl file.
  (initialize (mouse-start))
  (get-mouse-x))

(define-vug mouse-y ()
  :pre-hook #'mouse-start
  (initialize (mouse-start))
  (get-mouse-y))

(define-vug mouse-button ()
  :pre-hook #'mouse-start
  (initialize (mouse-start))
  (get-mouse-button))

(define-vug lin-mouse-x (min max)
  (lin->lin (mouse-x) 0 1 min max))

(define-vug lin-mouse-y (min max)
  (lin->lin (mouse-y) 0 1 min max))

(define-vug exp-mouse-x (min max)
  (lin->exp (mouse-x) 0 1 min max))

(define-vug exp-mouse-y (min max)
  (lin->exp (mouse-y) 0 1 min max))
