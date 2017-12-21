;;; Copyright (c) 2013-2017 Tito Latini
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

(in-package :incudine.util)

(defstruct (spinlock (:constructor make-spinlock (&optional name))
                     (:copier nil))
  (name "Anonymous spinlock" :type string)
  (state 0 :type fixnum))

(defmethod print-object ((obj spinlock) stream)
  (format stream "#<~A ~S>" (type-of obj) (spinlock-name obj)))

(declaim (inline acquire-spinlock))
(defun acquire-spinlock (spinlock)
  (declare (type spinlock spinlock))
  (loop if (zerop (compare-and-swap (spinlock-state spinlock) 0 1))
        return t))

(declaim (inline release-spinlock))
(defun release-spinlock (spinlock)
  (declare (type spinlock spinlock))
  (barrier (:memory)
    (setf (spinlock-state spinlock) 0))
  nil)

;;; Acquire spinlock for the dynamic scope of BODY.
;;; Inspired by SB-THREAD:WITH-MUTEX
#+sbcl
(progn
  (defun call-with-spinlock (function spinlock)
    (declare (type function function) (type spinlock spinlock))
    (sb-int:dx-let ((got-it nil))
      (without-interrupts
        (unwind-protect
             (when (setq got-it (sb-sys:allow-with-interrupts
                                  (acquire-spinlock spinlock)))
               (sb-sys:with-local-interrupts (funcall function)))
          (when got-it
            (release-spinlock spinlock))))))

  (defmacro with-spinlock-held ((place) &body body)
    `(sb-int:dx-flet ((with-spinlock-thunk () ,@body))
       (call-with-spinlock #'with-spinlock-thunk ,place))))

#-sbcl
(defmacro with-spinlock-held ((place) &body body)
  `(unwind-protect
        (progn
          (acquire-spinlock ,place)
          ,@body)
     (release-spinlock ,place)))
