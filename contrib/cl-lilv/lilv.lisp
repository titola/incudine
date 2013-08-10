;;; Copyright (c) 2013 Tito Latini
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :lilv)

(defvar *world* (make-world))

(defun init-world ()
  (when (free-p *world*)
    (setf *world* (world-new))
    (world-load-all *world*)
    *world*))

(declaim (inline free-p))
(defun free-p (obj)
  (declare (type pointer-wrap))
  (cffi:null-pointer-p (pointer-wrap-pointer obj)))

(defgeneric free (obj))

(defmethod free ((obj world))
  (unless (free-p obj)
    (world-free (world-pointer obj))
    (tg:cancel-finalization obj)
    (setf (world-pointer obj) (cffi:null-pointer))
    (values)))

(defmethod free ((obj instance))
  (unless (free-p obj)
    (let ((ptr (instance-pointer obj)))
      (when (instance-active-p obj)
        (instance-deactivate obj)
        (setf (instance-active-p obj) nil))
      (instance-free ptr)
      (tg:cancel-finalization obj)
      (setf (instance-pointer obj) (cffi:null-pointer))
      (values))))