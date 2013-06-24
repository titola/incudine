;;; Copyright (c) 2013 Tito Latini
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

(in-package :incudine)

(defstruct (foreign-array (:constructor %make-foreign-array)
                          (:copier nil))
  (data (null-pointer) :type foreign-pointer)
  (type :double)
  (size 1 :type positive-fixnum))

(declaim (inline make-foreign-array))
(defun make-foreign-array (dimension element-type &key zero-p
                           initial-element initial-contents)
  (let* ((data (foreign-rt-alloc element-type :count dimension
                                 :zero-p zero-p
                                 :initial-element initial-element
                                 :initial-contents initial-contents))
         (obj (%make-foreign-array :data data :size dimension
                                   :type element-type)))
    (tg:finalize obj (lambda () (foreign-rt-free data)))
    obj))

(declaim (inline free-foreign-array))
(defun free-foreign-array (obj)
  (declare (type foreign-array obj))
  (unless (null-pointer-p #1=(foreign-array-data obj))
    (foreign-rt-free #1#)
    (setf #1# (null-pointer))
    (tg:cancel-finalization obj)
    (values)))

(defmethod free ((obj foreign-array))
  (free-foreign-array obj))

(defmethod data ((obj foreign-array))
  (foreign-array-data obj))
