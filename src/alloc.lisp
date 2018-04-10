;;; Copyright (c) 2018 Tito Latini
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

(defstruct (incudine-object (:copier nil))
  (pool-ptr nil :type list))

(defmethod print-object ((obj incudine-object) stream)
  (format stream "#<INCUDINE-OBJECT ~A>" (type-of obj)))

(defstruct (incudine-object-pool
             (:include cons-pool)
             (:constructor %make-incudine-object-pool)
             (:copier nil))
  (real-time-p nil :type boolean)
  (spinlock nil :type (or null spinlock)))

(defmethod print-object ((obj incudine-object-pool) stream)
  (format stream "#<INCUDINE-OBJECT-POOL ~D>" (incudine-object-pool-size obj)))

(defun new-incudine-object-pointer (constructor &optional object-list)
  (let ((ptr (cons (funcall constructor) object-list)))
    (setf (incudine-object-pool-ptr (car ptr)) ptr)))

(defun make-incudine-object-list (size constructor)
    (do ((i 1 (1+ i))
         (lst (new-incudine-object-pointer constructor)
              (new-incudine-object-pointer constructor lst)))
        ((= i size) lst)))

(defun expand-incudine-object-pool-function (constructor)
  (lambda (pool &optional delta)
    (declare (type incudine-object-pool pool) (ignore delta))
    (prog1 (setf #1=(incudine-object-pool-data pool)
                 (new-incudine-object-pointer constructor #1#))
      (incf (incudine-object-pool-size pool)))))

(defun make-incudine-object-pool (size constructor real-time-p)
  (declare (type positive-fixnum size) (type function constructor))
  (%make-incudine-object-pool
    :data (make-incudine-object-list size constructor)
    :size size
    :expand-func (lambda (pool &optional delta)
                   (declare (type incudine-object-pool pool) (ignore delta))
                   (prog1 (setf #1=(incudine-object-pool-data pool)
                                (new-incudine-object-pointer constructor #1#))
                     (incf (incudine-object-pool-size pool))))
    :grow 1
    :real-time-p real-time-p
    :spinlock (unless real-time-p (make-spinlock))))

(defun incudine-object-pool-expand-1 (pool)
  (flet ((expand ()
           (funcall (cons-pool-expand-func pool) pool 1)))
    (if (and (incudine-object-pool-real-time-p pool) *rt-thread*)
        (rt-eval () (expand))
        (with-spinlock-held ((incudine-object-pool-spinlock pool)) (expand)))))

(defun incudine-object-pool-expand (pool delta)
  (declare (type incudine-object-pool pool) (type positive-fixnum delta))
  (if (= delta 1)
      (incudine-object-pool-expand-1 pool)
      (flet ((expand (lst last)
               (setf (cdr last) (incudine-object-pool-data pool))
               (setf (incudine-object-pool-data pool) lst)
               (incf (incudine-object-pool-size pool) delta)
               (values)))
        (do* ((i 1 (1+ i))
              (last (new-incudine-object-pointer #'incudine::%make-buffer))
              (lst last (new-incudine-object-pointer
                          #'incudine::%make-buffer lst)))
             ((= i delta)
              (if (and (incudine-object-pool-real-time-p pool) *rt-thread*)
                  (rt-eval () (expand lst last))
                  (with-spinlock-held ((incudine-object-pool-spinlock pool))
                    (expand lst last))))
          (declare (type fixnum i) (type cons last lst))))))

(defun ensure-incudine-object-pool-size (pool size)
  (declare (type incudine-object-pool pool) (type positive-fixnum size))
  (let ((curr-size (incudine-object-pool-size pool)))
    (unless (<= size curr-size)
      (incudine-object-pool-expand pool (- size curr-size)))
    (values)))

(defun alloc-object (object-pool)
  (declare (type incudine-object-pool object-pool))
  (with-spinlock-held ((incudine-object-pool-spinlock object-pool))
    (car (cons-pool-pop-cons object-pool))))

(declaim (inline alloc-rt-object))
(defun alloc-rt-object (object-pool)
  (declare (type incudine-object-pool object-pool))
  (car (cons-pool-pop-cons object-pool)))

(defun free-object (obj object-pool)
  (declare (type incudine-object obj)
           (type incudine-object-pool object-pool))
  (with-spinlock-held ((incudine-object-pool-spinlock object-pool))
    (cons-pool-push-cons object-pool (incudine-object-pool-ptr obj))
    (values)))

(defun free-rt-object (obj object-pool)
  (declare (type incudine-object obj)
           (type incudine-object-pool object-pool))
  (rt-eval ()
    (cons-pool-push-cons object-pool (incudine-object-pool-ptr obj)))
  (values))
