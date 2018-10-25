;;; Copyright (c) 2013-2018 Tito Latini
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

(define-constant +rt-node-pool-size+ 500)

(define-constant +rt-node-pool-grow+ 10)

(define-constant +nrt-node-pool-size+ 500)

(define-constant +nrt-node-pool-grow+ (floor (* +nrt-node-pool-size+ 0.1)))

(declaim (inline expand-node-pool))
(defun expand-node-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta (make-node nil 0)))

(defvar *rt-node-pool*
  (make-cons-pool
    :data (loop repeat +rt-node-pool-size+ collect (make-node nil 0))
    :size +rt-node-pool-size+
    :expand-function #'expand-node-pool
    :grow +rt-node-pool-grow+)
  "Pool of nodes used out of the graph in realtime.")
(declaim (type cons-pool *rt-node-pool*))

(defvar *nrt-node-pool*
  (make-cons-pool
    :data (loop repeat +nrt-node-pool-size+ collect (make-node nil 0))
    :size +nrt-node-pool-size+
    :expand-function #'expand-node-pool
    :grow +nrt-node-pool-grow+)
  "Pool of nodes used out of the graph in non-realtime.")
(declaim (type cons-pool *nrt-node-pool*))

(defglobal *nrt-node-pool-spinlock* (make-spinlock "NRT-NODE-POOL"))
(declaim (type spinlock *nrt-node-pool-spinlock*))

(declaim (inline rt-node-pool-pop))
(defun rt-node-pool-pop ()
  (let* ((cons (cons-pool-pop-cons *rt-node-pool*))
         (value (car cons)))
    (rt-global-pool-push-cons cons)
    value))

(declaim (inline rt-node-pool-push))
(defun rt-node-pool-push (node)
  (declare (type node node))
  (let ((cons (rt-global-pool-pop-cons)))
    (setf (car cons) node)
    (cons-pool-push-cons *rt-node-pool* cons)))

(defun nrt-node-pool-pop ()
  (let* ((cons (with-spinlock-held (*nrt-node-pool-spinlock*)
                 (cons-pool-pop-cons *nrt-node-pool*)))
         (value (car cons)))
    (nrt-global-pool-push-cons cons)
    value))

(defun nrt-node-pool-push (node)
  (declare (type node node))
  (let ((cons (nrt-global-pool-pop-cons)))
    (setf (car cons) node)
    (with-spinlock-held (*nrt-node-pool-spinlock*)
      (cons-pool-push-cons *nrt-node-pool* cons))))

(declaim (inline node-pool-pop))
(defun node-pool-pop ()
  (if (allow-rt-memory-p) (rt-node-pool-pop) (nrt-node-pool-pop)))

(declaim (inline node-pool-push))
(defun node-pool-push (node)
  (if (allow-rt-memory-p) (rt-node-pool-push node) (nrt-node-pool-push node)))
