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

(in-package :incudine.edf)

;;; Earliest Deadline First scheduling (EDF)

(defstruct (node (:copier nil))
  (time +sample-zero+ :type sample)
  (function #'identity :type function)
  (args nil :type list))

(define-constant +node-root+    1)
(define-constant +first-parent+ 2)

(defvar *next-node* +node-root+)
(declaim (type positive-fixnum *next-node*))

(defvar *temp-node* (make-node))
(declaim (type node *temp-node*))

(defvar *dummy-node* (make-node))
(declaim (type node *dummy-node*))

(defvar *heap-size* (if (numberp *rt-edf-heap-size*) *rt-edf-heap-size* 1024))
(declaim (type non-negative-fixnum *heap-size*))

(defvar *heap* (make-array *heap-size* :element-type 'node
                           :initial-contents (loop repeat *heap-size*
                                                   collect (make-node))))
(declaim (type simple-vector *heap*))

(declaim (inline heap-empty-p))
(defun heap-empty-p ()
  (= *next-node* +node-root+))

(declaim (inline heap-count))
(defun heap-count ()
  (1- *next-node*))

(declaim (inline heap-node))
(defun heap-node (index)
  (svref *heap* index))

(declaim (inline set-heap-node))
(defun set-heap-node (index node)
  (setf (svref *heap* index) node))

(defsetf heap-node set-heap-node)

(declaim (inline node-update))
(defun node-update (node time function args)
  (setf (node-time node)     time
        (node-function node) function
        (node-args node)     args)
  node)

(declaim (inline node-copy))
(defun node-copy (dest src)
  (node-update dest (node-time src) (node-function src)
               (node-args src)))

(declaim (inline node-move))
(defun node-move (dest src)
  (node-copy dest src)
  (node-update src +sample-zero+ #'identity nil)
  dest)

(declaim (inline %at))
(defun %at (time function args)
  (declare (type sample time) (type function function)
           (type list args))
  (unless (>= *next-node* *heap-size*)
    (let ((curr *next-node*)
          (t0 (sample time)))
      (declare #.incudine::*standard-optimize-settings*
               (type positive-fixnum curr) (type sample t0))
      (loop while (> curr +node-root+) do
           (let ((parent (ash curr -1)))
             (declare (type positive-fixnum parent))
             (cond ((< t0 (node-time (heap-node parent)))
                    (node-copy (heap-node curr) (heap-node parent))
                    (setf curr parent))
                   (t (return)))))
      (node-update (heap-node curr) t0 function args)
      (incf *next-node*))))

(declaim (inline at))
(defun at (time function &rest args)
  (declare (type sample time) (type function function)
           (type list args))
  (if (or (null *rt-thread*) (rt-thread-p))
      (%at time function args)
      (incudine:fast-nrt-funcall (lambda ()
                                   (incudine::fast-rt-funcall
                                    (lambda ()
                                      (%at time function args))))))
  nil)

(define-compiler-macro at (&whole form &environment env time &rest rest)
  (if (and (constantp time env)
           (not (typep time 'sample)))
      `(at ,(sample time) ,@rest)
      form))

;;; Anaphoric macro for AT.
;;; The variable IT is bound to the time, the first argument of AT.
(defmacro aat (time function &rest args)
  (let ((it (intern "IT")))
    `(let ((,it ,time))
       (at ,it ,function ,@args))))

(defun get-heap ()
  (declare #.incudine::*standard-optimize-settings*
           #+(or cmu sbcl) (values node))
  (cond ((> *next-node* +node-root+)
         ;; node 0 used for the result
         (node-copy (heap-node 0) (heap-node +node-root+))
         (decf *next-node*)
         (node-move (heap-node +node-root+) (heap-node *next-node*))
         (node-copy *temp-node* (heap-node +node-root+))
         (let ((parent +node-root+)
               (curr +first-parent+))
           (declare (type positive-fixnum parent curr))
           (loop while (< curr *next-node*) do
                (let ((sister (1+ curr)))
                  (when (and (< sister *next-node*)
                             (> (node-time (heap-node curr))
                                (node-time (heap-node sister))))
                    (incf curr))
                  (cond ((> (node-time *temp-node*)
                            (node-time (heap-node curr)))
                         (node-copy (heap-node parent) (heap-node curr))
                         (setf parent curr
                               curr   (ash parent 1)))
                        (t (return)))))
           (node-copy (heap-node parent) *temp-node*)
           (heap-node 0)))
        (t *dummy-node*)))

(declaim (inline sched-loop))
(defun sched-loop ()
  (loop while (and (> *next-node* +node-root+)
                   (>= (incudine:now) (node-time (heap-node +node-root+))))
        do (let ((curr-node (get-heap)))
             (declare (type node curr-node))
             (apply (node-function curr-node) (node-args curr-node)))))

(declaim (inline flush-pending))
(defun flush-pending ()
  (if (or (null *rt-thread*) (rt-thread-p))
      (setf *next-node* 1)
      (incudine:fast-nrt-funcall (lambda ()
                                   (incudine::fast-rt-funcall
                                    (lambda ()
                                      (setf *next-node* 1))))))
  (values))
