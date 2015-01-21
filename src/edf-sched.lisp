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

(in-package :incudine.edf)

;;; Earliest Deadline First scheduling (EDF)

(defstruct (node (:copier nil))
  (time +sample-zero+ :type sample)
  (function incudine.util::*dummy-function-without-args* :type function)
  (args nil :type list))

(define-constant +node-root+    1)
(define-constant +first-parent+ 2)

(defstruct (heap (:constructor %make-heap))
  (data (error "edf heap required") :type simple-vector)
  (next-node +node-root+ :type positive-fixnum)
  (temp-node (make-node) :type node))

(defmethod print-object ((obj heap) stream)
  (declare (ignore obj))
  (format stream "#<EDF:HEAP>"))

(defvar *dummy-node* (make-node))
(declaim (type node *dummy-node*))

(defvar *heap-size*
  (if (typep *rt-edf-heap-size* 'positive-fixnum)
      (if (power-of-two-p *rt-edf-heap-size*)
          *rt-edf-heap-size*
          (next-power-of-two *rt-edf-heap-size*))
      1024))
(declaim (type non-negative-fixnum *heap-size*))

(defun make-heap (&optional (size *heap-size*))
  (declare (type non-negative-fixnum size))
  (let ((size (next-power-of-two size)))
    (%make-heap
      :data (make-array size :element-type 'node
                        :initial-contents (loop repeat size
                                                collect (make-node))))))

(defvar *heap* (make-heap))
(declaim (type heap *heap*))

(declaim (inline heap-empty-p))
(defun heap-empty-p ()
  (= (heap-next-node *heap*) +node-root+))

(declaim (inline heap-count))
(defun heap-count ()
  (1- (heap-next-node *heap*)))

(declaim (inline heap-node))
(defun heap-node (index)
  (svref (heap-data *heap*) index))

(declaim (inline set-heap-node))
(defun set-heap-node (index node)
  (setf (svref (heap-data *heap*) index) node))

(defsetf heap-node set-heap-node)

(declaim (inline node-update))
(defun node-update (node time function args)
  (setf (node-time node) time
        (node-function node) function
        (node-args node) args)
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
  (unless (>= (heap-next-node *heap*) *heap-size*)
    (let ((curr (heap-next-node *heap*))
          (t0 (sample time)))
      (declare #.*standard-optimize-settings*
               (type positive-fixnum curr) (type sample t0))
      (loop while (> curr +node-root+) do
           (let ((parent (ash curr -1)))
             (declare (type positive-fixnum parent))
             (cond ((< t0 (node-time (heap-node parent)))
                    (node-copy (heap-node curr) (heap-node parent))
                    (setf curr parent))
                   (t (return)))))
      (node-update (heap-node curr) t0 function args)
      (incf (heap-next-node *heap*)))))

(declaim (inline %%at))
(defun %%at (time function args)
  (declare (type sample time) (type function function) (type list args))
  (if (or (null *rt-thread*) (rt-thread-p))
      (%at time function args)
      (incudine:fast-nrt-funcall (lambda ()
                                   (incudine::fast-rt-funcall
                                    (lambda () (%at time function args))))))
  (values))

(declaim (inline at))
(defun at (time function &rest args)
  (%%at time function args))

(define-compiler-macro at (&whole form &environment env time &rest rest)
  (if (and (constantp time env) (not (typep time 'sample)))
      `(at ,(sample time) ,@rest)
      form))

;;; Anaphoric macro for AT.
;;; The variable IT is bound to the time, the first argument of AT.
(defmacro aat (time function &rest args)
  (let ((it (intern "IT")))
    `(let ((,it ,time))
       (at ,it ,function ,@args))))

(defun get-heap ()
  (declare #.*standard-optimize-settings*
           #+(or cmu sbcl) (values node))
  (cond ((> (heap-next-node *heap*) +node-root+)
         ;; node 0 used for the result
         (node-copy (heap-node 0) (heap-node +node-root+))
         (decf (heap-next-node *heap*))
         (node-move (heap-node +node-root+) (heap-node (heap-next-node *heap*)))
         (node-copy (heap-temp-node *heap*) (heap-node +node-root+))
         (let ((parent +node-root+)
               (curr +first-parent+))
           (declare (type positive-fixnum parent curr))
           (loop while (< curr (heap-next-node *heap*)) do
                (let ((sister (1+ curr)))
                  (when (and (< sister (heap-next-node *heap*))
                             (> (node-time (heap-node curr))
                                (node-time (heap-node sister))))
                    (incf curr))
                  (cond ((> (node-time (heap-temp-node *heap*))
                            (node-time (heap-node curr)))
                         (node-copy (heap-node parent) (heap-node curr))
                         (setf parent curr curr (ash parent 1)))
                        (t (return)))))
           (node-copy (heap-node parent) (heap-temp-node *heap*))
           (heap-node 0)))
        (t *dummy-node*)))

(defmacro sched-loop ()
  `(loop while (and (> (heap-next-node *heap*) ,+node-root+)
                    (>= (+ (incudine:now) ,(sample 0.5))
                        (node-time (heap-node ,+node-root+))))
         do (let ((curr-node (get-heap)))
              (declare (type node curr-node))
              (apply (node-function curr-node) (node-args curr-node)))))

(defun last-time ()
  (declare #.*standard-optimize-settings* #.incudine.util:*reduce-warnings*)
  (labels ((rec (i t0)
             (declare (type non-negative-fixnum i) (type sample t0))
             (if (= i (heap-next-node *heap*))
                 t0
                 (rec (1+ i) (max t0 (node-time (heap-node i)))))))
    (let ((start (if (power-of-two-p (heap-next-node *heap*))
                     (ash (heap-next-node *heap*) -1)
                     (let ((n (ash (heap-next-node *heap*) -2)))
                       (if (< n 2) (+ n 1) (next-power-of-two n))))))
      (declare (type non-negative-fixnum start))
      (rec (1+ start) (node-time (heap-node start))))))

(defvar *flush-pending-hook* nil
  "List of functions without arguments to invoke during the next call
to FLUSH-PENDING. The functions are called in non-realtime thread.
The list is empty after FLUSH-PENDING.")

(defvar *flush-pending-spinlock* (incudine.util:make-spinlock "FLUSH-PENDING"))
(declaim (type incudine.util:spinlock *flush-pending-spinlock*))

(defun add-flush-pending-hook (function)
  (incudine.util:with-spinlock-held (*flush-pending-spinlock*)
    (push function *flush-pending-hook*)
    function))

(defun remove-flush-pending-hook (function)
  (incudine.util:with-spinlock-held (*flush-pending-spinlock*)
    (setf *flush-pending-hook*
          (delete function *flush-pending-hook* :test #'eq))
    (values)))

(defun clear-flush-pending-hook ()
  (incudine.util:with-spinlock-held (*flush-pending-spinlock*)
    (setf *flush-pending-hook* nil)))

(defun flush-pending ()
  (flet ((rt-flush ()
           (setf (heap-next-node *heap*) 1))
         (nrt-flush ()
           (dolist (fun *flush-pending-hook*)
             (funcall fun))
           (clear-flush-pending-hook)))
    (cond ((rt-thread-p)
           (rt-flush)
           (incudine:nrt-funcall #'nrt-flush))
          ((null *rt-thread*)
           (rt-flush)
           (nrt-flush))
          (t (incudine:fast-nrt-funcall
              (lambda () (incudine::fast-rt-funcall #'rt-flush)))
             (nrt-flush)))
    (values)))
