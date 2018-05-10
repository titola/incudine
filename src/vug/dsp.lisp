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

(in-package :incudine.vug)

(defstruct (dsp (:include incudine-object) (:copier nil))
  (name nil)
  (init-function #'dummy-function :type function)
  (perf-function #'dummy-function :type function)
  (free-function #'dummy-function :type function)
  (controls (make-hash-table :size 16 :test #'equal) :type hash-table)
  (real-time-p nil :type boolean))

(defmethod print-object ((obj dsp) stream)
  (format stream "#<DSP ~A>" (dsp-name obj)))

(defstruct (dsp-properties (:include incudine-object) (:copier nil))
  (rt-instances nil :type list)
  (nrt-instances nil :type list)
  (arguments nil :type list))

(define-constant +dsp-properties-pool-initial-size+ 200)

(defvar *dsp-properties-pool*
  (make-incudine-object-pool +dsp-properties-pool-initial-size+
                             #'make-dsp-properties nil))
(declaim (type incudine-object-pool *dsp-properties-pool*))

(defun get-dsp-properties (name)
  (or (dsp name)
      (setf (gethash name *dsps*)
            (incudine.util::alloc-object *dsp-properties-pool*))))

(define-constant +dsp-instance-pool-initial-size+ *max-number-of-nodes*)

(defun make-rt-dsp () (make-dsp :real-time-p t))

(defvar *dsp-instance-pool*
  (make-incudine-object-pool +dsp-instance-pool-initial-size+ #'make-dsp nil))
(declaim (type incudine-object-pool *dsp-instance-pool*))

(defvar *rt-dsp-instance-pool*
  (make-incudine-object-pool +dsp-instance-pool-initial-size+ #'make-rt-dsp t))
(declaim (type incudine-object-pool *rt-dsp-instance-pool*))

(defvar *nrt-dsp-spinlock* (make-spinlock "NRT-DSP"))
(declaim (type spinlock *nrt-dsp-spinlock*))

(declaim (inline make-dsp-instance))
(defun make-dsp-instance ()
  (if (allow-rt-memory-p)
      (incudine.util::alloc-rt-object *rt-dsp-instance-pool*)
      (incudine.util::alloc-object *dsp-instance-pool*)))

(defun store-dsp-instance (name obj)
  (declare (type symbol name) (type dsp obj))
  (let ((prop (dsp name))
        (cons (dsp-pool-ptr obj)))
    (if (dsp-real-time-p obj)
        (setf (cdr cons) (dsp-properties-rt-instances prop)
              (dsp-properties-rt-instances prop) cons)
        (with-spinlock-held (*nrt-dsp-spinlock*)
          (setf (cdr cons) (dsp-properties-nrt-instances prop)
                (dsp-properties-nrt-instances prop) cons)))
    obj))

(defun get-next-dsp-instance (name)
  (declare (type symbol name))
  (let ((prop (dsp name)))
    (if (allow-rt-memory-p)
        (pop (dsp-properties-rt-instances prop))
        (with-spinlock-held (*nrt-dsp-spinlock*)
          (pop (dsp-properties-nrt-instances prop))))))

(defmacro set-dsp-object (dsp &rest keys-values)
  `(progn
     ,@(loop for l on keys-values by #'cddr collect
               `(setf (,(format-symbol :incudine.vug "DSP-~A" (car l)) ,dsp)
                      ,(or (cadr l) '#'dummy-function)))))

(defmacro set-dummy-functions (&rest functions)
  `(progn ,@(mapcar (lambda (fn) `(setf ,fn #'dummy-function)) functions)))

(defun all-dsp-names ()
  "Return the name list of the defined DSP's."
  (loop for dsp being the hash-keys in *dsps* collect dsp))

(declaim (inline set-dsp-dummy-functions))
(defun set-dsp-dummy-functions (obj)
  (set-dummy-functions
    (dsp-init-function obj) (dsp-perf-function obj) (dsp-free-function obj)))

(declaim (inline call-dsp-free-function))
(defun call-dsp-free-function (obj)
  (funcall (dsp-free-function obj))
  (incudine.util::cancel-finalization (dsp-init-function obj)))

(declaim (inline %%free-dsp-instance))
(defun %%free-dsp-instance (obj)
  (if (dsp-real-time-p obj)
      (incudine.util::free-rt-object obj *rt-dsp-instance-pool*)
      (incudine.util::free-object obj *dsp-instance-pool*)))

(defun %free-dsp-instance (obj)
  (set-dsp-dummy-functions obj)
  (%%free-dsp-instance obj))

(defun free-dsp-instance (obj)
  (declare (type dsp obj))
  (clrhash (dsp-controls obj))
  (call-dsp-free-function obj)
  (%free-dsp-instance obj)
  (values))

(defun dealloc-dsp-instances (function lst)
  (declare (type function function) (type list lst))
  (do ((curr lst old)
       ;; FUNCTION changes the CDR because the cons cell is part
       ;; of a cons pool.
       (old (cdr lst) (cdr old)))
      ((null curr))
    (funcall function (car curr))))

(defun rt-dsp-free-functions (instances)
  (let ((lst instances))
    (rt-eval ()
      (dolist (inst lst)
        (call-dsp-free-function inst))
      (if (rt-thread-p)
          (incudine.edf:schedule-at (1+ (now)) #'dealloc-dsp-instances
                                    (list #'%free-dsp-instance lst))
          (dealloc-dsp-instances #'%free-dsp-instance lst)))))

(defun free-rt-dsp-instances (name)
  (flet ((free-instances (dsp-prop)
           (declare (type dsp-properties dsp-prop))
           (let ((instances (dsp-properties-rt-instances dsp-prop)))
             (declare (type list instances))
             (when instances
               (setf (dsp-properties-rt-instances dsp-prop) nil)
               (dolist (inst instances)
                 (clrhash (dsp-controls inst)))
               (rt-dsp-free-functions instances))
             (values))))
    (if name
        (let ((dsp-prop (dsp name)))
          (when dsp-prop (free-instances dsp-prop)))
        (maphash-values #'free-instances *dsps*))
    (values)))

(defun free-nrt-dsp-instances (name)
  (flet ((free-instances (dsp-prop)
           (declare (type dsp-properties dsp-prop))
           (let ((instances (dsp-properties-nrt-instances dsp-prop)))
             (declare (type list instances))
             (when instances
               (setf (dsp-properties-nrt-instances dsp-prop) nil)
               (dealloc-dsp-instances #'free-dsp-instance instances))
             (values))))
    (if name
        (let ((dsp-prop (dsp name)))
          (when dsp-prop (free-instances dsp-prop)))
        (maphash-values #'free-instances *dsps*))
    (values)))

(defun free-dsp-instances (&optional name)
  "If NAME is non-NIL, free the cached DSP instances named NAME.
Otherwise, free all the cached DSP instances."
  (with-spinlock-held (*nrt-dsp-spinlock*)
    (free-nrt-dsp-instances name))
  (free-rt-dsp-instances name))

(defun destroy-dsp (name)
  "Remove the DSP definition, if any, of NAME."
  (when (dsp name)
    (setf (symbol-function name) (constantly nil))
    (rt-eval (:return-value-p t)
      (incudine:dograph (n)
        (when (eq (incudine::node-name n) name)
          (incudine::%node-free n))))
    (free-dsp-instances name)
    (let ((obj (gethash name *dsps*)))
      (when obj
        (incudine.util::free-object obj *dsp-properties-pool*)
        (remhash name *dsps*)))
    (fmakunbound name)
    (values)))
