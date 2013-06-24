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

(in-package :incudine.vug)

(defvar *synth-hash* (make-hash-table :test #'eq))

(defstruct (synth (:copier nil))
  (name nil)
  (init-function #'dummy-function :type function)
  (perf-function #'dummy-function :type function)
  (free-function #'dummy-function :type function)
  (controls (make-hash-table :size 16 :test #'equal)
            :type hash-table))

(defstruct (synth-properties (:conc-name synth-)
                             (:copier nil))
  (instances nil :type list)
  (arguments nil :type list)
  (redefine-hook nil :type list)
  (remove-hook nil :type list)
  (fade-time 0 :type (real 0))
  (fade-curve :lin :type (or symbol real)))

(declaim (inline synth))
(defun synth (name)
  (declare (type symbol name))
  (values (gethash name *synth-hash*)))

(defmethod incudine::fade-time ((obj symbol))
  (let ((prop (synth obj)))
    (if prop (synth-fade-time prop) 0)))

(defmethod (setf incudine::fade-time) ((value number) (obj symbol))
  (let ((prop (synth obj)))
    (when prop
      (setf (synth-fade-time prop) value))))

(defmethod incudine::fade-curve ((obj symbol))
  (let ((prop (synth obj)))
    (if prop (synth-fade-curve prop) :lin)))

(defmethod (setf incudine::fade-curve) (value (obj symbol))
  (let ((prop (synth obj)))
    (when prop
      (setf (synth-fade-curve prop) value))))

(declaim (inline expand-synth-pool))
(defun expand-synth-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta (make-synth-properties)))

(define-constant +synth-pool-size+ 200)
(define-constant +synth-pool-grow+ 50)

(defvar *synth-pool*
  (make-cons-pool :data (loop repeat +synth-pool-size+
                              collect (make-synth-properties))
                  :size +synth-pool-size+
                  :expand-func #'expand-synth-pool
                  :grow +synth-pool-grow+))
(declaim (type cons-pool *synth-pool*))

(declaim (inline synth-pool-pop))
(defun synth-pool-pop ()
  #+(or cmu sbcl) (declare (values synth-properties))
  (let* ((cons (cons-pool-pop-cons *synth-pool*))
         (value (car cons)))
    (rt-global-pool-push-cons cons)
    value))

(declaim (inline synth-pool-push))
(defun synth-pool-push (obj)
  (declare (type synth-properties obj))
  (let ((cons (rt-global-pool-pop-cons)))
    (setf (car cons) obj)
    (cons-pool-push-cons *synth-pool* cons)))

(declaim (inline get-synth-properties))
(defun get-synth-properties (name)
  (or (synth name)
      (setf (gethash name *synth-hash*)
            (synth-pool-pop))))

(declaim (inline expand-synth-inst-pool))
(defun expand-synth-inst-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta (make-synth)))

(define-constant +synth-instance-pool-grow+ 128)

(defvar *synth-instance-pool*
  (make-cons-pool :data (loop repeat *max-number-of-nodes*
                           collect (make-synth))
                  :size *max-number-of-nodes*
                  :expand-func #'expand-synth-inst-pool
                  :grow +synth-instance-pool-grow+))
(declaim (type cons-pool *synth-instance-pool*))

(declaim (inline synth-inst-pool-pop-cons))
(defun synth-inst-pool-pop-cons ()
  (cons-pool-pop-cons *synth-instance-pool*))

(declaim (inline synth-inst-pool-push-cons))
(defun synth-inst-pool-push-cons (cons)
  (cons-pool-push-cons *synth-instance-pool* cons))

(declaim (inline synth-inst-pool-push-list))
(defun synth-inst-pool-push-list (lst)
  (cons-pool-push-list *synth-instance-pool* lst))

(declaim (inline store-synth-instance))
(defun store-synth-instance (name synth-cons)
  (declare (type symbol name) (type cons synth-cons))
  (let ((prop (synth name)))
    (setf (cdr synth-cons) #1=(synth-instances prop))
    (setf #1# synth-cons)))

(declaim (inline get-next-synth-instance))
(defun get-next-synth-instance (name)
  (declare (type symbol name))
  (let* ((prop (synth name))
         (synth-cons #1=(synth-instances prop)))
    (declare (type list synth-cons))
    (when synth-cons
      (setf #1# (cdr synth-cons)
            (cdr synth-cons) nil))
    synth-cons))

(defmacro set-synth-object (synth &rest keys-values)
  `(progn
     ,@(loop for l on keys-values by #'cddr collect
            `(setf (,(format-symbol :incudine.vug "SYNTH-~A" (car l)) ,synth)
                   ,(or (cadr l) '#'dummy-function)))))

(defmacro set-dummy-functions (&rest functions)
  `(progn ,@(mapcar (lambda (fn)
                      `(setf ,fn #'dummy-function))
                    functions)))

(declaim (inline all-synth-names))
(defun all-synth-names ()
  (loop for synth being the hash-keys in *synth-hash*
        collect synth))

(declaim (inline free-synth-cons))
(defun free-synth-cons (cons)
  (declare (type cons cons))
  (let ((s (car cons)))
    (clrhash (synth-controls s))
    (rt-eval ()
      ;; free rt memory
      (funcall (synth-free-function s))
      (set-dummy-functions (synth-init-function s)
                           (synth-perf-function s)
                           (synth-free-function s))
      ;; push/pop in the rt thread
      (synth-inst-pool-push-cons cons))))

(defun free-synth-instance (synth-prop)
  (declare (type synth-properties synth-prop))
  (let ((instances #1=(synth-instances synth-prop)))
    (declare (type list instances))
    (when instances
      (do ((inst instances old)
           (old (cdr instances) (cdr old)))
          ((null inst) (setf #1# nil))
        (declare (list inst old))
        (free-synth-cons inst)))))

(declaim (inline free-synth-instances))
(defun free-synth-instances (&optional name)
  (if name
      (let ((synth-prop (synth name)))
        (when synth-prop
          (free-synth-instance synth-prop)))
      (maphash-values #'free-synth-instance *synth-hash*))
  (values))

(defun destroy-synth (name)
  (when (synth name)
    (setf (symbol-function name)
          (lambda (&rest args) (declare (ignorable args))))
    (incudine:dograph (n)
      (when (eq (incudine::node-name n) name)
        (funcall (incudine::node-free-fn n))))
    (free-synth-instances name)
    (remhash name *synth-hash*)
    (fmakunbound name)
    (values)))
