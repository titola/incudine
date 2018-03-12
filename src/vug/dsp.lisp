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

(defstruct (dsp (:copier nil))
  (name nil)
  (init-function #'dummy-function :type function)
  (perf-function #'dummy-function :type function)
  (free-function #'dummy-function :type function)
  (controls (make-hash-table :size 16 :test #'equal) :type hash-table))

(defstruct (dsp-properties (:conc-name dsp-) (:copier nil))
  (instances nil :type list)
  (arguments nil :type list)
  (redefine-hook nil :type list)
  (remove-hook nil :type list)
  (fade-time 0 :type (real 0))
  (fade-curve :lin :type (or symbol real)))

(declaim (inline expand-dsp-pool))
(defun expand-dsp-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta (make-dsp-properties)))

(define-constant +dsp-pool-size+ 200)
(define-constant +dsp-pool-grow+ 50)

(defvar *dsp-pool*
  (make-cons-pool :data (loop repeat +dsp-pool-size+
                              collect (make-dsp-properties))
                  :size +dsp-pool-size+
                  :expand-func #'expand-dsp-pool
                  :grow +dsp-pool-grow+))
(declaim (type cons-pool *dsp-pool*))

(declaim (inline unwrap-dsp))
(defun unwrap-dsp (cons)
  (declare (type cons cons))
  (car cons))

(declaim (inline dsp-pool-pop))
(defun dsp-pool-pop ()
  #+(or cmu sbcl) (declare (values dsp-properties))
  (let* ((cons (cons-pool-pop-cons *dsp-pool*))
         (value (unwrap-dsp cons)))
    (rt-global-pool-push-cons cons)
    value))

(declaim (inline dsp-pool-push))
(defun dsp-pool-push (obj)
  (declare (type dsp-properties obj))
  (let ((cons (rt-global-pool-pop-cons)))
    (setf (car cons) obj)
    (cons-pool-push-cons *dsp-pool* cons)))

(declaim (inline get-dsp-properties))
(defun get-dsp-properties (name)
  (or (dsp name)
      (setf (gethash name *dsps*) (dsp-pool-pop))))

(declaim (inline expand-dsp-inst-pool))
(defun expand-dsp-inst-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta (make-dsp)))

(define-constant +dsp-instance-pool-grow+ 128)

(defvar *dsp-instance-pool*
  (make-cons-pool :data (loop repeat *max-number-of-nodes*
                              collect (make-dsp))
                  :size *max-number-of-nodes*
                  :expand-func #'expand-dsp-inst-pool
                  :grow +dsp-instance-pool-grow+))
(declaim (type cons-pool *dsp-instance-pool*))

(declaim (inline dsp-inst-pool-pop))
(defun dsp-inst-pool-pop ()
  (cons-pool-pop-cons *dsp-instance-pool*))

(declaim (inline dsp-inst-pool-push))
(defun dsp-inst-pool-push (cons)
  (cons-pool-push-cons *dsp-instance-pool* cons))

(declaim (inline dsp-inst-pool-push-list))
(defun dsp-inst-pool-push-list (lst)
  (cons-pool-push-list *dsp-instance-pool* lst))

(declaim (inline store-dsp-instance))
(defun store-dsp-instance (name dsp-wrap)
  (declare (type symbol name) (type cons dsp-wrap))
  (let ((prop (dsp name)))
    (setf (cdr dsp-wrap) #1=(dsp-instances prop))
    (setf #1# dsp-wrap)))

(declaim (inline get-next-dsp-instance))
(defun get-next-dsp-instance (name)
  (declare (type symbol name))
  (let* ((prop (dsp name))
         (dsp-wrap #1=(dsp-instances prop)))
    (declare (type list dsp-wrap))
    (when dsp-wrap
      (setf #1# (cdr dsp-wrap)
            (cdr dsp-wrap) nil))
    dsp-wrap))

(defmacro set-dsp-object (dsp &rest keys-values)
  `(progn
     ,@(loop for l on keys-values by #'cddr collect
               `(setf (,(format-symbol :incudine.vug "DSP-~A" (car l)) ,dsp)
                      ,(or (cadr l) '#'dummy-function)))))

(defmacro set-dummy-functions (&rest functions)
  `(progn ,@(mapcar (lambda (fn) `(setf ,fn #'dummy-function)) functions)))

(declaim (inline all-dsp-names))
(defun all-dsp-names ()
  (loop for dsp being the hash-keys in *dsps* collect dsp))

(declaim (inline free-dsp-wrap))
(defun free-dsp-wrap (cons)
  (declare (type cons cons))
  (let ((s (unwrap-dsp cons)))
    (clrhash (dsp-controls s))
    (funcall (dsp-free-function s))
    (rt-eval ()
      (set-dummy-functions (dsp-init-function s)
                           (dsp-perf-function s)
                           (dsp-free-function s))
      ;; push/pop in the rt thread
      (dsp-inst-pool-push cons))))

(defun free-dsp-instance (dsp-prop)
  (declare (type dsp-properties dsp-prop))
  (let ((instances #1=(dsp-instances dsp-prop)))
    (declare (type list instances))
    (when instances
      (do ((inst instances old)
           (old (cdr instances) (cdr old)))
          ((null inst) (setf #1# nil))
        (declare (list inst old))
        (free-dsp-wrap inst)))))

(declaim (inline free-dsp-instances))
(defun free-dsp-instances (&optional name)
  (if name
      (let ((dsp-prop (dsp name)))
        (when dsp-prop (free-dsp-instance dsp-prop)))
      (maphash-values #'free-dsp-instance *dsps*))
  (values))

(defun destroy-dsp (name)
  (when (dsp name)
    (setf (symbol-function name)
          (lambda (&rest args) (declare (ignorable args))))
    (incudine:dograph (n)
      (when (eq (incudine::node-name n) name)
        (incudine::%node-free n)))
    (free-dsp-instances name)
    (remhash name *dsps*)
    (fmakunbound name)
    (values)))
