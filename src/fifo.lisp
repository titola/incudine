;;; Copyright (c) 2013-2021 Tito Latini
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

(defvar incudine.config::*fifo-buffer-size* 128)
(declaim (type (unsigned-byte 16) incudine.config::*fifo-buffer-size*))

#+incudine-fifo-circular-list
(progn
  (defstruct (fifo (:constructor %make-fifo) (:copier nil))
    (items nil :type list :read-only t)
    (read-head nil :type list)
    (write-head nil :type list)
    (buffer-size 20 :type positive-fixnum)
    (name nil))

  (defun make-fifo (&key buffer-size name)
    (let* ((size (or buffer-size incudine.config::*fifo-buffer-size*))
           (lst (make-list size :initial-element #'identity))
           (obj (%make-fifo :items lst :read-head lst :write-head lst
                            :buffer-size size :name name)))
      (setf (cdr (last lst)) lst)
      obj))

  (declaim (inline fifo-head-equal))
  (defun fifo-head-equal (h0 h1) (eq h0 h1))

  (defmacro fifo-head-next (fifo head)
    `(cdr (,head ,fifo)))

  (defmacro fifo-value (fifo next)
    (declare (ignore fifo))
    `(car ,next)))

#-incudine-fifo-circular-list
(progn
  (defstruct (fifo (:constructor %make-fifo) (:copier nil))
    (items (make-array 16 :element-type 'function :initial-element #'identity)
           :type simple-vector :read-only t)
    (read-head 0 :type non-negative-fixnum)
    (write-head 0 :type non-negative-fixnum)
    (buffer-size 16 :type positive-fixnum :read-only t)
    (mask 15 :type positive-fixnum :read-only t)
    (name nil))

  (defun make-fifo (&key buffer-size name)
    (let ((size (next-power-of-two
                  (or buffer-size incudine.config::*fifo-buffer-size*))))
      (%make-fifo :items (make-array size :element-type 'function
                                     :initial-element #'identity)
                  :buffer-size size :mask (1- size) :name name)))

  (declaim (inline fifo-head-equal))
  (defun fifo-head-equal (h0 h1) (= h0 h1))

  (defmacro fifo-head-next (fifo head)
    `(logand (1+ (,head ,fifo)) (fifo-mask ,fifo)))

  (defmacro fifo-value (fifo index)
    `(aref (fifo-items ,fifo) ,index)))

;;; Communication from nrt-thread to rt-thread
;;; (single producer single consumer)
(defvar *to-engine-fifo* (make-fifo :name "to engine"))
(declaim (type fifo *to-engine-fifo*))

;;; Communication from rt-thread to nrt-thread
;;; (single producer single consumer)
(defvar *from-engine-fifo* (make-fifo :name "from engine"))
(declaim (type fifo *from-engine-fifo*))

;;; Communication from any (non rt) thread to fast-nrt-thread
;;; (multiple producer single consumer using a spinlock)
(defvar *from-world-fifo* (make-fifo :name "from world"))
(declaim (type fifo *from-world-fifo*))

;;; Communication from rt-thread to fast-nrt-thread
;;; (single producer single consumer)
(defvar *fast-from-engine-fifo* (make-fifo :name "fast from engine"))
(declaim (type fifo *fast-from-engine-fifo*))

;;; Communication from fast-nrt-thread to fast-rt-thread
;;; (single producer single consumer)
(defvar *fast-to-engine-fifo* (make-fifo :name "fast to engine"))
(declaim (type fifo *fast-to-engine-fifo*))

(defglobal *fast-nrt-spinlock* (make-spinlock "fast nrt"))
(declaim (type spinlock *fast-nrt-spinlock*))

(defvar *nrt-audio-sync* (make-sync-condition "nrt audio"))
(declaim (type sync-condition *nrt-audio-sync*))

(defvar *fast-nrt-audio-sync* (make-sync-condition "fast nrt audio"))
(declaim (type sync-condition *fast-nrt-audio-sync*))

(defmethod print-object ((obj fifo) stream)
  (let ((*print-circle* t))
    (print-unreadable-object (obj stream)
      (format stream "FIFO ~S" (fifo-name obj)))))

(declaim (inline fifo-empty-p))
(defun fifo-empty-p (fifo)
  (declare (type fifo fifo))
  (fifo-head-equal (fifo-read-head fifo) (fifo-write-head fifo)))

(declaim (inline fifo-flush))
(defun fifo-flush (fifo)
  (declare (type fifo fifo))
  (setf (fifo-read-head fifo) (fifo-write-head fifo)))

;;; Adds VALUE to the end of FIFO. Returns VALUE.
(defun enqueue (value fifo)
  (declare (type fifo fifo))
  (let ((next (fifo-head-next fifo fifo-write-head)))
    (unless (fifo-head-equal next (fifo-read-head fifo))
      (setf (fifo-value fifo next) value)
      (barrier (:memory))
      (setf (fifo-write-head fifo) next)
      value)))

(declaim (inline enqueue-function))
(defun enqueue-function (function fifo)
  (declare (type (or function null) function) (type fifo fifo))
  (when function (enqueue function fifo)))

(defun fast-nrt-enqueue-function (function)
  (declare (type (or function null) function))
  (when function
    (let ((fifo *from-world-fifo*))
      (with-spinlock-held (*fast-nrt-spinlock*)
        (let ((next (fifo-head-next fifo fifo-write-head)))
          (unless (fifo-head-equal next (fifo-read-head fifo))
            (setf (fifo-value fifo next) function)
            (setf (fifo-write-head fifo) next)
            function))))))

(defun fast-nrt-perform-functions ()
  (let ((fifo *from-world-fifo*))
    (with-spinlock-held (*fast-nrt-spinlock*)
      (loop until (fifo-empty-p fifo) do
           (let* ((next (fifo-head-next fifo fifo-read-head))
                  (fn (fifo-value fifo next)))
             (declare (type function fn))
             (funcall fn)
             (setf (fifo-read-head fifo) next))))))

(declaim (inline nrt-funcall))
(defun nrt-funcall (function)
  "Use a lock-free FIFO to run FUNCTION from \"audio-nrt-thread\".

This function has to be called from the real-time thread.
See also RT-FUNCALL.

Example:

    (in-package :scratch)

    (rt-start)

    (rt-eval ()
      ;; From rt-thread to nrt-thread.
      (nrt-funcall
        (lambda ()
          (msg warn \"[~A] sleeping...\"
               (bt:thread-name (bt:current-thread)))
          ;; Block in nrt-thread.
          (sleep 1)
          (msg warn \"[~A] ... and real-time funcall\"
               (bt:thread-name (bt:current-thread)))
          (rt-funcall
            (lambda ()
              ;; From rt-thread to nrt-thread.
              (nrt-funcall
                (lambda ()
                  (msg warn \"hello from ~S.\"
                       (bt:thread-name (bt:current-thread))))))))))
    ;; => WARN: [audio-nrt-thread] sleeping...
    ;;    WARN: [audio-nrt-thread] ... and real-time funcall
    ;;    WARN: hello from \"audio-nrt-thread\"."
  (enqueue-function function *from-engine-fifo*)
  (sync-condition-signal *nrt-audio-sync*)
  nil)

(declaim (inline fast-nrt-funcall))
(defun fast-nrt-funcall (function)
  "Use a lock-free FIFO to run FUNCTION from \"audio-fast-nrt-thread\".

This function has to be called from the real-time thread.
See also FAST-RT-FUNCALL."
  (if (rt-thread-p)
      ;; From rt-thread to fast-nrt-thread
      (enqueue-function function *fast-from-engine-fifo*)
      ;; From any (non rt) thread to fast-nrt-thread
      (fast-nrt-enqueue-function function))
  (sync-condition-signal *fast-nrt-audio-sync*)
  nil)

(declaim (inline rt-funcall))
(defun rt-funcall (function)
  "Use a lock-free FIFO to run FUNCTION from the real-time thread.

This function has to be called from \"audio-nrt-thread\".
See also NRT-FUNCALL."
  (enqueue-function function *to-engine-fifo*)
  nil)

(declaim (inline fast-rt-funcall))
(defun fast-rt-funcall (function)
  "Use a lock-free FIFO to run FUNCTION from the real-time thread.

This function has to be called from \"audio-fast-nrt-thread\".
See also FAST-NRT-FUNCALL."
  (enqueue-function function *fast-to-engine-fifo*)
  nil)

(defun %print-condition-error (cond)
  (declare (type condition cond))
  (flet ((print-error ()
           (format *error-output* "~A~%" cond)
           (force-output)))
    (if (rt-thread-p) (nrt-funcall #'print-error) (print-error))))

(defun fifo-perform-functions (fifo)
  (declare (type fifo fifo) #.*standard-optimize-settings*)
  (loop until (fifo-empty-p fifo) do
       (let* ((next (fifo-head-next fifo fifo-read-head))
              (fn (fifo-value fifo next)))
           (declare (type function fn))
           (barrier (:memory))
           (setf (fifo-read-head fifo) next)
           (handler-case (funcall fn)
             (condition (c) (%print-condition-error c))))))

(in-package :incudine.util)

(defun any-to-rt-funcall (function)
  (declare (type function function))
  (incudine:fast-nrt-funcall
    (lambda () (incudine:fast-rt-funcall function))))

(defun rt-eval-with-return (function)
  (declare (type function function))
  (let ((fn nil))
    (declare (type (or function null) fn))
    (any-to-rt-funcall
      (lambda ()
        (setf fn (handler-case (let ((res (funcall function)))
                                 (lambda () res))
                   (condition (c)
                     (uiop:symbol-call :incudine.util '#:%nrt-msg 'error "~A" c)
                     (constantly nil))))))
    (loop until fn do (sleep 1e-7))
    (funcall (the function fn))))

(defun %rt-eval (function return-value-p)
  (declare (type function function) (type boolean return-value-p)
           #.*standard-optimize-settings*)
  (cond ((or (null *rt-thread*) (rt-thread-p))
         (funcall function))
        (return-value-p (rt-eval-with-return function))
        (t (any-to-rt-funcall function))))

(defmacro rt-eval ((&key return-value-p) &body form)
  "Evaluate FORM in real-time thread.

If RETURN-VALUE-P is T, return the results of FORM."
  `(%rt-eval (lambda () ,@form) ,return-value-p))

(defun exit (&optional (code 0))
  "Exit lisp with CODE from a non-real-time thread.

CODE defaults to 0."
  (if (eq (bt:current-thread) *rt-thread*)
      (incudine::nrt-funcall (lambda () (%exit code)))
      (%exit code)))
