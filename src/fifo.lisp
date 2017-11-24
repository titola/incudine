;;; Copyright (c) 2013-2017 Tito Latini
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

(defvar *fast-nrt-spinlock* (make-spinlock "fast nrt"))
(declaim (type spinlock *fast-nrt-spinlock*))

(defvar *nrt-audio-sync* (make-sync-condition "nrt audio"))
(declaim (type sync-condition *nrt-audio-sync*))

(defvar *fast-nrt-audio-sync* (make-sync-condition "fast nrt audio"))
(declaim (type sync-condition *fast-nrt-audio-sync*))

(defmethod print-object ((obj fifo) stream)
  (let ((*print-circle* t))
    (format stream "#<FIFO ~S>" (fifo-name obj))))

(declaim (inline fifo-empty-p))
(defun fifo-empty-p (fifo)
  (declare (type fifo fifo))
  (fifo-head-equal (fifo-read-head fifo) (fifo-write-head fifo)))

(declaim (inline fifo-flush))
(defun fifo-flush (fifo)
  (declare (type fifo fifo))
  (setf (fifo-read-head fifo) (fifo-write-head fifo)))

(declaim (inline enqueue))
(defun enqueue (value fifo)
  "Adds VALUE to the end of FIFO. Returns VALUE."
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

(declaim (inline fast-nrt-perform-functions))
(defun fast-nrt-perform-functions ()
  (let ((fifo *from-world-fifo*))
    (with-spinlock-held (*fast-nrt-spinlock*)
      (loop until (fifo-empty-p fifo) do
           (let* ((next (fifo-head-next fifo fifo-read-head))
                  (fn (fifo-value fifo next)))
             (declare (type function fn))
             (funcall fn)
             (setf (fifo-read-head fifo) next))))))

(declaim (inline rt-funcall))
(defun rt-funcall (function)
  (enqueue-function function *to-engine-fifo*)
  nil)

(declaim (inline fast-rt-funcall))
(defun fast-rt-funcall (function)
  (enqueue-function function *fast-to-engine-fifo*)
  nil)

(declaim (inline nrt-funcall))
(defun nrt-funcall (function)
  (enqueue-function function *from-engine-fifo*)
  (sync-condition-signal *nrt-audio-sync*)
  nil)

(declaim (inline fast-nrt-funcall))
(defun fast-nrt-funcall (function)
  (if (rt-thread-p)
      ;; From rt-thread to fast-nrt-thread
      (enqueue-function function *fast-from-engine-fifo*)
      ;; From any (non rt) thread to fast-nrt-thread
      (fast-nrt-enqueue-function function))
  (sync-condition-signal *fast-nrt-audio-sync*)
  nil)

(declaim (inline %print-condition-error))
(defun %print-condition-error (cond)
  (declare (type condition cond))
  (flet ((print-error ()
           (format *error-output* "~A~%" cond)
           (force-output)))
    (if (rt-thread-p) (nrt-funcall #'print-error) (print-error))))

(declaim (inline fifo-perform-functions))
(defun fifo-perform-functions (fifo)
  (declare #.*standard-optimize-settings*
           (type fifo fifo))
  (loop until (fifo-empty-p fifo) do
       (let* ((next (fifo-head-next fifo fifo-read-head))
              (fn (fifo-value fifo next)))
           (declare (type function fn))
           (barrier (:memory))
           (setf (fifo-read-head fifo) next)
           (handler-case (funcall fn)
             (condition (c) (%print-condition-error c))))))

(in-package :incudine.util)

(defmacro rt-return-func (varname form)
  (with-gensyms (c)
    `(handler-case
         (let ((,varname (progn ,@form)))
           (lambda () ,varname))
       (condition (,c)
         (progn (nrt-msg error "~A" ,c)
                incudine.util::*dummy-function-without-args*)))))

(defmacro rt-eval-with-return (&body form)
  (with-gensyms (fn value)
    `(let (,fn)
       (declare (type (or function null) ,fn))
       (incudine:fast-nrt-funcall
        (lambda ()
          (incudine::fast-rt-funcall
           (lambda () (setf ,fn (rt-return-func ,value ,form))))))
       (loop until ,fn do (sleep 1e-7))
       (funcall (the function ,fn)))))

(defmacro rt-eval-without-return (&body form)
  `(incudine:fast-nrt-funcall
    (lambda ()
      (incudine:fast-rt-funcall
       (lambda () ,@form nil)))))

(defmacro rt-eval ((&key return-value-p) &body form)
  (with-gensyms (func)
    `(flet ((,func () (progn ,@form ,@(unless return-value-p '(nil)))))
       (if (or (null *rt-thread*) (rt-thread-p))
           (,func)
           (,(if return-value-p
                 'rt-eval-with-return
                 'rt-eval-without-return)
            (,func))))))

(defmacro rt-eval-if ((predicate) &body body)
  (with-gensyms (func)
    `(flet ((,func () (progn ,@body)))
       (if ,predicate (rt-eval () (,func)) (,func)))))

(declaim (inline exit))
(defun exit (&optional (code 0))
  (if (eq (bt:current-thread) *rt-thread*)
      (incudine::nrt-funcall (lambda () (%exit code)))
      (%exit code)))
