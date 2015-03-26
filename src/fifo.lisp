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

(in-package :incudine)

(defstruct (fifo (:constructor %make-fifo)
                 (:copier nil))
  (items nil)
  (read-head nil :type list)
  (write-head nil :type list)
  (buffer-size 0 :type non-negative-fixnum)
  (name nil))

(defun make-fifo (buffer-size &key name)
  (let* ((bsize (next-power-of-two buffer-size))
         (f (%make-fifo :buffer-size bsize :name name))
         (lst (make-list bsize :initial-element #'identity)))
    ;; Ring buffer with a circular list
    (setf (cdr (last lst)) lst)
    (setf (fifo-items f) lst)
    (setf (fifo-read-head f) lst)
    (setf (fifo-write-head f) lst)
    f))

(defmethod print-object ((obj fifo) stream)
  (let ((*print-circle* t))
    (format stream "#<FIFO ~S>" (fifo-name obj))))

(define-constant +fifo-size+ 128)

;;; Communication from nrt-thread to rt-thread
;;; (single producer single consumer)
(defvar *to-engine-fifo* (make-fifo +fifo-size+ :name "to engine"))
(declaim (type fifo *to-engine-fifo*))

;;; Communication from rt-thread to nrt-thread
;;; (single producer single consumer)
(defvar *from-engine-fifo* (make-fifo +fifo-size+ :name "from engine"))
(declaim (type fifo *from-engine-fifo*))

;;; Communication from any (non rt) thread to fast-nrt-thread
;;; (multiple producer single consumer using a spinlock)
(defvar *from-world-fifo* (make-fifo +fifo-size+ :name "from world"))
(declaim (type fifo *from-world-fifo*))

;;; Communication from rt-thread to fast-nrt-thread
;;; (single producer single consumer)
(defvar *fast-from-engine-fifo* (make-fifo +fifo-size+
                                           :name "fast from engine"))
(declaim (type fifo *fast-from-engine-fifo*))

;;; Communication from fast-nrt-thread to fast-rt-thread
;;; (single producer single consumer)
(defvar *fast-to-engine-fifo* (make-fifo +fifo-size+ :name "fast to engine"))
(declaim (type fifo *fast-to-engine-fifo*))

(declaim (inline fifo-flush))
(defun fifo-flush (fifo)
  (declare (type fifo fifo))
  (setf (fifo-read-head fifo) (fifo-write-head fifo)))

(declaim (inline fifo-empty-p))
(defun fifo-empty-p (fifo)
  (declare (type fifo fifo))
  (eq (fifo-read-head fifo) (fifo-write-head fifo)))

(declaim (inline enqueue))
(defun enqueue (value fifo)
  "Adds VALUE to the end of FIFO. Returns VALUE."
  (declare (type fifo fifo))
  (let ((next (cdr (fifo-write-head fifo))))
    (unless (eq next (fifo-read-head fifo))
      (setf (car next) value)
      (barrier (:memory))
      (setf (fifo-write-head fifo) next)
      value)))

(declaim (inline enqueue-function))
(defun enqueue-function (function fifo)
  (declare (type (or function null) function) (type fifo fifo))
  (when function (enqueue function fifo)))

(defvar *fast-nrt-spinlock* (make-spinlock "fast nrt"))

(defun fast-nrt-enqueue-function (function)
  (declare (type (or function null) function))
  (when function
    (let ((fifo *from-world-fifo*))
      (with-spinlock-held (*fast-nrt-spinlock*)
        (let ((next (cdr (fifo-write-head fifo))))
          (unless (eq next (fifo-read-head fifo))
            (setf (car next) function
                  (fifo-write-head fifo) next)
            function))))))

(declaim (inline fast-nrt-perform-functions))
(defun fast-nrt-perform-functions ()
  (let ((fifo *from-world-fifo*))
    (with-spinlock-held (*fast-nrt-spinlock*)
      (loop until (fifo-empty-p fifo) do
           (let* ((next (cdr (fifo-read-head fifo)))
                  (fn (car next)))
             (declare (type list next) (type function fn))
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

(defvar *nrt-audio-sync* (make-sync-condition "nrt audio"))

(declaim (inline nrt-funcall))
(defun nrt-funcall (function)
  (enqueue-function function *from-engine-fifo*)
  (sync-condition-signal *nrt-audio-sync*)
  nil)

(defvar *fast-nrt-audio-sync* (make-sync-condition "fast nrt audio"))

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
       (let* ((next (cdr (fifo-read-head fifo)))
              (fn (car next)))
           (declare (type list next) (type function fn))
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
       (incudine:fast-nrt-funcall
        (lambda ()
          (incudine::fast-rt-funcall
           (lambda () (setf ,fn (rt-return-func ,value ,form))))))
       (loop until ,fn)
       (funcall ,fn))))

(defmacro rt-eval-without-return (&body form)
  `(incudine:fast-nrt-funcall
    (lambda ()
      (incudine:fast-rt-funcall
       (lambda () ,@form nil)))))

(defmacro rt-eval ((&key return-value-p) &body form)
  (with-gensyms (func)
    `(flet ((,func () (progn ,@form)))
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
