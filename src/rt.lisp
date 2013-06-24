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

(in-package :incudine)

(defvar *nrt-thread* nil)
(defvar *fast-nrt-thread* nil)

(declaim (inline incf-time))
(defun incf-sample-counter ()
  (setf #1=(mem-ref *sample-counter* 'sample)
        (+ #1# 1.0))
  (values))

(declaim (inline reset-timer))
(defun reset-sample-counter ()
  (setf (mem-ref *sample-counter* 'sample) +sample-zero+)
  (values))

(defun nrt-start ()
  (unless *nrt-thread*
    (setf *nrt-thread*
          (bt:make-thread (lambda ()
                            (loop do
                                 (sync-condition-wait *nrt-audio-sync*)
                                 (fifo-perform-functions *from-engine-fifo*)))
                          :name "audio-nrt-thread"))
    (thread-set-priority *nrt-thread* *nrt-priority*))
  (unless *fast-nrt-thread*
    (setf *fast-nrt-thread*
          (bt:make-thread (lambda ()
                            (loop do
                                 (sync-condition-wait *fast-nrt-audio-sync*)
                                 (fifo-perform-functions *fast-from-engine-fifo*)
                                 (fast-nrt-perform-functions)))
                          :name "audio-fast-nrt-thread"))
    (thread-set-priority *fast-nrt-thread* *fast-nrt-priority*))
  (and *nrt-thread* *fast-nrt-thread*))

(defun nrt-stop ()
  (when *nrt-thread*
    (bt:destroy-thread *nrt-thread*)
    (bt:destroy-thread *fast-nrt-thread*)
    (loop while (bt:thread-alive-p *nrt-thread*))
    (loop while (bt:thread-alive-p *fast-nrt-thread*))
    (setf *nrt-thread* nil *fast-nrt-thread* nil)))

(defun make-rt-thread ()
  (setf *rt-thread*
        (bt:make-thread
         (lambda ()
           (thread-set-priority (bt:current-thread)
                                (rt-params-priority *rt-params*))
           (tg:gc :full t)
           (cond ((and (zerop (rt-audio-init
                               *sample-rate* *number-of-input-bus-channels*
                               *number-of-output-bus-channels*
                               (rt-params-frames-per-buffer *rt-params*)
                               *client-name*))
                       (zerop (rt-audio-start)))
                  (rt-loop (rt-params-frames-per-buffer *rt-params*)))
                 (t (setf *rt-thread* nil)
                    (msg error (rt-get-error-msg)))))
         :name "audio-rt-thread")))

(defun destroy-rt-thread ()
  (when (and *rt-thread* (bt:thread-alive-p *rt-thread*))
    (bt:destroy-thread *rt-thread*)))

(defun rt-start ()
  (unless *rt-thread*
    (init)
    (nrt-start)
    (make-rt-thread)
    (sleep .1)
    (setf (rt-params-status *rt-params*)
          (if *rt-thread* :started :stopped))))

(defun rt-status ()
  (rt-params-status *rt-params*))

(defun rt-stop ()
  (unless (eq (rt-status) :stopped)
    (destroy-rt-thread)
    (loop while (bt:thread-alive-p *rt-thread*))
    (setf *rt-thread* nil)
    (unless (zerop (rt-audio-stop))
      (error (rt-get-error-msg)))
    (setf (rt-params-status *rt-params*) :stopped)))

(declaim (inline tick-func))
(defun tick-func ()
  (let ((funcons (node-funcons *node-root*)))
    (loop for flist on funcons by #'cdr
          for fn function = (car flist) do
          (handler-case
              (dotimes (chan #.*number-of-output-bus-channels*)
                (funcall (the function fn) chan))
            (condition (c)
              ;; Set a dummy function and free the node at the next tick
              (let ((dummy-fn (lambda (ch) (declare (ignore ch)))))
                (setf (car flist) dummy-fn fn dummy-fn)
                (dograph (n)
                  (unless (group-p n)
                    (when (eq (car (node-funcons n)) dummy-fn)
                      (node-free n))))
                (nrt-msg error "~A" c)))))
    (when funcons
      (dotimes (chan #.*number-of-output-bus-channels*)
        (update-peak-values chan)))))

(defun rt-loop (frames-per-buffer)
  (declare #.*standard-optimize-settings*
           (type non-negative-fixnum frames-per-buffer))
  (reset-sample-counter)
  (loop do
       (rt-condition-wait)
       (fifo-perform-functions *to-engine-fifo*)
       (do ((i 0 (1+ i)))
           ((= i frames-per-buffer))
         (declare (type non-negative-fixnum i))
         (rt-get-input *input-pointer*)
         (fifo-perform-functions *fast-to-engine-fifo*)
         (incudine.edf::sched-loop)
         (tick-func)
         (rt-set-output *output-pointer*)
         (incf-sample-counter))))
