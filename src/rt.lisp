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

(defmacro with-new-thread ((varname name priority debug-message)
                           &body body)
  `(unless ,varname
     (setf ,varname
           (bt:make-thread (lambda ()
                             (thread-set-priority (bt:current-thread)
                                                  ,priority)
                             ,@body)
                           :name ,name))
     (msg debug ,debug-message)))

(defun nrt-start ()
  (with-new-thread (*nrt-thread* "audio-nrt-thread" *nrt-priority*
                    "non-realtime thread started")
    (loop (sync-condition-wait *nrt-audio-sync*)
          (fifo-perform-functions *from-engine-fifo*)))
  (with-new-thread (*fast-nrt-thread* "audio-fast-nrt-thread"
                    *fast-nrt-priority* "fast non-realtime thread started")
    (loop (sync-condition-wait *fast-nrt-audio-sync*)
          (fifo-perform-functions *fast-from-engine-fifo*)
          (fast-nrt-perform-functions)))
  (and *nrt-thread* *fast-nrt-thread*))

(defun nrt-stop ()
  (when *nrt-thread*
    (bt:destroy-thread *nrt-thread*)
    (bt:destroy-thread *fast-nrt-thread*)
    (loop while (bt:thread-alive-p *nrt-thread*))
    (msg debug "non-realtime thread stopped")
    (loop while (bt:thread-alive-p *fast-nrt-thread*))
    (msg debug "fast non-realtime thread stopped")
    (setf *nrt-thread* nil *fast-nrt-thread* nil)))

(defvar *foreign-client-name* (cffi:null-pointer))

(defun make-rt-thread ()
  (with-new-thread (*rt-thread* "audio-rt-thread"
                    (rt-params-priority *rt-params*)
                    "realtime thread started")
    (call-hooks "rt-thread-start" *rt-thread-start-hook* :on-error :warn)
    (tg:gc :full t)
    (cond ((and (zerop (rt-audio-init
                        *sample-rate*
                        *number-of-input-bus-channels*
                        *number-of-output-bus-channels*
                        (rt-params-frames-per-buffer *rt-params*)
                        *foreign-client-name*))
                (zerop (rt-audio-start)))
           (let ((buffer-size (rt-buffer-size)))
             (setf (rt-params-frames-per-buffer *rt-params*)
                   buffer-size)
             #+jack-audio (set-sample-rate (rt-sample-rate))
             (rt-loop buffer-size)))
          (t (setf *rt-thread* nil)
             (msg error (rt-get-error-msg))))
    (call-hooks "rt-thread-exit" *rt-thread-exit-hook* :on-error :warn)))

(defun destroy-rt-thread ()
  (when (and *rt-thread* (bt:thread-alive-p *rt-thread*))
    (bt:destroy-thread *rt-thread*)))

(defun get-foreign-client-name ()
  (cffi:foreign-string-to-lisp *foreign-client-name*))

(defun set-foreign-client-name (name &optional (max-size 64))
  (unless (and (not (null-pointer-p *foreign-client-name*))
               (string-equal (get-foreign-client-name) *client-name*))
    (unless (null-pointer-p *foreign-client-name*)
      (cffi:foreign-free *foreign-client-name*))
    (setf *foreign-client-name*
          (cffi:foreign-string-alloc name :end (min (length name) max-size))))
  name)

(defun rt-start ()
  (unless *rt-thread*
    (init)
    (nrt-start)
    (set-foreign-client-name *client-name*)
    (make-rt-thread)
    (sleep .1)
    (setf (rt-params-status *rt-params*)
          (cond (*rt-thread* :started)
                (t (msg warn "failed to start the realtime thread")
                   :stopped)))))

(defun rt-status ()
  (rt-params-status *rt-params*))

(defvar rt-state 1)
(declaim (type bit rt-state))

(defun rt-stop ()
  (unless (eq (rt-status) :stopped)
    (cond ((rt-thread-p)
           (nrt-funcall #'rt-stop))
          (t (when *rt-thread*
               (let ((thread *rt-thread*))
                 (setf *rt-thread* nil)
                 (setf rt-state 1)
                 (sleep .05)
                 (loop while (bt:thread-alive-p thread))
                 (msg debug "realtime thread stopped")))
             (unless (zerop (rt-audio-stop))
               (msg error (rt-get-error-msg)))
             (setf (rt-params-status *rt-params*) :stopped)))))

(declaim (inline tick-func))
(defun tick-func ()
  (let ((funcons (node-funcons *node-root*)))
    (loop for flist on funcons by #'cdr
          for fn function = (car flist) do
          (handler-case
              (funcall (the function fn))
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
      (dochannels (chan *number-of-output-bus-channels*)
        (update-peak-values chan)))))

(defmacro with-restart-point ((label) &body body)
  `(tagbody
    ;; [SBCL] Restart from here after the stop caused by the gc
    ,label
      (when (zerop rt-state)
        ;; Transfer the control of the client from c to lisp
        (rt-set-busy-state nil)
        (rt-condition-wait))
      ,@body))

(defmacro with-rt-cycle ((reset-label frames-var) &body body)
  (declare (ignorable frames-var))
  `(incudine.util::without-gcing
     (setf ,frames-var (rt-cycle-begin))
     (when sb-kernel:*stop-for-gc-pending*
       (setf ,frames-var 0))
     ,@body
     (rt-cycle-end #+portaudio ,frames-var)
     (incudine.util::with-stop-for-gc-pending
       #+portaudio (rt-cycle-begin)
       ;; No xruns, jack knows that lisp is busy.
       ;; The output buffer is filled with zeroes.
       (rt-transfer-to-c-thread)
       (go ,reset-label))))

(defun rt-loop (frames-per-buffer)
  (declare #.*standard-optimize-settings*
           (type non-negative-fixnum frames-per-buffer))
  (setf rt-state 0)
  (reset-sample-counter)
  (let ((frames frames-per-buffer))
    (declare (type non-negative-fixnum frames))
    (with-restart-point (reset)
      (loop while (zerop rt-state) do
           (with-rt-cycle (reset frames)
             (fifo-perform-functions *to-engine-fifo*)
             (do ((i 0 (1+ i)))
                 ((>= i frames))
               (declare (type non-negative-fixnum i))
               (rt-get-input *input-pointer*)
               (fifo-perform-functions *fast-to-engine-fifo*)
               (incudine.edf::sched-loop)
               (tick-func)
               (rt-set-output *output-pointer*)
               (incf-sample-counter))))
      (rt-transfer-to-c-thread)
      (nrt-funcall #'rt-stop))))
