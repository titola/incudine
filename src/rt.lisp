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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *after-gc-fn* (lambda () (nrt-msg debug "gc happened")))

  (incudine.util::add-after-gc-hook *after-gc-fn*)

  ;;; In JACK2, `sem_timedwait' is interrupted by SIGUSR2 during the
  ;;; gc and it is notified with a message to the standard error.
  ;;; The default is to hide these messages.
  #+jack-audio
  (pushnew #'incudine.external::silent-jack-errors *initialize-hook*))

(defmacro with-new-thread ((varname name priority debug-message) &body body)
  `(unless ,varname
     (setf ,varname
           (bt:make-thread
             (lambda ()
               (setf (thread-priority (bt:current-thread)) ,priority)
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

#-dummy-audio
(defun rt-thread-callback (loop-function)
  (declare (type function loop-function))
  #+portaudio (incudine.external::pa-set-devices
                incudine.config::*portaudio-output-device*
                incudine.config::*portaudio-input-device*)
  (cond ((and (zerop (rt-audio-init *number-of-input-bus-channels*
                                    *number-of-output-bus-channels*
                                    (rt-params-frames-per-buffer *rt-params*)
                                    *foreign-client-name*
                                    #+jack-audio *sample-counter*))
              (zerop (rt-audio-start)))
         (let ((buffer-size (rt-buffer-size)))
           (setf (rt-params-frames-per-buffer *rt-params*) buffer-size)
           (set-sample-rate (rt-sample-rate))
           #+jack-midi (nrt-funcall #'jackmidi::update-streams)
           (funcall loop-function buffer-size)))
        (t (setf *rt-thread* nil)
           (msg error (rt-get-error-msg)))))

#+dummy-audio
(defun rt-thread-callback (loop-function)
  (declare (ignore loop-function))
  (msg warn "using dummy audio driver; you could change the realtime callback"))

(defun make-rt-thread (name function args)
  (declare (type function function))
  (with-new-thread (*rt-thread* name (rt-params-priority *rt-params*)
                    "realtime thread started")
    (call-hooks "rt-thread-start" *rt-thread-start-hook* :on-error :warn)
    (apply function args)
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

(defmacro compute-tick (&optional (update-peak-p t))
  (with-gensyms (funcons flist fn dummy-fn c n chan)
    `(let ((,funcons (node-funcons *node-root*)))
       (do ((,flist ,funcons (cdr ,flist)))
           ((null ,flist))
         (declare (type list ,flist))
         (let ((,fn (car ,flist)))
           (declare (type function ,fn))
           (handler-case (funcall ,fn)
             (condition (,c)
               ;; Set a dummy function and free the node at the next tick
               (let ((,dummy-fn (lambda (ch) (declare (ignore ch)))))
                 (setf (car ,flist) ,dummy-fn ,fn ,dummy-fn)
                 (dograph (,n)
                   (unless (group-p ,n)
                     (when (eq (car (node-funcons ,n)) ,dummy-fn)
                       (node-free ,n))))
                 (nrt-msg error "~A" ,c))))))
       ,(if update-peak-p
            `(when ,funcons
               (dochannels (,chan *number-of-output-bus-channels*)
                 (update-peak-values ,chan)))))))

(defvar rt-state 1)
(declaim (type bit rt-state))

#-dummy-audio
(defmacro with-restart-point ((label) &body body)
  `(tagbody
    ;; [SBCL] Restart from here after the stop caused by the gc
    ,label
      (when (zerop rt-state)
        ;; Transfer the control of the client from c to lisp
        (rt-set-busy-state nil)
        (rt-condition-wait))
      ,@body))

#-dummy-audio
(defmacro with-rt-cycle ((reset-label frames-var) &body body)
  (declare (ignorable frames-var))
  `(incudine.util::without-gcing
     (setf ,frames-var (rt-cycle-begin))
     (when sb-kernel:*stop-for-gc-pending*
       (setf ,frames-var 0))
     ,@body
     (incudine.util::without-float-overflow-trap
       (rt-cycle-end ,frames-var))
     (incudine.util::with-stop-for-gc-pending
       ;; No xruns, jack knows that lisp is busy.
       ;; The output buffer is filled with zeroes.
       (rt-transfer-to-c-thread)
       (go ,reset-label))))

(defvar *block-size* (if (boundp 'incudine.config::*rt-block-size*)
                         incudine.config::*rt-block-size*
                         1))
(declaim (type positive-fixnum *block-size*))

(defvar *block-samples* (* *block-size* *number-of-output-bus-channels*))
(declaim (type positive-fixnum *block-samples*))

(declaim (inline block-size))
(defun block-size () *block-size*)

#-dummy-audio
(defmacro rt-loop-form (frames-per-buffer block-size)
  (with-gensyms (frames reset pt-started pm-time-delta)
    (declare (ignorable pt-started pm-time-delta))
    `(block nil
       (rt-set-io-buffers *%input-pointer* *%output-pointer*)
       (unless (= ,block-size 1)
         (reduce-warnings
           (when (plusp (rem ,frames-per-buffer ,block-size))
             (msg warn
                  "Block size ~D is not a multiple of ~D (frames per buffer)"
                  ,block-size ,frames-per-buffer)
             (call-after-stop)
             (return-from nil))))
       (setf *block-size* ,block-size)
       (setf *block-samples* (* *block-size* *number-of-output-bus-channels*))
       (setf rt-state 0)
       (reset-sample-counter)
       (let ((,frames ,frames-per-buffer)
             ,@(when incudine.config::*enable-portmidi-output-sample-offset*
                 `((,pt-started (pt:started))
                   (,pm-time-delta incudine.util::*sample-duration-msec*))))
         (declare (type non-negative-fixnum ,frames)
                  ,@(when incudine.config::*enable-portmidi-output-sample-offset*
                      `((type boolean ,pt-started)
                        (type sample ,pm-time-delta))))
         (with-restart-point (,reset)
           (loop while (zerop rt-state) do
                (reset-io-pointers)
                (with-rt-cycle (,reset ,frames)
                  ,@(when incudine.config::*enable-portmidi-output-sample-offset*
                      `((when ,pt-started
                          (setf (smp-ref *portmidi-time* 0)
                                (sample (the (unsigned-byte 32) (pt:time)))))))
                  #+jack-midi (jackmidi::process ,frames)
                  (fifo-perform-functions *to-engine-fifo*)
                  (do ((i 0 (+ i ,block-size)))
                      ((>= i ,frames))
                    (declare (type non-negative-fixnum i))
                    (fifo-perform-functions *fast-to-engine-fifo*)
                    (incudine.edf:sched-loop)
                    (compute-tick)
                    (inc-io-pointers ,block-size)
                    ,@(when incudine.config::*enable-portmidi-output-sample-offset*
                        `((when ,pt-started
                            (inc-portmidi-time ,pm-time-delta))))
                    (incf-sample-counter ,block-size))))
           (rt-transfer-to-c-thread)
           (nrt-funcall #'rt-stop))))))

#-dummy-audio
(defun rt-loop-1 (frames-per-buffer)
  "Realtime loop callback for sample by sample computation."
  (declare #.*standard-optimize-settings*
           (type non-negative-fixnum frames-per-buffer))
  (rt-loop-form frames-per-buffer 1))

#-dummy-audio
(defun rt-loop-64 (frames-per-buffer)
  "Realtime loop callback with a block size of 64 frames."
  (declare #.*standard-optimize-settings*
           (type non-negative-fixnum frames-per-buffer))
  (rt-loop-form frames-per-buffer 64))

#-dummy-audio
(defmacro rt-loop-callback (block-size)
  "Return a realtime loop callback with an arbitrary BLOCK-SIZE."
  (with-gensyms (%block-size)
    `(lambda (frames-per-buffer)
       (declare #.*standard-optimize-settings*
                (type non-negative-fixnum frames-per-buffer))
       (let ((,%block-size ,block-size))
         (declare (type non-negative-fixnum ,%block-size))
         (rt-loop-form frames-per-buffer ,%block-size)))))

(defun rt-preamble ()
  (nrt-start)
  #-dummy-audio
  (set-foreign-client-name *client-name*)
  (values))

#-dummy-audio
(defun after-rt-stop ()
  (unless (zerop (rt-audio-stop))
    (msg error (rt-get-error-msg)))
  #+jack-midi (jackmidi::reset)
  (values))

(defparameter *default-rt-loop-cb*
  #+dummy-audio #'identity
  #-dummy-audio
  (if (and (boundp 'incudine.config::*rt-block-size*)
           (> incudine.config::*rt-block-size* 1))
      (if (= incudine.config::*rt-block-size* 64)
          #'rt-loop-64
          (eval `(rt-loop-callback ,incudine.config::*rt-block-size*)))
      #'rt-loop-1))
(declaim (type function *default-rt-loop-cb*))

#-dummy-audio
(defmacro set-rt-block-size (value)
  "Change the block size and update the default realtime loop callback."
  `(progn
     (unless (eq (rt-status) :stopped)
       (rt-stop)
       (msg warn "rt-thread stopped."))
     (setf *default-rt-loop-cb*
           (case ,value
             ( 1 #'rt-loop-1)
             (64 #'rt-loop-64)
             (otherwise (rt-loop-callback ,value))))
     (setf *block-samples* (* ,value *number-of-output-bus-channels*))
     (msg debug "set realtime block size to ~D" ,value)
     (setf *block-size* ,value)))

(defvar *after-rt-stop-function* nil)
(declaim (type (or function null) *after-rt-stop-function*))

(defun rt-start (&key (preamble-function #'rt-preamble)
                 (thread-name "audio-rt-thread")
                 (thread-function #'rt-thread-callback)
                 (thread-function-args (list #-dummy-audio
                                             *default-rt-loop-cb*
                                             #+dummy-audio nil))
                 (after-stop-function #-dummy-audio #'after-rt-stop
                                      #+dummy-audio nil)
                 (gc-p t))
  (declare (type string thread-name)
           (type (or function null) preamble-function after-stop-function)
           (type function thread-function)
           (type cons thread-function-args)
           (type boolean gc-p))
  (unless *rt-thread*
    (init)
    (setf *after-rt-stop-function* after-stop-function)
    (when preamble-function (funcall preamble-function))
    (when gc-p (tg:gc :full t))
    (make-rt-thread thread-name thread-function thread-function-args)
    (sleep .1)
    (setf (rt-params-status *rt-params*)
          (cond ((and *rt-thread* (bt:thread-alive-p *rt-thread*))
                 :started)
                (t (msg warn "failed to start the realtime thread")
                   (setf *rt-thread* nil)
                   :stopped)))))

(defun rt-status ()
  (rt-params-status *rt-params*))

(defun call-after-stop ()
  (nrt-funcall (lambda ()
                 (when *after-rt-stop-function*
                   (funcall *after-rt-stop-function*)
                   (setf *after-rt-stop-function* nil)
                   (msg debug "after realtime stop")))))

(defun rt-stop ()
  (unless (eq (rt-status) :stopped)
    (cond ((rt-thread-p) (nrt-funcall #'rt-stop))
          (t (when *rt-thread*
               (let ((thread *rt-thread*))
                 (setf *rt-thread* nil)
                 (setf rt-state 1)
                 (sleep .05)
                 (loop while (bt:thread-alive-p thread))
                 (msg debug "realtime thread stopped")))
             (call-after-stop)
             (setf (rt-params-status *rt-params*) :stopped)))))

#+portaudio
(defun portaudio-set-device (output &optional (input output))
  (declare (type fixnum output input))
  (unless (and (= incudine.config::*portaudio-output-device* output)
               (= incudine.config::*portaudio-input-device* input))
    (unless (eq (rt-status) :stopped)
      (rt-stop)
      (msg warn "rt-thread stopped."))
    (setf incudine.config::*portaudio-output-device* output
          incudine.config::*portaudio-input-device* input))
  (values output input))
