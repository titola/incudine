;;; Copyright (c) 2013-2019 Tito Latini
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :portmidi)

(defvar *opened-streams* nil)
(declaim (type list *opened-streams*))

(declaim (inline from-device-info-ptr))
(defun from-device-info-ptr (ptr)
  (declare (cffi:foreign-pointer ptr))
  (cffi:with-foreign-slots
      ((struct-version interf name input output opened)
       ptr (:struct device-info))
    (list :struct-version struct-version :interf interf :name name
          :input input :output output :opened (plusp opened))))

(declaim (inline get-device-info))
(defun get-device-info (id)
  (from-device-info-ptr (get-device-info% id)))

(defun print-devices-info (&optional (direction :all) (stream *standard-output*))
  (declare (type keyword direction) (type cl:stream stream))
  (let ((fn (case direction
              (:output #'plusp)
              (:input #'zerop)
              (otherwise #'identity))))
    (dotimes (id (count-devices))
      (cffi:with-foreign-slots
          ((interf name input output opened)
           (get-device-info% id) (:struct device-info))
        (if (funcall fn output)
            (format stream "~D: ~A - ~A :IN ~D :OUT ~D ~:[(OPENED)~;~]~%"
                    id interf name input output (zerop opened)))))))

(defun register-stream (ptr direction device-id latency result)
  (declare (type cffi:foreign-pointer ptr) (type keyword direction result)
           (type non-negative-fixnum device-id))
  (if (eq result :pm-no-error)
      (if (cffi:null-pointer-p (cffi:mem-ref ptr :pointer))
          (allocation-error 'stream)
          (cffi:with-foreign-slots
              ((interf name) (get-device-info% device-id) (:struct device-info))
            (let ((stream (make-stream (cffi:mem-ref ptr :pointer) direction
                                       device-id interf name latency)))
              (push stream *opened-streams*)
              stream)))
      (error-generic result)))

(defun open (device-id &key (buffer-size default-sysex-buffer-size)
             (direction :input) (latency 1) (driver-info (cffi:null-pointer))
             (time-proc (cffi:null-pointer)) (time-info (cffi:null-pointer)))
  (cffi:with-foreign-object (stream-ptr :pointer)
    (let ((args (list stream-ptr device-id driver-info buffer-size
                      time-proc time-info latency)))
      (multiple-value-bind (func args)
          (if (eq direction :input)
              (values #'open-input (butlast args))
              (values #'open-output args))
        (register-stream stream-ptr direction device-id latency
                         (apply func args))))))

(defstruct (event-buffer (:constructor %make-event-buffer)
                         (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (events 0 :type non-negative-fixnum))

(defun make-event-buffer (&optional (size default-sysex-buffer-size))
  (declare (type non-negative-fixnum size))
  (let* ((ptr (cffi:foreign-alloc '(:struct event) :count size))
         (obj (%make-event-buffer :pointer ptr :size size)))
    (finalize obj (lambda () (free ptr)))
    obj))

(defmethod print-object ((obj event-buffer) stream)
  (format stream "#<PM:EVENT-BUFFER :SIZE ~D>"
          (event-buffer-size obj)))

(defun free (evbuf)
  (declare (type event-buffer evbuf))
  (setf (event-buffer-size evbuf) 0)
  (let ((ptr (event-buffer-pointer evbuf)))
    (unless (cffi:null-pointer-p ptr)
      (cffi:foreign-free ptr)))
  (cancel-finalization evbuf)
  (values))

(declaim (inline read))
(defun read (stream evbuf length)
  (declare (type stream stream) (type event-buffer evbuf)
           (type non-negative-fixnum length))
  (setf (event-buffer-events evbuf)
        (min (the non-negative-fixnum
               (read% stream (event-buffer-pointer evbuf) length))
             (the non-negative-fixnum (event-buffer-size evbuf)))))

(declaim (inline write))
(defun write (stream evbuf length)
  (declare (type stream stream) (type event-buffer evbuf)
           (type non-negative-fixnum length))
  (write% stream (event-buffer-pointer evbuf) length))

(defun close (stream)
  (declare (type stream stream))
  (let ((result (close% stream)))
    (setf (stream-pointer stream) (cffi:null-pointer)
          (stream-direction stream) :closed)
    (when (and (input-stream-p stream)
               (not (cffi:null-pointer-p
                      #1=(input-stream-sysex-pointer stream))))
      (cffi:foreign-free #1#)
      (setf #1# (cffi:null-pointer)))
    (cancel-finalization stream)
    (setf *opened-streams* (remove stream *opened-streams*))
    result))

(defun terminate ()
  (dolist (stream *opened-streams* (terminate%))
    (close stream)))

(defmacro event-slot (ev slot)
  `(cffi:mem-aref ,ev :int32 ,(if (eq slot 'message) 0 1)))

(defmacro with-input-sysex-event ((ptr-var stream) &body body)
  `(let ((,ptr-var (cffi:mem-ref (input-stream-sysex-pointer ,stream)
                                 :pointer)))
     ,@body))

(declaim (inline sysex-message-p))
(defun sysex-message-p (msg)
  (= (logand msg #xFF) #xF0))

(declaim (inline sysex-eox-message-p))
(defun sysex-eox-message-p (msg)
  (or (= (logand msg #xFF) #xF7)
      (= (ldb (byte 8 8) msg) #xF7)
      (= (ldb (byte 8 16) msg) #xF7)
      (= (ldb (byte 8 24) msg) #xF7)))

(defmacro doevent ((evbuf message-var stream &optional timestamp-var result)
                   &body body)
  (with-gensyms (ptr events remain tmp i j)
    (let ((offset (cffi:foreign-type-size '(:struct event))))
      `(do ((,i 0 (1+ ,i))
            (,ptr (event-buffer-pointer ,evbuf)
                  (cffi:inc-pointer ,ptr ,offset))
            (,events (event-buffer-events ,evbuf))
            (,remain (event-buffer-events ,evbuf) (1- ,remain)))
           ((>= ,i ,events) ,result)
         (declare (type non-negative-fixnum ,i ,events ,remain)
                  (type cffi:foreign-pointer ,ptr))
         (let ((,message-var (event-slot ,ptr message))
               ,@(when timestamp-var
                   `((,timestamp-var (event-slot ,ptr timestamp)))))
           (declare (type unsigned-byte ,message-var
                          ,@(when timestamp-var `(,timestamp-var))))
           (when (sysex-message-p ,message-var)
             (setf (cffi:mem-ref (input-stream-sysex-pointer ,stream) :pointer)
                   ,ptr)
             (setf (input-stream-events-remain ,stream) ,remain))
           ,@body
           (when (sysex-message-p ,message-var)
             ;; Jump to the end of the SysEx.
             (do ((,j 0 (1+ ,j))
                  (,tmp (cffi:inc-pointer ,ptr ,offset)
                        (cffi:inc-pointer ,tmp ,offset)))
                 ((>= ,j (- ,events ,i)) (setf ,i ,events))
               (declare (type non-negative-fixnum ,j)
                        (type cffi:foreign-pointer ,tmp))
               (when (sysex-eox-message-p (event-slot ,tmp message))
                 (incf ,i ,j)
                 (decf ,remain ,j)
                 (setf ,ptr ,tmp)
                 (return)))))))))

(defmacro with-event-buffer ((var &optional (size default-sysex-buffer-size))
                             &body body)
  `(let ((,var (make-event-buffer ,size)))
     (declare (type event-buffer ,var))
     (unwind-protect
          (progn ,@body)
       (free ,var))))

(defmacro with-receiver ((state-form stream message-var
                          &optional timestamp-var (sleep-time 1) thread-name)
                         &body body)
  (with-gensyms (evbuf timeout)
    `(if ,state-form
         (warn "PortMidi receiver already started.")
         (case (stream-direction ,stream)
           (:closed (warn "The stream is closed."))
           (:output (warn "I cannot receive from an output stream."))
           (otherwise
            (bt:make-thread
             (lambda ()
               (with-event-buffer (,evbuf)
                 (setf ,state-form t)
                 (unwind-protect
                      (loop initially (read ,stream ,evbuf
                                            default-sysex-buffer-size) ; flush
                            with ,timeout = ,sleep-time
                            while ,state-form do
                              (cond ((eq (poll ,stream) :pm-got-data)
                                     (read ,stream ,evbuf
                                           default-sysex-buffer-size)
                                     (doevent (,evbuf ,message-var ,stream
                                                      ,timestamp-var)
                                              ,@body))
                                    (t (pt:sleep ,timeout))))
                   (setf ,state-form nil))))
             :name ,(or thread-name
                        `(format nil "pm-recv ~A"
                                 (stream-device-name ,stream)))))))))
