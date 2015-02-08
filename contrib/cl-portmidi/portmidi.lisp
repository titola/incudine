;;; Copyright (c) 2013-2015 Tito Latini
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

(defvar *stream-opened* nil)
(declaim (type list *stream-opened*))

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

(defun verify-opened (ptr direction device-id result)
  (declare (type cffi:foreign-pointer ptr) (type keyword direction result)
           (type non-negative-fixnum device-id))
  (if (eq result :pm-no-error)
      (if (cffi:null-pointer-p (cffi:mem-ref ptr :pointer))
          (allocation-error 'stream)
          (cffi:with-foreign-slots
              ((interf name) (get-device-info% device-id) (:struct device-info))
            (let ((stream (make-stream (cffi:mem-ref ptr :pointer) direction
                                       device-id interf name)))
              (push stream *stream-opened*)
              stream)))
      (error-generic result)))

(defmacro open (device-id &key (buffer-size default-sysex-buffer-size)
                (direction :input) (latency 0) driver-info time-proc time-info)
  (let ((ptr (gensym)))
    (multiple-value-bind (open-func latency)
        (if (eq direction :input)
            (values 'open-input nil)
            (values 'open-output `(,latency)))
      `(cffi:with-foreign-object (,ptr :pointer)
         (verify-opened ,ptr ,direction ,device-id
           (,open-func ,ptr ,device-id ,(or driver-info '(cffi:null-pointer))
                       ,buffer-size ,(or time-proc '(cffi:null-pointer))
                       ,(or time-info '(cffi:null-pointer)) ,@latency))))))

(defstruct (event-buffer (:constructor %make-event-buffer)
                         (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (events 0 :type non-negative-fixnum))

(declaim (inline make-event-buffer))
(defun make-event-buffer (&optional (size default-sysex-buffer-size))
  (declare (type non-negative-fixnum size))
  (let* ((ptr (cffi:foreign-alloc '(:struct event) :count size))
         (obj (%make-event-buffer :pointer ptr :size size)))
    (tg:finalize obj (lambda () (free ptr)))
    obj))

(defmethod print-object ((obj event-buffer) stream)
  (format stream "#<PM:EVENT-BUFFER :SIZE ~D>"
          (event-buffer-size obj)))

(defun free (evbuf)
  (declare (type event-buffer))
  (setf (event-buffer-size evbuf) 0)
  (let ((ptr (event-buffer-pointer evbuf)))
    (unless (cffi:null-pointer-p ptr)
      (cffi:foreign-free ptr)))
  (tg:cancel-finalization evbuf)
  (values))

(declaim (inline read))
(defun read (stream evbuf length)
  (declare (type stream stream) (type event-buffer evbuf)
           (type non-negative-fixnum length))
  (with-slots (pointer events size) evbuf
    (setf events (min (the non-negative-fixnum
                        (read% stream pointer length))
                      (the non-negative-fixnum size)))))

(declaim (inline write))
(defun write (stream evbuf length)
  (declare (type stream stream) (type event-buffer evbuf)
           (type non-negative-fixnum length))
  (write% stream (event-buffer-pointer evbuf) length))

(declaim (inline close))
(defun close (stream)
  (declare (type stream stream))
  (let ((result (close% stream)))
    (setf (stream-pointer stream) (cffi:null-pointer)
          (stream-direction stream) :closed)
    (tg:cancel-finalization stream)
    (setf *stream-opened*
          (remove stream *stream-opened*))
    result))

(declaim (inline terminate))
(defun terminate ()
  (dolist (stream *stream-opened* (terminate%))
    (close stream)))

(defmacro event-slot (ev slot)
  `(cffi:mem-aref ,ev :int32 ,(if (eq slot 'message) 0 1)))

(defmacro doevent ((evbuf message-var &optional timestamp-var result)
                   &body body)
  (with-gensyms (ptr i)
    (let ((offset (cffi:foreign-type-size '(:struct event))))
      `(do ((,i 0 (1+ ,i))
            (,ptr (event-buffer-pointer ,evbuf)
                  (cffi:inc-pointer ,ptr ,offset)))
           ((= ,i (the non-negative-fixnum (event-buffer-events ,evbuf)))
            ,result)
         (declare (type non-negative-fixnum ,i)
                  (type cffi:foreign-pointer ,ptr))
         (let ((,message-var (event-slot ,ptr message))
               ,@(if timestamp-var
                     `((,timestamp-var (event-slot ,ptr timestamp)))))
           (declare (type unsigned-byte ,message-var
                          ,@(if timestamp-var `(,timestamp-var))))
           ,@body)))))

(defmacro with-event-buffer ((var &optional (size default-sysex-buffer-size))
                             &body body)
  `(let ((,var (make-event-buffer ,size)))
     (declare (type event-buffer ,var))
     (unwind-protect
          (progn ,@body)
       (free ,var))))

(defmacro with-receiver ((state-var stream message-var
                          &optional timestamp-var (sleep-time 1)
                                    thread-name)
                         &body body)
  (with-gensyms (evbuf)
    `(if ,state-var
         (warn "PortMidi receiver already started.")
         (case (stream-direction ,stream)
           (:closed (warn "The stream is closed."))
           (:output (warn "I cannot receive from an output stream."))
           (otherwise
            (bt:make-thread
             (lambda ()
               (with-event-buffer (,evbuf)
                 (setf ,state-var t)
                 (unwind-protect
                      (loop initially (read ,stream ,evbuf
                                            default-sysex-buffer-size) ; flush
                            while ,state-var
                            when (eq (poll ,stream) :pm-got-data) do
                              (read ,stream ,evbuf default-sysex-buffer-size)
                              (doevent (,evbuf ,message-var ,timestamp-var)
                                ,@body)
                            do (pt:sleep ,sleep-time))
                   (setf ,state-var nil))))
             :name ,(or thread-name
                        `(format nil "pm-recv ~A"
                                 (stream-device-name ,stream)))))))))
