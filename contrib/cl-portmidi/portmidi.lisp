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

(defun all-streams (&optional direction)
  (declare (type (member nil :input :output) direction))
  (if direction
      (loop for stream in *opened-streams*
            if (eq (stream-direction stream) direction)
              collect stream)
      (copy-list *opened-streams*)))

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

(defun port-name (stream)
  (stream-device-name stream))

(defun get-stream-by-name (port-name direction)
  (declare (type string port-name) (type (member :input :output) direction))
  (find port-name *opened-streams*
        :test (lambda (name stream)
                (and (string= name (port-name stream))
                     (eq direction (stream-direction stream))))))

(defun get-device-id-by-name (port-name direction)
  (declare (type string port-name) (type (member :input :output) direction))
  (dotimes (id (count-devices))
    (cffi:with-foreign-slots
        ((name input output) (get-device-info% id) (:struct device-info))
      (if (and (plusp (if (eq direction :input) input output))
               (string= port-name name))
          (return id)))))

(defun alloc-foreign-stream (direction &rest args)
  (let ((res (if (eq direction :input)
                 (apply 'open-input (butlast args))
                 (apply 'open-output args)))
        (ptr (first args)))
    (if (eq res :pm-no-error)
        (if (cffi:null-pointer-p (cffi:mem-ref ptr :pointer))
            (allocation-error 'stream))
        (error-generic res))
    ptr))

(defun %open (device-id direction buffer-size latency driver-info
              time-proc time-info &optional stream)
  ;; COUNT-DEVICES also initializes PortMidi if necessary.
  (assert (< device-id (count-devices)))
  (let ((ptr (cffi:with-foreign-object (stream-ptr :pointer)
               (cffi:mem-ref
                 (alloc-foreign-stream
                   direction stream-ptr device-id driver-info buffer-size
                   time-proc time-info latency)
                 :pointer))))
    (if stream
        ;; Reuse a lisp object. The Incudine code includes the utility
        ;; PM:REINITIALIZE to call PM:TERMINATE, PM:INITIALIZE and
        ;; reopen the streams without to create new lisp objects, so the
        ;; references and bindings (i.e. from receivers, responders,
        ;; variables, etc) continue to work. PM:REINITIALIZE is useful
        ;; to plug-and-play MIDI devices. It is defined in Incudine to
        ;; stop and restart the receivers if necessary.
        (setf (stream-pointer stream) ptr
              (stream-device-id stream) device-id
              (stream-direction stream) direction)
        ;; Create a new lisp object.
        (let ((input-p (eq direction :input)))
          (setf stream
                (cffi:with-foreign-slots
                    ((interf name) (get-device-info% device-id)
                     (:struct device-info))
                  (funcall (if input-p
                               'make-input-stream
                               'make-output-stream)
                           :pointer ptr :direction direction
                           :device-id device-id :device-interf interf
                           :device-name name :buffer-size buffer-size
                           :driver-info driver-info :time-proc time-proc
                           :time-info time-info)))
          (if input-p
              (let ((sysex-ptr (cffi:foreign-alloc :pointer)))
                (finalize stream
                  (lambda ()
                    (cffi:foreign-free sysex-ptr)
                    (close ptr)))
                (setf (input-stream-sysex-pointer stream) sysex-ptr))
              (progn
                (finalize stream (lambda () (close ptr)))
                (setf (output-stream-latency stream) latency)))))
    (push stream *opened-streams*)
    stream))

(defun open (device-id &key (buffer-size default-sysex-buffer-size)
             (direction :input) (latency 1) (driver-info (cffi:null-pointer))
             (time-proc (cffi:null-pointer)) (time-info (cffi:null-pointer)))
  (%open
    device-id direction buffer-size latency driver-info time-proc time-info))

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

(defun input-stream-sysex-octets (stream &optional octets (start 0))
  (declare (type input-stream stream)
           (type (or (simple-array (unsigned-byte 8) (*)) null) octets)
           (type non-negative-fixnum start))
  (with-input-sysex-event (ptr stream)
    (let ((remain (input-stream-events-remain stream)))
      (declare (type non-negative-fixnum remain)
               (optimize speed (safety 0)))
      (when (> remain 0)
        (let* ((butlast-events (1- remain))
               (last32 (cffi:mem-aref
                         (cffi:inc-pointer ptr (the fixnum (* 8 butlast-events)))
                         :uint32))
               (i (max 1 (ash (integer-length last32) -3)))
               (tail-size #+little-endian i #-little-endian (- 5 i))
               ;; Buffer size in bytes.
               (size (+ (* 4 butlast-events) tail-size)))
          (declare (type non-negative-fixnum butlast-events size))
          (when (<= size default-sysex-buffer-size)
            (multiple-value-bind (buf start size)
                (if octets
                    (values octets start (min (- (length octets) start) size))
                    (values (make-array size :element-type '(unsigned-byte 8))
                            0 size))
              (declare (type non-negative-fixnum start size))
              (cffi:with-pointer-to-vector-data (aptr buf)
                (if (> start 0) (cffi:incf-pointer aptr start))
                ;; Move blocks of 32 bits.
                (loop for i of-type fixnum from 0
                      for j of-type fixnum below (* 2 butlast-events) by 2 do
                        (setf (cffi:mem-aref aptr :int32 i)
                              ;; event->message
                              (cffi:mem-aref ptr :int32 j))
                      finally
                        ;; Last PortMidi message: move blocks of 8 bits.
                        (loop for k below tail-size
                              for m from (* i 4) below size
                              with last-ptr = (cffi:mem-aptr ptr :int32 j) do
                                (setf (cffi:mem-aref aptr :char m)
                                      (cffi:mem-aref last-ptr :char k)))))
              (values buf size))))))))

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
