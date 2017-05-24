;;; Copyright (c) 2016 Tito Latini
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

(in-package :incudine.net)

(defvar *buffer-size* (if (boundp 'incudine.config::*network-buffer-size*)
                          incudine.config::*network-buffer-size*
                          1500)
  "Size of the foreign buffer used to read/write octets.")
(declaim (type positive-fixnum *buffer-size*))

(defun print-stream (obj stream depth)
  (declare (ignore depth))
  (format stream "#<NET:~A-STREAM ~S ~S ~D>" (direction obj) (protocol obj)
          (host obj) (port obj)))

;;; The generic interface inherits the structures, the optimizations
;;; and the utilities to send/receive/manage OSC messages.

(defstruct (input-stream (:include osc:input-stream)
                         (:print-function print-stream)))

(defstruct (output-stream (:include osc:output-stream)
                          (:print-function print-stream)))

(deftype stream () `(or input-stream output-stream))

(defun open (&key (host "localhost") (port #36RIME) (direction :input)
             (protocol :tcp) (buffer-size *buffer-size*)
             (max-values osc:*max-values*) message-encoding)
  (osc:open :host host :port port :direction direction :protocol protocol
            :buffer-size buffer-size :max-values max-values
            :message-encoding message-encoding
            :input-stream-constructor #'make-input-stream
            :output-stream-constructor #'make-output-stream))

(declaim (inline read))
(defun read (stream &optional (flags +default-msg-flags+))
  "Store the received message into the STREAM buffer.
FLAGS are used with the recv and recvfrom calls.
Return the number of the received octets."
  (osc::%receive stream flags nil))

(declaim (inline foreign-read))
(defun foreign-read (stream buffer-pointer buffer-size
                     &optional (flags +default-msg-flags+))
  "A foreign buffer is filled with BUFFER-SIZE octets received from STREAM.
FLAGS are used with the recv and recvfrom calls.
Return the number of the copied octets."
  (declare (type input-stream stream) (type positive-fixnum buffer-size))
  (read stream flags)
  (let ((size (min (message-length stream) buffer-size)))
    (declare (type non-negative-fixnum size))
    (incudine.external:foreign-copy buffer-pointer
      (message-pointer stream) size)
    size))

(declaim (inline send))
(defun send (stream ptr size flags)
  (osc::force-fixnum
    (if (protocolp stream :udp)
        (osc::%sendto (socket-fd stream) ptr size flags
                      (osc::address-value stream 'osc::sockaddr)
                      (osc::address-value stream 'osc::socklen))
        (osc::%send (socket-fd stream) ptr size flags))))

(declaim (inline foreign-write))
(defun foreign-write (stream buffer-pointer buffer-size
                      &optional (flags +default-msg-flags+))
  "Send the octets stored into a foreign buffer.
FLAGS are used with the send and sendto calls.
Return the number of the octets."
  (declare (type output-stream stream)
           (type cffi:foreign-pointer buffer-pointer)
           (type positive-fixnum buffer-size))
  (multiple-value-bind (ptr size)
      (if (osc::slip-encoding-p stream)
          (values (osc::stream-aux-buffer-pointer stream)
                  (if (<= buffer-size (buffer-size stream))
                      (osc::force-fixnum
                        (osc::%slip-encode buffer-pointer
                          (osc::stream-aux-buffer-pointer stream) buffer-size))
                      0))
          (values buffer-pointer buffer-size))
    (declare (type cffi:foreign-pointer ptr) (type non-negative-fixnum size))
    (if (plusp size)
        (send stream ptr size flags)
        0)))

(defun write (stream obj &key (start 0) end (flags +default-msg-flags+))
  "Send the octets obtained from a simple-array (unsigned-byte 8) or
from a string.
START and END are the bounding index designators of the simple-array.
FLAGS are used with the send and sendto calls."
  (declare (type osc:stream stream)
           (type (or (simple-array (unsigned-byte 8)) string) obj)
           (type non-negative-fixnum start)
           (type (or positive-fixnum null) end)
           (optimize speed))
  (if (stringp obj)
      (let ((len (length obj)))
        (when (< len (buffer-size stream))
          (setf (osc::stream-message-length stream) len)
          (cffi:lisp-string-to-foreign obj
            (message-pointer stream) (1+ len))
          (send stream (message-pointer stream) len flags)))
      (let ((end (or end (length obj))))
        (declare (type non-negative-fixnum end))
        (unless (or (>= start end) (> end (length obj)))
          (cffi:with-pointer-to-vector-data (ptr obj)
            (foreign-write stream (cffi:inc-pointer ptr start)
                           (- end start) flags))))))

(defun buffer-to-string (stream)
  "Return the string stored into the STREAM buffer."
  (declare (optimize speed (safety 0)))
  (values (cffi:foreign-string-to-lisp (message-pointer stream)
                                       :count (message-length stream))))

(defun string-to-buffer (string stream)
  "Store STRING into the STREAM buffer."
  (declare (type string string) (optimize speed (safety 0)))
  (let ((len (min (length string) (1- (buffer-size stream)))))
    (setf (osc::stream-message-length stream) len)
    (cffi:lisp-string-to-foreign string (message-pointer stream) (1+ len))
    (values stream len)))
