;;; Copyright (c) 2016-2023 Tito Latini
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
  "Size in bytes of the NET:STREAM buffer used to read or write octets.")
(declaim (type positive-fixnum *buffer-size*))

(defun print-stream (obj stream depth)
  (declare (ignore depth))
  (format stream "#<NET:~A-STREAM ~S ~S ~D>" (direction obj) (protocol obj)
          (host obj) (port obj)))

;;; The generic interface inherits the structures, the optimizations
;;; and the utilities to send/receive/manage OSC messages.

(defstruct (input-stream (:include incudine.osc:input-stream)
                         (:print-function print-stream))
  "Input stream socket type.

Extension of OSC:INPUT-STREAM.")

(defstruct (output-stream (:include incudine.osc:output-stream)
                          (:print-function print-stream))
  "Output stream socket type.

Extension of OSC:OUTPUT-STREAM.")

(deftype stream ()
  "Stream socket type.

Extension of OSC:STREAM."
  `(or input-stream output-stream))

(setf
  (documentation 'input-stream-p 'function)
  "Return T if object is of type NET:INPUT-STREAM."
  (documentation 'output-stream-p 'function)
  "Return T if object is of type NET:OUTPUT-STREAM."
  ;; Readers defined in osc.lisp.
  (documentation 'host 'function)
  "Return the address associated with STREAM."
  (documentation 'port 'function)
  "Return the numeric port associated with STREAM."
  (documentation 'direction 'function)
  "Return the STREAM direction."
  (documentation 'protocol 'function)
  "Return the STREAM protocol."
  (documentation 'buffer-pointer 'function)
  "Return the foreign pointer to the STREAM buffer."
  (documentation 'buffer-size 'function)
  "Return the size in bytes of the STREAM buffer.")

(defun open (&key (host "localhost") (port #36RIME) (direction :input)
             (protocol :tcp) (buffer-size *buffer-size*) (auto-connect-p t)
             (max-values incudine.osc:*max-values*) message-encoding)
  "Create and return a new NET:STREAM.

The HOST address/name and PORT default to \"localhost\" and 24134
(aka #36RIME).

DIRECTION is :INPUT (default) or :OUTPUT to return a NET:INPUT-STREAM
or a NET:OUTPUT-STREAM respectively.

PROTOCOL is :TCP (default) or :UDP. If PROTOCOL is :TCP and
AUTO-CONNECT-P is T (default), try to connect the output stream
socket. See also INCUDINE.NET:CONNECT and INCUDINE.NET:CONNECTED-P.

BUFFER-SIZE is the size in bytes of the buffer used to read or
write octets. It defaults to NET:*BUFFER-SIZE*.

If the NET:STREAM is also used with the OSC (Open Sound Control)
utilities (i.e. OSC:SEND and OSC:RECEIVE), MAX-VALUES is the maximum
number of the required values of a OSC message. It defaults to
OSC:*MAX-VALUES*. MESSAGE-ENCODING is NIL (default) or :SLIP."
  (incudine.osc:open :host host :port port :direction direction
                     :protocol protocol :auto-connect-p auto-connect-p
                     :buffer-size buffer-size :max-values max-values
                     :message-encoding message-encoding
                     :input-stream-constructor #'make-input-stream
                     :output-stream-constructor #'make-output-stream))

(declaim (inline read))
(defun read (stream &optional (flags +default-msg-flags+))
  "Receive a message from a STREAM socket and return the number of
octets read.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
recv and recvfrom for details on the FLAGS argument."
  (incudine.osc::%receive stream flags nil))

(declaim (inline foreign-read))
(defun foreign-read (stream buffer-pointer buffer-size
                     &optional (flags +default-msg-flags+))
  "Read BUFFER-SIZE octets from a STREAM socket into a foreign buffer
pointed to by BUFFER-POINTER. Return the number of octets read.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
recv and recvfrom for details on the FLAGS argument."
  (declare (type input-stream stream) (type positive-fixnum buffer-size))
  (read stream flags)
  (let ((size (min (message-length stream) buffer-size)))
    (declare (type non-negative-fixnum size))
    (incudine.external:foreign-copy buffer-pointer
      (message-pointer stream) size)
    size))

(declaim (inline send))
(defun send (stream ptr size flags)
  (incudine.osc::force-fixnum
    (if (protocolp stream :udp)
        (incudine.osc::%sendto (socket-fd stream) ptr size flags
          (incudine.osc::address-value stream 'incudine.osc::sockaddr)
          (incudine.osc::address-value stream 'incudine.osc::socklen))
        (incudine.osc::%send (socket-fd stream) ptr size flags))))

(declaim (inline foreign-write))
(defun foreign-write (stream buffer-pointer buffer-size
                      &optional (flags +default-msg-flags+))
  "Send BUFFER-SIZE octets from a foreign buffer pointed to by
BUFFER-POINTER into a STREAM socket. Return the number of octets
written.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
send and sendto for details on the FLAGS argument."
  (declare (type output-stream stream)
           (type cffi:foreign-pointer buffer-pointer)
           (type positive-fixnum buffer-size))
  (multiple-value-bind (ptr size)
      (if (incudine.osc::slip-encoding-p stream)
          (values (incudine.osc::stream-aux-buffer-pointer stream)
                  (if (<= buffer-size (buffer-size stream))
                      (incudine.osc::force-fixnum
                        (incudine.osc::%slip-encode buffer-pointer
                          (incudine.osc::stream-aux-buffer-pointer stream) buffer-size))
                      0))
          (values buffer-pointer buffer-size))
    (declare (type cffi:foreign-pointer ptr) (type non-negative-fixnum size))
    (if (plusp size)
        (send stream ptr size flags)
        0)))

(defun write (stream obj &key (start 0) end (flags +default-msg-flags+))
  "Send the octets from OBJ into a STREAM socket and return the number
of octets written.

OBJ is one of NIL, a vector of octets or a string. If NIL, send the
octets stored in the STREAM buffer.

START and END are the bounding index designators of the array.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
send and sendto for details on the FLAGS argument."
  (declare (type incudine.osc:stream stream)
           (type (or (simple-array (unsigned-byte 8)) string null) obj)
           (type non-negative-fixnum start)
           (type (or positive-fixnum null) end))
  (incudine-optimize
    (cond ((null obj)
           (send stream (message-pointer stream)
                 (message-length stream) flags))
          ((stringp obj)
           (let ((len (length obj)))
             (when (< len (buffer-size stream))
               (setf (incudine.osc::stream-message-length stream) len)
               (cffi:lisp-string-to-foreign obj
                 (message-pointer stream) (1+ len))
               (send stream (message-pointer stream) len flags))))
          (t
           (let ((end (or end (length obj))))
             (declare (type non-negative-fixnum end))
             (unless (or (>= start end) (> end (length obj)))
               (cffi:with-pointer-to-vector-data (ptr obj)
                 (foreign-write stream (cffi:inc-pointer ptr start)
                                (- end start) flags))))))))

(defun buffer-to-string (stream)
  "Return the string stored in the buffer of the STREAM socket."
  (incudine-optimize
    (values (cffi:foreign-string-to-lisp (message-pointer stream)
                                         :count (message-length stream)))))

(defun string-to-buffer (string stream)
  "Copy the STRING into the buffer of the STREAM socket."
  (declare (type string string))
  (incudine-optimize
    (let ((len (min (length string) (1- (buffer-size stream)))))
      (setf (incudine.osc::stream-message-length stream) len)
      (cffi:lisp-string-to-foreign string (message-pointer stream) (1+ len))
      (values stream len))))

(in-package :incudine)

(defun net-remove-receiver-and-responders (stream)
  (when (incudine.osc:input-stream-p stream)
    (remove-receiver-and-responders stream))
  stream)

;; Also for generic networking.
(pushnew #'net-remove-receiver-and-responders incudine.osc:*before-close-hook*)

(defmethod valid-input-stream-p ((obj incudine.osc:input-stream)) t)

(defmethod valid-input-stream-p ((obj incudine.osc:output-stream)) nil)

(defmacro make-osc-responder (stream address types function)
  "Create and return a responder for a OSC:INPUT-STREAM that responds
to an OSC message with ADDRESS and TYPES.

FUNCTION is added to the list of receiver-functions for STREAM.

The function takes the OSC values as arguments.

Example:

    (make-osc-responder *oscin* \"/osc/test\" \"iii\"
                        (lambda (a b c)
                          (msg warn \"~D ~D ~D\" a b c)))"
  (let ((function (if (eq (car function) 'function)
                      (cadr function)
                      function)))
    (if (atom function)
        (let ((args (loop for i below (length types) collect (gensym))))
          (setf function `(lambda ,args (,function ,@args))))
        (assert (eq (car function) 'lambda)))
    (with-gensyms (s)
      `(make-responder ,stream
         (lambda (,s)
           (when (incudine.osc:check-pattern ,s ,address ,types)
             (incudine.osc:with-values ,(cadr function) (,s ,types)
               ,@(cddr function)))
           (values))))))

(defun start-osc-recv (receiver)
  (declare (type receiver receiver) #.*standard-optimize-settings*)
  (bt:make-thread
    (lambda ()
      (let ((stream (receiver-stream receiver)))
        (declare (type incudine.osc:input-stream stream))
        (incudine.osc:close-connections stream)
        ;; Flush pending writes.
        (incudine.osc:without-block (in stream)
          (loop while (plusp (the fixnum (incudine.osc:receive in)))))
        (setf (receiver-status receiver) t)
        (loop while (receiver-status receiver) do
                (when (and (plusp (the fixnum (incudine.osc:receive stream)))
                           (receiver-status receiver))
                  (handler-case
                      (dolist (fn (receiver-functions receiver))
                        (funcall (the function fn) stream))
                    (condition (c) (nrt-msg error "~A" c)))))))
    :name (format nil "osc-recv ~D"
                  (incudine.osc:port (receiver-stream receiver)))))

(defmethod recv-start ((stream incudine.osc::stream)
                       &key (priority *receiver-default-priority*))
  (unless (eq (recv-status stream) :running)
    (add-receiver stream (or (receiver stream) (make-receiver stream))
                  #'start-osc-recv priority)))

(defmethod recv-stop ((stream incudine.osc::stream))
  (let ((recv (receiver stream)))
    (when (and recv (receiver-status recv))
      (incudine.osc:with-stream (tmp :direction :output
                                 :protocol (incudine.osc:protocol stream)
                                 :host (incudine.osc:host stream)
                                 :port (incudine.osc:port stream)
                                 :buffer-size 32 :max-values 8)
        (compare-and-swap (receiver-status recv) t nil)
        ;; Unblock the receiver.
        (incudine.osc:message tmp "/receiver/quit" ""))
      (incudine.osc:close-connections stream)
      (recv-unset-thread recv)
      recv)))

(defun start-net-recv (receiver)
  (declare (type receiver receiver) #.*standard-optimize-settings*)
  (bt:make-thread
   (lambda ()
     (let ((stream (receiver-stream receiver)))
       (declare (type net:input-stream stream))
       (net:close-connections stream)
       ;; Flush pending writes.
       (net:without-block (in stream)
         (loop while (plusp (the fixnum (net:read in)))))
       (setf (receiver-status receiver) t)
       (loop while (receiver-status receiver) do
             (when (and (plusp (the fixnum (net:read stream)))
                        (receiver-status receiver))
               (handler-case
                   (dolist (fn (receiver-functions receiver))
                     (funcall (the function fn) stream))
                 (condition (c) (nrt-msg error "~A" c)))))))
   :name (format nil "network-recv ~D"
                 (net:port (receiver-stream receiver)))))

(defmethod recv-start ((stream incudine.net:input-stream)
                       &key (priority *receiver-default-priority*))
  (unless (eq (recv-status stream) :running)
    (add-receiver stream (or (receiver stream) (make-receiver stream))
                  #'start-net-recv priority)))
