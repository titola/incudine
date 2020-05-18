;;; Copyright (c) 2015-2020 Tito Latini
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

(in-package :incudine.osc)

(eval-when (:load-toplevel :execute)
  (let ((nick incudine.config::*osc-package-nicknames*))
    (when nick (rename-package "INCUDINE.OSC" "INCUDINE.OSC" nick))))

(defvar *buffer-size* (if (boundp 'incudine.config::*osc-buffer-size*)
                          incudine.config::*osc-buffer-size*
                          1500)
  "Size in bytes of the OSC:STREAM buffer used to read or write octets.")
(declaim (type positive-fixnum *buffer-size*))

(defvar *max-values* (if (boundp 'incudine.config::*osc-max-values*)
                         incudine.config::*osc-max-values*
                         50)
  "Maximum number of the values required by an OSC message.")
(declaim (type positive-fixnum *max-values*))

;;; addrinfo-flags for the argument 'hints' of the c-call getaddrinfo.
;;; The default is 0.
(defvar *addrinfo-hints-flags*
  (if (boundp 'incudine.config::*addrinfo-hints-flags*)
      incudine.config::*addrinfo-hints-flags*
      0))
(declaim (type non-negative-fixnum *addrinfo-hints-flags*))

(defvar *before-close-hook* nil
  "List of the functions of one argument called before to close a
stream socket. The function argument is the stream to close.")

(defvar *listen-backlog* 8
  "The maximum length to which the queue of pending connections for
a socket stream may grow.")
(declaim (type (unsigned-byte 8) *listen-backlog*))

(defstruct (stream (:constructor %make-stream)
                   (:print-function print-stream)
                   (:copier nil))
  "Stream socket type for OSC (Open Sound Control) messages."
  (host "localhost" :type simple-string)
  (port #36ROSE :type (unsigned-byte 16))
  ;; Pointer to a `struct address'.
  (address-ptr (cffi:null-pointer) :type cffi:foreign-pointer)
  ;; Pointer to a `struct addrinfo'.
  (addrinfo-ptr (cffi:null-pointer) :type cffi:foreign-pointer)
  ;; Pointer to a `struct osc_fds' (see osc.h) if the stream is a
  ;; OSC:INPUT-STREAM and the protocol is TCP, otherwise it is a pointer
  ;; to a file descriptor used to close the socket within the finalizer.
  (fds-ptr (cffi:null-pointer) :type cffi:foreign-pointer)
  (protocol :udp :type (member :udp :tcp))
  (socket-fd -1 :type fixnum)
  (direction :input :type (member :input :output))
  ;; Foreign buffer used to read/write an OSC packet.
  (buffer-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (buffer-size *buffer-size* :type positive-fixnum)
  (max-values *max-values* :type positive-fixnum)
  ;; T if the pointers to the required OSC values are to update.
  (buffer-to-index-p nil :type boolean)
  ;; Pointer to the memory where the OSC message starts.
  (message-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (message-length 0 :type non-negative-fixnum)
  (message-length-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (bundle-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (bundle-length 0 :type non-negative-fixnum)
  (message-encoding nil :type (member nil :slip))
  ;; Pointers to the required OSC values. The first slot is reserved
  ;; for the pointer to the first OSC type. The second and last slots
  ;; are reserved for the pointer to the free memory.
  (value-vec-ptr (cffi:null-pointer) :type cffi:foreign-pointer)
  ;; Pointer to the OSC types of the required values. The first slot
  ;; is reserved for the number of the required values.
  (type-vec-ptr (cffi:null-pointer) :type cffi:foreign-pointer)
  ;; Auxiliary foreign buffer used for Serial Line IP (SLIP).
  (aux-buffer-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  ;; Foreign memory space reserved to store a temporary value.
  (tmp-ptr (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (input-stream (:include stream))
  "Input stream socket type for OSC (Open Sound Control) messages.")

(defstruct (output-stream (:include stream))
  "Output stream socket type for OSC (Open Sound Control) messages."
  ;; Latency in seconds.
  (latency 0d0 :type double-float))

(setf
  (documentation 'input-stream-p 'function)
  "Return T if object is of type OSC:INPUT-STREAM."
  (documentation 'output-stream-p 'function)
  "Return T if object is of type OSC:OUTPUT-STREAM.")

(defun print-stream (obj stream depth)
  (declare (ignore depth))
  (format stream "#<OSC:~A-STREAM ~S ~S ~D>"
          (stream-direction obj) (stream-protocol obj)
          (stream-host obj) (stream-port obj)))

(declaim (inline protocolp))
(defun protocolp (stream protocol)
  "Return T if the STREAM protocol is PROTOCOL; otherwise, return NIL."
  (declare (type stream stream) (type (member :udp :tcp) protocol))
  (eq (stream-protocol stream) protocol))

(declaim (inline slip-encoding-p))
(defun slip-encoding-p (stream)
  (eq (stream-message-encoding stream) :slip))

(defmacro address-value (stream slot-name)
  `(cffi:foreign-slot-value (stream-address-ptr ,stream)
                            '(:struct address) ,slot-name))

(defmacro address-socklen-ptr (stream)
  `(cffi:foreign-slot-pointer (stream-address-ptr ,stream)
                              '(:struct address) 'socklen))

(defmacro addrinfo-value (stream slot-name)
  `(cffi:foreign-slot-value (stream-addrinfo-ptr ,stream)
                            '(:struct addrinfo) ,slot-name))

;;; send() and recv() return the number of the bytes received, or -1
;;; if an error occurred. We force a fixnum on 32-bit platforms.
(defmacro force-fixnum (form)
  (if (> (* 8 (cffi:foreign-type-size :int))
         incudine.util::n-fixnum-bits)
      (with-gensyms (x)
        `(let ((,x ,form))
           (if (plusp ,x)
               (logand ,x #xffffff)
               -1)))
      form))

(declaim (inline encode-timestamp))
(defun encode-timestamp (seconds)
  "Return the two 32-bit fields of the 64-bit NTP timestamp format (RFC 5905)."
  (if (zerop seconds)
      (values 0 1) ; Immediate 0x0000000000000001
      (multiple-value-bind (sec frac)
          ;; We don't send OSC bundles in the past, therefore we can use
          ;; the time with dual meaning: if SECONDS is greater than 63103
          ;; (about 17 hours), the time is absolute otherwise it is added
          ;; to the current time.  63104 is the offset of the NTP Timestamp
          ;; Era 1 (from 8 Feb 2036), so this hack will work for centuries.
          (floor (if (> seconds 63103) seconds (+ (incudine:timestamp) seconds)))
        (values sec (floor (* frac #x100000000))))))

(declaim (inline free-pointer))
(defun free-pointer (ptr)
  (unless (cffi:null-pointer-p ptr) (cffi:foreign-free ptr)))

(define-constant +bundle-reserved-bytes+ (+ 8 8 4))
(define-constant +zero-padding-bytes+ 4)
(define-constant +temp-space-bytes+ 8)
(define-constant +int-size+ (cffi:foreign-type-size :int))

(declaim (inline latency))
(defun latency (stream)
  "Latency in seconds for OSC bundles sent from STREAM. Setfable."
  (output-stream-latency stream))

(defun set-latency (stream value)
  (setf (output-stream-latency stream)
        (max 0d0 (coerce value 'double-float))))

(defsetf latency set-latency)

(declaim (inline set-bundle-time))
(defun set-bundle-time (stream seconds)
  (multiple-value-bind (sec frac) (encode-timestamp seconds)
    (setf (cffi:mem-aref (stream-bundle-pointer stream) :uint32 2)
          (swap-bytes:htonl sec))
    (setf (cffi:mem-aref (stream-bundle-pointer stream) :uint32 3)
          (swap-bytes:htonl frac))
    seconds))

(declaim (inline set-bundle-first-element-length))
(defun set-bundle-first-element-length (stream)
  (setf (cffi:mem-aref (stream-message-length-pointer stream) :uint32)
        (swap-bytes:htonl (stream-message-length stream))))

(defun init-stream (obj latency)
  (when (output-stream-p obj)
    (setf (latency obj) latency))
  (cffi:lisp-string-to-foreign "#bundle" (stream-bundle-pointer obj) 8)
  (set-bundle-time obj 0)
  (setf (stream-message-length-pointer obj)
        (cffi:inc-pointer (stream-bundle-pointer obj) 16))
  obj)

(defun open (&key (host "localhost") (port #36ROSE) (direction :input)
             (protocol :udp) (buffer-size *buffer-size*) (latency 0)
             (max-values *max-values*) message-encoding
             (input-stream-constructor #'make-input-stream)
             (output-stream-constructor #'make-output-stream))
  "Create and return a new OSC:STREAM.

The HOST address/name and PORT default to \"localhost\" and 32126
(aka #36ROSE).

DIRECTION is :INPUT (default) or :OUTPUT to return an OSC:INPUT-STREAM
or an OSC:OUTPUT-STREAM respectively.

PROTOCOL is :TCP or :UDP (default).

BUFFER-SIZE is the size in bytes of the buffer used to read or
write octets. It defaults to OSC:*BUFFER-SIZE*.

LATENCY (0 by default) is the latency in seconds for OSC bundles.

MAX-VALUES is the maximum number of the required values of an OSC
message. It defaults to OSC:*MAX-VALUES*.

MESSAGE-ENCODING is NIL (default) or :SLIP."
  (declare (type (member :input :output) direction)
           (type (member :udp :tcp) protocol) (type simple-string host)
           (type (unsigned-byte 16) port) (type real latency)
           (type positive-fixnum buffer-size max-values)
           (type (member nil :slip) message-encoding))
  (cffi:with-foreign-object (address-ptr :pointer)
    (let ((address nil)
          (buf-ptr nil)
          (aux-ptr nil)
          (value-vec-ptr nil)
          (type-vec-ptr nil)
          (fds-ptr nil)
          (obj nil))
      (unless (zerop (new-address address-ptr host port (eq protocol :udp)
                                  (eq direction :input) *addrinfo-hints-flags*))
        (incudine::foreign-alloc-error "OSC address allocation."))
      (handler-case
          (let* ((buffer-size (max 1500 buffer-size))
                 ;; Add 4 bytes with zero, so the loop in STREAM-BUFFER-STRLEN
                 ;; never fails if the message is wrong and without zeroes.
                 ;; Also reserve the space to store a temporary value (see GET-FLOAT
                 ;; for little-endian) and a file descriptor.
                 (buf-pad (+ +zero-padding-bytes+ +temp-space-bytes+
                             (if (and (eq direction :input) (eq protocol :tcp))
                                 0 +int-size+)))
                 (buffer-offset (if (and (eq protocol :tcp)
                                         (null message-encoding))
                                    ;; 4 bytes reserved for the size of the message.
                                    4 0))
                 (reserved-bytes (+ +bundle-reserved-bytes+ buffer-offset)))
            (setf address (cffi:mem-ref address-ptr :pointer))
            (setf buf-ptr
                  (cffi:foreign-alloc :char :count (+ buffer-size buf-pad)
                                      :initial-element 0))
            (setf aux-ptr
                  (if (eq message-encoding :slip)
                      (cffi:foreign-alloc :char :count (* 2 buffer-size)
                                          :initial-element 0)
                      (cffi:null-pointer)))
            (setf value-vec-ptr
                  (cffi:foreign-alloc :pointer :count (+ max-values 3)))
            (setf type-vec-ptr
                  (cffi:foreign-alloc :char :count (+ max-values 1)
                                      :initial-element 0))
            (setf fds-ptr (alloc-fds buf-ptr (+ buffer-size buf-pad) direction
                                     protocol))
            (setf obj (funcall
                        (if (eq direction :input)
                            input-stream-constructor
                            output-stream-constructor)
                        :host host
                        :port port
                        :protocol protocol
                        :address-ptr address
                        :fds-ptr fds-ptr
                        :addrinfo-ptr (cffi:mem-ref address :pointer)
                        :direction direction
                        :buffer-pointer buf-ptr
                        :message-pointer
                          (cffi:inc-pointer buf-ptr reserved-bytes)
                        :bundle-pointer
                          (if (zerop buffer-offset)
                              buf-ptr
                              (cffi:inc-pointer buf-ptr buffer-offset))
                        :message-encoding message-encoding
                        :buffer-size (- buffer-size reserved-bytes)
                        :max-values max-values
                        :value-vec-ptr value-vec-ptr
                        :type-vec-ptr type-vec-ptr
                        :aux-buffer-pointer aux-ptr
                        :tmp-ptr
                          (cffi:inc-pointer
                            buf-ptr (+ buffer-size +zero-padding-bytes+)))))
        (condition (c)
          (if fds-ptr (free-fds fds-ptr direction protocol))
          (if address (free-address address))
          (dolist (p (list buf-ptr aux-ptr value-vec-ptr type-vec-ptr))
            (if p (free-pointer p)))
          (error c)))
      (incudine.util::finalize obj
        (lambda ()
          (free-fds fds-ptr direction protocol)
          (free-address address)
          (mapc #'free-pointer
                (list buf-ptr aux-ptr value-vec-ptr
                      type-vec-ptr))))
      (handler-case
          (%open (init-stream obj latency))
        (error (c)
          (close obj)
          (network-error "OSC:OPEN ~A (~A)" c
                         (incudine.external:errno-to-string)))))))

(defun alloc-fds (buf-ptr bufsize direction protocol)
  (if (and (eq direction :input) (eq protocol :tcp))
      (%alloc-fds)
      (let ((ptr (cffi:inc-pointer buf-ptr (- bufsize +int-size+))))
        (setf (cffi:mem-ref ptr :int) -1)
        ptr)))

;;; Necessary only within the finalizer.
(defun free-fds (ptr direction protocol)
  (unless (cffi:null-pointer-p ptr)
    (cond ((and (eq direction :input) (eq protocol :tcp))
           (close-server ptr)
           (%close-connections ptr)
           (cffi:foreign-free ptr))
          (t (let ((fd (cffi:mem-ref ptr :int)))
               (unless (= fd -1)
                 (cffi:foreign-funcall "close" :int fd :int)))))))

(defun %open (stream)
  (declare (type stream stream))
  (let ((fd (cffi:foreign-funcall "socket"
              :int (addrinfo-value stream 'ai-family)
              :int (addrinfo-value stream 'ai-socktype)
              :int (addrinfo-value stream 'ai-protocol) :int)))
    (when (= fd -1)
      (network-error "Failed to create the socket."))
    (unless (and (input-stream-p stream) (protocolp stream :tcp))
      ;; Info for the finalizer.
      (setf (cffi:mem-ref (stream-fds-ptr stream) :int) fd))
    (setf (stream-socket-fd stream) fd)
    (cond ((input-stream-p stream)
           (when (and (protocolp stream :tcp)
                      (not (zerop (setsock-reuseaddr fd))))
             (warn "OSC:OPEN reuse of the local addresses is disabled"))
           (unless (zerop (cffi:foreign-funcall "bind"
                            :int fd :pointer (addrinfo-value stream 'ai-addr)
                            #.+socklen-type+ (addrinfo-value stream 'ai-addrlen)
                            :int))
             (network-error "Failed to assign the address."))
           (when (protocolp stream :tcp)
             (unless (zerop (cffi:foreign-funcall "listen" :int fd
                                                  :int *listen-backlog* :int))
               (network-error "listen call failed."))
             (set-server-fd (stream-fds-ptr stream) fd)))
          ((protocolp stream :tcp)
           (unless (zerop (cffi:foreign-funcall "connect"
                            :int fd :pointer (addrinfo-value stream 'ai-addr)
                            #.+socklen-type+ (addrinfo-value stream 'ai-addrlen)
                            :int))
             (warn "OSC:OPEN connection failed")))))
  stream)

(declaim (inline open-p))
(defun open-p (stream)
  "Whether STREAM is an open stream socket."
  (not (cffi:null-pointer-p (stream-address-ptr stream))))

(defun close-fd (stream)
  (when (plusp (stream-socket-fd stream))
    (cffi:foreign-funcall "close" :int (stream-socket-fd stream) :int)
    (setf (stream-socket-fd stream) -1))
  (unless (cffi:null-pointer-p (stream-fds-ptr stream))
    (if (and (input-stream-p stream) (protocolp stream :tcp))
        (%close-connections (stream-fds-ptr stream))
        (setf (cffi:mem-ref (stream-fds-ptr stream) :int) -1)))
  stream)

(defun close (stream)
  "Close the STREAM socket."
  (declare (type stream stream))
  (macrolet ((free-stream-pointer (stream reader)
               `(progn
                  (free-pointer (,reader ,stream))
                  (setf (,reader ,stream) (cffi:null-pointer)))))
    (when (open-p stream)
      (dolist (fn *before-close-hook*)
        (funcall fn stream))
      (incudine.util:without-interrupts
        (close-fd stream)
        (if (and (input-stream-p stream) (protocolp stream :tcp))
            (free-stream-pointer stream stream-fds-ptr)
            (setf (stream-fds-ptr stream) (cffi:null-pointer)))
        (free-address (stream-address-ptr stream))
        (free-stream-pointer stream stream-buffer-pointer)
        (free-stream-pointer stream stream-value-vec-ptr)
        (free-stream-pointer stream stream-type-vec-ptr)
        (free-stream-pointer stream stream-aux-buffer-pointer)
        (incudine.util::cancel-finalization stream)
        (setf (stream-address-ptr stream) (cffi:null-pointer))
        (setf (stream-addrinfo-ptr stream) (cffi:null-pointer))
        (setf (stream-message-pointer stream) (cffi:null-pointer))
        (setf (stream-tmp-ptr stream) (cffi:null-pointer)))))
  (values))

(defun reject (stream)
  "Close the input STREAM socket and create a new socket."
  (declare (type input-stream stream))
  (when (open-p stream)
    (dolist (fn *before-close-hook*)
      (funcall (the function fn) stream))
    (%open (close-fd stream))))

(defun close-connections (stream)
  "Close all the connections to the STREAM socket."
  (unless (or (cffi:null-pointer-p (stream-fds-ptr stream))
              (not (protocolp stream :tcp)))
    (%close-connections (stream-fds-ptr stream))))

(defun connect (stream)
  "Close the output STREAM socket, create a new socket and try to connect it."
  (declare (type output-stream stream))
  (when (and (protocolp stream :tcp) (open-p stream))
    (dolist (fn *before-close-hook*)
      (funcall (the function fn) stream))
    (%open (close-fd stream))))

(defmacro with-stream ((stream &rest arguments) &body body)
  "Use OSC:OPEN with ARGUMENTS to create an OSC:STREAM. When control
leaves the body, either normally or abnormally, the OSC:STREAM is
automatically closed."
  `(let ((,stream (open ,@arguments)))
     (unwind-protect (progn ,@body)
       (close ,stream))))

(macrolet ((define-stream-readers (names)
             `(progn
                (declaim (inline ,@names))
                ,@(mapcar (lambda (name)
                            `(defun ,name (stream)
                               (declare (type stream stream))
                               (,(alexandria:format-symbol *package*
                                                           "STREAM-~A" name)
                                 stream)))
                          names))))
  (define-stream-readers (host port protocol socket-fd direction buffer-pointer
                          buffer-size max-values message-pointer message-length
                          message-encoding)))

;; Other docstrings in incudine/src/network/generic.lisp.
(setf (documentation 'socket-fd 'function)
      "Return the socket file descriptor of the STREAM socket."
      (documentation 'max-values 'function)
      "Maximum number of the values required by an OSC message for STREAM."
      (documentation 'message-pointer 'function)
      "Return the foreign pointer to the OSC message stored in the STREAM buffer."
      (documentation 'message-length 'function)
      "Return the length of the OSC message stored in the STREAM buffer. Setfable.")

(defun set-message-length (stream value)
  (declare (type stream stream) (type positive-fixnum value))
  (setf (stream-message-length stream)
        (min value (stream-buffer-size stream))))

(defsetf message-length set-message-length)

(defun set-message-encoding (stream value)
  (declare (type (member nil :slip) value))
  (if (eq value :slip)
      (when (cffi:null-pointer-p (stream-aux-buffer-pointer stream))
        (setf (stream-aux-buffer-pointer stream)
              (cffi:foreign-alloc :char
                :count (* 2 (stream-buffer-size stream))
                :initial-element 0)))
      (unless (cffi:null-pointer-p (stream-aux-buffer-pointer stream))
        (cffi:foreign-free (stream-aux-buffer-pointer stream))
        (setf (stream-aux-buffer-pointer stream) (cffi:null-pointer))))
  (setf (stream-message-encoding stream) value))

(setf (documentation 'message-encoding 'function)
      "Return the encoding type associated with STREAM. Setfable.")

(defsetf message-encoding set-message-encoding)

(defun broadcast (stream)
  "Whether broadcasting is enabled for the STREAM socket. Setfable."
  (declare (type stream stream))
  (plusp (getsock-broadcast (stream-socket-fd stream))))

(defun set-broadcast (stream value)
  (declare (type stream stream) (type boolean value))
  (zerop (setsock-broadcast (stream-socket-fd stream)
                            (stream-addrinfo-ptr stream) value)))

(defsetf broadcast set-broadcast)

(defun block-p (stream)
  "Whether the STREAM socket is in blocking mode. Setfable."
  (not (getsock-nonblock (stream-socket-fd stream))))

(defun set-block (stream block-p)
  (declare (type stream stream) (type boolean block-p))
  (assert (zerop (setsock-nonblock (stream-socket-fd stream) (not block-p))))
  block-p)

(defsetf block-p set-block)

(defmacro without-block ((var input-stream) &body body)
  "Bind VAR to INPUT-STREAM socket and set the non-blocking mode
during BODY."
  (with-gensyms (without-block-body)
    `(let ((,var ,input-stream))
       (flet ((,without-block-body () ,@body))
         (if (block-p ,var)
             (unwind-protect
                  (progn (setf (block-p ,var) nil) (,without-block-body))
               (setf (block-p ,var) t))
             (,without-block-body))))))

(defun connections (stream)
  "Return the number of the connections accepted by the listening
STREAM socket."
  (declare (type input-stream stream))
  (if (and (protocolp stream :tcp)
           (not (cffi:null-pointer-p (stream-fds-ptr stream))))
      (cffi:foreign-funcall "osc_connections"
                            :pointer (stream-fds-ptr stream) :int)
      0))

(defun connections-fd (stream)
  "Return the file descriptors of the connections accepted by the
listening STREAM socket."
  (declare (type input-stream stream))
    (if (and (protocolp stream :tcp)
             (not (cffi:null-pointer-p (stream-fds-ptr stream))))
        (loop for i = 3 then fd
              for fd = (cffi:foreign-funcall "osc_next_fd_set"
                         :pointer (stream-fds-ptr stream) :int i :int)
              while (plusp fd)
                collect fd)))

;;; Sometimes we want to dialog with the sender.
(declaim (inline last-recv-fd))
(defun last-recv-fd (stream)
  "Return the file descriptor for the accepted connection used
to get the last received message."
  (cffi:foreign-funcall "osc_lastfd" :pointer (stream-fds-ptr stream) :int))

(defun socket-send (sockfd octets &optional (flags +default-msg-flags+))
  "Send OCTETS on a socket with file descriptor SOCKFD.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
send and sendto for details on the FLAGS argument."
  (declare (type positive-fixnum sockfd)
           (type (simple-array (unsigned-byte 8)) octets))
  (incudine-optimize
    (cffi:with-pointer-to-vector-data (buf octets)
      (force-fixnum
        (cffi:foreign-funcall "send" :int sockfd :pointer buf
                              :unsigned-int (length octets) :int flags :int)))))

;;; SLIP method for framing packets (RFC 1055).
(declaim (inline slip-encode))
(defun slip-encode (stream &optional ptr length)
  "Serial Line IP encoding."
  (%slip-encode (or ptr (stream-message-pointer stream))
                (stream-aux-buffer-pointer stream)
                (or length (stream-message-length stream))))

(declaim (inline slip-decode))
(defun slip-decode (stream)
  "Serial Line IP decoding."
  (setf (stream-message-length stream)
        (%slip-decode (stream-message-pointer stream)
                      (stream-message-length stream))))

(defun send-slip-message (stream msg-ptr length
                          &optional (flags +default-msg-flags+))
  (declare (type stream stream) (type non-negative-fixnum flags))
  (let ((len (slip-encode stream msg-ptr length)))
    (declare (type non-negative-fixnum len))
    (case (protocol stream)
      (:udp
       (cffi:foreign-funcall "sendto"
         :int (stream-socket-fd stream)
         :pointer (stream-aux-buffer-pointer stream) :unsigned-long len
         :int flags :pointer (address-value stream 'sockaddr)
         #.+socklen-type+ (address-value stream 'socklen) :int))
      (:tcp
       (cffi:foreign-funcall "send"
         :int (stream-socket-fd stream)
         :pointer (stream-aux-buffer-pointer stream) :unsigned-long len
         :int flags :int)))))

(defun net-recv (stream flags osc-message-p)
  (let ((slip-encoding-flag 1)
        (count-prefix-flag 2))
    (multiple-value-bind (ptr size maybe-count-prefix-flag)
        (if osc-message-p
            (values (stream-message-length-pointer stream)
                    (- (stream-buffer-size stream) +bundle-reserved-bytes+)
                    count-prefix-flag)
            (values (stream-message-pointer stream)
                    (- (stream-buffer-size stream) 4)
                    0))
      (%osc-recv (stream-fds-ptr stream)
                 (stream-address-ptr stream) ptr size
                 (if (slip-encoding-p stream)
                     slip-encoding-flag
                     maybe-count-prefix-flag)
                 flags))))

(defun %receive (stream flags osc-message-p)
  (declare (type stream stream) (type non-negative-fixnum flags)
           (type boolean osc-message-p))
  (incudine-optimize
    (let ((res (if (and (protocolp stream :tcp)
                        (input-stream-p stream))
                   (net-recv stream flags osc-message-p)
                   (force-fixnum
                     (%recvfrom (stream-socket-fd stream)
                       (stream-message-pointer stream)
                       (stream-buffer-size stream)
                       flags (address-value stream 'sockaddr)
                       (address-socklen-ptr stream))))))
      (declare (type fixnum res))
      (cond ((plusp res)
             (setf (stream-message-length stream) res)
             (when osc-message-p
               (setf (stream-buffer-to-index-p stream) t)))
            (t
             (setf (stream-message-length stream) 0)))
      (when (slip-encoding-p stream)
        (slip-decode stream))
      res)))

(declaim (inline receive))
(defun receive (stream &optional (flags +default-msg-flags+))
  "Store the received OSC message into the STREAM buffer.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
recv and recvfrom for details on the FLAGS argument."
  (%receive stream flags t))

(defun send (stream &optional (flags +default-msg-flags+))
  "Send the OSC message stored in the STREAM buffer.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
send and sendto for details on the FLAGS argument."
  (declare (type stream stream) (type non-negative-fixnum flags)
           #.incudine.util:*reduce-warnings*)
  (incudine-optimize
    (cond ((slip-encoding-p stream)
           (send-slip-message stream (stream-message-pointer stream)
                              (stream-message-length stream) flags))
          ((protocolp stream :udp)
           (%sendto (stream-socket-fd stream) (stream-message-pointer stream)
                    (stream-message-length stream) flags
                    (address-value stream 'sockaddr)
                    (address-value stream 'socklen)))
          (t
           ;; OSC 1.0 spec: length-count prefix on the start of the packet.
           (setf (cffi:mem-ref (stream-message-length-pointer stream) :uint32)
                 (htonl (stream-message-length stream)))
           (%send (stream-socket-fd stream)
                  (stream-message-length-pointer stream)
                  (+ 4 (stream-message-length stream)) flags)))))

(defun send-bundle (stream &optional (seconds 0.0) (flags +default-msg-flags+))
  "Send the OSC bundle stored in the STREAM buffer with OSC timestamp
SECONDS (0.0 by default) plus the stream latency.

See SIMPLE-BUNDLE for details about the OSC timestamp SECONDS.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages for
send and sendto for details on the FLAGS argument."
  (declare (type stream stream) (type non-negative-fixnum flags)
           #.incudine.util:*reduce-warnings*)
  (incudine-optimize
    (set-bundle-time stream (+ seconds (latency stream)))
    (cond ((slip-encoding-p stream)
           (send-slip-message stream (stream-bundle-pointer stream)
                              (stream-bundle-length stream)))
          ((protocolp stream :udp)
           (%sendto (stream-socket-fd stream) (stream-bundle-pointer stream)
                    (stream-bundle-length stream) flags
                    (address-value stream 'sockaddr)
                    (address-value stream 'socklen)))
          (t
           (%send (stream-socket-fd stream) (stream-buffer-pointer stream)
                  (+ 4 (stream-bundle-length stream)) flags)))))

#+little-endian
(defmacro get-float (ptr tmp-ptr &optional double-p)
  (multiple-value-bind (type1 type2 ntoh)
      (if double-p
          (values :uint64 :double 'ntohq)
          (values :uint32 :float 'ntohl))
    `(progn
       (setf (cffi:mem-ref ,tmp-ptr ,type1)
             (,ntoh (cffi:mem-ref ,ptr ,type1)))
       (cffi:mem-ref ,tmp-ptr ,type2))))

#-little-endian
(defmacro get-float (ptr tmp-ptr &optional double-p)
  (declare (ignore tmp-ptr))
  `(cffi:mem-ref ,ptr ,(if double-p :double :float)))

(defmacro get-foreign-array (ptr size)
  (with-gensyms (i arr p)
    `(let ((,arr (make-array ,size :element-type '(unsigned-byte 8)))
           (,p ,ptr))
       (dotimes (,i ,size ,arr)
         (setf (aref ,arr ,i)
               (cffi:mem-aref ,p :unsigned-char ,i))))))
(defmacro get-midi (ptr)
  `(values (cffi:mem-ref ,ptr :unsigned-char 0)
           (cffi:mem-ref ,ptr :unsigned-char 1)
           (cffi:mem-ref ,ptr :unsigned-char 2)
           (cffi:mem-ref ,ptr :unsigned-char 3)))

(define-constant +data-index-offset+ 2)

(defmacro arg-pointer (stream index)
  `(cffi:mem-aref (stream-value-vec-ptr ,stream) :pointer ,index))

(defun value-pointer (stream index)
  "Return the foreign pointer to a required value of the OSC message stored
in the STREAM buffer. The OSC value is specified by the zero-based INDEX."
  (arg-pointer stream (+ index +data-index-offset+)))

(define-compiler-macro value-pointer (stream index)
  (if (constantp index)
      `(arg-pointer ,stream ,(+ (eval index) +data-index-offset+))
      `(arg-pointer ,stream (+ ,index ,+data-index-offset+))))

(defmacro required-values (stream)
  "Number of the required values of the OSC message stored in the STREAM buffer."
  `(cffi:mem-ref (stream-type-vec-ptr ,stream) :unsigned-char))

(defmacro typetag-code (stream index)
  `(cffi:mem-aref (stream-type-vec-ptr ,stream) :unsigned-char (1+ ,index)))

;;; Note: SWAP-BYTES-I32 and SWAP-BYTES-I64 are used only on
;;; little-endian machines to debug OSC:VALUE with a
;;; OSC:OUTPUT-STREAM, when the value is int32 or int64.
#+(and sbcl little-endian (or x86 x86-64))
(defun swap-bytes-i32 (integer)
  (declare (type (signed-byte 32) integer))
  (swap-bytes-i32 integer))

#+(and sbcl little-endian x86-64)
(defun swap-bytes-i64 (integer)
  (declare (type (signed-byte 64) integer))
  (swap-bytes-i64 integer))

#+(and little-endian (not (and sbcl x86-64)))
(defmacro force-signed-integer (value bits)
  (let ((index (1- bits))
        (n (gensym)))
    `(let ((,n ,value))
       (declare #.incudine.util:*reduce-warnings*)
       (if (logbitp ,index ,n)
           (dpb ,n (byte ,index 0) -1)
           ,n))))

#+(and little-endian (not (and sbcl (or x86 x86-64))))
(defun swap-bytes-i32 (integer)
  (declare (type (signed-byte 32) integer))
  (incudine-optimize
    (force-signed-integer (ntohl (logand #xffffffff integer)) 32)))

#+(and sbcl little-endian (not x86-64))
(defun swap-bytes-i64 (integer)
  (declare (type (signed-byte 64) integer))
  (incudine-optimize
    (force-signed-integer (incudine.util:reduce-warnings
                            (ntohq (ldb (byte 64 0) integer))) 64)))

#+little-endian
(defmacro maybe-ntoh (stream ntoh-fname value)
  (with-gensyms (x)
    `(let ((,x ,value))
       (if (or (output-stream-p ,stream)
               (stream-buffer-to-index-p ,stream))
           (,ntoh-fname ,x)
           ,x))))

#-little-endian
(defmacro maybe-ntoh (stream ntoh-fname value)
  (declare (ignore stream ntoh-fname))
  value)

(defmacro case-char (keyform &body cases)
  (with-gensyms (k)
    `(let ((,k ,keyform))
       (cond ,@(mapcar (lambda (c)
                         `((= ,k ,(char-code (car c))) ,@(cdr c)))
                       cases)))))

(defun value (stream index)
  "Access a required value of the OSC message stored in the STREAM buffer.
The OSC value is specified by the zero-based INDEX.

Setfable."
  (declare (type stream stream) (type non-negative-fixnum index)
           #.incudine.util:*reduce-warnings*)
  (incudine-optimize
    (when (< index (required-values stream))
      (let* ((i (+ index +data-index-offset+))
             (ptr (arg-pointer stream i))
             (c (typetag-code stream index)))
        (declare (type (unsigned-byte 8) c) (type non-negative-fixnum i))
        (case-char c
          (#\i (maybe-ntoh stream swap-bytes-i32 (cffi:mem-ref ptr :int32)))
          (#\f (if (or (output-stream-p stream)
                       (stream-buffer-to-index-p stream))
                   (get-float ptr (stream-tmp-ptr stream))
                   (cffi:mem-ref ptr :float)))
          (#\h (maybe-ntoh stream swap-bytes-i64 (cffi:mem-ref ptr :int64)))
          ;; No implicit conversion from network to host byte order.
          (#\t (cffi:mem-ref ptr :uint64))
          (#\d (if (or (output-stream-p stream)
                       (stream-buffer-to-index-p stream))
                   (get-float ptr (stream-tmp-ptr stream) t)
                   (cffi:mem-ref ptr :double)))
          (#\s (values (cffi:foreign-string-to-lisp ptr)))
          (#\S (values (cffi:foreign-string-to-lisp ptr)))
          (#\b (get-foreign-array (cffi:inc-pointer ptr 4)
                                  (maybe-ntoh stream ntohl
                                              (cffi:mem-ref ptr :uint32))))
          (#\m (get-midi ptr))
          (#\c (cffi:mem-ref ptr :unsigned-char)))))))

(declaim (inline fix-size))
(defun fix-size (n)
  "Adjust the size N of a zero-padded OSC data. The new size will be a
multiple of four (bytes)."
  (declare (type non-negative-fixnum n))
  (the non-negative-fixnum (- (+ n 4) (logand n 3))))

#+little-endian
(defmacro data-swap-32 (ptr)
  `(setf #1=(cffi:mem-ref ,ptr :uint32) (htonl #1#)))

#+little-endian
(defmacro data-swap-64 (ptr)
  `(setf #1=(cffi:mem-ref ,ptr :uint64) (htonq #1#)))

(declaim (inline set-int32))
(defun set-int32 (ptr value)
  (setf (cffi:mem-ref ptr :uint32)
        #-little-endian
        value
        #+little-endian
        (htonl #+(or x86 x86-64) value
               #-(or x86 x86-64) (logand #xffffffff value))))

(declaim (inline set-int64))
(defun set-int64 (ptr value)
  (setf (cffi:mem-ref ptr :uint64)
        #-little-endian
        value
        #+little-endian
        (htonq #+(or x86 x86-64) value
               #-(or x86 x86-64) (ldb (byte 64 0) value))))

(declaim (inline set-char))
(defun set-char (ptr c)
  (declare (type standard-char c))
  (set-int32 ptr (char-code c)))

(declaim (inline set-float))
(defun set-float (ptr value)
  (declare (type single-float value))
  (prog1 (setf (cffi:mem-ref ptr :float) value)
    #+little-endian (data-swap-32 ptr)))

(declaim (inline set-double))
(defun set-double (ptr value)
  (declare (type double-float value))
  (prog1 (setf (cffi:mem-ref ptr :double) value)
    #+little-endian (data-swap-64 ptr)))

(declaim (inline set-timetag))
(defun set-timetag (ptr value)
  (declare (type (or (unsigned-byte 64) double-float) value))
  ;; No time tag semantics in OSC 1.1. However, the least significant
  ;; bit is reserved to mean "immediately".
  (if (integerp value)
      ;; No implicit conversion from host to network byte order.
      (setf (cffi:mem-ref ptr :int64) value)
      ;; Perhaps sometimes it is useful a double float value.
      (set-double ptr value)))

(defmacro maybe-reserve-space (stream index data-size)
  (with-gensyms (s i)
    `(let ((,s ,stream)
           (,i ,index))
         (setf (stream-message-length ,s)
               (%maybe-reserve-space (stream-message-pointer ,s)
                                     (stream-value-vec-ptr ,s) ,i
                                     ,data-size))
         (when (output-stream-p ,s)
           (setf (stream-bundle-length ,s)
                 (+ (stream-message-length ,s) ,+bundle-reserved-bytes+))
           (set-bundle-first-element-length ,s))
         ,s)))

(defun set-string (stream index string)
  (declare (type stream stream) (type non-negative-fixnum index)
           (type simple-string string))
  (let ((slen (length string)))
    (declare (type non-negative-fixnum slen))
    (maybe-reserve-space stream index (fix-size slen))
    (loop for c across string
          for i of-type non-negative-fixnum from 0 do
            (setf (cffi:mem-aref (arg-pointer stream index) :unsigned-char i)
                  (char-code c))))
  (values))

(defun set-blob (stream index buffer)
  (declare (type stream stream) (type non-negative-fixnum index)
           (type (or (simple-array (unsigned-byte 8) (*))
                     simple-vector)
                 buffer))
  (let ((size (length buffer)))
    (maybe-reserve-space stream index (the non-negative-fixnum
                                           (+ 4 (fix-size (1- size)))))
    (setf (cffi:mem-ref (arg-pointer stream index) :uint32) (htonl size))
    (loop for i below size
          with ptr = (cffi:inc-pointer (arg-pointer stream index) 4)
          do (setf (cffi:mem-aref ptr :unsigned-char i)
                   (if (typep buffer 'simple-vector)
                       (svref buffer i)
                       (aref buffer i)))))
  (values))

(declaim (inline midi))
(defun midi (port-id status data1 data2)
  "Encode a short MIDI message into an OSC value with MIDI message
type tag (m)."
  (declare (type (unsigned-byte 8) port-id status data1 data2))
  #-little-endian
  (logior (ash port-id 24) (ash status 16) (ash data1 8) data2)
  #+little-endian
  (logior (ash data2 24) (ash data1 16) (ash status 8) port-id))

(defun set-midi (stream index value)
  (declare (type stream stream) (type non-negative-fixnum index)
           (type (unsigned-byte 32) value))
  (setf (cffi:mem-ref (arg-pointer stream index) :uint32) value)
  (values))

(defun set-value (stream index value)
  (declare (type stream stream) (type non-negative-fixnum index)
           #.incudine.util:*reduce-warnings*)
  (incudine-optimize
    (let* ((i (+ index +data-index-offset+))
           (ptr (arg-pointer stream i))
           (c (typetag-code stream index)))
      (declare (type (unsigned-byte 8) c) (type non-negative-fixnum i))
      (case-char c
        (#\i (set-int32 ptr value))
        (#\f (set-float ptr value))
        (#\h (set-int64 ptr value))
        (#\d (set-double ptr value))
        (#\s (set-string stream i value))
        (#\S (set-string stream i value))
        (#\t (set-timetag ptr value))
        (#\b (set-blob stream i value))
        (#\m (set-midi stream i value))
        (#\c (set-char ptr value))))
    value))

(defsetf value set-value)

(declaim (inline start-message))
(defun start-message (stream address types)
  "Write the OSC ADDRESS pattern and the OSC TYPES to the STREAM buffer,
then index the required values."
  (let ((typetag-len (length types)))
    (when (> typetag-len (stream-max-values stream))
      (network-error
        "The length of the OSC type tag is ~D but the limit ~%~
         for this OSC:STREAM is ~D"
        typetag-len (stream-max-values stream))))
  (setf (stream-message-length stream)
        (%start-message (stream-message-pointer stream)
                        (stream-buffer-size stream)
                        (stream-value-vec-ptr stream)
                        (stream-type-vec-ptr stream)
                        address types)))

(defmacro message (stream address types &rest values)
  "Send an OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES.

|----------+---------------------------------------------------------|
| OSC type | Lisp type                                               |
|----------+---------------------------------------------------------|
| b        | (or simple-vector (simple-array (unsigned-byte 8) (*))) |
| c        | standard-char                                           |
| d        | double-float                                            |
| f        | single-float                                            |
| F        | no required value                                       |
| h        | (signed-byte 64)                                        |
| i        | (signed-byte 32)                                        |
| I        | no required value                                       |
| m        | (unsigned-byte 32)  i.e. (midifile:message 176 7 42)    |
| N        | no required value                                       |
| s        | string                                                  |
| S        | string                                                  |
| t        | (or (unsigned-byte 64) double-float)                    |
| T        | no required value                                       |
|----------+---------------------------------------------------------|"
  (with-gensyms (s)
    `(let ((,s ,stream))
       (start-message ,s ,address ,types)
       ,@(loop for val in values for i from 0
               collect `(set-value ,s ,i ,val))
       (send ,s))))

(defmacro simple-bundle (stream seconds address types &rest values)
  "Send an OSC message with timestamp SECONDS plus stream latency,
OSC ADDRESS, OSC TYPES and arbitrary VALUES.

The OSC timestamp SECONDS is used with dual meaning: if it is greater
than 63103 seconds (about 17 hours), the time is absolute otherwise it
is added to the current time. 63104 is the offset of the NTP Timestamp
Era 1 (from 8 Feb 2036), so this hack will work for centuries."
  (with-gensyms (s)
    `(let ((,s ,stream))
       (start-message ,s ,address ,types)
       ,@(loop for val in values for i from 0
               collect `(set-value ,s ,i ,val))
       (setf (stream-bundle-length ,s)
             (+ (stream-message-length ,s) ,+bundle-reserved-bytes+))
       (when (and (protocolp ,s :tcp) (null (stream-message-encoding ,s)))
         (setf (cffi:mem-ref (stream-buffer-pointer ,s) :uint32)
               (swap-bytes:htonl (stream-bundle-length ,s))))
       (set-bundle-first-element-length ,s)
       (send-bundle ,s ,seconds))))

(declaim (inline address-pattern))
(defun address-pattern (stream &optional typetag-p)
  "Return the OSC address pattern stored in the STREAM buffer.
If TYPETAG-P is T, the second returned value is the OSC type tag."
  (multiple-value-bind (str len)
      (cffi:foreign-string-to-lisp (message-pointer stream))
    (values str (when typetag-p
                  (cffi:foreign-string-to-lisp
                    (cffi:inc-pointer (message-pointer stream)
                                      (1+ (fix-size len))))))))

(declaim (inline check-pattern))
(defun check-pattern (stream address types)
  "Return T if the OSC address pattern and the OSC type tag stored in the
STREAM buffer are ADDRESS and TYPES."
  (%check-pattern (stream-message-pointer stream) address types))

(defun typetag-to-foreign-type (character)
  (case character
    ((#\b #\m #\t) :pointer)
    (#\c :unsigned-char)
    (#\d :double)
    (#\f :float)
    (#\h :int64)
    (#\i :int32)
    ((#\s #\S) :string)
    (otherwise
     (network-error
       "Known conversion from typetag '~A' to foreign type." character))))

(defun typetag-to-lisp-value (character)
  (case character
    (#\T t)
    (#\I :inf)
    (otherwise nil)))

(declaim (inline typetag-to-foreign-type-p))
(defun typetag-to-foreign-type-p (character)
  (find character "bcdfhimsSt" :test #'char=))

(defun data-getter (stream-var types index)
  (multiple-value-bind (type index)
      (loop for c across types
            for i from 0
            with j = 0
            until (= i index)
            when (typetag-to-foreign-type-p c)
            do (incf j)
            finally (if (typetag-to-foreign-type-p c)
                        (return (values (typetag-to-foreign-type c)
                                        (+ j +data-index-offset+)))
                        (return (values (typetag-to-lisp-value c) -1))))
    (if (minusp index)
        type
        (let ((vec `(arg-pointer ,stream-var ,index)))
          (case type
            (:string `(cffi:foreign-string-to-lisp ,vec))
            (:pointer vec)
            (otherwise `(cffi:mem-ref ,vec ,type)))))))

;;; Length of the first string in the stream buffer.
;;; If OFFSET is non-zero, the first OFFSET bytes are part
;;; of the string.
(declaim (inline stream-buffer-strlen))
(defun stream-buffer-strlen (stream &optional (offset 0))
  (declare (type stream stream) (type non-negative-fixnum offset))
  (loop for i of-type non-negative-fixnum from (+ offset 3) by 4
        when (zerop (cffi:mem-ref (stream-message-pointer stream)
                                  :unsigned-char i))
        return (1+ i)))

(defun index-values (stream &optional force-p swap-p)
  "If necessary or FORCE-P is T, update the foreign pointers to the
required values of the OSC message stored in the STREAM buffer.

If SWAP-P is T, the byte order of the values is reversed on little
endian machine."
  (declare (type stream stream)
           #+little-endian (type boolean swap-p)
           #-little-endian (ignore swap-p))
  (incudine-optimize
    (when (or force-p (stream-buffer-to-index-p stream))
      (let* ((types-start (stream-buffer-strlen stream))
             (data-start (stream-buffer-strlen stream types-start)))
        (declare (type non-negative-fixnum types-start data-start))
        (incf types-start)  ; skip #\,
        (macrolet ((idata (fname)
                     `(,fname (stream-message-pointer stream)
                              (stream-value-vec-ptr stream)
                              (stream-type-vec-ptr stream)
                              types-start data-start)))
          #+little-endian (if swap-p
                              (idata %index-values-le)
                              (idata %index-values))
          #-little-endian (idata %index-values))
        (setf (stream-buffer-to-index-p stream) nil)))
    stream))

(defmacro with-values (value-names (stream types) &body body)
  "Create new symbol macro bindings VALUE-NAMES to the OSC values of
an OSC:STREAM with OSC TYPES during BODY.

Example:

    (incudine.osc:with-values (a b c) (*oscin* \"iii\")
      (msg info \"~D ~D ~D\" a b c))"
  (let ((typetag-len (length types)))
    (with-gensyms (%stream)
      `(let ((,%stream ,stream))
         (when (> ,typetag-len (stream-max-values ,%stream))
           (network-error
             "The length of the OSC type tag is ~D but the limit ~%~
              for this OSC:STREAM is ~D"
             ,typetag-len (stream-max-values ,%stream)))
         (index-values ,%stream nil t)
         (symbol-macrolet
             ,(loop for i below typetag-len
                    for name in value-names
                    collect `(,name ,(data-getter %stream types i)))
           ,@body)))))

(defun buffer-to-octets (stream &optional octets (start 0) end)
  "Return the vector of octets stored in the buffer of the STREAM socket.

Create a new vector if OCTETS is NIL (default).

START and END are the bounding index designators of the vector."
  (declare (type stream stream)
           (type (or (simple-array (unsigned-byte 8) (*)) null) octets)
           (type non-negative-fixnum start)
           (type (or non-negative-fixnum null) end))
  (incudine-optimize
    (let ((len (stream-message-length stream)))
      (unless (zerop len)
        (multiple-value-bind (seq start end)
            (if octets
                (values octets start (min (or end (length octets)) len))
                (values (make-array len :element-type '(unsigned-byte 8)) 0 len))
          (loop for i from start below end do
                  (setf (aref seq i)
                        (cffi:mem-aref (stream-message-pointer stream)
                                       :unsigned-char i)))
          (values seq len))))))

(defun octets-to-buffer (octets stream &optional (start 0) end
                         (osc-message-p t))
  "Copy the OCTETS into the buffer of the STREAM socket.

START and END are the bounding index designators of the vector.

If OSC-MESSAGE-P is T (default), the content of the buffer is an OSC message
and the OSC:STREAM structure is specially updated."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type stream stream) (type non-negative-fixnum start)
           (type (or non-negative-fixnum null) end)
           (type boolean osc-message-p))
  (incudine-optimize
    (let ((len (- (or end (length octets)) start)))
      (when (<= len (stream-buffer-size stream))
        (setf (stream-message-length stream) len)
        (dotimes (i len)
          (setf (cffi:mem-aref (stream-message-pointer stream) :unsigned-char i)
                (aref octets i)))
        (when osc-message-p
          (setf (stream-buffer-to-index-p stream) t)
          (set-bundle-first-element-length stream)
          (setf (stream-bundle-length stream)
                (+ (stream-message-length stream) +bundle-reserved-bytes+)))
        (values stream len)))))
