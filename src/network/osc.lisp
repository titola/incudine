;;; Copyright (c) 2015-2023 Tito Latini
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

(deftype socket ()
  '(unsigned-byte #+(or (not win32) (and win32 (not 64-bit))) 32
                  #+(and win32 64-bit) 64))

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
  (socket-fd -1 :type #+(or (not win32) (and win32 (not 64-bit))) fixnum
                      #+(and win32 64-bit) (signed-byte 64))
  (direction :input :type (member :input :output))
  ;; Foreign buffer used to read/write an OSC packet.
  (buffer-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (buffer-size *buffer-size* :type positive-fixnum)
  (max-values *max-values* :type positive-fixnum)
  ;; T if the pointers to the required OSC values are to update.
  (buffer-to-index-p nil :type boolean)
  ;; Pointer to the memory where the OSC message starts.
  (message-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (message-offset 0 :type non-negative-fixnum)
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
  "Input stream socket type for OSC (Open Sound Control) messages."
  (time-seconds 0d0 :type double-float)
  (time-samples 0d0 :type double-float))

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

(declaim (inline decode-timestamp))
(defun decode-timestamp (sec frac)
  (declare (type (unsigned-byte 32) sec frac))
  (if (and (= frac 1) (= sec 0))
      (incudine:timestamp)
      (+ sec (* frac #.(/ 1d0 #x100000000)))))

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
             (auto-connect-p t) (max-values *max-values*) message-encoding
             (input-stream-constructor #'make-input-stream)
             (output-stream-constructor #'make-output-stream))
  "Create and return a new OSC:STREAM.

The HOST address/name and PORT default to \"localhost\" and 32126
(aka #36ROSE).

DIRECTION is :INPUT (default) or :OUTPUT to return an OSC:INPUT-STREAM
or an OSC:OUTPUT-STREAM respectively.

PROTOCOL is :TCP or :UDP (default). If PROTOCOL is :TCP and
AUTO-CONNECT-P is T (default), try to connect the output stream
socket. See also INCUDINE.NET:CONNECT and INCUDINE.NET:CONNECTED-P.

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
          (%open (init-stream obj latency) auto-connect-p)
        (error (c)
          (cond ((and (eq (type-of c) 'incudine:incudine-network-error)
                      (eq direction :output))
                 (warn "OSC:OPEN connection failed (~A).~%    ~
                        Try again by calling ~A:CONNECT on the output stream."
                       (incudine.external:errno-to-string)
                       (package-name (symbol-package (type-of obj))))
                 obj)
                (t
                 (close obj)
                 (network-error "OSC:OPEN ~A (~A)" c
                   (incudine.external:errno-to-string)))))))))

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
                 (close-socket fd)))))))

(defun %open (stream &optional (connect-p t))
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
                            socket fd :pointer (addrinfo-value stream 'ai-addr)
                            #.+socklen-type+ (addrinfo-value stream 'ai-addrlen)
                            :int))
             (network-error "Failed to assign the address."))
           (when (protocolp stream :tcp)
             (unless (zerop (cffi:foreign-funcall "listen" socket fd
                                                  :int *listen-backlog* :int))
               (network-error "listen call failed."))
             (set-server-fd (stream-fds-ptr stream) fd)))
          ((protocolp stream :tcp)
           (when connect-p
             (unless (zerop (cffi:foreign-funcall "connect"
                              socket fd :pointer (addrinfo-value stream 'ai-addr)
                              #.+socklen-type+ (addrinfo-value stream 'ai-addrlen)
                              :int))
               (network-error "Connection failed:~%~S"
                 (incudine.external:errno-to-string)))))))
  stream)

(declaim (inline open-p))
(defun open-p (stream)
  "Whether STREAM is an open stream socket."
  (not (cffi:null-pointer-p (stream-address-ptr stream))))

(defun close-fd (stream)
  (when (plusp (stream-socket-fd stream))
    (close-socket (stream-socket-fd stream))
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

(defun connected-p (stream)
  "Whether STREAM socket is connected."
  (declare (type incudine.osc:stream stream))
  (if (output-stream-p stream)
      (let ((fd (output-stream-socket-fd stream)))
        (when (> fd 0)
          (cffi:with-foreign-objects ((p :char 128) (len #.+socklen-type+))
            (or (zerop (cffi:foreign-funcall "getpeername"
                         socket fd :pointer p :pointer len :int))
                (unless (= incudine.external::*errno* posix-enotconn)
                  (network-error "getpeername failed (~A)"
                    (incudine.external:errno-to-string)))))))
      (plusp (connections stream))))

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

(define-constant +osc-bundle-magic-number+
  #+little-endian 28548151253492259
  #-little-endian 2549729456036799744)

(defun message-from-bundle-p (stream)
  (declare (type input-stream stream))
  (= (cffi:mem-ref (stream-message-pointer stream) :uint64)
     +osc-bundle-magic-number+))

(defun maybe-update-message-time-seconds (stream)
  (when (and (zerop (input-stream-time-seconds stream))
             (message-from-bundle-p stream))
    (macrolet ((u32p (offset)
                 `(cffi:mem-ref (message-pointer stream) :uint32 ,offset)))
      (setf (input-stream-time-seconds stream)
            (decode-timestamp (swap-bytes:ntohl (u32p 8))
                              (swap-bytes:ntohl (u32p 12))))))
  stream)

(declaim (inline message-time-seconds))
(defun message-time-seconds (stream)
  (input-stream-time-seconds
    (maybe-update-message-time-seconds stream)))

(defun maybe-update-message-time-samples (stream)
  (symbol-macrolet ((time (input-stream-time-samples stream)))
    (when (and (zerop time) (message-from-bundle-p stream))
      (let ((now-seconds (incudine:timestamp))
            (now-samples (incudine:now)))
      (setf time (- (message-time-seconds stream) now-seconds))
      (setf time (if (minusp time)
                     1d0
                     (+ now-samples (* time incudine.util:*sample-rate*)))))))
  stream)

(declaim (inline message-time-samples))
(defun message-time-samples (stream)
  (input-stream-time-samples
    (maybe-update-message-time-samples stream)))

(defun message-time (stream &optional time-unit)
  "If the OSC packet received through the STREAM socket is an OSC bundle,
return the time obtained from the OSC time tag. Otherwise, return zero.

If TIME-UNIT is NIL (default), the time value in samples is suitable as
first argument to INCUDINE:AT

    (at (osc:message-time stream) function ...)

where message-time is the OSC bundle time or zero.

If TIME-UNIT is SECONDS, the bundle time is in universal time format."
  (declare (type input-stream stream)
           (type symbol time-unit))
  ;; STRING= but it is generally a compile-time test.
  (cond ((not time-unit)
         (message-time-samples stream))
        ((string= (symbol-name time-unit) "SECONDS")
         (message-time-seconds stream))))

(declaim (inline reset-time))
(defun reset-time (stream)
  (declare (type input-stream stream))
  (setf (input-stream-time-seconds stream) 0d0
        (input-stream-time-samples stream) 0d0)
  nil)

(declaim (inline update-length-count-prefix))
(defun update-length-count-prefix (ptr value)
  (let ((len (htonl value)))
    (unless (= len #1=(cffi:mem-ref ptr :uint32))
      (setf #1# len))))

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
  #-win32
  (not (getsock-nonblock (stream-socket-fd stream)))
  #+win32
  (zerop (address-value stream 'non-blocking)))

(defun set-block (stream block-p)
  (declare (type stream stream) (type boolean block-p))
  (cond ((zerop (setsock-nonblock (stream-socket-fd stream) (not block-p)))
         #+win32
         (setf (address-value stream 'non-blocking) (if block-p 0 1)))
        (t (network-error "Failed to ~A nonblocking mode."
                          (if block-p "disable" "enable"))))
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
  (when (and (protocolp stream :tcp)
             (not (cffi:null-pointer-p (stream-fds-ptr stream))))
    (let (#+win32 (fd-array (cached-fd-array (stream-fds-ptr stream))))
      (loop for curr = #-win32 -1
                       #+win32  0  ; 0 is the server
                       then fd
            for fd = (cffi:foreign-funcall "osc_next_fd_set"
                       :pointer (stream-fds-ptr stream) :int curr :int)
            while (plusp fd)
              collect #-win32 fd
                      #+win32 (cffi:mem-aref fd-array 'socket fd)))))

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
  (declare (type socket sockfd)
           (type (simple-array (unsigned-byte 8)) octets))
  (incudine-optimize
    (cffi:with-pointer-to-vector-data (buf octets)
      (force-fixnum
        (cffi:foreign-funcall "send" socket sockfd :pointer buf
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
         socket (stream-socket-fd stream)
         :pointer (stream-aux-buffer-pointer stream) :unsigned-long len
         :int flags :pointer (address-value stream 'sockaddr)
         #.+socklen-type+ (address-value stream 'socklen) :int))
      (:tcp
       (cffi:foreign-funcall "send"
         socket (stream-socket-fd stream)
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
      (cond ((<= res 0)
             (setf (stream-message-length stream) 0))
            ((and osc-message-p (message-from-bundle-p stream))
             (setf (cffi:mem-ref (stream-message-length-pointer stream) :uint32)
                   res)
             (setf (stream-message-length stream)
                   (swap-bytes:ntohl
                     (cffi:mem-ref (stream-message-pointer stream) :uint32
                                   (- +bundle-reserved-bytes+ 4))))
             ;; The first OSC message after 20 bytes (8+8+4).
             (setf (stream-message-offset stream) +bundle-reserved-bytes+)
             ;; The call to %%INDEX-VALUES is forced here because INDEX-VALUES
             ;; (the exported function) works without STREAM-MESSAGE-OFFSET.
             ;; It is safer and more efficient for OSC messages (no bundles).
             ;; Note: the byte order of the next message from the bundle
             ;; is swapped on little endian machines, therefore a (probably rare)
             ;; copy of the received OSC bundle requires a swap of the read OSC
             ;; messages.
             (%%index-values stream t t +bundle-reserved-bytes+)
             (setf res (stream-message-length stream)))
            (t
             (setf (stream-message-length stream) res)
             (when osc-message-p
               (setf (stream-buffer-to-index-p stream) t))))
      (when (slip-encoding-p stream)
        (slip-decode stream))
      res)))

(defun receive (stream &optional (flags +default-msg-flags+))
  "Store the received OSC packet into the STREAM buffer.

If the packet is an OSC bundle, the current OSC message is the
first message of that bundle. The other messages are queued for
the successive calls to OSC:RECEIVE. See OSC:MESSAGE-TIME for
the time value obtained from the OSC time tag.

FLAGS defaults to NET:+DEFAULT-MSG-FLAGS+. See the manual pages
for recv and recvfrom for details on the FLAGS argument.

Return the number of bytes of the OSC message."
  (or (and (message-from-bundle-p stream)
           (let ((offset (+ (stream-message-offset stream)
                            ;; Length of the last message from this bundle.
                            (message-length stream))))
             (cond ((>= offset
                        (cffi:mem-ref (stream-message-length-pointer stream)
                                      :uint32))
                    (setf (stream-message-offset stream) 0)
                    ;; Alter the magic number otherwise MESSAGE-FROM-BUNDLE-P
                    ;; still returns T.
                    (setf (cffi:mem-ref (stream-message-pointer stream) :char) 0)
                    (reset-time stream)
                    nil)
                   (t
                    (setf (stream-message-length stream)
                          (swap-bytes:ntohl
                            (cffi:mem-ref (stream-message-pointer stream)
                                          :uint32 offset)))
                    (setf (stream-message-offset stream) (+ offset 4))
                    (%%index-values stream t t (stream-message-offset stream))
                    (stream-message-length stream)))))
      (%receive stream flags t)))

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
           (update-length-count-prefix (stream-message-length-pointer stream)
                                       (stream-message-length stream))
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
  (with-gensyms (s i len)
    `(let* ((,s ,stream)
            (,i ,index)
            (,len (%maybe-reserve-space
                    (stream-message-pointer ,s)
                    (stream-value-vec-ptr ,s)
                    ,i ,data-size)))
       (unless (= (stream-message-length ,s) ,len)
         (setf (stream-message-length ,s) ,len)
         (when (output-stream-p ,s)
           (setf (stream-bundle-length ,s)
                 (+ (stream-message-length ,s) ,+bundle-reserved-bytes+))
           (when (and (protocolp ,s :tcp)
                      (null (stream-message-encoding ,s)))
             (setf (cffi:mem-ref (stream-buffer-pointer ,s) :uint32)
                   (htonl (stream-bundle-length ,s))))
           (set-bundle-first-element-length ,s)))
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

(defun start-message (stream address types)
  "Write the OSC ADDRESS pattern and the OSC TYPES to the STREAM buffer,
then index the required values."
  (declare (type stream stream)
           (type string address types))
  ;; Generally START-MESSAGE works with output streams.
  (when (and (incudine.osc:input-stream-p stream)
             (message-from-bundle-p stream))
    (reset-time stream))
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
       ,(if (or values
                (not (stringp types))
                (not (required-values-p types)))
            `(send ,s)
            0))))

(defun send-bundle-p (types values)
  (and (not (and (eq (first values) :send-p)
                 (rest values)
                 (not (second values))))
       (or values
           (not (stringp types))
           (not (required-values-p types)))))

(defmacro simple-bundle (stream seconds address types &rest values)
  "Send an OSC message with timestamp SECONDS plus stream latency,
OSC ADDRESS, OSC TYPES and arbitrary VALUES.

If VALUES is :SEND-P NIL, or there are no VALUES and the string TYPES
implies some required values, prepare the OSC message but don't send it.

The OSC timestamp SECONDS is used with dual meaning: if it is greater
than 63103 seconds (about 17 hours), the time is absolute otherwise it
is added to the current time. 63104 is the offset of the NTP Timestamp
Era 1 (from 8 Feb 2036), so this hack will work for centuries.

Example:

    (osc:simple-bundle stream .5 \"/osc/test\" \"iii\" 1 2 3)
    ;; => 52

is equivalent to

    (osc:simple-bundle stream 0 \"/osc/test\" \"iii\")
    ;; => 0

    (setf (osc:value stream 0) 1)
    (setf (osc:value stream 1) 2)
    (setf (osc:value stream 2) 3)
    (osc:send-bundle stream .5)
    ;; => 52"
  (with-gensyms (s)
    `(let ((,s ,stream))
       (start-message ,s ,address ,types)
       ,@(unless (keywordp (first values))
           (loop for val in values for i from 0
                 collect `(set-value ,s ,i ,val)))
       (setf (stream-bundle-length ,s)
             (+ (stream-message-length ,s) ,+bundle-reserved-bytes+))
       (when (and (protocolp ,s :tcp) (null (stream-message-encoding ,s)))
         (setf (cffi:mem-ref (stream-buffer-pointer ,s) :uint32)
               (swap-bytes:htonl (stream-bundle-length ,s))))
       (set-bundle-first-element-length ,s)
       ,(if (send-bundle-p types values)
            `(send-bundle ,s ,seconds)
            0))))

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

(declaim (inline current-message-pointer))
(defun current-message-pointer (stream)
  (cffi:inc-pointer (stream-message-pointer stream)
                    (stream-message-offset stream)))

(declaim (inline check-pattern))
(defun check-pattern (stream address types)
  "Return T if the OSC address pattern and the OSC type tag stored in the
STREAM buffer are ADDRESS and TYPES."
  (%check-pattern (current-message-pointer stream) address types))

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

(defun required-values-p (types)
  (some #'typetag-to-foreign-type-p types))

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

(defun %%index-values (stream force-p swap-p message-offset)
  (declare (type stream stream)
           #+little-endian (type boolean swap-p)
           #-little-endian (ignore swap-p)
           (type non-negative-fixnum message-offset))
  (incudine-optimize
    (when (or force-p (stream-buffer-to-index-p stream))
      (let* ((types-start (stream-buffer-strlen stream message-offset))
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

(defun index-values (stream &optional force-p swap-p)
  "If necessary or FORCE-P is T, update the foreign pointers to the
required values of the OSC message stored in the STREAM buffer.

If SWAP-P is T, the byte order of the values is reversed on little
endian machine."
  (declare (type stream stream)
           #+little-endian (type boolean swap-p)
           #-little-endian (ignore swap-p))
  (%%index-values stream force-p swap-p 0))

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
          (loop for i from start below end
                with ptr = (if (input-stream-p stream)
                               (current-message-pointer stream)
                               (stream-message-pointer stream))
                do (setf (aref seq i)
                         (cffi:mem-aref ptr :unsigned-char i)))
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
