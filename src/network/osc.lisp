;;; Copyright (c) 2015-2017 Tito Latini
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

(defvar *buffer-size* (if (boundp 'incudine.config::*osc-buffer-size*)
                          incudine.config::*osc-buffer-size*
                          1000)
  "Size of the foreign buffer used to read/write a OSC packet.")
(declaim (type positive-fixnum *buffer-size*))

(defvar *max-values* (if (boundp 'incudine.config::*osc-max-values*)
                         incudine.config::*osc-max-values*
                         50)
  "Maximum number of the required values in a OSC message.")
(declaim (type positive-fixnum *max-values*))

(defvar *addrinfo-hints-flags*
  (if (boundp 'incudine.config::*addrinfo-hints-flags*)
      incudine.config::*addrinfo-hints-flags*
      0)
  "addrinfo-flags for the argument 'hints' of the c-call getaddrinfo.
The default is 0.")
(declaim (type non-negative-fixnum *addrinfo-hints-flags*))

(defvar *before-close-hook* nil
  "List of the functions called before to close a OSC:STREAM.
The argument of a function is the OSC:STREAM to close.")

(defvar *listen-backlog* 8)
(declaim (type (unsigned-byte 8) *listen-backlog*))

(defstruct (stream (:constructor %make-stream)
                   (:print-function print-stream)
                   (:copier nil))
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
  ;; Foreign buffer used to read/write a OSC packet.
  (buffer-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (buffer-size *buffer-size* :type positive-fixnum)
  (max-values *max-values* :type positive-fixnum)
  ;; T if the pointers to the required OSC values are to update.
  (buffer-to-index-p nil :type boolean)
  ;; Pointer to the memory where the OSC message starts.
  (message-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (message-length 0 :type non-negative-fixnum)
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

(defstruct (input-stream (:include stream)))

(defstruct (output-stream (:include stream)))

(defun print-stream (obj stream depth)
  (declare (ignore depth))
  (format stream "#<OSC:~A-STREAM ~S ~S ~D>"
          (stream-direction obj) (stream-protocol obj)
          (stream-host obj) (stream-port obj)))

(declaim (inline protocolp))
(defun protocolp (stream protocol)
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

(declaim (inline free-pointer))
(defun free-pointer (ptr)
  (unless (cffi:null-pointer-p ptr) (cffi:foreign-free ptr)))

(define-constant +zero-padding-bytes+ 4)
(define-constant +temp-space-bytes+ 8)
(define-constant +int-size+ (cffi:foreign-type-size :int))

(defun open (&key (host "localhost") (port #36ROSE) (direction :input)
             (protocol :udp) (buffer-size *buffer-size*)
             (max-values *max-values*) message-encoding
             (input-stream-constructor #'make-input-stream)
             (output-stream-constructor #'make-output-stream))
  (declare (type (member :input :output) direction)
           (type (member :udp :tcp) protocol) (type simple-string host)
           (type (unsigned-byte 16) port)
           (type positive-fixnum buffer-size max-values)
           (type (member nil :slip) message-encoding))
  (cffi:with-foreign-object (address-ptr :pointer)
    (unless (zerop (new-address address-ptr host port (eq protocol :udp)
                                (eq direction :input) *addrinfo-hints-flags*))
      (incudine::foreign-alloc-error "OSC address allocation."))
    (let* ((buffer-size (max 250 (* max-values 20) buffer-size))
           (address (cffi:mem-ref address-ptr :pointer))
           ;; Add 4 bytes with zero, so the loop in STREAM-BUFFER-STRLEN
           ;; never fails if the message is wrong and without zeroes.
           ;; Also reserve the space to store a temporary value (see GET-FLOAT
           ;; for little-endian) and a file descriptor.
           (buf-pad (+ +zero-padding-bytes+ +temp-space-bytes+
                       (if (and (eq direction :input) (eq protocol :tcp))
                           0 +int-size+)))
           (buf-ptr (cffi:foreign-alloc :char :count (+ buffer-size buf-pad)
                                        :initial-element 0))
           (aux-ptr (if (eq message-encoding :slip)
                        (cffi:foreign-alloc :char :count (* 2 buffer-size)
                                            :initial-element 0)
                        (cffi:null-pointer)))
           (value-vec-ptr (cffi:foreign-alloc :pointer :count (+ max-values 3)))
           (type-vec-ptr (cffi:foreign-alloc :char :count (+ max-values 1)
                                             :initial-element 0))
           (fds-ptr (alloc-fds buf-ptr (+ buffer-size buf-pad) direction
                               protocol))
           (obj (funcall (if (eq direction :input)
                             input-stream-constructor
                             output-stream-constructor)
                         :host host :port port :protocol protocol
                         :address-ptr address :fds-ptr fds-ptr
                         :addrinfo-ptr (cffi:mem-ref address :pointer)
                         :direction direction :buffer-pointer buf-ptr
                         :message-pointer (if (and (eq protocol :tcp)
                                                   (null message-encoding))
                                              ;; 4 bytes reserved for the size
                                              ;; of the message.
                                              (cffi:inc-pointer buf-ptr 4)
                                              buf-ptr)
                         :message-encoding message-encoding
                         :buffer-size buffer-size :max-values max-values
                         :value-vec-ptr value-vec-ptr
                         :type-vec-ptr type-vec-ptr :aux-buffer-pointer aux-ptr
                         :tmp-ptr (cffi:inc-pointer buf-ptr
                                                    (+ buffer-size
                                                       +zero-padding-bytes+)))))
      (tg:finalize obj (lambda ()
                         (free-fds fds-ptr direction protocol)
                         (free-address address)
                         (mapc #'free-pointer
                               (list buf-ptr aux-ptr value-vec-ptr
                                     type-vec-ptr))))
      (handler-case
          (%open obj)
        (error (c)
          (close obj)
          (incudine.util:msg error "OSC:OPEN ~A (~A)" c
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
      (incudine::network-error "Failed to create the socket."))
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
             (incudine::network-error "Failed to assign the address."))
           (when (protocolp stream :tcp)
             (unless (zerop (cffi:foreign-funcall "listen" :int fd
                                                  :int *listen-backlog* :int))
               (incudine::network-error "listen call failed."))
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
        (tg:cancel-finalization stream)
        (setf (stream-address-ptr stream) (cffi:null-pointer))
        (setf (stream-addrinfo-ptr stream) (cffi:null-pointer))
        (setf (stream-message-pointer stream) (cffi:null-pointer))
        (setf (stream-tmp-ptr stream) (cffi:null-pointer)))))
  (values))

(defun reject (stream)
  "Close the STREAM socket and create a new socket."
  (declare (type input-stream stream))
  (when (open-p stream)
    (dolist (fn *before-close-hook*)
      (funcall (the function fn) stream))
    (%open (close-fd stream))))

(defun close-connections (stream)
  (unless (or (cffi:null-pointer-p (stream-fds-ptr stream))
              (not (protocolp stream :tcp)))
    (%close-connections (stream-fds-ptr stream))))

(defun connect (stream)
  "Close the STREAM socket, create a new socket and try to connect it."
  (declare (type output-stream stream))
  (when (and (protocolp stream :tcp) (open-p stream))
    (%open (close-fd stream))))

(defmacro with-stream ((stream &rest options) &body body)
  `(let ((,stream (open ,@options)))
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

(defsetf message-encoding set-message-encoding)

(defun broadcast (stream)
  (declare (type stream stream))
  (plusp (getsock-broadcast (stream-socket-fd stream))))

(defun set-broadcast (stream value)
  (declare (type stream stream) (type boolean value))
  (zerop (setsock-broadcast (stream-socket-fd stream)
                            (stream-addrinfo-ptr stream) value)))

(defsetf broadcast set-broadcast)

(defun block-p (stream)
  (not (getsock-nonblock (stream-socket-fd stream))))

(defun set-block (stream block-p)
  (declare (type stream stream) (type boolean block-p))
  (assert (zerop (setsock-nonblock (stream-socket-fd stream) (not block-p))))
  block-p)

(defsetf block-p set-block)

(defmacro without-block ((var input-stream) &body body)
  (with-gensyms (without-block-body)
    `(let ((,var ,input-stream))
       (flet ((,without-block-body () ,@body))
         (if (block-p ,var)
             (unwind-protect
                  (progn (setf (block-p ,var) nil) (,without-block-body))
               (setf (block-p ,var) t))
             (,without-block-body))))))

(defun connections (stream)
  "Return the number of the accepted connections."
  (declare (type input-stream stream))
  (if (and (protocolp stream :tcp)
           (not (cffi:null-pointer-p (stream-fds-ptr stream))))
      (cffi:foreign-funcall "osc_connections"
                            :pointer (stream-fds-ptr stream) :int)
      0))

(defun connections-fd (stream)
  "Return the file descriptors for the accepted connections."
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
  "Return the file descriptor for the accepted connection used to get
the last received message."
  (cffi:foreign-funcall "osc_lastfd" :pointer (stream-fds-ptr stream) :int))

(defun socket-send (sockfd octets &optional (flags +default-msg-flags+))
  "Send OCTETS on a socket."
  (declare (type positive-fixnum sockfd)
           (type (simple-array (unsigned-byte 8)) octets)
           (optimize speed (safety 0)))
  (cffi:with-pointer-to-vector-data (buf octets)
    (force-fixnum
      (cffi:foreign-funcall "send" :int sockfd :pointer buf
                            :unsigned-int (length octets) :int flags :int))))

;;; SLIP method for framing packets (RFC 1055).
(declaim (inline slip-encode))
(defun slip-encode (stream)
  "Serial Line IP encoding."
  (%slip-encode (stream-message-pointer stream)
                (stream-aux-buffer-pointer stream)
                (stream-message-length stream)))

(declaim (inline slip-decode))
(defun slip-decode (stream)
  "Serial Line IP decoding."
  (setf (stream-message-length stream)
        (%slip-decode (stream-message-pointer stream)
                      (stream-message-length stream))))

(defun send-slip-message (stream &optional (flags +default-msg-flags+))
  (declare (type stream stream) (type non-negative-fixnum flags))
  (let ((len (slip-encode stream)))
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
            (values (stream-buffer-pointer stream)
                    (stream-buffer-size stream)
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
  (declare (type input-stream stream) (type non-negative-fixnum flags)
           (type boolean osc-message-p)
           (optimize speed))
  (let ((res (if (protocolp stream :tcp)
                 (net-recv stream flags osc-message-p)
                 (force-fixnum
                   (%recvfrom (stream-socket-fd stream)
                     (stream-message-pointer stream) (stream-buffer-size stream)
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
    res))

(declaim (inline receive))
(defun receive (stream &optional (flags +default-msg-flags+))
  "Store the received OSC message into the STREAM buffer.
FLAGS are used with the recv and recvfrom calls."
  (%receive stream flags t))

(defun send (stream &optional (flags +default-msg-flags+))
  "Send the OSC message stored into the STREAM buffer.
FLAGS are used with the send and sendto calls."
  (declare (type stream stream) (type non-negative-fixnum flags)
           (optimize speed) #.incudine.util:*reduce-warnings*)
  (cond ((slip-encoding-p stream)
         (send-slip-message stream flags))
        ((protocolp stream :udp)
         (%sendto (stream-socket-fd stream) (stream-message-pointer stream)
                  (stream-message-length stream) flags
                  (address-value stream 'sockaddr)
                  (address-value stream 'socklen)))
        (t
         ;; OSC 1.0 spec: length-count prefix on the start of the packet.
         (setf (cffi:mem-ref (stream-buffer-pointer stream) :uint32)
               (htonl (stream-message-length stream)))
         (%send (stream-socket-fd stream) (stream-buffer-pointer stream)
                (+ 4 (stream-message-length stream)) flags))))

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

(defmacro value-pointer (stream index)
  `(arg-pointer ,stream (+ ,index ,+data-index-offset+)))

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

#+(and sbcl little-endian x86)
(defun swap-bytes-i64 (integer)
  (declare (type (signed-byte 64) integer)
           (optimize speed (safety 0)))
  (let ((n (ntohq integer)))
    (declare (type (unsigned-byte 64) n))
    (incudine.util:reduce-warnings
      (if (logbitp 63 integer)
          (dpb n (byte 63 0) -1)
          n))))

#+(and little-endian (not (and sbcl (or x86 x86-64))))
(progn
  (declaim (inline swap-bytes-i32 swap-bytes-i64))
  (defun swap-bytes-i32 (integer)
    (ntohl integer))

  (defun swap-bytes-i64 (integer)
    (ntohq integer)))

#+little-endian
(defmacro maybe-ntoh (stream ntoh-fname value)
  (with-gensyms (x)
    `(let ((,x ,value))
       (if (or (output-stream-p ,stream)
               (stream-buffer-to-index-p ,stream))
           (,ntoh-fname ,x)
           ,x))))

#-little-endian
(defmacro maybe-ntoh (stream ntoh-fname size value)
  (declare (ignore stream ntoh-fname size))
  value)

(defmacro case-char (keyform &body cases)
  (with-gensyms (k)
    `(let ((,k ,keyform))
       (cond ,@(mapcar (lambda (c)
                         `((= ,k ,(char-code (car c))) ,@(cdr c)))
                       cases)))))

(defun value (stream index)
  "Access a required value of the OSC message stored in the STREAM buffer."
  (declare (type stream stream) (type non-negative-fixnum index)
           (optimize speed (safety 0))
           #.incudine.util:*reduce-warnings*)
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
        (#\c (cffi:mem-ref ptr :unsigned-char))))))

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
  (setf (cffi:mem-ref ptr :uint32) (htonl value)))

(declaim (inline set-int64))
(defun set-int64 (ptr value)
  (setf (cffi:mem-ref ptr :uint64) (htonq value)))

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
                                     ,data-size)))))

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
           (optimize speed (safety 0))
           #.incudine.util:*reduce-warnings*)
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
  value)

(defsetf value set-value)

(declaim (inline start-message))
(defun start-message (stream address types)
  "Write the OSC ADDRESS pattern and the OSC TYPES on the STREAM buffer,
then index the required values."
  (let ((typetag-len (length types)))
    (when (> typetag-len (stream-max-values stream))
      (incudine::network-error
        "The length of the OSC type tag is ~D but the limit ~%for this OSC:STREAM is ~D"
        typetag-len (stream-max-values stream))))
  (setf (stream-message-length stream)
        (%start-message (stream-message-pointer stream)
                        (stream-buffer-size stream)
                        (stream-value-vec-ptr stream)
                        (stream-type-vec-ptr stream)
                        address types)))

(defmacro message (stream address types &rest values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES."
  (with-gensyms (s)
    `(let ((,s ,stream))
       (start-message ,s ,address ,types)
       ,@(loop for val in values for i from 0
               collect `(set-value ,s ,i ,val))
       (send ,s))))

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
     (incudine::network-error
       "Known conversion from typetag '~A' to foreign type." character))))

(defun typetag-to-lisp-value (character)
  (case character
    (#\T t)
    (#\I :inf)
    (otherwise nil)))

(declaim (inline typetag-to-foreign-type-p))
(defun typetag-to-foreign-type-p (character)
  (member character '(#\b #\c #\d #\f #\h #\i #\m #\s #\S #\t) :test #'char=))

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
  "If it is necessary, update the foreign pointers to the the required
values of the OSC message stored in the STREAM buffer.
If FORCE-P is T, force the update. If SWAP-P is T, the byte order of
the values is reversed on little endian machine."
  (declare (type stream stream)
           #+little-endian (type boolean swap-p)
           #-little-endian (ignore swap-p)
           (optimize speed (safety 0)))
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
  stream)

(defmacro with-values (value-names (stream-var types) &body body)
  (let ((typetag-len (length types)))
    (with-gensyms (stream)
      `(let ((,stream ,stream-var))
         (when (> ,typetag-len (stream-max-values ,stream))
           (incudine::network-error
             "The length of the OSC type tag is ~D but the limit ~%for this OSC:STREAM is ~D"
             ,typetag-len (stream-max-values ,stream)))
         (index-values ,stream nil t)
         (symbol-macrolet
             ,(loop for i below typetag-len
                    for name in value-names
                    collect `(,name ,(data-getter stream types i)))
           ,@body)))))

(defun buffer-to-octets (stream &optional octets (start 0) end)
  (declare (type stream stream)
           (type (or (simple-array (unsigned-byte 8) (*)) null) octets)
           (type non-negative-fixnum start)
           (type (or non-negative-fixnum null) end)
           (optimize speed (safety 0)))
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
        (values seq len)))))

(defun octets-to-buffer (octets stream &optional (start 0) end
                         (osc-message-p t))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type stream stream) (type non-negative-fixnum start)
           (type (or non-negative-fixnum null) end)
           (type boolean osc-message-p)
           (optimize speed (safety 0)))
  (let ((len (- (or end (length octets)) start)))
    (when (<= len (stream-buffer-size stream))
      (setf (stream-message-length stream) len)
      (dotimes (i len)
        (setf (cffi:mem-aref (stream-message-pointer stream) :unsigned-char i)
              (aref octets i)))
      (when osc-message-p
        (setf (stream-buffer-to-index-p stream) t))
      (values stream len))))
