;;; Copyright (c) 2016-2020 Tito Latini
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

(defpackage :jackmidi
  (:use :cl)
  (:shadow #:open #:close #:read #:write #:stream #:input-stream-p
           #:output-stream-p)
  (:import-from #:alexandria #:define-constant #:with-gensyms #:positive-fixnum
                #:non-negative-fixnum)
  (:import-from #:incudine.external #:rt-client)
  (:import-from #:incudine.util #:rt-eval #:msg #:nrt-msg #:u8-ref
                #:incudine-optimize)
  (:import-from #:incudine #:free #:free-p
                #:incudine-finalize #:incudine-cancel-finalization)
  (:export #:data #:event-buffer #:open #:close #:stream
           #:input-stream #:input-stream-p #:output-stream #:output-stream-p
           #:input-stream-sysex-timestamp #:input-stream-sysex-size
           #:input-stream-sysex-pointer #:input-stream-sysex-octets
           #:port-name #:get-stream-by-name #:all-streams
           #:message #:decode-message #:read #:write-short #:write
           #:foreign-read #:foreign-write
           #:sysex-message-p #:make-event-buffer #:with-event-buffer
           #:doevent #:with-receiver))

(in-package :jackmidi)

(deftype data ()
  "Type designator for a vector of octets."
  '(simple-array (unsigned-byte 8) (*)))

(defstruct (stream (:copier nil))
  "Jack MIDI stream type."
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (direction :closed :type (member :input :output :closed))
  (port-name "" :type string))

(defstruct (input-stream (:include stream) (:copier nil)
                         (:constructor %make-input-stream))
  "Jack MIDI input stream type."
  ;; Pointer to the event that contains the received SysEx message.
  (sysex-pointer* (cffi:null-pointer) :type cffi:foreign-pointer))

(defun make-input-stream (port-name)
  (declare (type string port-name))
  (let ((ptr (new-stream-pointer t)))
    (when (cffi:null-pointer-p ptr)
      (incudine::foreign-alloc-error "Jack MIDI pointer allocation."))
    (let* ((sysex-ptr (cffi:foreign-alloc :pointer))
           (obj (handler-case
                    (%make-input-stream :pointer ptr :direction :input
                      :port-name port-name :sysex-pointer* sysex-ptr)
                  (condition (c)
                    (cffi:foreign-free ptr)
                    (cffi:foreign-free sysex-ptr)
                    (error c)))))
      (when (input-stream-p obj)
        (incudine.util::finalize obj (lambda ()
                                       (cffi:foreign-free ptr)
                                       (cffi:foreign-free sysex-ptr)))
        (setf (cffi:mem-ref (input-stream-sysex-pointer* obj) :pointer)
              (cffi:null-pointer))
        obj))))

(defstruct (output-stream (:include stream) (:copier nil)
                          (:constructor %make-output-stream))
  "Jack MIDI output stream type.")

(setf
  (documentation 'input-stream-p 'function)
  "Return T if object is of type JACKMIDI:INPUT-STREAM."
  (documentation 'output-stream-p 'function)
  "Return T if object is of type JACKMIDI:OUTPUT-STREAM.")

(defun make-output-stream (port-name)
  (declare (type string port-name))
  (let ((ptr (new-stream-pointer t)))
    (when (cffi:null-pointer-p ptr)
      (incudine::foreign-alloc-error "Jack MIDI pointer allocation."))
    (let ((obj (handler-case
                   (%make-output-stream :pointer ptr :direction :output
                                        :port-name port-name)
                 (condition (c)
                   (cffi:foreign-free ptr)
                   (error c)))))
      (when (output-stream-p obj)
        (incudine.util::finalize obj (lambda () (cffi:foreign-free ptr)))
        obj))))

(defmethod print-object ((obj stream) stream)
  (let ((port-name (stream-port-name obj)))
    (format stream "#<JACKMIDI:~A-STREAM~:[ ~S~;~]>"
            (stream-direction obj) (zerop (length port-name))
            port-name)))

;;; Ordered list of opened Jack MIDI streams.
(defvar *streams* nil)
(declaim (type list *streams*))

;;; Ordered vector of opened Jack MIDI input streams.
(defvar *input-streams* (vector))
(declaim (type simple-vector *input-streams*))

;;; Temporary Jack MIDI input streams used in non-realtime.
(defvar *pending-input-streams* (vector))
(declaim (type simple-vector *pending-input-streams*))

;;; Spinlock to update *PENDING-INPUT-STREAMS* in nrt-thread.
(defvar *input-streams-spinlock*
  (incudine.util:make-spinlock "jackmidi pending-input-streams"))
(declaim (type incudine.util:spinlock *input-streams-spinlock*))

;;; Ordered vector of opened Jack MIDI output streams.
(defvar *output-streams* (vector))
(declaim (type simple-vector *output-streams*))

;;; Temporary Jack MIDI output streams used in non-realtime.
(defvar *pending-output-streams* (vector))
(declaim (type simple-vector *pending-output-streams*))

;;; Spinlock to update *PENDING-OUTPUT-STREAMS* in nrt-thread.
(defvar *output-streams-spinlock*
  (incudine.util:make-spinlock "jackmidi pending-output-streams"))
(declaim (type incudine.util:spinlock *output-streams-spinlock*))

(declaim (inline jack-stopped-p))
(defun jack-stopped-p ()
  (cffi:null-pointer-p (rt-client)))

(declaim (inline stream-port-pointer))
(defun stream-port-pointer (stream)
  (cffi:mem-ref (stream-pointer stream) :pointer))

(declaim (inline set-stream-port-pointer))
(defun set-stream-port-pointer (stream pointer)
  (setf (cffi:mem-ref (stream-pointer stream) :pointer) pointer))

(defsetf stream-port-pointer set-stream-port-pointer)

(declaim (inline null-port-pointer-p))
(defun null-port-pointer-p (stream)
  (cffi:null-pointer-p (stream-port-pointer stream)))

(declaim (inline nullify-port-buffer))
(defun nullify-port-buffer (stream)
  (setf (cffi:mem-aref (stream-pointer stream) :pointer 1) (cffi:null-pointer)))

(declaim (inline port-name))
(defun port-name (stream)
  "Return the port name of a Jack MIDI stream."
  (stream-port-name stream))

(defun set-port-name (stream name)
  (when (jack-stopped-p)
    (if (get-stream-by-name name)
        (incudine:incudine-error "Jack MIDI port name ~S is used" name)
        (setf (stream-port-name stream) name))))

(defsetf port-name set-port-name)

(defun default-port-name (input-p)
  (if input-p "midi_in" "midi_out"))

(declaim (inline process))
(cffi:defcfun ("jm_process" process) :void
  (frames :uint32))

(cffi:defcfun ("jm_clear_cached_events" clear-cached-events) :void)

(cffi:defcfun ("jm_read_cached_midi_inputs" read-cached-inputs) :int
  (frames :uint32))

(cffi:defcfun ("jm_write_cached_midi_outputs" write-cached-outputs) :int)

(cffi:defcfun ("jm_alloc_data" new-stream-pointer) :pointer
  (input-p :boolean))

(cffi:defcfun ("jm_free_data" free-stream-pointer) :void
  (ptr :pointer)
  (input-p :boolean))

(cffi:defcfun "jm_copy_data_vec" :pointer
  (input-p :boolean))

(cffi:defcfun "jm_free_data_vec" :void
  (data-vec :pointer))

(cffi:defcfun "jm_port_register" :pointer
  (port-name :string)
  (input-p :boolean))

(declaim (inline jm-append-pending-data))
(cffi:defcfun "jm_append_pending_data" :int
  (data :pointer)
  (input-p :boolean))

(declaim (inline jm-delete-from-pending-data))
(cffi:defcfun "jm_delete_from_pending_data" :void
  (data :pointer)
  (input-p :boolean))

(declaim (inline foreign-write-short))
(defun foreign-write-short (stream msg size)
  (logand (cffi:foreign-funcall "jm_write_short" :pointer (stream-pointer stream)
                                :uint32 msg :unsigned-int size :int)
          #xffffff))

(declaim (inline foreign-write))
(defun foreign-write (stream buffer-pointer buffer-size)
  "Write BUFFER-SIZE bytes of a MIDI message stored into a foreign
array pointed to by BUFFER-POINTER to the Jack MIDI output STREAM."
  (logand (cffi:foreign-funcall "jm_write" :pointer (stream-pointer stream)
            :pointer buffer-pointer :unsigned-int buffer-size :int)
          #xffffff))

(declaim (inline foreign-read))
(defun foreign-read (stream buffer-pointer buffer-size)
    "Read the events received from a Jack MIDI input STREAM into a
foreign array of size BUFFER-SIZE bytes pointed to by BUFFER-POINTER.
Return the number of events read.

The header of the event is 12 bytes long: a timestamp (foreign double float)
and the length of the MIDI message (foreign uint32).

The MIDI messages are aligned to four bytes."
  (logand (cffi:foreign-funcall "jm_read" :pointer (stream-pointer stream)
            :pointer buffer-pointer :unsigned-int buffer-size :int)
          #xffffff))

(declaim (inline %waiting-for))
(cffi:defcfun ("jm_waiting_for" %waiting-for) :void
  (stream-pointer :pointer))

(declaim (inline force-cond-signal))
(cffi:defcfun ("jm_force_cond_signal" force-cond-signal) :void
  (stream-pointer :pointer))

(declaim (inline %flush-pending))
(cffi:defcfun ("jm_flush_pending" %flush-pending) :void
  (stream-pointer :pointer))

(declaim (inline set-freqs-from-midi-data-format))
(cffi:defcfun "set_freqs_from_midi_data_format" :int
  (freq-buffer :pointer)
  (midi-freq-buffer :pointer)
  (size :unsigned-int))

(defstruct (foreign-data-vector (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (stream-direction :input :type (member :input :output)))

(defun new-foreign-data-vector (input-p)
  (let ((ptr (jm-copy-data-vec input-p)))
    (when (cffi:null-pointer-p ptr)
      (incudine::foreign-alloc-error "Jack MIDI pointer allocation."))
    (let ((obj (make-foreign-data-vector :pointer ptr
                 :stream-direction (if input-p :input :output))))
      (incudine.util::finalize obj (lambda () (jm-free-data-vec ptr)))
      obj)))

(defun free-foreign-data-vector (obj)
  (declare (type foreign-data-vector obj))
  (unless (cffi:null-pointer-p (foreign-data-vector-pointer obj))
    (jm-free-data-vec (foreign-data-vector-pointer obj))
    (incudine.util::cancel-finalization obj)
    (setf (foreign-data-vector-pointer obj) (cffi:null-pointer)))
  (values))

(declaim (inline %jm-update-data))
(cffi:defcfun ("jm_update_data" %jm-update-data) :void
  (data-vec :pointer)
  (input-p :boolean))

(declaim (inline jm-update-data))
(defun jm-update-data (obj input-p)
  (%jm-update-data (foreign-data-vector-pointer obj) input-p))

(defun port-register (input-p &optional port-name)
  (declare (type boolean input-p) (type (or string null) port-name))
  (jm-port-register (or port-name (default-port-name input-p)) input-p))

(defun port-unregister (port)
  (declare (type (or cffi:foreign-pointer string) port))
  (let ((client (rt-client)))
    (unless (cffi:null-pointer-p client)
      (let ((port (if (stringp port)
                      (get-port-by-name port)
                      port)))
        (and (not (cffi:null-pointer-p port))
             (zerop (cffi:foreign-funcall "jack_port_unregister" :pointer client
                                          :pointer port :int)))))))

(defun get-port-by-name (name)
  "Return the address of the Jack MIDI port named NAME."
  (let ((client (rt-client)))
    (if (cffi:null-pointer-p client)
        (cffi:null-pointer)
        (cffi:foreign-funcall "jack_port_by_name" :pointer client
                              :string name :pointer))))

(defun get-stream-by-name (name)
  "Return the Jack MIDI stream with port-name NAME."
  (find name *streams* :key #'stream-port-name :test #'string=))

(defmacro rt-update-data (data-vec-var streams new-streams input-p)
  ;; Use a copy of the pending data to avoid side effects in rt-thread.
  `(let ((,data-vec-var (new-foreign-data-vector ,input-p)))
     ;; Return after the update in rt-thread (waiting for the return value).
     (rt-eval (:return-value-p t)
       (jm-update-data ,data-vec-var ,input-p)
       (setf ,streams ,new-streams)
       (incudine:nrt-funcall
         (lambda () (free-foreign-data-vector ,data-vec-var))))))

(defmacro append-to-streams (obj input-p streams pending-streams type-filter
                             spinlock)
  (with-gensyms (inputs len vec data-vec)
    `(let ((,inputs (if (,type-filter ,obj)
                        (vector ,obj)
                        (delete-if-not #',type-filter
                                       (coerce ,obj 'simple-vector)))))
       (when (plusp (length ,inputs))
         (incudine.util:with-spinlock-held (,spinlock)
           (let ((,inputs (remove-if (lambda (s)
                                       (find s ,pending-streams))
                                     ,inputs)))
             (when (plusp (length ,inputs))
               (let* ((,len (length ,pending-streams))
                      (,vec (if (zerop ,len)
                                ,inputs
                                (concatenate 'simple-vector
                                             ,pending-streams ,inputs))))
                 (setf ,pending-streams ,vec)
                 (dotimes (i (length ,vec))
                   (jm-append-pending-data (stream-pointer (aref ,vec i))
                                           ,input-p))
                 (rt-update-data ,data-vec ,streams ,vec ,input-p)))))))))

(defun append-to-input-streams (obj)
  (declare (type (or input-stream sequence) obj))
  (append-to-streams obj t *input-streams* *pending-input-streams*
                     input-stream-p *input-streams-spinlock*))

(defun append-to-output-streams (obj)
  (declare (type (or output-stream sequence) obj))
  (append-to-streams obj nil *output-streams* *pending-output-streams*
                     output-stream-p *output-streams-spinlock*))

(defmacro delete-from-streams (obj input-p streams pending-streams spinlock)
  (with-gensyms (vec data-vec)
    `(incudine.util:with-spinlock-held (,spinlock)
       (if (jack-stopped-p)
           (setf ,streams (remove ,obj ,pending-streams)
                 ,pending-streams ,streams)
           (let ((,vec (remove ,obj ,pending-streams)))
             (setf ,pending-streams ,vec)
             (jm-delete-from-pending-data (stream-pointer ,obj) ,input-p)
             (rt-update-data ,data-vec ,streams ,vec ,input-p))))))

(defun delete-from-input-streams (stream)
  (declare (type input-stream stream))
  (delete-from-streams stream t *input-streams* *pending-input-streams*
                       *input-streams-spinlock*))

(defun delete-from-output-streams (stream)
  (declare (type output-stream stream))
  (delete-from-streams stream nil *output-streams* *pending-output-streams*
                       *output-streams-spinlock*))

(defun open (&key (direction :input) port-name)
  "Create and return a new JACKMIDI:STREAM.

DIRECTION is :INPUT (default) or :OUTPUT to return a JACKMIDI:INPUT-STREAM
or a JACKMIDI:OUTPUT-STREAM respectively.

PORT-NAME defaults to \"midi_in\" if DIRECTION is :INPUT or \"midi_out\"
if DIRECTION is :OUTPUT."
  (declare (type (member :input :output) direction)
           (type (or string null) port-name))
  (let* ((input-p (eq direction :input))
         (port-name (or port-name (default-port-name input-p))))
    (if (get-stream-by-name port-name)
        (incudine:incudine-error "Jack MIDI port name ~S is used" port-name)
        (let ((s (funcall (if input-p
                              #'make-input-stream
                              #'make-output-stream)
                          port-name)))
          (unless (jack-stopped-p)
            (setf (stream-port-pointer s)
                  (port-register input-p port-name)))
          (let ((l (list s)))
            (setf *streams* (if *streams* (nconc *streams* l) l))
            (unless (null-port-pointer-p s)
              (if input-p
                  (append-to-input-streams s)
                  (append-to-output-streams s)))
            s)))))

(defun close (obj)
  "Close a JACKMIDI:STREAM. OBJ is a JACKMIDI:STREAM or the
port-name of the stream to close."
  (declare (type (or stream string) obj))
  (let ((stream (if (stream-p obj)
                    obj
                    (get-stream-by-name obj))))
    (when stream
      (setf *streams* (delete stream *streams*))
      (cond ((input-stream-p stream)
             (incudine:recv-stop stream)
             (incudine:remove-all-responders stream)
             (delete-from-input-streams stream))
            (t
             (delete-from-output-streams stream)))
      (unless (cffi:null-pointer-p (stream-pointer stream))
        (unless (null-port-pointer-p stream)
          (port-unregister (stream-port-pointer stream))
          (setf (stream-port-pointer stream) (cffi:null-pointer)))
        (free-stream-pointer (stream-pointer stream) (input-stream-p stream))
        (setf (stream-pointer stream) (cffi:null-pointer))
        (when (and (input-stream-p obj)
                   (not (cffi:null-pointer-p (input-stream-sysex-pointer* obj))))
          (cffi:foreign-free (input-stream-sysex-pointer* obj))
          (setf (input-stream-sysex-pointer* obj) (cffi:null-pointer)))
        (incudine.util::cancel-finalization stream))
      (setf (stream-direction stream) :closed)
      (setf (stream-port-name stream) "")
      stream)))

(declaim (inline message))
(defun message (status &optional (data1 0) (data2 0))
  "Encode a short MIDI message into four bytes."
  (declare (type (unsigned-byte 8) status data1 data2))
  (the (unsigned-byte 32)
    #+little-endian
    (logior (ash data2 16) (ash data1 8) status)
    #-little-endian
    (logior (ash status 24) (ash data1 16) (ash data2 8))))

(declaim (inline decode-message))
(defun decode-message (msg)
  "Decode a MIDI message encoded into four bytes."
  (declare (type (unsigned-byte 32) msg))
  (incudine-optimize
    #+little-endian
    (let* ((msg (logand msg #xFFFFFF))
           (ash-8 (ldb (byte 16 8) msg)))
      (values (ldb (byte 8 0) msg)       ; status
              (ldb (byte 8 0) ash-8)     ; data1
              (ldb (byte 8 8) ash-8)))   ; data2
    #-little-endian
    (let* ((m (ash msg -8))
           (m2 (ash m -8)))
      (values (ldb (byte 8 8) m2)
              (ldb (byte 8 0) m2)
              (ldb (byte 8 0) m)))))

(declaim (inline sysex-message-p))
(defun sysex-message-p (msg)
  "Whether the MIDI message MSG is a SysEx."
  (= (if (typep msg 'data)
         (aref msg 0)
         #+little-endian (logand msg #xFF)
         #-little-endian (ldb (byte 8 24) msg))
     #xF0))

(declaim (inline sysex-eox-message-p))
(defun sysex-eox-message-p (msg)
  (or (= (logand msg #xFF) #xF7)
      (= (ldb (byte 8 8) msg) #xF7)
      (= (ldb (byte 8 16) msg) #xF7)
      (= (ldb (byte 8 24) msg) #xF7)))

(declaim (inline data))
(defun data (&rest octets)
  "Return a vector of OCTETS."
  (coerce octets 'data))

(declaim (inline write-short))
(defun write-short (stream message size)
  "Write a MIDI event with the MESSAGE of size SIZE encoded into four
bytes to the Jack MIDI output STREAM.

Example:

    (defvar *midiout* (jackmidi:open :direction :output))
    (rt-start)
    (at (now) #'jackmidi:write-short *midiout* (jackmidi:message 144 60 96) 3)"
  (declare (type output-stream stream) (type (unsigned-byte 32) message)
           (type positive-fixnum size))
  (rt-eval ()
    (foreign-write-short stream message size)
    (values)))

(defun write (stream data &key (start 0) end)
  "Write the octets DATA of a MIDI message into a Jack MIDI output STREAM.

START and END are the bounding index designators of DATA.

Example:

    (defvar *midiout* (jackmidi:open :direction :output))

    (defvar *msg0* (make-array 6 :element-type '(unsigned-byte 8)
                     :initial-contents '(#xf0 #x7e #x7f #x09 #x01 #xf7)))
    (rt-start)
    (at (now) #'jackmidi:write *midiout* *msg0*)

    (at (now) #'jackmidi:write *midiout*
      (jackmidi:data #xf0 #x7e #x7f #x09 #x01 #xf7))

    (defvar *msg1* (coerce '(144 60 96 128 60 0) 'jackmidi:data))

    (at (now) #'jackmidi:write *midiout* *msg1* :end 3)           ; note on
    (at (now) #'jackmidi:write *midiout* *msg1* :start 3 :end 6)  ; note off"
  (declare (type output-stream stream)
           (type data data)
           (type (or positive-fixnum null) end)
           (type non-negative-fixnum start))
  (let ((end (or end (length data))))
    (declare (type non-negative-fixnum end))
    (unless (or (>= start end) (> end (length data)))
      (rt-eval ()
        (cffi:with-pointer-to-vector-data (ptr data)
          (foreign-write stream (cffi:inc-pointer ptr start)
                         (- end start)))
        (values)))))

(defun read (stream octets)
  "Read the events received from a Jack MIDI input STREAM into a
vector of OCTETS. Return the number of the events read.

The header of the event is 12 bytes long: a timestamp (foreign double float)
and the length of the MIDI message (foreign uint32).

The MIDI messages are aligned to four bytes.

Example:

    (defvar *midiin* (jackmidi:open))
    (defvar *buf* (make-array 1024 :element-type '(unsigned-byte 8)))
    (rt-start)
    (prog1 (zerop (jackmidi:read *midiin* *buf*))
      (print *buf*))"
  (declare (type input-stream stream) (type data octets))
  (cffi:with-pointer-to-vector-data (ptr octets)
    (foreign-read stream ptr (length octets))))

(declaim (inline waiting-for))
(defun waiting-for (stream)
  (declare (type input-stream stream))
  (%waiting-for (stream-pointer stream)))

(declaim (inline flush-pending))
(defun flush-pending (stream)
  (declare (type input-stream stream))
  (%flush-pending (stream-pointer stream)))

(defun all-streams (&optional direction)
  "Return a new list with the opened Jack MIDI streams.

If DIRECTION is :INPUT or :OUTPUT, return the list of the opened input
or output streams, respectively."
  (macrolet ((get-streams (type-filter spinlock pending)
               `(if (jack-stopped-p)
                    (remove-if-not ,type-filter *streams*)
                    (incudine.util:with-spinlock-held (,spinlock)
                      (coerce ,pending 'list)))))
    (cond ((eq direction :input)
           (get-streams #'input-stream-p *input-streams-spinlock*
                        *pending-input-streams*))
          ((eq direction :output)
           (get-streams #'output-stream-p *output-streams-spinlock*
                        *pending-output-streams*))
          (t (copy-list *streams*)))))

(defun waiting-for-jack-stop (max-time)
  (do ((i 0 (1+ i)))
      ((jack-stopped-p) t)
    (if (>= i (* max-time 10))
        ;; Run Baby Run
        (return-from waiting-for-jack-stop)
        (sleep .1))))

(defun reset ()
  (cond ((waiting-for-jack-stop 5)
         (msg debug "JACKMIDI:RESET")
         (setf *input-streams* (vector))
         (setf *pending-input-streams* *input-streams*)
         (setf *output-streams* (vector))
         (setf *pending-output-streams* *output-streams*)
         (dolist (s *streams*)
           (unless (null-port-pointer-p s)
             (setf (stream-port-pointer s) (cffi:null-pointer))
             (nullify-port-buffer s))))
        (t
         (msg warn "JACKMIDI:RESET failed because Jack is running"))))

(defun update-streams ()
  (if (jack-stopped-p)
      (reset)
      (let ((inputs nil)
            (outputs nil))
        (dolist (s *streams*)
          (when (stream-p s)
            (let ((ptr (get-port-by-name
                         (format nil "~A:~A"
                                 (cffi:foreign-funcall "jack_get_client_name"
                                   :pointer (rt-client) :string)
                                 (stream-port-name s))))
                  (input-p (eq (stream-direction s) :input)))
              (setf (stream-port-pointer s)
                    (if (cffi:null-pointer-p ptr)
                        (port-register input-p (stream-port-name s))
                        ptr))
              (unless (null-port-pointer-p s)
                (if input-p
                    (push s inputs)
                    (push s outputs))))))
        (append-to-input-streams (nreverse inputs))
        (append-to-output-streams (nreverse outputs)))))

(cffi:defcstruct event
  (timestamp :double)
  (message-length :uint32)
  (message :uint32))

(defmacro event-slot (ptr slot-name)
  `(cffi:foreign-slot-value ,ptr '(:struct event) ',slot-name))

(defstruct (event-buffer (:constructor %make-event-buffer)
                         (:copier nil))
  "Jack MIDI event buffer type."
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (events 0 :type non-negative-fixnum))

(define-constant +sysex-max-size+ 1024)

(defun make-event-buffer (&optional (size +sysex-max-size+))
  "Create and return a new EVENT-BUFFER structure of size SIZE
(1024 by default)."
  (declare (type positive-fixnum size))
  (let* ((ptr (cffi:foreign-alloc :char :count size))
         (obj (%make-event-buffer :pointer ptr :size size)))
    (incudine-finalize obj (lambda () (cffi:foreign-free ptr)))))

(defmethod print-object ((obj event-buffer) stream)
  (format stream "#<JACKMIDI:EVENT-BUFFER :SIZE ~D>"
          (event-buffer-size obj)))

(defmethod free-p ((obj event-buffer))
  (cffi:null-pointer-p (event-buffer-pointer obj)))

(defmethod free ((obj event-buffer))
  (setf (event-buffer-size obj) 0)
  (unless (free-p obj)
    (cffi:foreign-free (event-buffer-pointer obj))
    (incudine-cancel-finalization obj)
    (setf (event-buffer-pointer obj) (cffi:null-pointer))
    (nrt-msg debug "Free ~A" (type-of obj)))
  (values))

(declaim (inline input-stream-sysex-event))
(defun input-stream-sysex-event (stream)
  (cffi:mem-ref (input-stream-sysex-pointer* stream) :pointer))

(declaim (inline set-input-stream-sysex-event))
(defun set-input-stream-sysex-event (stream ptr)
  (setf (cffi:mem-ref (input-stream-sysex-pointer* stream) :pointer) ptr))

(defsetf input-stream-sysex-event set-input-stream-sysex-event)

(defun input-stream-sysex-timestamp (stream)
  "Return the timestamp of the MIDI SysEx message stored in the buffer
of the MIDI Jack input STREAM."
  (let ((ptr (input-stream-sysex-event stream)))
    (if (cffi:null-pointer-p ptr)
        0d0
        (event-slot ptr timestamp))))

(declaim (inline event-sysex-size))
(defun event-sysex-size (ptr)
  (let ((size (event-slot ptr message-length)))
    (declare (type non-negative-fixnum size))
    (if (and (plusp size) (= (u8-ref ptr 12) #xf0))
        size
        0)))

(defun input-stream-sysex-size (stream)
  "Return the length of the MIDI SysEx message stored in the buffer of
the MIDI Jack input STREAM."
  (let ((ptr (input-stream-sysex-event stream)))
    (if (cffi:null-pointer-p ptr)
        0
        (event-sysex-size ptr))))

(defun input-stream-sysex-pointer (stream)
  "Return the foreign pointer to the MIDI SysEx message stored in the
buffer of the MIDI Jack input STREAM."
  (let ((ptr (input-stream-sysex-event stream)))
    (if (cffi:null-pointer-p ptr)
        (values ptr 0)
        (values (cffi:mem-aptr ptr :uint32 3)
                (event-sysex-size ptr)))))

(defun input-stream-sysex-octets (stream &optional octets (start 0))
  "Return the vector of octets stored in the buffer of the MIDI Jack
input STREAM and the MIDI SysEx message size.

Create a new vector if OCTETS is NIL (default).

START specifies an offset into OCTETS and marks the beginning position
of that vector."
  (declare (type input-stream stream) (type (or data null) octets)
           (type non-negative-fixnum start))
  (multiple-value-bind (ptr size) (input-stream-sysex-pointer stream)
    (declare (type cffi:foreign-pointer ptr) (type non-negative-fixnum size)
             #.incudine.util:*standard-optimize-settings*)
    (when (and (plusp size) (<= size +sysex-max-size+))
      (multiple-value-bind (buf start size)
          (if octets
              (values octets start (min size (- (length octets) start)))
              (values (make-array size :element-type '(unsigned-byte 8))
                      0 size))
        (declare (type non-negative-fixnum size start))
        (do ((i 0 (1+ i))
             (j start (1+ j)))
            ((>= i size) (values buf size))
          (declare (type non-negative-fixnum i j))
          (setf (aref buf j) (u8-ref ptr i)))))))

(defmacro doevent ((event-buffer message-var stream count-form
                    &optional timestamp-var result) &body body)
  "Iterate over the MIDI events of the EVENT-BUFFER structure related
to a Jack MIDI input STREAM with MESSAGE-VAR bound to each message.
Then RESULT form is evaluated.

COUNT-FORM is evaluated to get the number of events.

If TIMESTAMP-VAR is non-NIL, it is the variable bound to the timestamp
of each message."
  (with-gensyms (ptr remain len tmp i j n)
    (let ((offset (cffi:foreign-type-size '(:struct event))))
      `(let ((,n ,count-form))
         (declare (type non-negative-fixnum ,n))
         (do ((,i 0 (1+ ,i))
              (,ptr (event-buffer-pointer ,event-buffer)
                    (cffi:inc-pointer ,ptr ,offset))
              (,remain ,n (1- ,remain)))
             ((>= ,i ,n) ,result)
           (declare (type non-negative-fixnum ,i ,remain)
                    (type cffi:foreign-pointer ,ptr))
           (let ((,message-var (event-slot ,ptr message))
                 ,@(when timestamp-var
                     `((,timestamp-var (event-slot ,ptr timestamp)))))
             (declare (type unsigned-byte ,message-var
                            ,@(when timestamp-var `(,timestamp-var))))
             (when (sysex-message-p ,message-var)
               (setf (input-stream-sysex-event ,stream) ,ptr))
             ,@body
             (when (sysex-message-p ,message-var)
               ;; Jump to the end of the SysEx.
               (do ((,j 0 (1+ ,j))
                    (,len (event-slot ,ptr message-length))
                    (,tmp (cffi:inc-pointer ,ptr 4) (cffi:inc-pointer ,tmp 4)))
                   ((>= ,j ,len))
                 (declare (type non-negative-fixnum ,j ,len)
                          (type cffi:foreign-pointer ,tmp))
                 (when (sysex-eox-message-p (event-slot ,tmp message))
                   (setf ,ptr ,tmp)
                   (return))))))))))

(defmacro with-event-buffer ((var &optional (size #.+sysex-max-size+))
                             &body body)
  "Bind VAR to a newly allocated EVENT-BUFFER structure with dynamic
extent during BODY."
  `(let ((,var (make-event-buffer ,size)))
     (declare (type event-buffer ,var))
     (incudine::maybe-unwind-protect (progn ,@body) (free ,var))))

(defmacro with-receiver ((state-form stream message-var
                          &optional timestamp-var thread-name) &body body)
  "If the setfable STATE-FORM is T, start receiving from the Jack MIDI
input STREAM with MESSAGE-VAR bound to the received MIDI message.

If TIMESTAMP-VAR is non-NIL, it is the variable bound to the timestamp
of each message.

Optionally, the receiver thread is named THREAD-NAME.

See also INCUDINE:MAKE-RESPONDER and INCUDINE:RECV-START."
  (with-gensyms (evbuf)
    `(if ,state-form
         (warn "Jack MIDI receiver already started.")
         (case (stream-direction ,stream)
           (:closed (warn "The stream is closed."))
           (:output (warn "Cannot receive from an output stream."))
           (otherwise
            (bt:make-thread
              (lambda ()
                (with-event-buffer (,evbuf)
                  (setf ,state-form t)
                  (unwind-protect
                       (loop initially (flush-pending ,stream)
                             while ,state-form do
                               (doevent (,evbuf ,message-var ,stream
                                          (foreign-read ,stream
                                            (event-buffer-pointer ,evbuf)
                                            (event-buffer-size ,evbuf))
                                          ,timestamp-var)
                                 ,@body)
                               (when ,state-form
                                 (waiting-for ,stream)))
                    (setf ,state-form nil))))
              :name ,(or thread-name
                         `(format nil "jackmidi-recv ~A"
                                  (stream-port-name ,stream)))))))))

(in-package :incudine)

(defmethod valid-input-stream-p ((obj jackmidi:input-stream)) t)

(defmethod valid-input-stream-p ((obj jackmidi:output-stream)) nil)

(defun start-jackmidi-recv (receiver update-midi-table-p)
  (declare #.*standard-optimize-settings*
           (type receiver receiver) (type boolean update-midi-table-p))
  (if (receiver-status receiver)
      (msg warn "Jack MIDI receiver already started.")
      (let ((stream (receiver-stream receiver)))
        (msg debug "Jack MIDI receiver for ~S" (jackmidi:port-name stream))
        (jackmidi:with-receiver ((receiver-status receiver) stream msg nil)
          (handler-case
              (multiple-value-bind (status data1 data2)
                  (jackmidi:decode-message msg)
                (when update-midi-table-p
                  (incudine.vug::set-midi-message status data1 data2))
                (midi-recv-funcall-all receiver status data1 data2 stream))
            (condition (c) (nrt-msg error "~A" c)))))))

(defun start-jackmidi-recv-update-mtab (receiver)
  (start-jackmidi-recv receiver t))

(defun start-jackmidi-recv-no-mtab (receiver)
  (start-jackmidi-recv receiver nil))

(defmethod responder-wrapper ((stream jackmidi:input-stream)
                              (function function))
  (midi-responder-wrapper function))

(defmethod recv-start ((stream jackmidi:input-stream)
                       &key (priority *receiver-default-priority*)
                       (update-midi-table-p t))
  (unless (eq (recv-status stream) :running)
    (add-receiver stream (or (receiver stream) (make-receiver stream))
                  (if update-midi-table-p
                      #'start-jackmidi-recv-update-mtab
                      #'start-jackmidi-recv-no-mtab)
                  priority)))

(defmethod recv-stop ((stream jackmidi:input-stream))
  (let ((recv (receiver stream)))
    (when (and recv (receiver-status recv))
      (compare-and-swap (receiver-status recv) t nil)
      (sleep .1)
      (jackmidi::force-cond-signal (jackmidi::stream-pointer stream))
      (recv-unset-thread recv)
      (msg debug "Jack MIDI receiver for ~S stopped"
           (jackmidi:port-name stream))
      recv)))
