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

(defpackage :jackmidi
  (:use :cl)
  (:shadow #:open #:close #:write #:stream #:input-stream-p #:output-stream-p)
  (:import-from #:alexandria #:define-constant #:with-gensyms)
  (:import-from #:incudine.external #:rt-client)
  (:import-from #:incudine.util #:rt-eval #:msg)
  (:export #:data #:open #:close #:stream #:input-stream-p #:output-stream-p
           #:port-name #:get-stream-by-name #:all-streams
           #:message #:write-short #:write))

(in-package :jackmidi)

(deftype data () '(simple-array (unsigned-byte 8) (*)))

(defstruct (stream (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (direction :closed :type (member :input :output :closed))
  (port-name "" :type string))

(defstruct (input-stream (:include stream) (:copier nil))
  (condition (incudine::make-sync-condition "jackmidi")))

(defstruct (output-stream (:include stream) (:copier nil)))

(defmethod print-object ((obj stream) stream)
  (let ((port-name (stream-port-name obj)))
    (format stream "#<JACKMIDI:~A-STREAM~:[ ~S~;~]>"
            (stream-direction obj) (zerop (length port-name))
            port-name)))

(defvar *streams* nil
  "Ordered list of opened Jack MIDI streams.")
(declaim (type list *streams*))

(defvar *input-streams* (vector)
  "Ordered vector of opened Jack MIDI input streams.")
(declaim (type simple-vector *input-streams*))

(defvar *pending-input-streams* (vector)
  "Temporary Jack MIDI input streams used in non-realtime.")
(declaim (type simple-vector *pending-input-streams*))

(defvar *input-streams-spinlock*
  (incudine.util:make-spinlock "jackmidi pending-input-streams")
  "Spinlock to update *PENDING-INPUT-STREAMS* in nrt-thread.")
(declaim (type incudine.util:spinlock *input-streams-spinlock*))

(defvar *output-streams* (vector)
  "Ordered vector of opened Jack MIDI output streams.")
(declaim (type simple-vector *output-streams*))

(defvar *pending-output-streams* (vector)
  "Temporary Jack MIDI output streams used in non-realtime.")
(declaim (type simple-vector *pending-output-streams*))

(defvar *output-streams-spinlock*
  (incudine.util:make-spinlock "jackmidi pending-output-streams")
  "Spinlock to update *PENDING-OUTPUT-STREAMS* in nrt-thread.")
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
  (stream-port-name stream))

(defun set-port-name (stream name)
  (when (jack-stopped-p)
    (if (get-stream-by-name name)
        (error "Jack MIDI port name ~S is used" name)
        (setf (stream-port-name stream) name))))

(defsetf port-name set-port-name)

(defun default-port-name (input-p)
  (if input-p "midi_in" "midi_out"))

(declaim (inline process))
(cffi:defcfun ("jm_process" process) :void
  (frames :unsigned-int))

(cffi:defcfun ("jm_alloc_data" new-stream-pointer) :pointer)

(cffi:defcfun ("jm_free_data" free-stream-pointer) :void
  (ptr :pointer))

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

(declaim (inline %jm-update-data))
(cffi:defcfun ("jm_update_data" %jm-update-data) :void
  (data-vec :pointer)
  (input-p :boolean))

(declaim (inline jm-update-data))
(defun jm-update-data (obj input-p)
  (%jm-update-data (foreign-data-vector-pointer obj) input-p))

(declaim (inline %write-short))
(cffi:defcfun ("jm_write_short" %write-short) :int
  (stream-pointer :pointer)
  (msg :uint32)
  (size :unsigned-int))

(declaim (inline %write))
(cffi:defcfun ("jm_write" %write) :int
  (stream-pointer :pointer)
  (msg :pointer)
  (size :unsigned-int))

(defstruct (foreign-data-vector (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (stream-direction :input :type (member :input :output)))

(defun new-foreign-data-vector (input-p)
  (let ((ptr (jm-copy-data-vec input-p)))
    (when (cffi:null-pointer-p ptr)
      (error "Jack MIDI pointer allocation"))
    (let ((obj (make-foreign-data-vector :pointer ptr
                 :stream-direction (if input-p :input :output))))
      (tg:finalize obj (lambda () (jm-free-data-vec ptr)))
      obj)))

(defun free-foreign-data-vector (obj)
  (declare (type foreign-data-vector obj))
  (unless (cffi:null-pointer-p (foreign-data-vector-pointer obj))
    (jm-free-data-vec (foreign-data-vector-pointer obj))
    (tg:cancel-finalization obj)
    (setf (foreign-data-vector-pointer obj) (cffi:null-pointer)))
  (values))

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
     (rt-eval ()
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
  "Create and return a new JACKMIDI:STREAM."
  (declare (type (member :input :output) direction)
           (type (or string null) port-name))
  (let* ((input-p (eq direction :input))
         (port-name (or port-name (default-port-name input-p))))
    (if (get-stream-by-name port-name)
        (error "Jack MIDI port name ~S is used" port-name)
        (let ((ptr (new-stream-pointer)))
          (when (cffi:null-pointer-p ptr)
            (error "Jack MIDI pointer allocation"))
          (let ((s (funcall (if input-p
                                #'make-input-stream
                                #'make-output-stream)
                            :pointer ptr
                            :direction direction
                            :port-name port-name)))
            (tg:finalize s (lambda () (free-stream-pointer ptr)))
            (unless (jack-stopped-p)
              (setf (stream-port-pointer s)
                    (port-register input-p port-name)))
            (let ((l (list s)))
              (setf *streams* (if *streams* (nconc *streams* l) l))
              (unless (null-port-pointer-p s)
                (if input-p
                    (append-to-input-streams s)
                    (append-to-output-streams s)))
              s))))))

(defun close (obj)
  "Close a JACKMIDI:STREAM. OBJ is a JACKMIDI:STREAM or the
port-name of the stream to close."
  (declare (type (or stream string) obj))
  (let ((stream (if (stream-p obj)
                    obj
                    (get-stream-by-name obj))))
    (when stream
      (setf *streams* (delete stream *streams*))
      (if (input-stream-p stream)
          (delete-from-input-streams stream)
          (delete-from-output-streams stream))
      (unless (cffi:null-pointer-p (stream-pointer stream))
        (unless (null-port-pointer-p stream)
          (port-unregister (stream-port-pointer stream))
          (setf (stream-port-pointer stream) (cffi:null-pointer)))
        (free-stream-pointer (stream-pointer stream))
        (setf (stream-pointer stream) (cffi:null-pointer))
        (tg:cancel-finalization stream))
      (setf (stream-direction stream) :closed)
      (setf (stream-port-name stream) "")
      stream)))

(declaim (inline message))
(defun message (status data1 data2)
  "Encode a short MIDI message into a (UNSIGNED-BYTE 32)."
  (declare (type (unsigned-byte 8) status data1 data2))
  (the (unsigned-byte 32)
    #+little-endian
    (logior (ash data2 16) (ash data1 8) status)
    #-little-endian
    (logior (ash status 24) (ash data1 16) (ash data2 8))))

(declaim (inline data))
(defun data (&rest values)
  (coerce values 'data))

;;; WRITE-SHORT
;;;
;;; Example:
;;;
;;;     (defvar *midiout* (jackmidi:open :direction :output))
;;;     (jackmidi:write-short *midiout* (jackmidi:message 144 60 96) 3)
;;;
(declaim (inline write-short))
(defun write-short (stream message size)
  "Write SIZE bytes of a MIDI message encoded into four bytes."
  (declare (type output-stream stream) (type (unsigned-byte 32) message)
           (type fixnum size))
  (rt-eval () (%write-short (stream-pointer stream) message size)))

;;; WRITE
;;;
;;; Examples:
;;;
;;;     (defvar *midiout* (jackmidi:open :direction :output))
;;;
;;;     (defvar *msg0* (make-array 6 :element-type '(unsigned-byte 8)
;;;                      :initial-contents '(#xf0 #x7e #x7f #x09 #x01 #xf7)))
;;;
;;;     (jackmidi:write *midiout* *msg0*)
;;;
;;;     (jackmidi:write *midiout*
;;;       (jackmidi:data #xf0 #x7e #x7f #x09 #x01 #xf7))
;;;
;;;     (defvar *msg1* (coerce '(144 60 96 128 60 0) 'jackmidi:data))
;;;
;;;     (jackmidi:write *midiout* *msg1* :end 3)           ; note on
;;;     (jackmidi:write *midiout* *msg1* :start 3 :end 6)  ; note off
;;;
(defun write (stream data &key (start 0) end)
  "Write a MIDI message stored into the octets DATA."
  (declare (type output-stream stream)
           (type data data)
           (type (or alexandria:positive-fixnum null) end)
           (type alexandria:non-negative-fixnum start))
  (let ((m (subseq data start end)))
    (rt-eval () (sb-sys:with-pinned-objects (data)
                  (cffi:with-pointer-to-vector-data (ptr m)
                    (%write (stream-pointer stream) ptr
                            (length m)))))))

(defun all-streams (&optional direction)
  "Return a new list with the opened Jack MIDI streams."
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
